-- | The reader monad for the type graph info, and some monadic functions.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Monad
    ( R(..), startTypes, typeInfo, edges

    , makeTypeGraph

    , FoldPathControl(..)
    , foldPath
    , allLensKeys
    , allPathKeys
    , makePathLenses
    , reachableFrom
    , goalReachableFull
    , goalReachableSimple
    ) where

import Control.Applicative ((<$>))
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (filterM)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Default (Default)
import Data.Graph (Graph, reachable, transposeG, Vertex)
import Data.List as List (intercalate, map)
import Data.Map as Map (keys, lookup, Map, map, mapWithKey)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set as Set (difference, empty, filter, fromList, map, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContextState, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.Prune (pruneTypeGraph, SinkType)
import Language.Haskell.TH.Path.View (View(viewLens), viewInstanceType)
import Language.Haskell.TH.TypeGraph.Core (pprint')
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (dissolveM, GraphEdges, graphFromMap, isolate)
import Language.Haskell.TH.TypeGraph.Info (infoMap, TypeGraphInfo, typeGraphInfo)
import Language.Haskell.TH.TypeGraph.Monad (simpleEdges, simpleVertex, typeGraphEdges, vertex)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype, field, typeNames)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

import Data.Foldable
#if ! MIN_VERSION_base(4,8,0)
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

data R
    = R
      { _startTypes :: [Type]
      , _typeInfo :: TypeGraphInfo
      , _edges :: GraphEdges () TypeGraphVertex
      , _graph :: (Graph, Vertex -> ((), TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex)
      , _gsimple :: (Graph, Vertex -> ((), TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex)
      }

$(makeLenses ''R)

-- | A lens key is a pair of vertexes corresponding to a Path instance.
allLensKeys :: (DsMonad m, MonadReader R m) => m (Set (TypeGraphVertex, TypeGraphVertex))
allLensKeys = do
  pathKeys <- allPathKeys
  Set.fromList <$> filterM (uncurry goalReachableSimple) [ (g, k) | g <- toList (Set.map simpleVertex pathKeys), k <- toList (Set.map simpleVertex pathKeys) ]

allPathKeys :: forall m. (DsMonad m, MonadReader R m) => m (Set TypeGraphVertex)
allPathKeys = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view graph
  kernel <- view startTypes >>= \st -> view typeInfo >>= runReaderT (mapM expandType st >>= mapM (vertex Nothing))
  let kernel' = mapMaybe kf kernel
  let keep = Set.fromList $ concatMap (reachable g) kernel'
      keep' = Set.map (\(_, key, _) -> key) . Set.map vf $ keep
  return keep'

makePathLenses :: (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> m ()
makePathLenses key = do
  simplePath <- (not . null) <$> evalContextState (reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ])
  case simplePath of
    False -> mapM make (toList (typeNames key)) >>= tell
    _ -> return ()
    where
      make tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))

reachableFrom :: forall m. (DsMonad m, MonadReader R m) => TypeGraphVertex -> m (Set TypeGraphVertex)
reachableFrom v = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view graph
  case kf v of
    Nothing -> return Set.empty
    Just v' -> return $ Set.map (\(_, key, _) -> key) . Set.map vf $ Set.fromList $ reachable (transposeG g) v'

isReachable :: (Functor m, DsMonad m, MonadReader R m) =>
               TypeGraphVertex -> TypeGraphVertex -> (Graph, Vertex -> ((), TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex) -> m Bool
isReachable gkey key0 (g, _vf, kf) = do
  es <- view edges
  case kf key0 of
    Nothing -> error ("isReachable - unknown key: " ++ pprint' key0)
    Just key -> do
      let gvert = fromMaybe (error $ "Unknown goal type: " ++ pprint' gkey ++ "\n" ++ intercalate "\n  " ("known:" : List.map pprint' (Map.keys es))) (kf gkey)
      -- Can we reach any node whose type matches (ConT gname)?  Fields don't matter.
      return $ elem gvert (reachable g key)

-- | Can we reach the goal type from the start type in this key?
goalReachableFull :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
goalReachableFull gkey key0 = view graph >>= isReachable gkey key0

goalReachableSimple :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
goalReachableSimple gkey key0 = view gsimple >>= isReachable (simpleVertex gkey) (simpleVertex key0)

data FoldPathControl m r
    = FoldPathControl
      { simplef :: m r
      , pathyf :: m r
      , substf :: Exp -> Type -> m r
      , namedf :: Name -> m r
      , maybef :: Type -> m r
      , listf :: Type -> m r
      , orderf :: Type -> Type -> m r
      , mapf :: Type -> Type -> m r
      , pairf :: Type -> Type -> m r
      , eitherf :: Type -> Type -> m r
      , otherf :: m r
      }

foldPath :: (DsMonad m, MonadReader R m) => FoldPathControl m r -> TypeGraphVertex -> m r
foldPath (FoldPathControl{..}) v = do
  selfPath <- (not . null) <$> evalContextState (reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ])
  simplePath <- (not . null) <$> evalContextState (reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ])
  viewType <- evalContextState (viewInstanceType (let (E typ) = view etype v in typ))
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | simplePath -> simplef
    typ
      | isJust viewType -> do
          let b = fromJust viewType
          exp <- runQ [|viewLens :: Lens' $(return typ) $(return b)|]
          substf exp b
    ConT tname -> namedf tname
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf

makeTypeGraph :: DsMonad m => Q [Type] -> m R
makeTypeGraph st = do
  st' <- runQ st
  ti <- typeGraphInfo st'
  es <- runReaderT (makeTypeGraphEdges st') ti
  return $ R { _startTypes = st'
             , _typeInfo = ti
             , _edges = es
             , _graph = graphFromMap es
             , _gsimple = graphFromMap (simpleEdges es)
             }

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, MonadReader TypeGraphInfo m) =>
                      [Type] -> m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges st = do
  im <- view infoMap
  -- Dissolve the vertices for types whose arity is not zero.  Each of
  -- their in-edges become connected to each of their out-edges.
  let victim :: TypeGraphVertex -> m Bool
      victim v = do
        let (E etyp) = view etype v
        k <- runQ $ inferKind etyp
        fv <- runQ $ freeTypeVars etyp
        let prim = case etyp of
                     ConT tname -> maybe False (\ x -> case x of PrimTyConI _ _ _ -> True; _ -> False) (Map.lookup tname im)
                     _ -> False
        return $ k /= Right StarT || fv /= Set.empty || prim
  edges' <- typeGraphEdges >>= pruneTypeGraph >>= dissolveM victim >>= return . removePathsToOrderKeys . removeUnnamedFieldEdges
  let (g, vf, kf) = graphFromMap edges'
  -- Isolate all nodes that are not reachable from the start types.
  kernel <- mapM expandType st >>= mapM (vertex Nothing) >>= return . mapMaybe kf
  let keep :: Set TypeGraphVertex
      keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
      victims = difference (Set.fromList (Map.keys edges')) keep
      edges'' :: GraphEdges hint TypeGraphVertex
      edges'' = isolate victims edges'
  -- trace (pprint edges'') (return ())
  return edges''
    where
      removeUnnamedFieldEdges :: (GraphEdges hint TypeGraphVertex) -> (GraphEdges hint TypeGraphVertex)
      removeUnnamedFieldEdges =
          -- If the _field field of the goal key is a Left it is a
          -- positional field rather than named - we don't make lenses
          -- for such fields so we don't want their edges in our graph.
          Map.map (\(hint, gkeys) -> (hint, Set.filter (\gkey -> maybe True (\(_, _, fld) -> either (const False) (const True) fld) (view field gkey)) gkeys))
      removePathsToOrderKeys :: (GraphEdges hint TypeGraphVertex) -> (GraphEdges hint TypeGraphVertex)
      removePathsToOrderKeys =
          -- The graph will initially include edges from (Map a b) to a, but we
          -- only create lenses to the b values of a map, so remove the edge to a.
          Map.mapWithKey (\key (hint, gkeys) -> (hint, Set.filter (\gkey -> case view etype key of
                                                                              E (AppT (AppT (ConT mtyp) ityp) _etyp) | elem mtyp [''Map, ''Order] -> E ityp /= view etype gkey
                                                                              _ -> True) gkeys))
