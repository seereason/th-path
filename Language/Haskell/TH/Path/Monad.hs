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
    ( R(..), startTypes, lensHintMap, typeInfo, edges

    , makeTypeGraph

    , FoldPathControl(..)
    , foldPath
    , allLensKeys
    , allPathKeys
    , makePathLenses
    , pathHints
    , reachableFrom
    , isReachable
    , goalReachable
    ) where

import Control.Applicative ((<$>))
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (filterM)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Default (Default)
import Data.Graph (Graph, reachable, transposeG, Vertex)
import Data.List as List (filter, intercalate, map)
import Data.Map as Map (findWithDefault, fromListWith, keys, lookup, Map, map, mapWithKey)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set as Set (difference, empty, filter, fromList, map, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContextState, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.TypeGraph.Core (pprint')
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (dissolveM, GraphEdges, graphFromMap, isolate)
import Language.Haskell.TH.TypeGraph.Hints (VertexHint(..), HasVertexHints)
import Language.Haskell.TH.TypeGraph.Info (infoMap, TypeGraphInfo, typeGraphInfo)
import Language.Haskell.TH.TypeGraph.Monad (typeGraphEdges, vertex)
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
      -- ^ Start types
      , _lensHintMap :: Map TypeGraphVertex [LensHint] -- FIXME: [LensHint] -> Set LensHint
      , _typeInfo :: TypeGraphInfo LensHint
      , _edges :: GraphEdges LensHint TypeGraphVertex
      , _graph :: (Graph, Vertex -> (LensHint, TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex)
      }

$(makeLenses ''R)

-- | Find all the hints that match either the exact key vertex, or the
-- key _field set to Nothing.
pathHints :: (DsMonad m, MonadReader R m) => TypeGraphVertex -> m [(TypeGraphVertex, LensHint)]
pathHints key = do
    mp <- view lensHintMap
    let key' = set field Nothing key
    let s1 = Set.fromList $ Map.findWithDefault [] key mp
        s2 = Set.fromList $ Map.findWithDefault [] key' mp
    return $ if null (Set.difference s1 s2)
             then List.map (key',) (toList s2)
             else List.map (key,) (toList s1)

-- | A lens key is a pair of vertexes corresponding to a Path instance.
allLensKeys :: (DsMonad m, MonadReader R m) => m (Set (TypeGraphVertex, TypeGraphVertex))
allLensKeys = do
  pathKeys <- allPathKeys
  Set.fromList <$> filterM (uncurry goalReachable) [ (g, k) | g <- toList pathKeys, k <- toList pathKeys ]

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
  hints <- pathHints key
  case List.filter noLensHint (List.map snd hints) of
    [] -> mapM make (toList (typeNames key)) >>= tell
    _ -> return ()
    where
      make tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))
      -- The presence of most hints means we don't need a lens
      -- noLensHint Self = True
      noLensHint (VertexHint Sink) = True
      noLensHint (Substitute _ _) = True
      noLensHint _ = False

reachableFrom :: forall m. (DsMonad m, MonadReader R m) => TypeGraphVertex -> m (Set TypeGraphVertex)
reachableFrom v = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view graph
  case kf v of
    Nothing -> return Set.empty
    Just v' -> return $ Set.map (\(_, key, _) -> key) . Set.map vf $ Set.fromList $ reachable (transposeG g) v'

isReachable :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
isReachable gkey key0 = do
  es <- view edges
  let (g, _vf, kf) = graphFromMap es
  case kf key0 of
    Nothing -> error ("isReachable - unknown key: " ++ pprint' key0)
    Just key -> do
      let gvert = fromMaybe (error $ "Unknown goal type: " ++ pprint' gkey ++ "\n" ++ intercalate "\n  " ("known:" : List.map pprint' (Map.keys es))) (kf gkey)
      -- Can we reach any node whose type matches (ConT gname)?  Fields don't matter.
      return $ elem gvert (reachable g key)

-- | Can we reach the goal type from the start type in this key?
-- | Can we reach the goal type from the start type in this key?
goalReachable :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
goalReachable gkey key0 = isReachable gkey key0

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

foldPath :: (DsMonad m, MonadReader R m) => FoldPathControl m r -> TypeGraphVertex -> [(TypeGraphVertex, LensHint)] -> m r
foldPath (FoldPathControl{..}) v hints = do
  let substs = [x | x@(_, Substitute _ _) <- hints]
  selfPath <- (not . null) <$> evalContextState (reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ])
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | elem (VertexHint Sink) (List.map snd hints) -> simplef
      | not (null substs) -> let ((_, Substitute exp typ) : _) = substs in substf exp typ
    ConT tname -> namedf tname
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf

makeTypeGraph :: DsMonad m => Q [Type] -> [(Maybe Field, Name, Q LensHint)] -> m R
makeTypeGraph st hs = do
  st' <- runQ st
  hl <- runQ $ makeHintList hs
  ti <- typeGraphInfo hl st'
  es <- runReaderT (makeTypeGraphEdges st') ti
  hm <- runReaderT (makeHintMap hl) ti
  return $ R { _startTypes = st'
             , _lensHintMap = hm
             , _typeInfo = ti
             , _edges = es
             , _graph = graphFromMap es
             }

makeHintList :: [(Maybe Field, Name, Q hint)] -> Q [(Maybe Field, Name, hint)]
makeHintList hs = do
  mapM (\(fld, tname, hintq) -> do
          hint <- hintq
          return (fld, tname, hint)) hs

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, HasVertexHints hint, MonadReader (TypeGraphInfo hint) m) =>
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
  edges' <- typeGraphEdges >>= dissolveM victim >>= return . removePathsToOrderKeys . removeUnnamedFieldEdges
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

makeHintMap :: (Functor m, DsMonad m, MonadReader (TypeGraphInfo hint) m) => [(Maybe Field, Name, hint)] -> m (Map TypeGraphVertex [hint])
makeHintMap hs = Map.fromListWith (++) <$> mapM (\(fld, tname, hint) -> expandType (ConT tname) >>= vertex fld >>= \v -> return (v, [hint])) hs
