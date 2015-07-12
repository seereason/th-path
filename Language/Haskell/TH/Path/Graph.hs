-- | The reader monad for the type graph info, and some monadic functions.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Language.Haskell.TH.Path.Graph
    ( makePathLenses
    , makeTypeGraphEdges
    , FoldPathControl(..)
    , foldPath
    , SinkType
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#else
import Control.Applicative
#endif
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (execWriterT, MonadWriter, tell, WriterT)
import Data.Default (Default(def))
import Data.Foldable as Foldable (toList)
import Data.Foldable.Compat
import Data.Graph hiding (edges)
import Data.Map as Map (alter, keys, lookup, Map, map, mapWithKey)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set as Set (difference, empty, filter, fromList, map, Set, singleton, minView)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Core (fieldLensName, SelfPath)
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.View (View(viewLens), viewInstanceType)
import Language.Haskell.TH.TypeGraph.Edges (cut, cutM, dissolveM, GraphEdges, isolate, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (graphFromMap, TypeGraph(..), typeInfo)
import Language.Haskell.TH.TypeGraph.Info (startTypes, synonyms, TypeInfo, vertex)
import Language.Haskell.TH.TypeGraph.Prelude (listen_, pass_)
import Language.Haskell.TH.TypeGraph.Shape (unlifted)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex, etype, field, typeNames)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, MonadReader TypeInfo m) =>
                      m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges = do
  -- im <- view infoMap
  -- Dissolve the vertices for types whose arity is not zero.  Each of
  -- their in-edges become connected to each of their out-edges.
  edges' <- typeGraphEdges >>= pruneTypeGraph >>= dissolveM victim >>= return . removePathsToOrderKeys . removeUnnamedFieldEdges
  let (g, vf, kf) = graphFromMap edges'
  -- Isolate all nodes that are not reachable from the start types.
  kernel <- view startTypes >>= mapM expandType >>= mapM (vertex Nothing) >>= return . mapMaybe kf
  let -- Remove all edges involving unreachable vertices, but not the
      -- vertices themselves - they might still be queried.
      keep :: Set TypeGraphVertex
      keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
      victims = difference (Set.fromList (Map.keys edges')) keep
      edges'' :: GraphEdges hint TypeGraphVertex
      edges'' = isolate victims edges'
  -- trace (pprint edges'') (return ())
  return edges''
    where
      victim :: TypeGraphVertex -> m Bool
      victim v = do
        let (E etyp) = view etype v
        k <- runQ $ inferKind etyp
        fv <- runQ $ freeTypeVars etyp
        prim' <- unlifted etyp
        return $ k /= Right StarT || fv /= Set.empty || prim'
      removeUnnamedFieldEdges :: (GraphEdges hint TypeGraphVertex) -> (GraphEdges hint TypeGraphVertex)
      removeUnnamedFieldEdges =
          -- If the _field field of the goal key is a Left it is a
          -- positional field rather than named - we don't make lenses
          -- for such fields so we don't want their edges in our graph.
          Map.map (\(hint, gkeys) -> (hint, Set.filter (\gkey -> maybe True (\(_, _, fld) -> either (const False) (const True) fld) (view field gkey)) gkeys))
      removePathsToOrderKeys :: (GraphEdges hint TypeGraphVertex) -> (GraphEdges hint TypeGraphVertex)
      removePathsToOrderKeys =
          -- The graph will initially include edges from (Map a b) to
          -- a, and (Order a b) to a, but we only create lenses to the
          -- b values of a map, so remove the edge to a.
          Map.mapWithKey (\key (hint, gkeys) -> (hint, Set.filter (\gkey -> case view etype key of
                                                                              E (AppT (AppT (ConT mtyp) ityp) _etyp) | elem mtyp [''Map, ''Order] -> E ityp /= view etype gkey
                                                                              _ -> True) gkeys))

makePathLenses :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [[Dec]] m) => TypeGraphVertex -> m ()
makePathLenses key = do
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ])
  case simplePath of
    False -> mapM make (Foldable.toList (typeNames key)) >>= tell
    _ -> return ()
    where
      make tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))

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

foldPath :: (DsMonad m, MonadReader TypeGraph m) => FoldPathControl m r -> TypeGraphVertex -> m r
foldPath (FoldPathControl{..}) v = do
  selfPath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ])
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ])
  viewType <- evalContext (viewInstanceType (let (E typ) = view etype v in typ))
  syns <- Map.lookup (view etype v) <$> view (typeInfo . synonyms)
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | simplePath -> simplef
    typ
      | isJust viewType -> do
          let b = fromJust viewType
          expr <- runQ [|viewLens :: Lens' $(return typ) $(return b)|]
          substf expr b
    ConT tname -> namedf tname
    _ | maybe False (not . null) syns -> namedf (fst (fromJust (Set.minView (fromJust syns)))) -- yes, I'm sure
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf

-- | 'Path' instances can be customized by declaring types to be
-- instances of this class and the ones that follow.  If a type is an
-- instance of 'SinkType', no paths that lead to the internal stucture
-- of the value will be created - the value is considered atomic.
class SinkType a

-- | Like SinkType, but no paths out or into the type will be created.
class HideType a

-- | Remove any vertices that are labelled with primitive types, and then
-- apply the hints obtained from the
-- a new graph which incorporates the information from the hints.
pruneTypeGraph :: forall m label. (DsMonad m, Default label, Eq label, MonadReader TypeInfo m) =>
                  (GraphEdges label TypeGraphVertex) -> m (GraphEdges label TypeGraphVertex)
pruneTypeGraph edges = do
  cutPrimitiveTypes edges >>= dissolveHigherOrder >>= \g ->
      execWriterT (tell g >>
                   listen_ >>= mapM_ doSink . Map.keys >>
                   listen_ >>= mapM_ doHide . Map.keys >>
                   listen_ >>= mapM_ doView . Map.keys)
    where
      doSink :: TypeGraphVertex -> WriterT (GraphEdges label TypeGraphVertex) m ()
      doSink v = do
        let (E typ) = view etype v
        sinkHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
        when sinkHint (pass_ (return (Map.alter (alterFn (const Set.empty)) v)))

      doHide :: TypeGraphVertex -> WriterT (GraphEdges label TypeGraphVertex) m ()
      doHide v = do
        let (E typ) = view etype v
        hideHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
        when hideHint (pass_ (return (cut (singleton v))))

      doView :: TypeGraphVertex -> WriterT (GraphEdges label TypeGraphVertex) m ()
      doView v = let (E a) = view etype v in evalContext (viewInstanceType a) >>= maybe (return ()) (doDivert v)

      -- Replace all of v's out edges with a single edge to typ'
      doDivert v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        pass_ (return (Map.alter (alterFn (const (singleton v'))) v))
#if 0
      -- Add an extra out edge from v to typ' (unused)
      doExtra v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        pass_ (return (Map.alter (alterFn (Set.insert v')) v))
#endif

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: Default label => (Set TypeGraphVertex -> Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex)
alterFn setf (Just (label, s)) = Just (label, setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (def, setf Set.empty)

-- | Primitive (unlifted) types can not be used as parameters to a
-- type class, which makes them unusable in this system.
cutPrimitiveTypes :: (DsMonad m, Default label, Eq label, MonadReader TypeInfo m) => GraphEdges label TypeGraphVertex -> m (GraphEdges label TypeGraphVertex)
cutPrimitiveTypes edges = cutM (\v -> let (E typ) = view etype v in unlifted typ) edges

dissolveHigherOrder :: (DsMonad m, Default label, Eq label, MonadReader TypeInfo m) => GraphEdges label TypeGraphVertex -> m (GraphEdges label TypeGraphVertex)
dissolveHigherOrder edges = dissolveM (\v -> do let (E typ) = view etype v
                                                kind <- runQ $ inferKind typ
                                                return $ kind /= Right StarT) edges
