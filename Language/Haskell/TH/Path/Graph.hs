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
import Control.Monad.State (execStateT, get, modify, StateT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Default (Default(def))
import Data.Foldable as Foldable (toList)
import Data.Foldable.Compat
import Data.Graph hiding (edges)
import Data.Map as Map (alter, keys, lookup, Map)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set as Set (difference, empty, fromList, map, Set, singleton, minView)
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
import Language.Haskell.TH.TypeGraph.Edges (cut, cutEdges, cutM, dissolveM, GraphEdges, isolate, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (graphFromMap, TypeGraph(..), typeInfo)
import Language.Haskell.TH.TypeGraph.Info (startTypes, synonyms, TypeInfo, vertex)
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
makeTypeGraphEdges =
  typeGraphEdges >>=
  cutM isUnlifted >>=
  dissolveM higherOrder >>=
  pruneTypeGraph >>=
  dissolveM hasFreeVars >>=
  dissolveM isUnlifted >>= -- looks redundant
  return . cutEdges isMapOrOrder . cutEdges isAnonymous >>=
  isolateUnreachable >>= t1
    where
      higherOrder :: TypeGraphVertex -> m Bool
      higherOrder v = (/= Right StarT) <$> runQ (inferKind (runExpanded (view etype v)))
      hasFreeVars :: TypeGraphVertex -> m Bool
      hasFreeVars v = (/= Set.empty) <$> runQ (freeTypeVars (runExpanded (view etype v)))
      -- Primitive (unlifted) types can not be used as parameters to a
      -- type class, which makes them unusable in this system.
      isUnlifted :: TypeGraphVertex -> m Bool
      isUnlifted v = do
        let (E etyp) = view etype v
        unlifted etyp
      -- If the _field field of the goal key is a Left it is a
      -- positional field rather than named - we don't make lenses
      -- for such fields so we don't want their edges in our graph.
      isAnonymous :: TypeGraphVertex -> TypeGraphVertex -> Bool
      isAnonymous _ b = maybe True (\(_, _, fld) -> either (const False) (const True) fld) (view field b)
      -- The graph will initially include edges from (Map a b) to
      -- a, and (Order a b) to a, but we only create lenses to the
      -- b values of a map, so remove the edge to a.
      isMapOrOrder :: TypeGraphVertex -> TypeGraphVertex -> Bool
      isMapOrOrder a b =
          case view etype a of
            E (AppT (AppT (ConT mtyp) ityp) _etyp) | elem mtyp [''Map, ''Order] -> E ityp /= view etype b
            _ -> True
      isolateUnreachable :: GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
      isolateUnreachable es = do
        let (g, vf, kf) = graphFromMap es
        (kernel :: [Vertex]) <- view startTypes >>= mapM expandType >>= mapM (vertex Nothing) >>= return . mapMaybe kf
        let keep :: Set TypeGraphVertex
            keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
            victims = difference (Set.fromList (Map.keys es)) keep
        return $ isolate victims es

      t1 :: GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
      t1 edges = runQ (runIO (putStr ("Language.Haskell.TH.Path.Graph.makeTypeGraphEdges - " ++ pprint edges))) >> return edges

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
  execStateT (get >>= mapM_ doSink . Map.keys >>
              get >>= mapM_ doHide . Map.keys >>
              get >>= mapM_ doView . Map.keys) edges
    where
      doSink :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doSink v = do
        let (E typ) = view etype v
        sinkHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
        when sinkHint (modify (Map.alter (alterFn (const Set.empty)) v))

      doHide :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doHide v = do
        let (E typ) = view etype v
        hideHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
        when hideHint (modify (cut (singleton v)))

      doView :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doView v = let (E a) = view etype v in evalContext (viewInstanceType a) >>= maybe (return ()) (doDivert v)

      -- Replace all of v's out edges with a single edge to typ'
      doDivert v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        modify (Map.alter (alterFn (const (singleton v'))) v)
#if 0
      -- Add an extra out edge from v to typ' (unused)
      doExtra v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        modify (Map.alter (alterFn (Set.insert v')) v)
#endif

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: Default label => (Set TypeGraphVertex -> Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex)
alterFn setf (Just (label, s)) = Just (label, setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (def, setf Set.empty)
