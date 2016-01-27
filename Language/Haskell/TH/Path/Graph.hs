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
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.TH.Path.Graph
    ( runContextT
    , runTypeGraphT
    -- * Hint classes
    , SinkType
    , HideType
    , SelfPath
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#else
import Control.Applicative
#endif
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (filterM)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Control.Monad.State (execStateT, evalStateT, get, modify, put, StateT)
import Control.Monad.States (getPoly, modifyPoly, MonadStates, putPoly)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import Data.Foldable.Compat
import Data.Graph as Graph (reachable)
import Data.List as List (filter, map)
import Data.Map as Map (filterWithKey, fromList, keys, Map, mapWithKey, toList)
import Data.Maybe (mapMaybe)
import Data.Set as Set (difference, empty, fromList, map, member, Set, singleton, toList)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.View (viewInstanceType, viewTypes)
import Language.Haskell.TH.PprLib (hang, text)
import Language.Haskell.TH.TypeGraph.Edges ({-cut, cutEdgesM,-} cutEdges, cutM, dissolveM, GraphEdges, isolate, linkM, simpleEdges, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Expand (E(E), unE, ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Prelude ({-OverTypes(overTypes),-} unlifted)
import Language.Haskell.TH.TypeGraph.TypeGraph (graphFromMap, makeTypeGraph, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (makeTypeInfo, startTypes, TypeInfo, typeVertex, typeVertex')
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGV(TGV, _vsimple), vsimple, TGVSimple(TGVSimple, _etype))
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

#if 0
import Data.Map as Map (fromList, lookup, toList)
import Data.Set as Set (difference, toList)
import Debug.Trace (trace)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.Graph (typeInfo)
import Language.Haskell.TH.TypeGraph.Info (synonyms)
#endif

data S = S { _expanded :: ExpandMap
           , _instMap :: InstMap
           , _prefix :: String }

$(makeLenses ''S)

instance Monad m => MonadStates ExpandMap (StateT S m) where
    getPoly = use expanded
    putPoly s = expanded .= s

instance Monad m => MonadStates InstMap (StateT S m) where
    getPoly = use instMap
    putPoly s = instMap .= s

instance Monad m => MonadStates String (StateT S m) where
    getPoly = use prefix
    putPoly s = prefix .= s

instance DsMonad m => ContextM (StateT S m)

instance ContextM m => ContextM (ReaderT t m)

instance (Monoid w, ContextM m) => ContextM (WriterT w m)

runContextT :: Monad m => StateT S m a -> m a
runContextT action = evalStateT action (S mempty mempty "")

runTypeGraphT :: DsMonad m => ReaderT TypeGraph (ReaderT TypeInfo (StateT S m)) a -> [Type] -> m a
runTypeGraphT action st = runContextT (runTypeGraphT' action st)

runTypeGraphT' :: ContextM m => ReaderT TypeGraph (ReaderT TypeInfo m) a -> [Type] -> m a
runTypeGraphT' action st = do
  vt <- viewTypes -- Every instance of ViewType
  let st' = st ++ Set.toList vt
  ti <- makeTypeInfo (\t -> maybe mempty singleton <$> runQ (viewInstanceType t)) st'
  runReaderT (pathGraphEdges >>= makeTypeGraph >>= runReaderT action) ti

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
pathGraphEdges :: forall m. (MonadReaders TypeInfo m, ContextM m) => m (GraphEdges TGV)
pathGraphEdges =
    typeGraphEdges >>= execStateT finalizeEdges
    where
      finalizeEdges = do
        -- get >>= runQ . runIO . putStr . show . hang (text "initial edges of the type graph:") 2 . ppr
        modify (cutEdges isMapKey)
        _modify "unlifted" (cutM isUnlifted)
        _modify "higherOrder" (dissolveM higherOrder)
        _modify "view edges" (linkM viewEdges)
        _modify "unlifed" pruneTypeGraph
        _modify "freeVars" (dissolveM hasFreeVars)
        _modify "unlifted2" (dissolveM isUnlifted)
        _modify "unreachable" isolateUnreachable
        get >>= runQ . runIO . putStr . show . hang (text "final edges of the type graph:") 2 . ppr

      viewEdges :: TGV -> m (Maybe (Set TGV))
      viewEdges v =
          do let (typ :: Type) = view (vsimple . etype . unE) v
             viewInstanceType typ >>= maybe (return Nothing) (\t -> expandType t >>= typeVertex' >>= return . Just . singleton)

      higherOrder :: TGV -> m Bool
      higherOrder v = (/= Right StarT) <$> runQ (inferKind (view (vsimple . etype . unE) v))
      hasFreeVars :: TGV -> m Bool
      hasFreeVars v = (/= Set.empty) <$> runQ (freeTypeVars (view (vsimple . etype . unE) v))
      -- Primitive (unlifted) types can not be used as parameters to a
      -- type class, which makes them unusable in this system.
      isUnlifted :: TGV -> m Bool
      isUnlifted v = unlifted (view (vsimple . etype . unE) v)

      isMapKey :: TGV -> TGV -> Bool
      isMapKey (TGV {_vsimple = TGVSimple {_etype = E (AppT a@(AppT (ConT name) _) _b)}}) a' |
          (name == ''Order || name == ''Map) && a == view (vsimple . etype . unE) a' = True
      isMapKey _ _ = False

      isolateUnreachable :: GraphEdges TGV -> m (GraphEdges TGV)
      isolateUnreachable es = do
        st <- askPoly >>= return . view startTypes >>= mapM expandType >>= mapM typeVertex
        let (g, vf, kf) = graphFromMap (simpleEdges es)
        let keep :: Set TGVSimple
            keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) (mapMaybe kf st)
            -- Discard any nodes whose simplified version is not in keep
            victims = Set.fromList $ List.filter (\ v -> not (Set.member (view vsimple v) keep)) (Map.keys es)
            -- victims = {- t3 $ -} Set.difference (Set.fromList (Map.keys es)) keep
        -- trace ("isolateUnreachable - " ++ pprint es ++ "\n" ++
        --        intercalate "\n  " ("startTypes:" : List.map pprint' st) ++ "\n" ++
        --        intercalate "\n  " ("Unreachable:" : List.map pprint' victims)) (return ())
        let es' = isolate (flip member victims) es
            es'' = Map.filterWithKey (\k _ -> not (Set.member k victims)) es'
        return es''

      _modify :: String -> (GraphEdges TGV -> m (GraphEdges TGV)) -> StateT (GraphEdges TGV) m ()
      _modify _s f = do
        old <- get
        new <- lift $ f old
        put new
{-
        runQ (runIO (putStr ("\n\f\nLanguage.Haskell.TH.Path.Graph.makeTypeGraphEdges " ++ s ++
                             " - added " ++ indent "+" (pprint (diff new old)) ++
                             "\nremoved " ++  indent "-" (pprint (diff old new)))))
      indent s t = unlines . List.map (s ++) . lines $ t

      -- Exact difference between two maps
      diff m1 m2 = Map.fromList $ Set.toList $ Set.difference (Set.fromList (Map.toList m1))
                                                              (Set.fromList (Map.toList m2))
-}

-- | 'Path' instances can be customized by declaring types to be
-- instances of this class and the ones that follow.  If a type is an
-- instance of 'SinkType', no paths that lead to the internal stucture
-- of the value will be created - the value is considered atomic.
class SinkType a

-- | Like SinkType, but no paths out or into the type will be created.
class HideType a

-- | Types for which
-- a 'SelfPath' instance is declared will be used as their own Path
-- Type.  For example, a UUID or some enumerated type contained in a
-- record could be used directly to reference the object that contains
-- it.
class SelfPath a

-- | Remove any vertices that are labelled with primitive types, and then
-- apply the hints obtained from the
-- a new graph which incorporates the information from the hints.
pruneTypeGraph :: forall m. (MonadReaders TypeInfo m, ContextM m) => (GraphEdges TGV) -> m (GraphEdges TGV)
pruneTypeGraph edges =
  doSink edges >>= doHide
  -- execStateT (get >>= mapM_ doHide . Map.keys) edges >>=
  -- execStateT (get >>= mapM_ doView . Map.keys)
    where
      doSink :: (GraphEdges TGV) -> m (GraphEdges TGV)
      doSink es =
          execStateT (do victims <- getPoly >>= \(x :: GraphEdges TGV) -> filterM (sink . view (vsimple . etype)) (Map.keys x) >>= return . Set.fromList
                         modifyPoly (Map.mapWithKey (\k s -> if Set.member k victims then Set.empty else s) :: GraphEdges TGV -> GraphEdges TGV)) es

      sink (E typ) = (not . null) <$> lift (reifyInstancesWithContext ''SinkType [typ])

      doHide :: (GraphEdges TGV) -> m (GraphEdges TGV)
      doHide es =
          execStateT (do victims <- getPoly >>= \(x :: GraphEdges TGV) -> filterM (hidden . view (vsimple . etype)) (Map.keys x) >>= return . Set.fromList
                         modifyPoly (Map.mapWithKey (\_ s -> Set.difference s victims) :: GraphEdges TGV -> GraphEdges TGV)
                         modifyPoly (Map.filterWithKey (\k _ -> not (Set.member k victims)) :: GraphEdges TGV -> GraphEdges TGV)) es

      hidden (E typ) = (not . null) <$> lift (reifyInstancesWithContext ''HideType [typ])
