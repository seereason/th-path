-- | Operations using @MonadReader (TypeGraphInfo hint)@.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.TH.Path.Prune
    ( SinkType
    , pruneTypeGraph
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Lens -- (makeLenses, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (execStateT, get, modify, StateT)
import Data.Default (Default(def))
import Data.Map as Map (alter, keys)
import Data.Set as Set (empty, insert, Set, singleton)
import Language.Haskell.TH -- (Con, Dec, nameBase, Type)
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Context.Reify (evalContextState, reifyInstancesWithContext)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.TypeGraph.Core (Field, unlifted)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType)
import Language.Haskell.TH.TypeGraph.Graph (cut, cutM, dissolveM, GraphEdges)
import Language.Haskell.TH.TypeGraph.Hints (HasVertexHints(hasVertexHints), VertexHint(..))
import Language.Haskell.TH.TypeGraph.Info (TypeGraphInfo, hints)
import Language.Haskell.TH.TypeGraph.Monad (allVertices, vertex)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.Instances ()
import Prelude hiding (foldr, mapM_, null)

import Data.Foldable
#if MIN_VERSION_base(4,8,0)
import Data.Foldable (null)
#else
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

-- | If a type is an instance of this class no paths that lead to the
-- internal stucture of the value will be created - the value is
-- considered atomic.
class SinkType a

-- | Like SinkType, but no paths out or into the type will be created.
class HideType a

-- | Remove any vertices that are labelled with primitive types, and then
-- apply the hints obtained from the
-- a new graph which incorporates the information from the hints.
pruneTypeGraph :: forall m hint. (DsMonad m, Default hint, Eq hint, HasVertexHints hint, MonadReader (TypeGraphInfo hint) m) =>
                  (GraphEdges hint TypeGraphVertex) -> m (GraphEdges hint TypeGraphVertex)
pruneTypeGraph edges = do
  cutPrimitiveTypes edges >>= dissolveHigherOrder >>= execStateT (view hints >>= mapM_ doHint >>
                                                                  get >>= mapM_ doSink . Map.keys >>
                                                                  get >>= mapM_ doHide . Map.keys >>
                                                                  get >>= mapM_ doView . Map.keys)
    where
      doSink :: TypeGraphVertex -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doSink v = do
        let (E typ) = view etype v
        sinkHint <- (not . null) <$> evalContextState (reifyInstancesWithContext ''SinkType [typ])
        when sinkHint (modify $ Map.alter (alterFn (const Set.empty)) v)

      doHide :: TypeGraphVertex -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doHide v = do
        let (E typ) = view etype v
        hideHint <- (not . null) <$> evalContextState (reifyInstancesWithContext ''HideType [typ])
        when hideHint (modify $ cut (singleton v))

      doView :: TypeGraphVertex -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doView v = do
        let (E a) = view etype v
        evalContextState (viewInstanceType a) >>=
          maybe (return ())
                (\b -> do
                   v' <- expandType b >>= vertex Nothing
                   modify $ Map.alter (alterFn (const (singleton v'))) v)

      doHint :: (Maybe Field, Name, hint) -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doHint (fld, tname, hint) = hasVertexHints hint >>= mapM_ (\vh -> expandType (ConT tname) >>= allVertices fld >>= mapM_ (\v -> doVertexHint v vh))

      doVertexHint :: TypeGraphVertex -> VertexHint -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doVertexHint _ Normal = return ()

      -- Replace all out edges with a single edge to typ'
      doVertexHint v (Divert typ') = do
        v' <- expandType typ' >>= vertex Nothing
        modify $ Map.alter (alterFn (const (singleton v'))) v
      doVertexHint v (Extra typ') = do
        v' <- expandType typ' >>= vertex Nothing
        modify $ Map.alter (alterFn (Set.insert v')) v

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: Default hint => (Set TypeGraphVertex -> Set TypeGraphVertex) -> Maybe (hint, Set TypeGraphVertex) -> Maybe (hint, Set TypeGraphVertex)
alterFn setf (Just (hint, s)) = Just (hint, setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (def, setf Set.empty)

-- | Primitive (unlifted) types can not be used as parameters to a
-- type class, which makes them unusable in this system.
cutPrimitiveTypes :: (DsMonad m, Default hint, Eq hint, MonadReader (TypeGraphInfo hint) m) => GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
cutPrimitiveTypes edges = cutM (\v -> let (E typ) = view etype v in unlifted typ) edges

dissolveHigherOrder :: (DsMonad m, Default hint, Eq hint, MonadReader (TypeGraphInfo hint) m) => GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
dissolveHigherOrder edges = dissolveM (\v -> do let (E typ) = view etype v
                                                kind <- runQ $ inferKind typ
                                                return $ kind /= Right StarT) edges
