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
import Data.Set as Set (empty, {-insert,-} Set, singleton)
import Language.Haskell.TH -- (Con, Dec, nameBase, Type)
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.TypeGraph (unlifted, E(E), expandType, cut, cutM, dissolveM,
                                      GraphEdges, TypeGraphInfo, vertex, TypeGraphVertex(..), etype)
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
pruneTypeGraph :: forall m label. (DsMonad m, Default label, Eq label, MonadReader TypeGraphInfo m) =>
                  (GraphEdges label TypeGraphVertex) -> m (GraphEdges label TypeGraphVertex)
pruneTypeGraph edges = do
  cutPrimitiveTypes edges >>= dissolveHigherOrder >>= execStateT (get >>= mapM_ doSink . Map.keys >>
                                                                  get >>= mapM_ doHide . Map.keys >>
                                                                  get >>= mapM_ doView . Map.keys)
    where
      doSink :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doSink v = do
        let (E typ) = view etype v
        sinkHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
        when sinkHint (modify $ Map.alter (alterFn (const Set.empty)) v)

      doHide :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doHide v = do
        let (E typ) = view etype v
        hideHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
        when hideHint (modify $ cut (singleton v))

      doView :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doView v = let (E a) = view etype v in evalContext (viewInstanceType a) >>= maybe (return ()) (doDivert v)

      -- Replace all of v's out edges with a single edge to typ'
      doDivert v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        modify $ Map.alter (alterFn (const (singleton v'))) v
#if 0
      -- Add an extra out edge from v to typ' (unused)
      doExtra v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        modify $ Map.alter (alterFn (Set.insert v')) v
#endif

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: Default label => (Set TypeGraphVertex -> Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex)
alterFn setf (Just (label, s)) = Just (label, setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (def, setf Set.empty)

-- | Primitive (unlifted) types can not be used as parameters to a
-- type class, which makes them unusable in this system.
cutPrimitiveTypes :: (DsMonad m, Default label, Eq label, MonadReader TypeGraphInfo m) => GraphEdges label TypeGraphVertex -> m (GraphEdges label TypeGraphVertex)
cutPrimitiveTypes edges = cutM (\v -> let (E typ) = view etype v in unlifted typ) edges

dissolveHigherOrder :: (DsMonad m, Default label, Eq label, MonadReader TypeGraphInfo m) => GraphEdges label TypeGraphVertex -> m (GraphEdges label TypeGraphVertex)
dissolveHigherOrder edges = dissolveM (\v -> do let (E typ) = view etype v
                                                kind <- runQ $ inferKind typ
                                                return $ kind /= Right StarT) edges
