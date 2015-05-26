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
    ( pruneTypeGraph
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
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH -- (Con, Dec, nameBase, Type)
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Syntax (qReify)
import Language.Haskell.TH.Context.Reify (evalContextState, reifyInstancesWithContext)
import Language.Haskell.TH.TypeGraph.Core (Field, unlifted)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType)
import Language.Haskell.TH.TypeGraph.Graph (cut, cutM, GraphEdges)
import Language.Haskell.TH.TypeGraph.Hints (SinkType, HasVertexHints(hasVertexHints), VertexHint(..))
import Language.Haskell.TH.TypeGraph.Info (TypeGraphInfo, hints)
import Language.Haskell.TH.TypeGraph.Monad (allVertices, vertex)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype, typeNames)
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

-- | Remove any vertices that are labelled with primitive types, and then
-- apply the hints obtained from the
-- a new graph which incorporates the information from the hints.
pruneTypeGraph :: forall m hint. (DsMonad m, Default hint, Eq hint, HasVertexHints hint, MonadReader (TypeGraphInfo hint) m) =>
                  (GraphEdges hint TypeGraphVertex) -> m (GraphEdges hint TypeGraphVertex)
pruneTypeGraph edges = do
  cutPrimitiveTypes edges >>= execStateT (view hints >>= mapM_ doHint >> doClasses)
    where
      -- Primitive (unlifted) types can not be used as parameters to a
      -- type class, which makes them unusable in this system.
      doClasses :: StateT (GraphEdges hint TypeGraphVertex) m ()
      doClasses = get >>= mapM_ doVert . Map.keys
      doVert :: TypeGraphVertex -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doVert v = do
        let (E typ) = view etype v
        prim <- case typ of
                  ConT name -> qReify name >>= return . doInfo
                      where
                        doInfo (PrimTyConI _ _ _) = True
                        doInfo _ = False
                  _ -> return False
        kind <- runQ (inferKind typ)
        sinkHint <- if kind == Right StarT && not prim then (not . null) <$> evalContextState (reifyInstancesWithContext ''SinkType [typ]) else return False
        when sinkHint (modify $ Map.alter (alterFn (const Set.empty)) v)

      doHint :: (Maybe Field, Name, hint) -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doHint (fld, tname, hint) = hasVertexHints hint >>= mapM_ (\vh -> expandType (ConT tname) >>= allVertices fld >>= mapM_ (\v -> {-t3 v vh >>-} doVertexHint v vh))

      doVertexHint :: TypeGraphVertex -> VertexHint -> StateT (GraphEdges hint TypeGraphVertex) m ()
      doVertexHint _ Normal = return ()
{-
      doVertexHint v Sink =
        modify $ Map.alter (alterFn (const Set.empty)) v
-}
      doVertexHint v Hidden =
        modify $ cut (singleton v)
      -- Replace all out edges with a single edge to typ'
      doVertexHint v (Divert typ') = do
        v' <- expandType typ' >>= vertex Nothing
#if 0
        modify $ Map.alter (alterFn (const (singleton v'))) v
#else
        -- This is here because we want a path to ReportIntendedUse even
        -- though there is a substitution of String on Maybe ReportIntendedUse.
        -- I'm going to try to remove the Maybe from that substitution.
        case (null $ typeNames v) of
          False -> modify $ Map.alter (alterFn (const (singleton v'))) v
          True -> modify $ Map.alter (alterFn (Set.insert v')) v
#endif
      doVertexHint v (Extra typ') = do
        v' <- expandType typ' >>= vertex Nothing
        modify $ Map.alter (alterFn (Set.insert v')) v

      -- t1 x = trace ("before hints:\n" ++ pprint x) (return x)
      -- t2 x = trace ("after hints:\n" ++ pprint x) (return x)
      -- t3 v x = trace ("doVertexHint " ++ pprint' v ++ ": " ++ pprint x) (return ())

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: Default hint => (Set TypeGraphVertex -> Set TypeGraphVertex) -> Maybe (hint, Set TypeGraphVertex) -> Maybe (hint, Set TypeGraphVertex)
alterFn setf (Just (hint, s)) = Just (hint, setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (def, setf Set.empty)

cutPrimitiveTypes :: (DsMonad m, Default hint, Eq hint, MonadReader (TypeGraphInfo hint) m) => GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
cutPrimitiveTypes edges = cutM (\v -> let (E typ) = view etype v in unlifted typ) edges

