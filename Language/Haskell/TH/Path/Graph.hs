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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Graph
    ( makeTypeGraphEdges
    ) where

import Control.Applicative ((<$>))
import Control.Lens -- (makeLenses, over, view)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Default (Default)
import Data.Graph (reachable)
import Data.Map as Map (keys, Map, map, mapWithKey)
import Data.Maybe (mapMaybe)
import Data.Set as Set (difference, empty, filter, fromList, map, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.Prune (pruneTypeGraph)
import Language.Haskell.TH.TypeGraph (unlifted, E(E), expandType, freeTypeVars,
                                      dissolveM, GraphEdges, graphFromMap, isolate,
                                      TypeGraphInfo, typeGraphInfo,
                                      simpleEdges, typeGraphEdges, vertex,
                                      TypeGraphVertex, etype, field)
import Prelude hiding (any, concat, exp, foldr, mapM_, null, or)

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, MonadReader TypeGraphInfo m) =>
                      [Type] -> m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges st = do
  -- im <- view infoMap
  -- Dissolve the vertices for types whose arity is not zero.  Each of
  -- their in-edges become connected to each of their out-edges.
  let victim :: TypeGraphVertex -> m Bool
      victim v = do
        let (E etyp) = view etype v
        k <- runQ $ inferKind etyp
        fv <- runQ $ freeTypeVars etyp
        prim <- unlifted etyp
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
          -- The graph will initially include edges from (Map a b) to
          -- a, and (Order a b) to a, but we only create lenses to the
          -- b values of a map, so remove the edge to a.
          Map.mapWithKey (\key (hint, gkeys) -> (hint, Set.filter (\gkey -> case view etype key of
                                                                              E (AppT (AppT (ConT mtyp) ityp) _etyp) | elem mtyp [''Map, ''Order] -> E ityp /= view etype gkey
                                                                              _ -> True) gkeys))
