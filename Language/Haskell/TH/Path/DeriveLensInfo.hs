{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.DeriveLensInfo
    ( deriveLensInfo
    ) where

import Control.Applicative ((<$>))
import Control.Lens -- (makeLenses, over, view)
import Control.Monad.Reader (MonadReader, ask, runReaderT)
import Control.Monad.RWS (evalRWST)
import Data.Default (Default)
import Data.Function (on)
import Data.Graph (reachable)
import Data.List as List (groupBy, map, sort, sortBy)
import Data.Map as Map (empty, fromListWith, keys, lookup, Map)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Set as Set (difference, fromList, map, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (Field, LensHint, R(..), makePathLenses, allPathKeys, allLensKeys)
import Language.Haskell.TH.Path.PathToLensDecs
import Language.Haskell.TH.Path.PathTypeDecs
import Language.Haskell.TH.TypeGraph.Core (typeArity)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType)
import Language.Haskell.TH.TypeGraph.Graph (isolate, GraphEdges, graphFromMap, dissolveM)
import Language.Haskell.TH.TypeGraph.Hints (HasVertexHints)
import Language.Haskell.TH.TypeGraph.Info (infoMap, TypeGraphInfo, withTypeGraphInfo)
import Language.Haskell.TH.TypeGraph.Monad (typeGraphEdges, vertex)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

import Data.Foldable

-- | Create the lenses, path types, and path to lens functions for a
-- named type and all of its subtypes.  Each lens extracts or updates
-- a portion of a value.  Each path type describes the correspondence
-- between a value and the portions of that value available via lens.
-- Each path to lens function turns a path type value into a lens.
deriveLensInfo :: Q [Type] -> Q [Type] -> [(Maybe Field, Q Type, Q LensHint)] -> Q [Dec]
deriveLensInfo st gt hs = do
  st' <- st
  gt' <- gt
  hl <- makeHintList hs
  ti <- makeTypeGraphInfo st' hl
  edges <- runReaderT (makeTypeGraphEdges st') ti
  hm <- runReaderT (makeHintMap hl) ti
  let r = R { _startTypes = st'
            , _goalTypes = gt'
            , _lensHintMap = hm
            , _typeInfo = ti
            , _edges = edges
            , _graph = graphFromMap edges
            }
  -- let validPathTypeNames = mapMaybe validPathTypeName toLensInstances
  (lenses :: [Dec]) <-
      evalRWST (allPathKeys >>= mapM_ makePathLenses . toList) r Map.empty >>=
      return . concat . uniqOn decName1 . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedLenses.hs"
  (pathTypes :: [Dec]) <-
      evalRWST (allPathKeys >>= mapM pathTypeDecs . toList) r Map.empty >>=
      sequence . snd >>=
      -- filter out impossible paths
      -- return . List.map (filter (hasValidPathType validPathTypeNames)) >>=
      return . concat . uniqOn decName1 >>=
      runIO . compareSaveAndReturn changeError "GeneratedPathTypes.hs"
  (dataPathNames :: [Dec]) <-
      (stringList "dataPathTypes" . List.map show . sort . catMaybes . List.map dataName $ pathTypes) >>=
      runIO . compareSaveAndReturn changeError "GeneratedDataPathNames.hs"
  (toLensInstances :: [Dec]) <-
      evalRWST (allLensKeys >>= mapM (uncurry pathToLensDecs) . toList) r Map.empty >>=
      return . uniqOn instType . concat . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedToLensInstances.hs"

  return $ pathTypes ++ dataPathNames ++ lenses ++ toLensInstances


makeHintList :: [(Maybe Field, Q Type, Q hint)] -> Q [(Maybe Field, E Type, hint)]
makeHintList hs = do
  mapM (\(fld, typeq, hintq) -> do
          typ <- typeq >>= expandType
          hint <- hintq
          return (fld, typ, hint)) hs

makeTypeGraphInfo :: DsMonad m => [Type] -> [(Maybe Field, E Type, LensHint)] -> m (TypeGraphInfo LensHint)
makeTypeGraphInfo st hintList = withTypeGraphInfo hintList st ask

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, HasVertexHints hint, MonadReader (TypeGraphInfo hint) m) =>
                      [Type] -> m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges st = do
  edges <- typeGraphEdges
  -- trace ("Type graph edges:\n" ++ pprint edges) (return ())
  im <- view infoMap
  -- Dissolve the vertices for types whose arity is not zero.  Each of
  -- their in-edges become connected to each of their out-edges.
  let victim :: TypeGraphVertex -> m Bool
      victim v = do
        let (E etyp) = view etype v
        n <- typeArity etyp
        let prim = case etyp of
                     ConT tname -> maybe False (\ x -> case x of PrimTyConI _ _ _ -> True; _ -> False) (Map.lookup tname im)
                     _ -> False
        return $ n /= 0 || prim
  edges' <- dissolveM victim edges
  let (g, vf, kf) = graphFromMap edges'
  -- Isolate all nodes that are not reachable from the start types.
  kernel <- mapM expandType st >>= mapM (vertex Nothing) >>= return . mapMaybe kf
  let keep :: Set TypeGraphVertex
      keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
      victims = difference (Set.fromList (Map.keys edges')) keep
      edges'' :: GraphEdges hint TypeGraphVertex
      edges'' = isolate victims edges'
  return edges''

makeHintMap :: (Functor m, DsMonad m, MonadReader (TypeGraphInfo hint) m) => [(Maybe Field, E Type, hint)] -> m (Map TypeGraphVertex [hint])
makeHintMap hs = Map.fromListWith (++) <$> mapM (\(fld, typ, hint) -> vertex fld typ >>= \v -> return (v, [hint])) hs

-- | Sort and uniquify by the result of f.
uniqOn :: forall a b. Ord b => (a -> b) -> [a] -> [a]
uniqOn f = List.map head . groupBy ((==) `on` f) . sortBy (compare `on` f)

instType :: Dec -> Type
instType (InstanceD _ typ _) = typ
instType _ = error "instType"

decName1 :: [Dec] -> Maybe Name
decName1 = maybe Nothing decName . listToMaybe

decName :: Dec -> Maybe Name
decName (TySynD x _ _) = Just x
decName (SigD x _) = Just x
decName dec = dataName dec

dataName :: Dec -> Maybe Name
dataName (NewtypeD _ x _ _ _) = Just x
dataName (DataD _ x _ _ _) = Just x
dataName _ = Nothing

-- | Declare a list of strings with the given name
stringList :: String -> [String] -> Q [Dec]
stringList listName names =
    sequence [ sigD (mkName listName) [t| [String] |]
             , valD (varP (mkName listName))
                    (normalB [| $(listE (List.map (litE . stringL) names)) |]) [] ]
