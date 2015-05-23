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

import Control.Monad.RWS (evalRWST)
import Data.Function (on)
import Data.List as List (groupBy, map, sort, sortBy)
import Data.Map as Map (empty)
import Data.Maybe (catMaybes)
import Data.Set as Set (map)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (Field, LensHint)
import Language.Haskell.TH.Path.Monad (makePathLenses, allPathKeys, allLensKeys)
import Language.Haskell.TH.Path.Monad (makeTypeGraph)
import Language.Haskell.TH.Path.PathInstanceDecs
import Language.Haskell.TH.Path.PathTypeDecs
import Language.Haskell.TH.TypeGraph.Monad (simpleVertex)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

import Data.Foldable

-- | Create the lenses, path types, and path to lens functions for a
-- named type and all of its subtypes.  Each lens extracts or updates
-- a portion of a value.  Each path type describes the correspondence
-- between a value and the portions of that value available via lens.
-- Each path to lens function turns a path type value into a lens.
deriveLensInfo :: Q [Type] -> Q [Type] -> [(Maybe Field, Q Type, Q LensHint)] -> Q [Dec]
deriveLensInfo st gt hs = makeTypeGraph st gt hs >>= \r -> do
  (lenses :: [Dec]) <-
      evalRWST (allPathKeys >>= mapM_ makePathLenses . toList . Set.map simpleVertex) r Map.empty >>=
      runIO . compareSaveAndReturn changeError "GeneratedLenses.hs" . concat . snd
  (pathTypes :: [Dec]) <-
      evalRWST (allPathKeys >>= mapM pathTypeDecs . toList . Set.map simpleVertex) r Map.empty >>=
      sequence . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedPathTypes.hs" . concat
  (dataPathNames :: [Dec]) <-
      (stringList "dataPathTypes" . List.map show . sort . catMaybes . List.map dataName $ pathTypes) >>=
      runIO . compareSaveAndReturn changeError "GeneratedDataPathNames.hs"
  (toLensInstances :: [Dec]) <-
      evalRWST (allLensKeys >>= mapM (uncurry pathInstanceDecs) . toList) r Map.empty >>=
      return . uniqOn instType . concat . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs"

  return $ pathTypes ++ dataPathNames ++ lenses ++ toLensInstances

-- | Sort and uniquify by the result of f.
uniqOn :: forall a b. Ord b => (a -> b) -> [a] -> [a]
uniqOn f = List.map head . groupBy ((==) `on` f) . sortBy (compare `on` f)

instType :: Dec -> Type
instType (InstanceD _ typ _) = typ
instType _ = error "instType"

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
