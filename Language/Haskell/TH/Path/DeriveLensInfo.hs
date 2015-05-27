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
import Data.List as List (sort)
import Data.Map as Map (empty)
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
deriveLensInfo :: Q [Type] -> [(Maybe Field, Name, Q LensHint)] -> Q [Dec]
deriveLensInfo st hs = makeTypeGraph st hs >>= \r -> do
  (lenses :: [Dec]) <-
      evalRWST (allPathKeys >>= mapM_ makePathLenses . toList . Set.map simpleVertex) r Map.empty >>=
      return . concat . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedLenses.hs"
  (pathTypes :: [Dec]) <-
      evalRWST (allPathKeys >>= mapM pathTypeDecs . toList . Set.map simpleVertex) r Map.empty >>=
      return . sort . concat . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedPathTypes.hs"
  (toLensInstances :: [Dec]) <-
      evalRWST (allLensKeys >>= mapM (uncurry pathInstanceDecs) . toList) r Map.empty >>=
      return . sort . concat . snd >>=
      runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs"

  return $ pathTypes ++ lenses ++ toLensInstances
