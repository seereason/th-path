-- | This module re-exports 'Appraisal.ReportPaths', which contains
-- the path types generated for AppraisalScribe, and adds PathInfo
-- instances, SafeCopy instances, and Migrate instances.

{-# LANGUAGE CPP, DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -O0 #-} -- Speed up compile during development

module Appraisal.ReportMigrations
    ( module Appraisal.ReportPaths
    ) where

import Appraisal.ReportPaths
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.Map as Map (fromList, lookup)
import Data.Set as Set (toList)
import Data.SafeCopy (base, deriveSafeCopy, extension)
import Language.Haskell.TH.Path.Core (pathTypeNames)
import Web.Routes.TH (derivePathInfo)

-- Generate code to create SafeCopy, Indexable, HasPathInfo, and
-- (eventually) other instances for the site data model types.
#if 0
$(let doPathInfoInstance name =
        kind <- inferKind (ConT name)
        case kind of
          _ | nameBase name == "Path_Maybe" -> return []
          Right (AppT StarT StarT) -> do
            insts <- runQ $ evalContextState (reifyInstancesWithContext ''PathInfo [AppT (ConT name) (VarT (mkName "a"))])
            case insts of
              [] -> derivePathInfo name
              _ -> return []
          _ -> return []
 in concat <$> (pathTypeNames >>= mapM doPathInfoInstance . Set.toList))

$(let migratedPathTypeNames = Map.fromList []
      doSafeCopyInstance name = do
        kind <- inferKind (ConT name)
        case kind of
          Right (AppT StarT StarT) -> do
            insts <- runQ $ evalContextState (reifyInstancesWithContext ''SafeCopy [AppT (ConT name) (VarT (mkName "a"))])
            case insts of
              [] -> case Map.lookup name migratedPathTypeNames of
                      Nothing -> deriveSafeCopy 0 'base name
                      Just n -> deriveSafeCopy n 'extension name
              _ -> return []
          _ -> return []
  in concat <$> (pathTypeNames >>= mapM doSafeCopyInstance . Set.toList))
#else
$(concat <$> (pathTypeNames >>= mapM derivePathInfo . Set.toList))

$(let migratedPathTypeNames = Map.fromList []
      doSafeCopyInstance name = do
        case Map.lookup name migratedPathTypeNames of
          Nothing -> deriveSafeCopy 0 'base name
          Just n  -> deriveSafeCopy n 'extension name
  in concat <$> (pathTypeNames >>= mapM doSafeCopyInstance . Set.toList))
#endif
