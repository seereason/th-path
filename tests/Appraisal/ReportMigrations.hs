-- | This module re-exports 'Appraisal.ReportPaths', which contains
-- the path types generated for AppraisalScribe, and adds PathInfo
-- instances, SafeCopy instances, and Migrate instances.

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Appraisal.ReportMigrations
    ( module Appraisal.ReportPaths
    ) where

import Appraisal.ReportPaths
import Appraisal.ReportMap (ReportMap)

#if !__GHCJS__
import Control.Monad.Extra (ifM)
import Control.Monad.Writer (tell)
import Data.Map as Map (fromList, lookup, Map)
import Data.Monoid ((<>))
import Data.Order (Path_OMap)
import Data.Set as Set (difference, fromList, toList)
import Data.SafeCopy (base, deriveSafeCopy, extension, SafeCopy, Version)
import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (qReifyInstances)
import Language.Haskell.TH.Context (noInstance)
import Language.Haskell.TH.Path.Common (asName, asTypeQ)
import Language.Haskell.TH.Path.Core -- (pathTypeNames)
import Language.Haskell.TH.Path.Graph (runContextT, runTypeGraphT)
import Language.Haskell.TH.Syntax (addDependentFile, qReify)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathStarts)
import Web.Routes.PathInfo (PathInfo)
import Web.Routes.TH (derivePathInfo)

-- Generate code to create SafeCopy, , HasPathInfo, and
-- (eventually) other instances for the generated path types.
$(do let migratedPathTypeNames :: Map Name (Version a)
         migratedPathTypeNames = Map.fromList [] -- add stuff here
         doInfo (FamilyI (FamilyD typefam upath [KindedTV u StarT,KindedTV s StarT] (Just StarT)) insts) =
             concat <$> mapM doInstance insts
         doInstance (TySynInstD upath (TySynEqn [univ, typ] (ConT pname))) =
             (<>) <$> derivePathInfo pname
                  <*> case Map.lookup pname migratedPathTypeNames of
                        Nothing -> deriveSafeCopy 0 'base pname
                        Just v  -> deriveSafeCopy v 'extension pname
         -- Types such as Path_Pair already have instances
         doInstance (TySynInstD upath (TySynEqn [univ,typ] ptype)) = pure []
         doInstance x = trace ("Unexpected UPath instance: " ++ show x) (pure [])
     qReify ''UPath >>= doInfo)
#endif
