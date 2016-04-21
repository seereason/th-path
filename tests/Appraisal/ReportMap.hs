{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Appraisal.ReportMap
    ( ReportID (..)
    , ReportMap (..)
    , reportMapFromList
    , reportMapSize
    -- , lens_ReportMap_ReportMap
    , lookupReport
    , MRR
    ) where

import Appraisal.Report (Report(reportUUID))
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import qualified Data.Map as M (fromList, lookup, Map, size)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.UUID.Types.Internal (UUID(..))
import GHC.Generics (Generic)
import Language.Haskell.TH.Path.Core (SelfPath)

type MRR = M.Map ReportID Report

newtype ReportID = ReportID { unReportID :: UUID } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
newtype ReportMap = ReportMap { unReportMap :: MRR } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)

instance SelfPath ReportID

-- $(makeLensesFor [("unReportMap", "lens_ReportMap_ReportMap")] ''ReportMap)

#if !__GHCJS__
$(deriveSafeCopy 1 'base ''ReportID)
$(deriveSafeCopy 2 'base ''ReportMap)
#endif

reportMapFromList :: [Report] -> ReportMap
reportMapFromList = ReportMap . M.fromList . map (\r -> (ReportID (reportUUID r), r))

reportMapSize :: ReportMap -> Int
reportMapSize (ReportMap rm) = M.size rm

lookupReport :: ReportMap -> ReportID -> Maybe Report
lookupReport (ReportMap rmap) rid = M.lookup rid rmap
