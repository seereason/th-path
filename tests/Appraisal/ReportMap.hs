{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Appraisal.ReportMap
    ( ReportID (..)
    , ReportMap (..)
    , reportMapFromList
    , reportMapSize
    , lens_ReportMap_ReportMap
    , lookupReport
    ) where

import Appraisal.Report (Report(reportUUID))
import Appraisal.Utils.UUID.Internal (UUID'(..))
import Data.UUID.Types.Internal (UUID(..))
import Control.Lens (makeLensesFor)
import Data.Data (Data, Typeable)
import qualified Data.Map as M (fromList, lookup, Map, size)
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(..))
import Language.Haskell.TH.Path.Graph (SelfPath)
-- import Language.Haskell.TH.Path.Lens (nameMakeLens)

newtype ReportID_0 = ReportID_0 UUID' deriving (Eq, Ord, Read, Show, Typeable, Data)
newtype ReportID = ReportID { unReportID :: UUID } deriving (Eq, Ord, Read, Show, Typeable, Data)
newtype ReportMap_v1 = ReportMap_v1 { _unReportMap_v1 :: [Report] } deriving (Eq, Ord, Read, Show, Typeable, Data)
newtype ReportMap = ReportMap { unReportMap :: M.Map ReportID Report } deriving (Eq, Ord, Read, Show, Typeable, Data)

instance SelfPath ReportID

$(makeLensesFor [("unReportMap", "lens_ReportMap_ReportMap")] ''ReportMap)

$(deriveSafeCopy 0 'base ''ReportID_0)
$(deriveSafeCopy 1 'extension ''ReportID)
$(deriveSafeCopy 1 'base ''ReportMap_v1)
$(deriveSafeCopy 2 'extension ''ReportMap)

instance Migrate ReportMap where
    type MigrateFrom ReportMap = ReportMap_v1
    migrate (ReportMap_v1 rs) = reportMapFromList rs

instance Migrate ReportID where
    type MigrateFrom ReportID = ReportID_0
    migrate (ReportID_0 (Appraisal.Utils.UUID.Internal.UUID a b c d)) = ReportID (Data.UUID.Types.Internal.UUID a b c d)

reportMapFromList :: [Report] -> ReportMap
reportMapFromList = ReportMap . M.fromList . map (\r -> (ReportID (reportUUID r), r))

reportMapSize :: ReportMap -> Int
reportMapSize (ReportMap rm) = M.size rm

lookupReport :: ReportMap -> ReportID -> Maybe Report
lookupReport (ReportMap rmap) rid = M.lookup rid rmap
