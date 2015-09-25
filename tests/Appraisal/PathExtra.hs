-- | These are the hand crafted path to lens functions.
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
module Appraisal.PathExtra
    ( extractReportPath
    , extractReportID
    , extractReportElemID
    , omapPaths
    ) where

import Appraisal.ReportPaths
import Appraisal.ReportMap (ReportID)
import Control.Lens (Lens', view)
import Language.Haskell.TH.Path.Core (Path_Map(Path_Look), Path_OMap(Path_At), idPath)
import Language.Haskell.TH.Path.Order (OrderKey, Order, toPairs)

omapPaths :: OrderKey k => Lens' a (Order k b) -> a -> [(k, c -> Path_OMap k c)]
omapPaths lns a = map (\ (k, _) -> (k, Path_At k)) (toPairs (view lns a))

extractReportPath :: Path_ReportMap a -> Path_Report a
extractReportPath (Path_ReportMap_unReportMap (Path_Look _ rp)) = rp
extractReportPath path = error "extractReportPath - unexpected path"

extractReportID :: (Path_Report a -> Path_ReportMap a) -> ReportID
extractReportID f =
  let Path_ReportMap_unReportMap (Path_Look rid _) = f (Path_Report_View (Path_ReportView__reportFolder idPath))
  in rid

extractReportElemID :: (Path_ReportElem a -> Path_ReportMap a) -> ReportID
extractReportElemID f =
  let Path_ReportMap_unReportMap (Path_Look rid _) = f (Path_ReportElem_elemItem (Path_Item_itemName idPath))
  in rid
