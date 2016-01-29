-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- The generated toLens instances will have incomplete patterns where
-- we tried to generate a clause but we found no path to the goal type.

import Appraisal.Markup (rawMarkdown)
import Appraisal.Report
import Appraisal.ReportInstances
import Appraisal.ReportMap
import Appraisal.Utils.CIString
import Control.Lens (Lens', toListOf)
import Control.Monad.Readers (MonadReaders)
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile)
import Data.List (sort)
import Data.Proxy (Proxy)
import Data.Tree
import Debug.Trace
import Editor (editor)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.Order (Path_OMap(Path_At))
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import Language.Haskell.TH.TypeGraph.TypeInfo (startTypes, TypeInfo)
import Language.Haskell.TH.TypeGraph.TypeGraph (adjacent, TypeGraph, typeGraphVertex)
import System.Exit
import Test.HUnit
import Text.PrettyPrint (text)

import Appraisal.ReportTH (decs)
import Report (report)
import ReportPaths

elem :: ReportElem
elem = let p = head (pathsOf Report.report (undefined :: Proxy ReportElem)) in
       head (toListOf (toLens p) Report.report)

main :: IO ()
main = do
  let code = (unlines . map (pprint . friendlyNames) . sort) decs
  r <- runTestTT $ TestList $
         [ let expected :: String
               expected = toString $(embedFile "tests/expected.hs")
               actual :: String
               actual = code in
           TestCase $ assertString $ show $ prettyContextDiff (text "expected") (text "actual") text (getContextDiff 2 (lines expected) (lines actual))

         , let expected = [Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 1}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 2}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 3}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 4}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 5}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 6}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 7}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 8}) Path_ReportElem)),
                           Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 9}) Path_ReportElem))]
               actual = take 10 (pathsOf Report.report (undefined :: Proxy ReportElem)) in
           TestCase $ assertEqual "reportelems" expected actual

         , let expected :: PV_Report
               expected = PV_Report_ReportElem (Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) Path_ReportElem))) (ReportParagraph {elemText = rawMarkdown "#Manuel Robbe\n\n####Biography\n\nA Paris-born painter and printmaker, Manuel Robbe was noted for his experimental approach to printmaking, and during his career created more than 200 aquatints and drypoints.  He was a great technician, inventing a technique known as \"sugar-life\" which gave his prints a startling subtlety. Robbe's technique was developed over several phases.  He printed his design with a mixture of sugar, India ink and gum Arabic, on his zinc plate.  This was followed by heating the plate and working with the soft-ground etching process until the desired result was achieved. Finally, Robbe painted the subject on the zinc plate with an oil paint brush.  For this process he used a special brush made of rags, which was called 'a la poup\233e' (with a doll).  This process was used by French engravers of the 18th century.  In completing his image, Robbe used his fingers to play with the tone on the zinc plate, whereby many of these color prints appear completely unique. He arrived at new shades of color every time he pulled an impression; for example, park scenes appear in spring colors and also in colors associated with autumn.\"\n\nRobbe did images of daily life, especially of beautifully gowned women of obvious comfortable means with their children or involved in leisure activities such as looking in mirrors or perusing artwork.  He also did some landscape subjects.  With his images, Robbe reflected \"la belle \233poque\" or a happy, peaceful time in France at the end of the 19th century and early 20th century before the outbreak of World War I when spririts were relatively high because life seemed peaceful and progressive.  \n\nHe was born in Paris to French parents, refugees from the Franco-Prussion War, from Berthune in northern France.  Robbe attended the Acad\233mie Julian and the Ecole des Beaux Arts and was mentored in his etching and aquatint methods by Eugene Del\226tre.  By 1898, he was exhibiting at the Salon of the Soci\233t\233 Nationale des Beaux-Arts, having changed his commitment from the Societ\233 des Artistes Fran\231ais.  Along with Jacques Villon, Robbe was promoted by Edmond Sagot, Parisian publisher who was one of the most prominent print publishers in western culture at the turn of the century.\n\nBefore 1914, Robbe completed a large number of aquatints in color as well as in black and white, and for his excellence received a Gold Medal at the 1900 Universal Exhibition.  During World War I, he was a pilot, and after the war, his art career waned because of increasing focus on modernist movements such as Cubism and because of his basic shyness that meant he did not aggressively promote his work.  He turned his atelier into a commercial printing shop that has continued into the 21st century."})
               actual :: PV_Report
               actual = let p = head (pathsOf Report.report (undefined :: Proxy ReportElem)) in
                        PV_Report_ReportElem p (head (toListOf (toLens p) Report.report)) in
            TestCase $ assertEqual "PV_Report_ReportElem" expected actual

         , let expected :: [Tree PV_Report]
               expected = let p = Path_Report_View Path_ReportView in
                          case toListOf (toLens p) Report.report of
                            [] -> []
                            [x] -> [Node (PV_Report_ReportView p x) [{-actually we expect stuff in here-}]]
                            _ -> error "multi"
               actual :: [Tree PV_Report]
               actual = pvTree Report.report in
           TestLabel "pvTree view" $ TestCase $ assertEqual "pvTree view" expected actual

         , let expected :: [Tree PV_AbbrevPairs]
               expected = [Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 0}) Path_Pair) (CIString {unCIString = "USPAP"},rawMarkdown "_Uniform Standards of Professional Appraisal Practice_, the 2012-2013 Edition (USPAP)"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 1}) Path_Pair) (CIString {unCIString = "Client"},rawMarkdown "Dr. Albert A. Cutri and Sharon Cutri"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 2}) Path_Pair) (CIString {unCIString = "IntendedUseShortDescription"},rawMarkdown "Insurance Coverage.  \\footnote{See below for definition of Replacement Value}"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 3}) Path_Pair) (CIString {unCIString = "ReportUsers"},rawMarkdown "Dr. Albert A. Cutri, Sharon Cutri and the University of San Diego"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 4}) Path_Pair) (CIString {unCIString = "theseappraisers"},rawMarkdown "these appraisers"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 5}) Path_Pair) (CIString {unCIString = "IntendedUseDescription"},rawMarkdown "The intended use of this report, as stipulated, is to evaluate the subject properties and provide an assessment of Replacement Value Comparable of a painting for the intended use of Insurance Coverage.  \\footnote{See below for definition of Replacement Value}"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 6}) Path_Pair) (CIString {unCIString = "TypeOfValueDescription"},rawMarkdown "This report serves as documented evidence of this appraiser having witnessed the subject properties, and identified, described and valued the subject properties in the context of determining Replacement Value (Comparable).  No other use of this report is valid or condoned.  \\footnote{See below for definition of Replacement Value}"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 7}) Path_Pair) (CIString {unCIString = "LOTInspectedBy"},rawMarkdown "Garrett Goldfield, ASA"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 8}) Path_Pair) (CIString {unCIString = "certify"},rawMarkdown "certify"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 9}) Path_Pair) (CIString {unCIString = "appraisers"},rawMarkdown "appraisers"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 10}) Path_Pair) (CIString {unCIString = "agree"},rawMarkdown "agree"), subForest = []},
                           Node {rootLabel = PV_AbbrevPairs_AbbrevPair (Path_At (AbbrevPairID {unAbbrevPairID = 11}) Path_Pair) (CIString {unCIString = "ValueTypeDescription"},rawMarkdown "The intended use of this report, as stipulated, is to evaluate the subject properties and provide an assessment of Fair Market Value Comparable of a collection of fine art for the intended use of Charitable Donation.  \\footnote{See below for definition of Fair Market Value}"), subForest = []}]
               actual :: [Tree PV_AbbrevPairs]
               actual = pvTree (reportAbbrevs Report.report) in
           TestLabel "pvTree order" $ TestCase $ assertEqual "pvTree order" expected actual
{-
         , let p :: Path_Report ReportElem
               p = head (pathsOf Report.report (undefined :: Proxy ReportElem))
               e :: ReportElem
               e = head (toListOf (toLens p) Report.report)
               expected :: [PV_ReportElem]
               expected = [PV_ReportElem_ReportElem idPath e]
               -- Convert a report into a tree of PVs, used to implement an editor.
               actual :: [PV_ReportElem]
               actual = [] {-$(runTypeGraphT (editor ''ReportElem [|e|]) [ConT ''ReportMap] :: ExpQ)-} in
           TestCase $ assertEqual "editor" expected actual
-}
         ]
  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r
