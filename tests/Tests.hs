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

import Appraisal.Utils.CIString
import Appraisal.Markup (rawMarkdown)
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.Image
import Appraisal.ReportInstances
import Control.Lens (iso, toListOf, view)
-- import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
-- import Data.ByteString.UTF8 (toString)
import Data.Maybe (fromJust, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Tree
import Language.Haskell.TH
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.Ppr as TH (Ppr)
import Language.Haskell.TH.PprLib as TH (text)
import System.Exit
import Test.HUnit hiding (path)

import ReportPaths
import Tests.Data (pathReportView, peekReportView, peekAbbrevPairs, pathLabels)
import Tests.Report as Report (report, image)

{-
newtype F a = F {unF :: a}

instance Show a => TH.Ppr (F (Forest a)) where
    ppr = TH.text . drawForest . forestMap show . unF
-}

instance Show a => TH.Ppr (Tree a) where
    ppr = TH.text . drawTree . treeMap show

assertEqual' :: (Eq a, Show a) => String -> a -> a -> Test
assertEqual' label expected actual = TestLabel label $ TestCase $ assertEqual label expected actual
-- assertString' :: String -> String -> Test
-- assertString' label string = TestLabel label $ TestCase $ assertString string

testReportElems :: Test
testReportElems =
    assertEqual' "reportelems" expected actual
    where
      expected = [Path_To Proxy (UPath_ReportView__reportFolder Path_Self),
                  Path_To Proxy (UPath_ReportView__reportName UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportDate UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportContractDate UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportInspectionDate UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportEffectiveDate UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportAuthors Path_OMap),
                  Path_To Proxy (UPath_ReportView__reportPreparer UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportPreparerEIN UPath_Markup),
                  Path_To Proxy (UPath_ReportView__reportPreparerAddress UPath_Markup)]
      actual :: [UPath Univ Report]
      actual = let Node _ [Node _ xs] = upeekTree (Proxy :: Proxy Univ) Report.report in
               map (upeekPath . rootLabel) (take 10 xs)

testShowInstance :: Test
testShowInstance =
    assertEqual' "Show instance" expected actual
    where
      expected = "UPeek_ReportView (UPath_ReportView__reportContractDate UPath_Markup) (Just (U13 (rawMarkdown \"\")))"
      actual = show (UPeek_ReportView (UPath_ReportView__reportContractDate UPath_Markup) (Just (u (rawMarkdown ""))))

testPeekReportView :: Test
testPeekReportView =
    assertEqual' "peek ReportView" expected actual
    -- assertEqual' "peek ReportView" (pprint expected) (pprint actual)
    where
      expected :: Tree (UPeek Univ ReportView)
      expected = peekReportView
      actual :: Tree (UPeek Univ ReportView)
      actual = let [Just reportview] = map unU' (toListOf (toLens (Path_To Proxy idPath)) Report.report :: [Univ]) in
               upeekTree (Proxy :: Proxy Univ) reportview

testLabels :: Test
testLabels =
    assertEqual' "path labels" expected actual
    where
      expected :: Tree (Maybe String)
      expected = pathLabels
      actual :: Tree (Maybe String)
      actual = pathReportView

testPeekReport :: Test
testPeekReport =
    assertEqual' "Peek_Report_ReportElem" expected actual
    where
      expected :: ReportElem
      expected = ReportParagraph {elemText = (rawMarkdown "## Market Overview\n\nThe collection consists of a group of nine contemporary Chinese jade and agate sculptures, one glass sculpture of a horse and three ink paintings which were purchased in the United States and in China. \n\nIn recent years the rising affluence of mainland Chinese buyers has fueled the market for both antique and contemporary jade at auction and at  retail venues. There are two types of jade, nephrite and jadeite. Nephrite has been used in China since prehistoric times for weapons and ritual objects. It wasn\8217t until the 18th century that large quantities of jadeite were imported from Burma, the country recognized as having some of the best jadeite in the world. The surface of jadeite tends to be vitreous or glassy while nephrite\8217s surface tends to appear more waxy. Pale colors such as lavender, light green, yellow are desirable, and the combination of colors such as lavender, white and green even more so.  Design, carving technique, and skillful exploitation of the jade\8217s colors are important characteristics of value. The same value characteristics  pertain to agate carving. Contemporary jade and agate carvings are typically found at decorative art galleries and regional auction houses that cater to enthusiasts of Asian collectibles. \n\nThe three ink paintings in the collection were acquired in mainland China in 2002. Only one of the artists, Xiao Shunzhi, has an international market. Market data for the other two artists, Liu Zuozhong and Li Jialin was not available, and the valuation of their works is based on comparable works by Chinese artists available in galleries in the United States and China. \n\n\n\n \n\t")}
      actual :: ReportElem
      actual = let path = Path_To Proxy (UPath_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) UPath_ReportElem))
                   [Just x] = map unU' (toListOf (toLens path) Report.report :: [Univ]) :: [Maybe ReportElem] in
               x

testPeekOrder :: Test
testPeekOrder =
    assertEqual' "peekNodes order" expected actual
    -- assertEqual' "peekNodes order" (pprint expected) (pprint actual)
    where
      expected :: Tree (UPeek Univ AbbrevPairs)
      expected = peekAbbrevPairs
      actual :: Tree (UPeek Univ AbbrevPairs)
      actual = upeekTree (Proxy :: Proxy Univ) (reportAbbrevs Report.report)

testUPaths :: Test
testUPaths =
    TestList
    [ let expected :: [UPath Univ AbbrevPairs]
          expected = [Path_At (AbbrevPairID {unAbbrevPairID = 0}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 1}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 2}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 3}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 4}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 5}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 6}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 7}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 8}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 9}) Path_Pair,
                      Path_At (AbbrevPairID {unAbbrevPairID = 10}) Path_Pair]
          actual :: [UPath Univ AbbrevPairs]
          actual = upaths (Proxy :: Proxy Univ) ((:)) [] (reportAbbrevs Report.report) in
      assertEqual' "testUPaths 1" expected actual
    , let expected :: [AbbrevPair]
          expected = [(CIString "USPAP", rawMarkdown "_Uniform Standards of Professional Appraisal Practice_, the 2012-2013 Edition (USPAP)")]
          abbrevs = reportAbbrevs Report.report
          actual :: [AbbrevPair]
          actual = mapMaybe unU' (toListOf (toLens (upaths (Proxy :: Proxy Univ) (:) [] abbrevs !! 3)) abbrevs :: [Univ]) in
      assertEqual' "testUPaths 2" expected actual
    , let expected = ImageSize TheHeight 3.0 Inches
          [actual] = mapMaybe unU' (toListOf (toLens imageSizePath) Report.report :: [Univ]) in
      assertEqual' "testUPaths 3" expected actual
    , let expected = [UPath_ImageSize_dim idPath,
                      UPath_ImageSize_size idPath,
                      UPath_ImageSize_units idPath]
          [usize] = toListOf (toLens imageSizePath) Report.report :: [Univ]
          actual = upaths (Proxy :: Proxy Univ) (:) [] (fromJust (unU' usize) :: ImageSize) in
      assertEqual' "testUPaths 4" expected actual
    , let expected = Node {rootLabel = UPeek_ImageSize UPath_ImageSize Nothing,
                           subForest = [Node {rootLabel = UPeek_ImageSize (UPath_ImageSize_dim idPath) (Just (U6 TheHeight)), subForest = []},
                                        Node {rootLabel = UPeek_ImageSize (UPath_ImageSize_size idPath) (Just (U5 3.0)), subForest = []},
                                        Node {rootLabel = UPeek_ImageSize (UPath_ImageSize_units idPath) (Just (U9 Inches)), subForest = []}]}
          [usize] = mapMaybe unU' (toListOf (toLens imageSizePath) Report.report :: [Univ]) :: [ImageSize]
          actual = upeekRow (Proxy :: Proxy Univ) usize in
      assertEqual' "testUPaths 5" expected actual
    ]
    where imageSizePath =
                 Path_To Proxy
                  (UPath_ReportView__reportBody
                   (Path_At
                    (ReportElemID {unReportElemID = 1})
                    (UPath_ReportElem_elemItem
                     (UPath_Item_images
                      (Path_At
                       (ReportImageID {unReportImageID = 0})
                       (Path_To Proxy
                        (UPath_ReportImageView__picSize
                         (Path_To Proxy idPath))))))))

main :: IO ()
main = do
  r <- runTestTT $ TestList $
         [ testReportElems
         , testShowInstance
         , testPeekReportView
         , testLabels
         , testPeekReport
         , testPeekOrder
         , testUPaths
         , assertEqual' "toLens3" (mapMaybe unU' (toListOf (toLens (UPath_ImageSize_dim (idPath))) (picSize image) :: [Univ])) [dim (picSize image) :: Dimension]
         , assertEqual' "toLens4" (mapMaybe unU' (toListOf (toLens (UPath_ImageSize_units (idPath))) (picSize image) :: [Univ])) [units (picSize image)]
         , assertEqual' "toLens5" (mapMaybe unU' (toListOf (toLens (Path_To Proxy (idPath))) image :: [Univ])) [view viewLens image]
         , assertEqual' "toLens6" (mapMaybe unU' (toListOf (toLens (UPath_ReportImageView__picCrop (idPath))) (view viewLens image) :: [Univ])) [picCrop image]
{-
         , assertEqual' "toLens7" (toListOf (toLens ((UPath_ReportImage_View (idPath :: UPath_ReportImageView {-ReportImageView-})) :.:
                                                     (UPath_ReportImageView__picCrop (idPath :: UPath_ImageCrop {-ImageCrop-})))) image) [picCrop image :: ImageCrop]
         , assertEqual' "toLens8" ((UPath_ReportImage_View (idPath :: UPath_ReportImageView) :.: UPath_ReportImage_View (idPath :: UPath_ReportImageView)) ==
                                   (UPath_ReportImage_View (idPath :: UPath_ReportImageView) :.: UPath_ReportImage_View (idPath :: UPath_ReportImageView))) True
-}
         , assertEqual' "label 1" (Just "Report Intended Use") (describe (Path_To (Proxy :: Proxy Report) (UPath_ReportView__reportIntendedUse idPath)))
         -- There is a custom Describe instance for Markup that returns Nothing, so
         --    > describe (Just ("ReportView","ReportView",Right "_reportFooter")) (Peek_Markup_Markup Path_Markup undefined)
         -- returns Nothing.
         , assertEqual' "label 2" (Just "Report Footer") (describe (Path_To (Proxy :: Proxy Report) (UPath_ReportView__reportFooter idPath)))
         , assertEqual' "label 3" (Just "Letter of Transmittal") (describe (Path_To (Proxy :: Proxy Report) (UPath_ReportView__reportLetterOfTransmittal (UPath_Markup_markdownText idPath))))
         , assertEqual' "label 4" (Just "Letter of Transmittal") (describe (UPath_ReportView__reportLetterOfTransmittal (UPath_Markup_markdownText idPath)))
         , assertEqual' "Report letter of transmittal field"
                                  (Just "Letter of Transmittal") (describe' (Just (nameBase '_reportLetterOfTransmittal))
                                                                            (UPath_ReportView__reportLetterOfTransmittal (UPath_Markup_markdownText (Path_To Proxy UPath_JSONText))))
         , assertEqual' "lens to turn Univ lens into regular lens"
                        (view (iso ((fromJust . unU') :: Univ -> Int) (u :: Int -> Univ)) (u (123 :: Int)))
                        123
         , assertEqual' "lens to turn Univ lens into regular lens"
                        (view (iso (u :: Int -> Univ) ((fromJust . unU') :: Univ -> Int)) 123)
                        (u (123 :: Int))
         ]

  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r
