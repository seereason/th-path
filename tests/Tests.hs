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
import Control.Lens (toListOf, view)
-- import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
-- import Data.ByteString.UTF8 (toString)
import Data.Proxy (Proxy(Proxy))
import Data.Tree
import Language.Haskell.TH
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.Ppr as TH (Ppr)
import Language.Haskell.TH.PprLib as TH (text)
import System.Exit
import Test.HUnit

import ReportPaths
import Tests.Data (peekReportView, peekAbbrevPairs, peekLabels)
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
      expected = [Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 1}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 2}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 3}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 4}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 5}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 6}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 7}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 8}) Path_ReportElem)),
                  Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 9}) Path_ReportElem))]
      actual = take 10 (paths (Proxy :: Proxy Univ) Report.report (Proxy :: Proxy ReportElem) (:) [])

testShowInstance :: Test
testShowInstance =
    assertEqual' "Show instance" expected actual
    where
      expected = "Peek_ReportView_Markup (Path_ReportView__reportContractDate Path_Markup) (Just (rawMarkdown \"\"))"
      actual = show (Peek_ReportView_Markup (Path_ReportView__reportContractDate Path_Markup) (Just (rawMarkdown "")))

testPeekReportView :: Test
testPeekReportView =
    assertEqual' "peek ReportView" expected actual
    -- assertEqual' "peek ReportView" (pprint expected) (pprint actual)
    where
      expected :: Forest (UPeek Univ ReportView)
      expected = peekReportView
      actual :: Forest (UPeek Univ ReportView)
      actual = peekTree (Proxy :: Proxy Univ) (unU (head (toListOf (toLens (UPath_Report_View (idPath :: UPath_ReportView))) Report.report) :: Univ))

testLabels :: Test
testLabels =
    assertEqual' "peek labels" expected actual
    where
      expected :: Forest (Maybe String)
      expected = peekLabels
      actual :: Forest (Maybe String)
      actual = map (fmap describe) peekReportView

testPeekReport :: Test
testPeekReport =
    assertEqual' "Peek_Report_ReportElem" expected actual
    where
      expected :: UPeek Univ Report
      expected = UPeek_Report (UPath_Report_View (UPath_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) UPath_ReportElem))) (Just (u (ReportParagraph {elemText = (rawMarkdown "## Market Overview\n\nThe collection consists of a group of nine contemporary Chinese jade and agate sculptures, one glass sculpture of a horse and three ink paintings which were purchased in the United States and in China. \n\nIn recent years the rising affluence of mainland Chinese buyers has fueled the market for both antique and contemporary jade at auction and at  retail venues. There are two types of jade, nephrite and jadeite. Nephrite has been used in China since prehistoric times for weapons and ritual objects. It wasn\8217t until the 18th century that large quantities of jadeite were imported from Burma, the country recognized as having some of the best jadeite in the world. The surface of jadeite tends to be vitreous or glassy while nephrite\8217s surface tends to appear more waxy. Pale colors such as lavender, light green, yellow are desirable, and the combination of colors such as lavender, white and green even more so.  Design, carving technique, and skillful exploitation of the jade\8217s colors are important characteristics of value. The same value characteristics  pertain to agate carving. Contemporary jade and agate carvings are typically found at decorative art galleries and regional auction houses that cater to enthusiasts of Asian collectibles. \n\nThe three ink paintings in the collection were acquired in mainland China in 2002. Only one of the artists, Xiao Shunzhi, has an international market. Market data for the other two artists, Liu Zuozhong and Li Jialin was not available, and the valuation of their works is based on comparable works by Chinese artists available in galleries in the United States and China. \n\n\n\n \n\t")})))
      actual :: UPeek Univ Report
      actual = let p = head (upaths (Proxy :: Proxy Univ) Report.report (:) []) in
               UPeek_Report p (Just (head (toListOf (toLens p) Report.report)))

testPeekOrder :: Test
testPeekOrder =
    assertEqual' "peekNodes order" expected actual
    -- assertEqual' "peekNodes order" (pprint expected) (pprint actual)
    where
      expected :: Forest (UPeek Univ AbbrevPairs)
      expected = peekAbbrevPairs
      actual :: Forest (UPeek Univ AbbrevPairs)
      actual = peekTree (Proxy :: Proxy Univ) (reportAbbrevs Report.report)

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
          actual = map unU (toListOf (toLens (upaths (Proxy :: Proxy Univ) (:) [] abbrevs !! 3)) abbrevs) in
      assertEqual' "testUPaths 2" expected actual
    , let expected = ImageSize TheHeight 3.0 Inches
          [actual] = map unU (toListOf (toLens imageSizePath) Report.report) in
      assertEqual' "testUPaths 3" expected actual
    , let expected = [UPath_ImageSize_dim UPath_Dimension,
                      UPath_ImageSize_size UPath_Double,
                      UPath_ImageSize_units UPath_Units]
          [usize] = toListOf (toLens imageSizePath) Report.report
          actual = upaths (Proxy :: Proxy Univ) (:) [] (unU usize :: ImageSize) in
      assertEqual' "testUPaths 4" expected actual
    , let expected = [UPeek_ImageSize (UPath_ImageSize_dim UPath_Dimension) (Just (U6 TheHeight)),
                      UPeek_ImageSize (UPath_ImageSize_size UPath_Double) (Just (U5 3.0)),
                      UPeek_ImageSize (UPath_ImageSize_units UPath_Units) (Just (U9 Inches))]
          [usize] = map unU (toListOf (toLens imageSizePath) Report.report) :: [ImageSize]
          actual = peekRow (Proxy :: Proxy Univ) usize in
      assertEqual' "testUPaths 5" expected actual
    ]
    where imageSizePath =
                 UPath_Report_View
                  (UPath_ReportView__reportBody
                   (Path_At
                    (ReportElemID {unReportElemID = 1})
                    (UPath_ReportElem_elemItem
                     (UPath_Item_images
                      (Path_At
                       (ReportImageID {unReportImageID = 0})
                       (UPath_ReportImage_View
                        (UPath_ReportImageView__picSize
                         (UPath_SaneSizeImageSize_View idPath))))))))

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
         , assertEqual' "toLens3" (toListOf (toLens (Path_ImageSize_dim (idPath :: Path_Dimension Dimension))) (picSize image)) [dim (picSize image) :: Dimension]
         , assertEqual' "toLens4" (toListOf (toLens (Path_ImageSize_units (idPath :: Path_Units Units))) (picSize image)) [units (picSize image)]
         , assertEqual' "toLens5" (toListOf (toLens (Path_ReportImage_View (idPath :: Path_ReportImageView ReportImageView))) image) [view viewLens image]
         , assertEqual' "toLens6" (toListOf (toLens (Path_ReportImageView__picCrop (idPath :: Path_ImageCrop ImageCrop))) (view viewLens image)) [picCrop image]
         , assertEqual' "toLens7" (toListOf (toLens ((Path_ReportImage_View (idPath :: Path_ReportImageView ReportImageView)) :.:
                                                     (Path_ReportImageView__picCrop (idPath :: Path_ImageCrop ImageCrop)))) image) [picCrop image]
         , assertEqual' "toLens8" ((Path_ReportImage_View (idPath :: Path_ReportImageView Bool) :.: Path_ReportImage_View (idPath :: Path_ReportImageView Bool)) ==
                                   (Path_ReportImage_View (idPath :: Path_ReportImageView Bool) :.: Path_ReportImage_View (idPath :: Path_ReportImageView Bool))) True
         , assertEqual' "label 1" (Just "Report Intended Use") (describe (Peek_Report_MaybeReportIntendedUse (Path_Report_View (Path_ReportView__reportIntendedUse Path_MaybeReportIntendedUse)) Nothing))
         -- There is a custom Describe instance for Markup that returns Nothing, so
         --    > describe (Just ("ReportView","ReportView",Right "_reportFooter")) (Peek_Markup_Markup Path_Markup undefined)
         -- returns Nothing.
         , assertEqual' "label 2" (Just "Report Footer") (describe (Peek_Report_Markup (Path_Report_View (Path_ReportView__reportFooter Path_Markup)) Nothing))
         , assertEqual' "label 3" (Just "Letter of Transmittal") (describe
                                                                           (Peek_Report_Text (Path_Report_View (Path_ReportView__reportLetterOfTransmittal (Path_Markup_markdownText Path_Text))) Nothing))
         , assertEqual' "label 4" (Just "Letter of Transmittal") (describe (Peek_ReportView_Text (Path_ReportView__reportLetterOfTransmittal (Path_Markup_markdownText Path_Text)) Nothing))
         , assertEqual' "Report letter of transmittal field"
                                  (Just "Letter of Transmittal") (describe' (Just $(fieldStrings (''ReportView, 'ReportView, Right '_reportLetterOfTransmittal)))
                                                                           (Peek_ReportView_JSONText (Path_ReportView__reportLetterOfTransmittal (Path_Markup_markdownText (Path_Text_View Path_JSONText))) Nothing))
         ]

  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r
