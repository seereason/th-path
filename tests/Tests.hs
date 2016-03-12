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

import Appraisal.Markup (rawMarkdown, rawHtml)
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.File
import Appraisal.Permissions
import Appraisal.Image
import Appraisal.ImageFile
import Appraisal.IntJS (JSONText(..))
import Appraisal.ReportItem
import Appraisal.ReportInstances
import Appraisal.ReportMap
import Appraisal.Utils.CIString
import Control.Lens (Lens', toListOf, view)
import Control.Monad.Readers (MonadReaders)
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile)
import Data.List (sort)
import Data.Map (fromList)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy)
import Data.Tree
import Data.UserId
import Data.UUID (fromString, UUID)
import Debug.Trace
import Editor (editor)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.Order (Path_OMap(..), fromPairs)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.Ppr as TH (Ppr)
import Language.Haskell.TH.PprLib as TH (text)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import Language.Haskell.TH.TypeGraph.TypeInfo (startTypes, TypeInfo)
import Language.Haskell.TH.TypeGraph.TypeGraph (TypeGraph, typeGraphVertex)
import System.Exit
import Test.HUnit
import Tests.Data (peekReportView, peekAbbrevPairs)
import Text.PrettyPrint (text)

import Appraisal.ReportTH (decs)
import Report (report, image)

import ReportPaths

newtype F a = F {unF :: a}

instance Show a => TH.Ppr (F (Forest a)) where
    ppr = TH.text . drawForest . forestMap show . unF

instance Show a => TH.Ppr (Tree a) where
    ppr = TH.text . drawTree . treeMap show

elem :: ReportElem
elem = let p = head (pathsOf Report.report (undefined :: Proxy ReportElem)) in
       head (toListOf (toLens p) Report.report)

assertEqual' :: (Eq a, Show a) => String -> a -> a -> Test
assertEqual' label expected actual = TestLabel label $ TestCase $ assertEqual label expected actual
assertString' :: String -> String -> Test
assertString' label string = TestLabel label $ TestCase $ assertString string

main :: IO ()
main = do
  r <- runTestTT $ TestList $
         [ let expected = [Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) Path_ReportElem)),
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
           assertEqual' "reportelems" expected actual

         , let expected = "Peek_ReportView_Markup (Path_ReportView__reportContractDate Path_Markup) (Just (rawMarkdown \"\"))"
               actual = show (Peek_ReportView_Markup (Path_ReportView__reportContractDate Path_Markup) (Just (rawMarkdown ""))) in
           assertEqual' "Show instance" expected actual

         , let expected :: Forest (Peek ReportView)
               expected = peekReportView
               actual :: Forest (Peek ReportView)
               actual = peek (head (toListOf (toLens (Path_Report_View (idPath :: Path_ReportView ReportView))) Report.report) :: ReportView) in
           assertEqual' "peek ReportView" expected actual
           -- assertEqual' "peek ReportView" (pprint expected) (pprint actual)

         , let expected :: Peek Report
               expected = Peek_Report_ReportElem (Path_Report_View (Path_ReportView__reportBody (Path_At (ReportElemID {unReportElemID = 0}) Path_ReportElem))) (Just (ReportParagraph {elemText = (rawMarkdown "## Market Overview\n\nThe collection consists of a group of nine contemporary Chinese jade and agate sculptures, one glass sculpture of a horse and three ink paintings which were purchased in the United States and in China. \n\nIn recent years the rising affluence of mainland Chinese buyers has fueled the market for both antique and contemporary jade at auction and at  retail venues. There are two types of jade, nephrite and jadeite. Nephrite has been used in China since prehistoric times for weapons and ritual objects. It wasn\8217t until the 18th century that large quantities of jadeite were imported from Burma, the country recognized as having some of the best jadeite in the world. The surface of jadeite tends to be vitreous or glassy while nephrite\8217s surface tends to appear more waxy. Pale colors such as lavender, light green, yellow are desirable, and the combination of colors such as lavender, white and green even more so.  Design, carving technique, and skillful exploitation of the jade\8217s colors are important characteristics of value. The same value characteristics  pertain to agate carving. Contemporary jade and agate carvings are typically found at decorative art galleries and regional auction houses that cater to enthusiasts of Asian collectibles. \n\nThe three ink paintings in the collection were acquired in mainland China in 2002. Only one of the artists, Xiao Shunzhi, has an international market. Market data for the other two artists, Liu Zuozhong and Li Jialin was not available, and the valuation of their works is based on comparable works by Chinese artists available in galleries in the United States and China. \n\n\n\n \n\t")}))
               actual :: Peek Report
               actual = let p = head (pathsOf Report.report (undefined :: Proxy ReportElem)) in
                        Peek_Report_ReportElem p (Just (head (toListOf (toLens p) Report.report))) in
            assertEqual' "Peek_Report_ReportElem" expected actual

         , let expected :: Forest (Peek AbbrevPairs)
               expected = peekAbbrevPairs
               actual :: Forest (Peek AbbrevPairs)
               actual = peek (reportAbbrevs Report.report) in
           assertEqual' "peekNodes order" expected actual
           -- assertEqual' "peekNodes order" (pprint expected) (pprint actual)
{-
         , let p :: Path_Report ReportElem
               p = head (pathsOf Report.report (undefined :: Proxy ReportElem))
               e :: ReportElem
               e = head (toListOf (toLens p) Report.report)
               expected :: [Peek_ReportElem]
               expected = [Peek_ReportElem_ReportElem idPath e]
               -- Convert a report into a tree of Peeks, used to implement an editor.
               actual :: [Peek_ReportElem]
               actual = [] {-$(runTypeGraphT (editor ''ReportElem [|e|]) [ConT ''ReportMap] :: ExpQ)-} in
           assertEqual' "editor" expected actual
-}
       , assertEqual' "toLens3" (toListOf (toLens (Path_ImageSize_dim (idPath :: Path_Dimension Dimension))) (picSize image)) [dim (picSize image) :: Dimension]
       , assertEqual' "toLens4" (toListOf (toLens (Path_ImageSize_units (idPath :: Path_Units Units))) (picSize image)) [units (picSize image)]
       , assertEqual' "toLens5" (toListOf (toLens (Path_ReportImage_View (idPath :: Path_ReportImageView ReportImageView))) image) [view viewLens image]
       , assertEqual' "toLens6" (toListOf (toLens (Path_ReportImageView__picCrop (idPath :: Path_ImageCrop ImageCrop))) (view viewLens image)) [picCrop image]
       , assertEqual' "toLens7" (toListOf (toLens ((Path_ReportImage_View (idPath :: Path_ReportImageView ReportImageView)) :.:
                                                    (Path_ReportImageView__picCrop (idPath :: Path_ImageCrop ImageCrop)))) image) [picCrop image]
       , assertEqual' "toLens8" ((Path_ReportImage_View (idPath :: Path_ReportImageView Bool) :.: Path_ReportImage_View (idPath :: Path_ReportImageView Bool)) ==
                                 (Path_ReportImage_View (idPath :: Path_ReportImageView Bool) :.: Path_ReportImage_View (idPath :: Path_ReportImageView Bool))) True
       ]

  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r
