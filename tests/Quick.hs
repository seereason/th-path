{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Appraisal.File
import Appraisal.Image
import Appraisal.ImageFile
import Appraisal.LaTeX
import Appraisal.Markup
import Appraisal.ReportImage
import Appraisal.ReportInstances
import Control.Lens (toListOf, view)
import Control.Monad.Readers (askPoly)
import Text.LaTeX (LaTeX)
import Data.UUID.Types (UUID)
import Language.Haskell.TH (pprint, runQ, runIO)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Decs (allDecs)
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.TypeGraph.TypeGraph (TypeGraph)
import System.Exit
import Test.HUnit

$([t|ReportImage|] >>= runTypeGraphT (do askPoly >>= \(g :: TypeGraph) -> (runQ . runIO . putStrLn . pprint) g
                                         allDecs) . (: []))

image :: ReportImage
image = Pic {picSize = ImageSize {dim = TheArea, size = 6.0, units = Inches}, picCrop = ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 0, rightCrop = 0, rotation = 0}, picCaption = rawMarkdown "", picOriginal = Just (Right (ImageFile {imageFile = File {fileSource = Nothing, fileChksum = "b2ba73ef42b951e095eb927c0fc4d45b", fileMessages = []}, imageFileType = JPEG, imageFileWidth = 2048, imageFileHeight = 1536, imageFileMaxVal = 255})), picEditedDeprecated = Nothing, picThumbDeprecated = Nothing, picPrinterDeprecated = Nothing, picMustEnlarge = False, picEnlargedDeprecated = Nothing}

main :: IO ()
main = do
  r <- runTestTT $ TestList $
         [ assertEqual' "toLens3" (toListOf (toLens (Path_ImageSize_dim (idPath :: Path_Dimension Dimension))) (picSize image)) [dim (picSize image) :: Dimension]
         , assertEqual' "toLens4" (toListOf (toLens (Path_ImageSize_units (idPath :: Path_Units Units))) (picSize image)) [units (picSize image)]
         , assertEqual' "toLens5" (toListOf (toLens (Path_ReportImage_View (idPath :: Path_ReportImageView ReportImageView))) image) [view viewLens image]
         , assertEqual' "toLens6" (toListOf (toLens (Path_ReportImageView__picCrop (idPath :: Path_ImageCrop ImageCrop))) (view viewLens image)) [picCrop image]
         , assertEqual' "toLens7" (toListOf (toLens ((Path_ReportImage_View (idPath :: Path_ReportImageView ReportImageView)) :.:
                                                     (Path_ReportImageView__picCrop (idPath :: Path_ImageCrop ImageCrop)))) image) [picCrop image]
         , assertEqual' "toLens8" ((Path_ReportImage_View (idPath :: Path_ReportImageView Bool) :.: Path_ReportImage_View (idPath :: Path_ReportImageView Bool)) ==
                                   (Path_ReportImage_View (idPath :: Path_ReportImageView Bool) :.: Path_ReportImage_View (idPath :: Path_ReportImageView Bool))) True
         , assertEqual' "label01"
             (Just "Letter of Transmittal")
             (describe (Just (''ReportView, 'ReportView, Right '_reportLetterOfTransmittal)) (Peek_ReportView_JSONText (Path_ReportView__reportLetterOfTransmittal (Path_Markup_markdownText (Path_Text_View Path_JSONText))) Nothing)) ]
  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r

assertEqual' :: (Eq a, Show a) => String -> a -> a -> Test
assertEqual' label expected actual = TestLabel label $ TestCase $ assertEqual label expected actual
assertString' :: String -> String -> Test
assertString' label string = TestLabel label $ TestCase $ assertString string
