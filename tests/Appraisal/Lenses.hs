{-# LANGUAGE PatternGuards, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Appraisal.Lenses
    ( lens_ReportImage_SrcURI
    , pathTo_lens_ReportMap_ReportImages
    ) where

import Appraisal.File (File(File, fileSource), FileSource(TheURI), parseURI, URI)
import Appraisal.ImageFile (ImageFile(..))
import Appraisal.Report (ReportElemID)
import Appraisal.ReportImage (ReportImage(picOriginal), ReportImages)
import Appraisal.ReportMap (ReportID(..), ReportMap(..))
import Appraisal.ReportPaths
import Appraisal.Utils.Debug (trace'')
import Control.Lens (prism', Traversal', Lens', lens)
import Data.Text as Text (Text)
import Data.Typeable (cast)
import Language.Haskell.TH.Path.Core (mat, Path_Map(Path_Look), Path_OMap(Path_OMap, Path_At))
import Language.Haskell.TH.Path.Order (lens_omat)
import Network.URI (nullURI)
import Text.Blaze hiding (Markup)
import Text.JSON.Generic (Typeable)

lens_ReportMap_itemImages :: ReportID -> ReportElemID -> Traversal' ReportMap ReportImages
lens_ReportMap_itemImages rid i = lens_ReportMap_unReportMap . mat rid . lens_Report_reportBody . lens_omat i . lens_ReportElem_elemItem . lens_Item_images

lens_ReportImage_SrcURI :: Lens' ReportImage String
lens_ReportImage_SrcURI = lens (srcURI . picOriginal) (\ri uri -> ri { picOriginal = checkURI (trace'' "checkURI" uri) })
  where
        srcURI :: Maybe (Either URI ImageFile) -> String
        srcURI Nothing = ""
        srcURI (Just (Left uri)) = show uri
        srcURI (Just (Right (ImageFile {imageFile = File {fileSource = Just (TheURI sURI)}}))) = sURI
        srcURI (Just (Right _)) = "" -- this means there was no external source,
                                     -- something we need to account for with image upload.
        checkURI :: String -> Maybe (Either URI ImageFile)
        checkURI s | s == "" = Just (Left nullURI)
        checkURI s =
          case parseURI s of
            Nothing -> Nothing
            -- This may be the old or the new URI, the IO code will verify
            Just uri' -> Just (Left uri')

_dataToValue :: (Show a, Typeable a) => a -> AttributeValue
_dataToValue d | Just t <- cast d = toValue (t :: Text)
               | Just s <- cast d = toValue (s :: String)
               | Just b <- cast d = toValue (b :: Bool)
               | Just i <- cast d = toValue (i :: Int)
               | Just ii <- cast d = toValue (ii :: Integer)
               | Just f <- cast d = toValue (f :: Float)
               | Just dd <- cast d = toValue (dd :: Double)
               | Just c <- cast d = toValue (c :: Char)
               | otherwise        = error $ "dataToValue -- Unsupported value: " ++ show d

pathTo_lens_ReportMap_ReportImages :: Path_ReportMap a -> Traversal' ReportMap ReportImages
pathTo_lens_ReportMap_ReportImages rmp =
  case rmp of
    Path_ReportMap_unReportMap  -- Whew! what a pattern!  No wonder it took so damn long.
      (Path_Look rid   -- This is here because sets have been simple field updates until now.
       (Path_Report_View
        (Path_ReportView__reportBody   -- Clearly they need more sophistication
         (Path_At n
          (Path_ReportElem_elemItem
           (Path_Item_images
            Path_OMap)))))) -> lens_ReportMap_itemImages rid n
    _ -> prism' undefined (const Nothing)
