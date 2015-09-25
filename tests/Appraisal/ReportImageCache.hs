-- | Operations to build or retrieve the image types used in a report
-- - original, thumbnail, printed, etc.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Appraisal.ReportImageCache
    ( originalKey
    , uprightKey
    , editedKey
    , thumbKey
    , printedKey
    , enlargedKey
    , updateReportImage
    ) where

import Appraisal.Cache (MonadCache, cacheInsert)
import Appraisal.Config (printerDPI, screenDPI)
import Appraisal.File (MonadFileCacheTop, URI)
import Appraisal.Image (Dimension(TheHeight), ImageSize(ImageSize, dim, size, units), Units(Inches))
import Appraisal.ImageCache (ImageKey(..))
import Appraisal.ImageFile (ImageFile, imageFileFromURI, imageFileFromBytes)
import Appraisal.ReportImage (enlargedSize, ReportImage(Pic, picOriginal, picCrop, picSize))
import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO)
import qualified Data.ByteString as P (ByteString)

-- | This is what I measured on the current site
thumbSize :: ImageSize
thumbSize = ImageSize {dim = TheHeight, size = 1.0, units = Inches}

-- | This describes how the keys we use are constructed from the
-- information in a ReportImage.
originalKey :: ReportImage -> ImageKey
originalKey (Pic {picOriginal = Just (Right x)}) = ImageOriginal x
originalKey (Pic {picOriginal = x}) = error $ "originalKey: " ++ show x

uprightKey :: ReportImage -> ImageKey
uprightKey img@(Pic {picOriginal = Just (Right _)}) = ImageUpright (originalKey img)
uprightKey (Pic {picOriginal = x}) = error $ "uprightKey: " ++ show x

editedKey :: ReportImage -> ImageKey
editedKey img@(Pic {picCrop = crop}) = ImageCropped crop (uprightKey img)

thumbKey :: ReportImage -> ImageKey
thumbKey img = ImageScaled thumbSize screenDPI (editedKey img)

printedKey :: ReportImage -> ImageKey
printedKey img@(Pic {picSize = sz}) = ImageScaled sz printerDPI (editedKey img)

enlargedKey :: ReportImage -> ImageKey
enlargedKey img@(Pic {picOriginal = Just (Right x)}) = ImageScaled (enlargedSize x) printerDPI (editedKey img)
enlargedKey (Pic {picOriginal = x}) = error $ "enlargedKey: " ++ show x

-- | Set or change the original image for a ReportImage, inserting it
-- into the cache if necessary.
updateReportImage :: (MonadCatch m, MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                     Either URI P.ByteString -> ReportImage -> m ReportImage
updateReportImage src old = do
  img <- either imageFileFromURI imageFileFromBytes src
  (_ :: ImageFile) <- cacheInsert (ImageOriginal img)
  return $ old {picOriginal = Just (Right img)}
