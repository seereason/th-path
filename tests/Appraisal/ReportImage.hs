{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall -fwarn-incomplete-patterns -fno-warn-orphans #-}
-- |Manage the images used by the report generator - original, edited, thumbnail, and printable.
module Appraisal.ReportImage
    ( ReportImage(..)
    , MaybeImageFile
    , enlargedSize
    , ReportImages
    , ReportImageID(..)
    ) where

import Debug.Trace

import Appraisal.Image (ImageSize(..), ImageCrop(..), Dimension(..), Units(..))
import Appraisal.ImageFile (ImageFile(..))
import Appraisal.IntJS (deriveOrderJS)
import Appraisal.LaTeX.Margins (textHeightInInches, textWidthInInches)
import Appraisal.Markup (Markup)
import Data.Generics (Data, Typeable)
import Data.Ratio ((%))
import Data.SafeCopy (deriveSafeCopy, base, extension, Migrate(..))
import Extra.URI ({- instances only -})
import GHC.Float (fromRat)
import Language.Haskell.TH.Path.Graph (SelfPath)
import Network.URI (URI(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Web.Routes.TH (derivePathInfo)

data ReportImage_1
    = Pic_1
      { picSize_1 :: ImageSize
      , picCrop_1 :: ImageCrop
      , picCaption_1 :: Markup
      , picImage_1 :: Maybe (Either URI ImageFile)
      , picEdited_1 :: Maybe ImageFile
      , picThumb_1 :: Maybe ImageFile
      , picPrinter_1 :: Maybe ImageFile
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Migrate ReportImage_2 where
    type MigrateFrom ReportImage_2 = ReportImage_1
    migrate x =
        Pic_2
            { picSize_2 = picSize_1 x
            , picCrop_2 = picCrop_1 x
            , picCaption_2 = picCaption_1 x
            , picImage_2 = picImage_1 x
            , picEdited_2 = picEdited_1 x
            , picThumb_2 = picThumb_1 x
            , picPrinter_2 = picPrinter_1 x
            , picEnlargement_2 = False }

-- |This is all the information we store about an image in the report.
-- The picSize, picCrop, and picCaption fields describe how the image
-- will appear in the report.  The picImage field describes how to
-- obtain the actual image data.  If this is an URI we need to
-- download the image data and store it in our image database, and
-- generate a new ReportImage value from that.  The remaining fields
-- give the location of three images derived from the original.
data ReportImage_2
    = Pic_2
      { picSize_2 :: ImageSize
      , picCrop_2 :: ImageCrop
      , picCaption_2 :: Markup
      , picImage_2 :: Maybe (Either URI ImageFile) -- ^ Original image
      , picEdited_2 :: Maybe ImageFile -- ^ Cropped version of image
      , picThumb_2 :: Maybe ImageFile -- ^ Image sized for thumbnail
      , picPrinter_2 :: Maybe ImageFile -- ^ Image sized for printing
      , picEnlargement_2 :: Bool -- ^ Include an enlarged copy of this image in the report appendix.
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Migrate ReportImage_3 where
    type MigrateFrom ReportImage_3 = ReportImage_2
    migrate x =
        Pic_3
            { picSize_3 = picSize_2 x
            , picCrop_3 = picCrop_2 x
            , picCaption_3 = picCaption_2 x
            , picImage_3 = picImage_2 x
            , picEdited_3 = picEdited_2 x
            , picThumb_3 = picThumb_2 x
            , picEnlarged_3 = Nothing
            , picPrinter_3 = picPrinter_2 x
            , picMustEnlarge_3 = picEnlargement_2 x }

data ReportImage_3
    = Pic_3
      { picSize_3 :: ImageSize
      , picCrop_3 :: ImageCrop
      , picCaption_3 :: Markup
      , picImage_3 :: Maybe (Either URI ImageFile) -- ^ Original image
      , picEdited_3 :: Maybe ImageFile -- ^ Cropped version of image
      , picThumb_3 :: Maybe ImageFile -- ^ Image sized for thumbnail
      , picPrinter_3 :: Maybe ImageFile -- ^ Image sized for printing
      , picEnlarged_3 :: Maybe ImageFile -- ^ Image at the maximum printable size
      , picMustEnlarge_3 :: Bool        -- ^ Put an enlargement of this image in the appendix
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Migrate ReportImage where
    type MigrateFrom ReportImage = ReportImage_3
    migrate x =
        Pic { picSize = picSize_3 x
            , picCrop = picCrop_3 x
            , picCaption = picCaption_3 x
            -- migration error 1 (missing checksum) occurs in our database, error 2 does not.
            , picOriginal = maybe (trace ("migration error 1: " ++ show x) Nothing) (either (const (trace ("migration error 2: " ++ show x) Nothing)) (Just . Right)) (picImage_3 x)
            , picEditedDeprecated = picEdited_3 x
            , picThumbDeprecated = picThumb_3 x
            , picPrinterDeprecated = picPrinter_3 x
            , picMustEnlarge = picMustEnlarge_3 x
            , picEnlargedDeprecated = picEnlarged_3 x }

type MaybeImageFile = Maybe ImageFile

-- Unchanged, this migration is just to flag unexpected values in the database
data ReportImage
    = Pic
      { picSize :: ImageSize
      , picCrop :: ImageCrop
      , picCaption :: Markup
      , picOriginal :: Maybe (Either URI ImageFile) -- ^ Original image
      , picEditedDeprecated :: MaybeImageFile -- ^ Cropped version of image
      , picThumbDeprecated :: MaybeImageFile -- ^ Image sized for thumbnail
      , picPrinterDeprecated :: MaybeImageFile -- ^ Image sized for printing
      , picMustEnlarge :: Bool        -- ^ Put an enlargement of this image in the appendix
      , picEnlargedDeprecated :: MaybeImageFile -- ^ Image at the maximum printable size
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

$(deriveOrderJS ''ReportImage)

-- | Return the ImageSize value for the image enlarged to fill the
-- page.  It is not unexpected that picEdited would be set to Nothing,
-- but you never know.  Unless the types tell you.
enlargedSize :: ImageFile -> ImageSize
enlargedSize edited =
    if (fromRat (toInteger (imageFileWidth edited) % toInteger (imageFileHeight edited))) < (textWidthInInches / textHeightInInches)
    then ImageSize {dim = TheHeight, size = textHeightInInches, units = Inches}
    else ImageSize {dim = TheWidth, size = textWidthInInches, units = Inches}


$(deriveSafeCopy 1 'base ''ReportImage_1)
$(deriveSafeCopy 2 'extension ''ReportImage_2)
$(deriveSafeCopy 3 'extension ''ReportImage_3)
$(deriveSafeCopy 4 'extension ''ReportImage)

$(deriveSafeCopy 1 'base ''ReportImageID)
$(derivePathInfo ''ReportImageID)

instance Pretty ReportImageID where
    pPrint = text . show . unReportImageID

instance SelfPath ReportImageID
