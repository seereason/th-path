{-# LANGUAGE CPP, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies #-}
{-# OPTIONS -Wall -fwarn-incomplete-patterns -fno-warn-orphans #-}
-- |Manage the images used by the report generator - original, edited, thumbnail, and printable.
module Appraisal.ReportImage
    ( ReportImage(..)
    , MaybeImageFile
    , enlargedSize
    , ReportImages
    , ReportImageID(..)
    , EUI, MEUI
    ) where

import Appraisal.Image (ImageSize(..), ImageCrop(..), Dimension(..), Units(..))
import Appraisal.ImageFile (ImageFile(..))
#if !__GHCJS__
import Appraisal.IntJS (deriveOrderJS)
#else
import Appraisal.IntJS ()
import Language.Haskell.TH.Path.Order (Order(..))
#endif
import Appraisal.LaTeX.Margins (textHeightInInches, textWidthInInches)
import Appraisal.Markup (Markup)
import Data.Aeson (ToJSON, FromJSON)
import Data.Generics (Data, Typeable)
import Data.Ratio ((%))
import Data.SafeCopy (deriveSafeCopy, base, extension, Migrate(..))
import Extra.URI ({- instances only -})
import GHC.Float (fromRat)
import GHC.Generics (Generic)
import Language.Haskell.TH.Path.Core (SelfPath)
import Network.URI (URI(..), URIAuth(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Web.Routes.TH (derivePathInfo)

type MaybeImageFile = Maybe ImageFile

type MEUI = Maybe EUI
type EUI = Either URI ImageFile

-- Unchanged, this migration is just to flag unexpected values in the database
#if !__GHCJS__
data ReportImage_4
    = Pic_4
      { picSize_4 :: ImageSize
      , picCrop_4 :: ImageCrop
      , picCaption_4 :: Markup
      , picOriginal_4 :: MEUI -- ^ Original image
      , _picEditedDeprecated_4 :: MaybeImageFile -- ^ Cropped version of image
      , _picThumbDeprecated_4 :: MaybeImageFile -- ^ Image sized for thumbnail
      , _picPrinterDeprecated_4 :: MaybeImageFile -- ^ Image sized for printing
      , picMustEnlarge_4 :: Bool        -- ^ Put an enlargement of this image in the appendix
      , _picEnlargedDeprecated_4 :: MaybeImageFile -- ^ Image at the maximum printable size
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, ToJSON, FromJSON)

instance Migrate ReportImage where
    type MigrateFrom ReportImage = ReportImage_4
    migrate i = Pic
                { picSize = picSize_4 i
                , picCrop = picCrop_4 i
                , picCaption = picCaption_4 i
                , picOriginal = picOriginal_4 i
                , picMustEnlarge = picMustEnlarge_4 i }
#endif

data ReportImage
    = Pic
      { picSize :: ImageSize
      , picCrop :: ImageCrop
      , picCaption :: Markup
      , picOriginal :: MEUI -- ^ Original image
      , picMustEnlarge :: Bool        -- ^ Put an enlargement of this image in the appendix
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, ToJSON, FromJSON)

deriving instance FromJSON URI
deriving instance ToJSON URI
deriving instance Generic URIAuth
deriving instance FromJSON URIAuth
deriving instance ToJSON URIAuth

#if !__GHCJS__
$(deriveOrderJS ''ReportImage)
#else
newtype ReportImageID = ReportImageID {unReportImageID :: Integer} deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, FromJSON, ToJSON)
instance Enum ReportImageID where fromEnum = fromInteger . unReportImageID; toEnum = ReportImageID . toInteger
type ReportImages = Order ReportImageID ReportImage
#endif

-- | Return the ImageSize value for the image enlarged to fill the
-- page.  It is not unexpected that picEdited would be set to Nothing,
-- but you never know.  Unless the types tell you.
enlargedSize :: ImageFile -> ImageSize
enlargedSize edited =
    if (fromRat (toInteger (imageFileWidth edited) % toInteger (imageFileHeight edited))) < (textWidthInInches / textHeightInInches)
    then ImageSize {dim = TheHeight, size = textHeightInInches, units = Inches}
    else ImageSize {dim = TheWidth, size = textWidthInInches, units = Inches}

#if !__GHCJS__
$(deriveSafeCopy 4 'base ''ReportImage_4)
$(deriveSafeCopy 5 'extension ''ReportImage)
$(deriveSafeCopy 1 'base ''ReportImageID)
$(derivePathInfo ''ReportImageID)
#endif

instance Pretty ReportImageID where
    pPrint = text . show . unReportImageID

instance SelfPath ReportImageID
