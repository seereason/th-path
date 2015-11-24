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

import Appraisal.Image (ImageSize(..), ImageCrop(..), Dimension(..), Units(..))
import Appraisal.ImageFile (ImageFile(..))
import Appraisal.IntJS (deriveOrderJS)
import Appraisal.LaTeX.Margins (textHeightInInches, textWidthInInches)
import Appraisal.Markup (Markup)
import Data.Generics (Data, Typeable)
import Data.Ratio ((%))
import Data.SafeCopy (deriveSafeCopy, base)
import Extra.URI ({- instances only -})
import GHC.Float (fromRat)
import Language.Haskell.TH.Path.Graph (SelfPath)
import Network.URI (URI(..))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Web.Routes.TH (derivePathInfo)

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

$(deriveSafeCopy 4 'base ''ReportImage)
$(deriveSafeCopy 1 'base ''ReportImageID)
$(derivePathInfo ''ReportImageID)

instance Pretty ReportImageID where
    pPrint = text . show . unReportImageID

instance SelfPath ReportImageID
