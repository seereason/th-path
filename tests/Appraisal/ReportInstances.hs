{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Appraisal.ReportInstances where

import Appraisal.File (URI, File)
import Appraisal.Image (Dimension, ImageCrop, ImageSize, lens_saneSize, Units)
import Appraisal.ImageFile (ImageFile)
import Appraisal.IntJS (IntJS, gjsonLens, JSONText)
import Appraisal.Markup as M (Markup, lens_CIString_Text)
import Appraisal.Maybe (lens_mrs')
import Appraisal.Permissions (Permissions, UserIds)
import Appraisal.Report (Authors, AbbrevPairs, EpochMilli, MarkupPairs, Markups, ReportElems, ReportFlags, ReportValueTypeInfo, ReportValueApproachInfo, Branding, Report(Report, reportLetterOfTransmittal), reportBrandingLens, MaybeReportIntendedUse, ReportIntendedUse(..), ReportStatus, ReportStandard, ReportStatus(Draft),
                         AbbrevPairID, AuthorID, MarkupID, MarkupPairID, ReportElemID)
import Appraisal.ReportImage (ReportImage(Pic), MaybeImageFile, ReportImageID)
import Appraisal.ReportItem (ItemFieldName)
import Appraisal.ReportMap (ReportID, ReportMap)
import Appraisal.Utils.CIString (CIString)
import Data.Aeson (FromJSON, ToJSON)
import Data.UUID.Types as UUID (UUID)
import Control.Lens
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as T (Text)
import Data.UserId (UserId(..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Path (camelWords, Describe(describe'), HideType, lens_mrs, lens_UserIds_Text,
                                 readOnlyLens, readShowIso, SelfPath, SinkType, View(ViewType, viewLens))
import Text.LaTeX (LaTeX)
import Text.Pandoc as P (Pandoc, Meta)
import Web.Routes.TH (derivePathInfo)

-- Hiding these types will hide three fields of Markup we don't want
-- to appear in the UI.
instance HideType LaTeX
instance HideType [Markup]
instance HideType P.Pandoc

instance Describe (Proxy JSONText) where describe' _ Proxy = Nothing
instance Describe (Proxy String) where describe' _ Proxy = Nothing
instance Describe (Proxy Text) where describe' _ Proxy = Nothing
instance Describe (Proxy Bool) where describe' _ Proxy = Nothing
instance Describe (Proxy Double) where describe' _ Proxy = Nothing

instance Describe (Proxy Markup) where
    describe' (Just f) Proxy | f == camelWords (nameBase 'reportLetterOfTransmittal) = Just "Letter of Transmittal"
    describe' x _ = x

newtype ReadOnly a = ReadOnly {unReadOnly :: a} deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

instance View (ReadOnly a) where
    type ViewType (ReadOnly a) = a
    viewLens = iso unReadOnly ReadOnly . readOnlyLens

newtype SaneSize a = SaneSize {unSaneSize :: a} deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

instance View (SaneSize ImageSize) where
    type ViewType (SaneSize ImageSize) = ImageSize
    viewLens = iso unSaneSize SaneSize . lens_saneSize

type SaneSizeImageSize = SaneSize ImageSize

-- | Like ReportImage, but with picSize marked SaneSize.  There is a
-- View instance that turns any ReportImage into a ReportImageView,
-- and there is another View instance (just above) that applies
-- lens_saneSize to the image size value to avoid displaying very tiny
-- or very large images in the UI.
data ReportImageView
    = ReportImageView
      { _picSize :: SaneSizeImageSize
      , _picCrop :: ImageCrop
      , _picCaption :: Markup
      , _picOriginal :: Maybe (Either URI ImageFile) -- ^ Original image
      , _picMustEnlarge :: Bool        -- ^ Put an enlargement of this image in the appendix
      }
    deriving (Eq, Ord, Show, Read, Data, Typeable, Generic, FromJSON, ToJSON)

instance View ReportImage where
    type ViewType ReportImage = ReportImageView
    viewLens = iso getter setter
        where
          getter (Pic a1 a2 a3 a4 a5) = ReportImageView (SaneSize a1) a2 a3 a4 a5
          setter (ReportImageView (SaneSize a1) a2 a3 a4 a5) = Pic a1 a2 a3 a4 a5

type ReadOnlyFilePath = ReadOnly FilePath -- views require type names

-- | Like Report, but with reportFolder marked ReadOnly.  This is used
-- to demonstrate that the View mechanism works, and because here we can
-- make modifications without having to add migrations.
data ReportView
    = ReportView
             { _reportFolder :: ReadOnlyFilePath
             , _reportName :: Markup
             , _reportDate :: Markup
             , _reportContractDate :: Markup
             , _reportInspectionDate :: Markup
             , _reportEffectiveDate :: Markup
             , _reportAuthors :: Authors
             , _reportPreparer :: Markup
             , _reportPreparerEIN :: Markup
             , _reportPreparerAddress :: Markup
             , _reportPreparerEMail :: Markup
             , _reportPreparerWebsite :: Markup
             , _reportAbbrevs :: AbbrevPairs
             , _reportTitle :: Markup
             , _reportHeader :: Markup
             , _reportFooter :: Markup
             , _reportIntendedUse :: MaybeReportIntendedUse
             , _reportValueTypeInfo :: ReportValueTypeInfo
             , _reportValueApproachInfo :: ReportValueApproachInfo
             , _reportClientName :: Markup
             , _reportClientAddress :: Markup
             , _reportClientGreeting :: Markup
             , _reportItemsOwnerFull :: Markup
             , _reportItemsOwner :: Markup
             , _reportBriefItems :: Markup
             , _reportInspectionLocation :: Markup
             , _reportBody :: ReportElems
             , _reportGlossary :: MarkupPairs
             , _reportSources :: MarkupPairs
             , _reportLetterOfTransmittal :: Markup
             , _reportScopeOfWork :: Markup
             , _reportCertification :: Markups
             , _reportLimitingConditions :: Markups
             , _reportPrivacyPolicy :: Markup
             , _reportPerms :: Permissions
             , _reportRevision :: Integer
             , _reportCreated :: EpochMilli
             , _reportBranding :: Branding
             , _reportStatus :: ReportStatus
             , _reportRedacted :: Bool
             , _reportFlags :: ReportFlags
             , _reportUUID :: UUID
             , _reportOrderByItemName :: Bool
             , _reportDisplayItemName :: Bool
             , _reportStandardsVersion :: ReportStandard
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

instance View Report where
    type ViewType Report = ReportView
    viewLens = iso getter setter
        where
          getter (Report a01 a02 a03 a04 a05 a06 a07 a08 a09 a10
                         a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                         a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
                         a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
                         a41 a42 a43 a44 a45) =
              ReportView (ReadOnly a01) a02 a03 a04 a05 a06 a07 a08 a09 a10
                         a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                         a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
                         a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
                         a41 a42 a43 a44 a45
          setter (ReportView (ReadOnly a01) a02 a03 a04 a05 a06 a07 a08 a09 a10
                             a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                             a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
                             a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
                             a41 a42 a43 a44 a45) =
              Report a01 a02 a03 a04 a05 a06 a07 a08 a09 a10
                     a11 a12 a13 a14 a15 a16 a17 a18 a19 a20
                     a21 a22 a23 a24 a25 a26 a27 a28 a29 a30
                     a31 a32 a33 a34 a35 a36 a37 a38 a39 a40
                     a41 a42 a43 a44 a45

instance View ReportIntendedUse where
    type ViewType ReportIntendedUse = String
    viewLens = readShowIso SalesAdvisory

instance View MaybeReportIntendedUse where
    type ViewType MaybeReportIntendedUse = String
    viewLens = lens_mrs'

instance SelfPath AbbrevPairID
instance SelfPath AuthorID
instance SelfPath MarkupID
instance SelfPath MarkupPairID
instance SelfPath ReportElemID
instance SelfPath ReportImageID
instance SelfPath ItemFieldName
instance SelfPath ReportID

instance SinkType File
instance SinkType ImageCrop
instance SinkType ImageFile
instance SinkType Int64
instance SinkType Integer
instance SinkType IntJS
instance SinkType Int
instance SinkType JSONText
instance SinkType LaTeX
instance SinkType Meta
instance SinkType Pandoc
instance SinkType URI
instance SinkType UserId
instance SinkType UUID
instance SinkType Word32

instance View Bool where type ViewType Bool = String; viewLens = readShowIso False
instance View Branding where type ViewType Branding = Text; viewLens = reportBrandingLens
instance View Dimension where type ViewType Dimension = JSONText; viewLens = gjsonLens
instance View Double where type ViewType Double = String; viewLens = readShowIso 0.0
instance View MaybeImageFile where type ViewType MaybeImageFile = String; viewLens = lens_mrs
instance View ReportStatus where type ViewType ReportStatus = String; viewLens = readShowIso Draft
instance View String where type ViewType String = JSONText; viewLens = gjsonLens
instance View Text where type ViewType Text = JSONText; viewLens = gjsonLens
instance View Units where type ViewType Units = JSONText; viewLens = gjsonLens
instance View UserIds where type ViewType UserIds = Text; viewLens = lens_UserIds_Text
instance View CIString where type ViewType CIString = Text; viewLens = lens_CIString_Text

#if !__GHCJS__
startTypes :: Q [Type]
startTypes = (: []) <$> [t|ReportMap|]

$(deriveSafeCopy 1 'base ''SaneSize)
$(deriveSafeCopy 1 'base ''ReadOnly)

$(derivePathInfo ''Maybe)
$(derivePathInfo ''ItemFieldName)
$(derivePathInfo ''ReportID)
#endif
