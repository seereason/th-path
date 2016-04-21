-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-incomplete-patterns #-}
module ReportDecs where

import Appraisal.File (File)
import Appraisal.Image
import Appraisal.ImageFile
import Appraisal.IntJS
import Appraisal.Markup (Markup(..))
import Appraisal.Permissions
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.ReportInstances
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID(..), ReportMap(..), MRR)
import Appraisal.Utils.CIString (CIString(..))
import Control.Lens (Iso', iso, lens, _Just, _1, _2, _Left, _Right, Lens', toListOf, Traversal')
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (toList)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node))
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import GHC.Generics (Generic)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(Path_OMap, Path_At), toPairs)
import Language.Haskell.TH.Path.View (View(viewLens))
import Network.URI (URI(URI), URIAuth)

ulens = ulens' Proxy
data UPath_Author
    = UPath_Author_authorName UPath_Markup | UPath_Author_authorCredentials UPath_Markup | UPath_Author
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ImageCrop = UPath_ImageCrop deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ImageFile = UPath_ImageFile deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ImageSize
    = UPath_ImageSize_dim (Path_View Dimension UPath_JSONText)
    | UPath_ImageSize_size (Path_View Double (Path_View String UPath_JSONText))
    | UPath_ImageSize_units (Path_View Units UPath_JSONText)
    | UPath_ImageSize
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_Int = UPath_Int deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_Int64 = UPath_Int64 deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_Integer = UPath_Integer deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_Item
    = UPath_Item_itemName (Path_View Text UPath_JSONText)
    | UPath_Item_fields (Path_Map ItemFieldName UPath_Markup)
    | UPath_Item_images (Path_OMap ReportImageID (Path_View ReportImage UPath_ReportImageView))
    | UPath_Item
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_JSONText = UPath_JSONText deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_Markup
    = UPath_Markup_markdownText (Path_View Text UPath_JSONText) | UPath_Markup_htmlText (Path_View Text UPath_JSONText) | UPath_Markup
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_Permissions
    = UPath_Permissions_owner UPath_UserId
    | UPath_Permissions_writers (Path_View UserIds (Path_View Text UPath_JSONText))
    | UPath_Permissions_readers (Path_View UserIds (Path_View Text UPath_JSONText))
    | UPath_Permissions
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportElem
    = UPath_ReportElem_elemItem UPath_Item | UPath_ReportElem_elemText UPath_Markup | UPath_ReportElem
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportFlags
    = UPath_ReportFlags_hideEmptyItemFields (Path_View Bool (Path_View String UPath_JSONText)) | UPath_ReportFlags
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportImageView
    = UPath_ReportImageView__picSize (Path_View SaneSizeImageSize UPath_ImageSize)
    | UPath_ReportImageView__picCrop UPath_ImageCrop
    | UPath_ReportImageView__picCaption UPath_Markup
    | UPath_ReportImageView__picOriginal (Path_Maybe (Path_Either UPath_URI UPath_ImageFile))
    | UPath_ReportImageView__picMustEnlarge (Path_View Bool (Path_View String UPath_JSONText))
    | UPath_ReportImageView
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportMap
    = UPath_ReportMap_unReportMap (Path_Map ReportID (Path_View Report UPath_ReportView)) | UPath_ReportMap
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportStandard
    = UPath_ReportStandard_unReportStandard UPath_Int | UPath_ReportStandard
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportValueApproachInfo
    = UPath_ReportValueApproachInfo_reportValueApproachName UPath_Markup
    | UPath_ReportValueApproachInfo_reportValueApproachDescription UPath_Markup
    | UPath_ReportValueApproachInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportValueTypeInfo
    = UPath_ReportValueTypeInfo_reportValueTypeName UPath_Markup
    | UPath_ReportValueTypeInfo_reportValueTypeDescription UPath_Markup
    | UPath_ReportValueTypeInfo_reportValueTypeDefinition UPath_Markup
    | UPath_ReportValueTypeInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_ReportView
    = UPath_ReportView__reportFolder (Path_View ReadOnlyFilePath (Path_View String UPath_JSONText))
    | UPath_ReportView__reportName UPath_Markup
    | UPath_ReportView__reportDate UPath_Markup
    | UPath_ReportView__reportContractDate UPath_Markup
    | UPath_ReportView__reportInspectionDate UPath_Markup
    | UPath_ReportView__reportEffectiveDate UPath_Markup
    | UPath_ReportView__reportAuthors (Path_OMap AuthorID UPath_Author)
    | UPath_ReportView__reportPreparer UPath_Markup
    | UPath_ReportView__reportPreparerEIN UPath_Markup
    | UPath_ReportView__reportPreparerAddress UPath_Markup
    | UPath_ReportView__reportPreparerEMail UPath_Markup
    | UPath_ReportView__reportPreparerWebsite UPath_Markup
    | UPath_ReportView__reportAbbrevs (Path_OMap AbbrevPairID (Path_Pair (Path_View CIString (Path_View Text UPath_JSONText)) UPath_Markup))
    | UPath_ReportView__reportTitle UPath_Markup
    | UPath_ReportView__reportHeader UPath_Markup
    | UPath_ReportView__reportFooter UPath_Markup
    | UPath_ReportView__reportIntendedUse (Path_View MaybeReportIntendedUse (Path_View String UPath_JSONText))
    | UPath_ReportView__reportValueTypeInfo UPath_ReportValueTypeInfo
    | UPath_ReportView__reportValueApproachInfo UPath_ReportValueApproachInfo
    | UPath_ReportView__reportClientName UPath_Markup
    | UPath_ReportView__reportClientAddress UPath_Markup
    | UPath_ReportView__reportClientGreeting UPath_Markup
    | UPath_ReportView__reportItemsOwnerFull UPath_Markup
    | UPath_ReportView__reportItemsOwner UPath_Markup
    | UPath_ReportView__reportBriefItems UPath_Markup
    | UPath_ReportView__reportInspectionLocation UPath_Markup
    | UPath_ReportView__reportBody (Path_OMap ReportElemID UPath_ReportElem)
    | UPath_ReportView__reportGlossary (Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup))
    | UPath_ReportView__reportSources (Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup))
    | UPath_ReportView__reportLetterOfTransmittal UPath_Markup
    | UPath_ReportView__reportScopeOfWork UPath_Markup
    | UPath_ReportView__reportCertification (Path_OMap MarkupID UPath_Markup)
    | UPath_ReportView__reportLimitingConditions (Path_OMap MarkupID UPath_Markup)
    | UPath_ReportView__reportPrivacyPolicy UPath_Markup
    | UPath_ReportView__reportPerms UPath_Permissions
    | UPath_ReportView__reportRevision UPath_Integer
    | UPath_ReportView__reportCreated UPath_Int64
    | UPath_ReportView__reportBranding (Path_View Branding (Path_View Text UPath_JSONText))
    | UPath_ReportView__reportStatus (Path_View ReportStatus (Path_View String UPath_JSONText))
    | UPath_ReportView__reportRedacted (Path_View Bool (Path_View String UPath_JSONText))
    | UPath_ReportView__reportFlags UPath_ReportFlags
    | UPath_ReportView__reportUUID UPath_UUID
    | UPath_ReportView__reportOrderByItemName (Path_View Bool (Path_View String UPath_JSONText))
    | UPath_ReportView__reportDisplayItemName (Path_View Bool (Path_View String UPath_JSONText))
    | UPath_ReportView__reportStandardsVersion UPath_ReportStandard
    | UPath_ReportView
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_URI = UPath_URI deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_UUID = UPath_UUID deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data UPath_UserId = UPath_UserId deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data Univ
    = U1 String
    | U2 Int64
    | U3 Bool
    | U4 Double
    | U5 Int
    | U6 Dimension
    | U7 ImageCrop
    | U8 ImageSize
    | U9 Units
    | U10 ImageFile
    | U11 Integer
    | U12 JSONText
    | U13 Markup
    | U14 Permissions
    | U15 UserIds
    | U16 AbbrevPair
    | U17 AbbrevPairs
    | U18 Author
    | U19 Authors
    | U20 Branding
    | U21 MarkupPair
    | U22 MarkupPairs
    | U23 Markups
    | U24 MaybeReportIntendedUse
    | U25 Report
    | U26 ReportElem
    | U27 ReportElems
    | U28 ReportFlags
    | U29 ReportIntendedUse
    | U30 ReportStandard
    | U31 ReportStatus
    | U32 ReportValueApproachInfo
    | U33 ReportValueTypeInfo
    | U34 EUI
    | U35 MEUI
    | U36 MaybeImageFile
    | U37 ReportImage
    | U38 ReportImages
    | U39 ReadOnlyFilePath
    | U40 ReportImageView
    | U41 ReportView
    | U42 SaneSizeImageSize
    | U43 Item
    | U44 MIM
    | U45 MRR
    | U46 ReportMap
    | U47 CIString
    | U48 URI
    | U49 Text
    | U50 UserId
    | U51 UUID
    deriving (Eq, Ord, Show, Data, Typeable, Generic, FromJSON, ToJSON)
class HasAuthor c
    where lens_author :: Lens' c Author
          lens_Author_authorCredentials :: forall . Lens' c Markup
          lens_Author_authorCredentials = (.) lens_author lens_Author_authorCredentials
          {-# INLINE lens_Author_authorCredentials #-}
          lens_Author_authorName :: forall . Lens' c Markup
          lens_Author_authorName = (.) lens_author lens_Author_authorName
          {-# INLINE lens_Author_authorName #-}
class HasBool c
    where lens_bool :: Lens' c Bool
class HasBranding c
    where lens_branding :: Lens' c Branding
class HasCIString c
    where lens_cIString :: Lens' c CIString
          lens_CIString_unCIString :: forall . Lens' c String
          lens_CIString_unCIString = (.) lens_cIString lens_CIString_unCIString
          {-# INLINE lens_CIString_unCIString #-}
class HasDimension c
    where lens_dimension :: Lens' c Dimension
class HasDouble c
    where lens_double :: Lens' c Double
class HasImageCrop c
    where lens_imageCrop :: Lens' c ImageCrop
          lens_ImageCrop_bottomCrop :: forall . Lens' c Int
          lens_ImageCrop_bottomCrop = (.) lens_imageCrop lens_ImageCrop_bottomCrop
          {-# INLINE lens_ImageCrop_bottomCrop #-}
          lens_ImageCrop_leftCrop :: forall . Lens' c Int
          lens_ImageCrop_leftCrop = (.) lens_imageCrop lens_ImageCrop_leftCrop
          {-# INLINE lens_ImageCrop_leftCrop #-}
          lens_ImageCrop_rightCrop :: forall . Lens' c Int
          lens_ImageCrop_rightCrop = (.) lens_imageCrop lens_ImageCrop_rightCrop
          {-# INLINE lens_ImageCrop_rightCrop #-}
          lens_ImageCrop_rotation :: forall . Lens' c Int
          lens_ImageCrop_rotation = (.) lens_imageCrop lens_ImageCrop_rotation
          {-# INLINE lens_ImageCrop_rotation #-}
          lens_ImageCrop_topCrop :: forall . Lens' c Int
          lens_ImageCrop_topCrop = (.) lens_imageCrop lens_ImageCrop_topCrop
          {-# INLINE lens_ImageCrop_topCrop #-}
class HasImageFile c
    where lens_imageFile :: Lens' c ImageFile
          lens_ImageFile_imageFile :: forall . Lens' c File
          lens_ImageFile_imageFile = (.) lens_imageFile lens_ImageFile_imageFile
          {-# INLINE lens_ImageFile_imageFile #-}
          lens_ImageFile_imageFileHeight :: forall . Lens' c Int
          lens_ImageFile_imageFileHeight = (.) lens_imageFile lens_ImageFile_imageFileHeight
          {-# INLINE lens_ImageFile_imageFileHeight #-}
          lens_ImageFile_imageFileMaxVal :: forall . Lens' c Int
          lens_ImageFile_imageFileMaxVal = (.) lens_imageFile lens_ImageFile_imageFileMaxVal
          {-# INLINE lens_ImageFile_imageFileMaxVal #-}
          lens_ImageFile_imageFileType :: forall . Lens' c ImageType
          lens_ImageFile_imageFileType = (.) lens_imageFile lens_ImageFile_imageFileType
          {-# INLINE lens_ImageFile_imageFileType #-}
          lens_ImageFile_imageFileWidth :: forall . Lens' c Int
          lens_ImageFile_imageFileWidth = (.) lens_imageFile lens_ImageFile_imageFileWidth
          {-# INLINE lens_ImageFile_imageFileWidth #-}
class HasImageSize c
    where lens_imageSize :: Lens' c ImageSize
          lens_ImageSize_dim :: forall . Lens' c Dimension
          lens_ImageSize_dim = (.) lens_imageSize lens_ImageSize_dim
          {-# INLINE lens_ImageSize_dim #-}
          lens_ImageSize_size :: forall . Lens' c Double
          lens_ImageSize_size = (.) lens_imageSize lens_ImageSize_size
          {-# INLINE lens_ImageSize_size #-}
          lens_ImageSize_units :: forall . Lens' c Units
          lens_ImageSize_units = (.) lens_imageSize lens_ImageSize_units
          {-# INLINE lens_ImageSize_units #-}
class HasInt c
    where lens_int :: Lens' c Int
class HasInt64 c
    where lens_int64 :: Lens' c Int64
class HasInteger c
    where lens_integer :: Lens' c Integer
class HasItem c
    where lens_item :: Lens' c Item
          lens_Item_fields :: forall . Lens' c MIM
          lens_Item_fields = (.) lens_item lens_Item_fields
          {-# INLINE lens_Item_fields #-}
          lens_Item_images :: forall . Lens' c ReportImages
          lens_Item_images = (.) lens_item lens_Item_images
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName :: forall . Lens' c Text
          lens_Item_itemName = (.) lens_item lens_Item_itemName
          {-# INLINE lens_Item_itemName #-}
class HasJSONText c
    where lens_jSONText :: Lens' c JSONText
          lens_JSONText_unJSONText :: forall . Lens' c String
          lens_JSONText_unJSONText = (.) lens_jSONText lens_JSONText_unJSONText
          {-# INLINE lens_JSONText_unJSONText #-}
class HasMarkup c
    where lens_markup :: Lens' c Markup
          lens_Markup_htmlText :: forall . Traversal' c Text
          lens_Markup_htmlText = (.) lens_markup lens_Markup_htmlText
          {-# INLINE lens_Markup_htmlText #-}
          lens_Markup_markdownText :: forall . Traversal' c Text
          lens_Markup_markdownText = (.) lens_markup lens_Markup_markdownText
          {-# INLINE lens_Markup_markdownText #-}
class HasPermissions c
    where lens_permissions :: Lens' c Permissions
          lens_Permissions_owner :: forall . Lens' c UserId
          lens_Permissions_owner = (.) lens_permissions lens_Permissions_owner
          {-# INLINE lens_Permissions_owner #-}
          lens_Permissions_readers :: forall . Lens' c UserIds
          lens_Permissions_readers = (.) lens_permissions lens_Permissions_readers
          {-# INLINE lens_Permissions_readers #-}
          lens_Permissions_writers :: forall . Lens' c UserIds
          lens_Permissions_writers = (.) lens_permissions lens_Permissions_writers
          {-# INLINE lens_Permissions_writers #-}
class HasReport c
    where lens_report :: Lens' c Report
          lens_Report_reportAbbrevs :: forall . Lens' c AbbrevPairs
          lens_Report_reportAbbrevs = (.) lens_report lens_Report_reportAbbrevs
          {-# INLINE lens_Report_reportAbbrevs #-}
          lens_Report_reportAuthors :: forall . Lens' c Authors
          lens_Report_reportAuthors = (.) lens_report lens_Report_reportAuthors
          {-# INLINE lens_Report_reportAuthors #-}
          lens_Report_reportBody :: forall . Lens' c ReportElems
          lens_Report_reportBody = (.) lens_report lens_Report_reportBody
          {-# INLINE lens_Report_reportBody #-}
          lens_Report_reportBranding :: forall . Lens' c Branding
          lens_Report_reportBranding = (.) lens_report lens_Report_reportBranding
          {-# INLINE lens_Report_reportBranding #-}
          lens_Report_reportBriefItems :: forall . Lens' c Markup
          lens_Report_reportBriefItems = (.) lens_report lens_Report_reportBriefItems
          {-# INLINE lens_Report_reportBriefItems #-}
          lens_Report_reportCertification :: forall . Lens' c Markups
          lens_Report_reportCertification = (.) lens_report lens_Report_reportCertification
          {-# INLINE lens_Report_reportCertification #-}
          lens_Report_reportClientAddress :: forall . Lens' c Markup
          lens_Report_reportClientAddress = (.) lens_report lens_Report_reportClientAddress
          {-# INLINE lens_Report_reportClientAddress #-}
          lens_Report_reportClientGreeting :: forall . Lens' c Markup
          lens_Report_reportClientGreeting = (.) lens_report lens_Report_reportClientGreeting
          {-# INLINE lens_Report_reportClientGreeting #-}
          lens_Report_reportClientName :: forall . Lens' c Markup
          lens_Report_reportClientName = (.) lens_report lens_Report_reportClientName
          {-# INLINE lens_Report_reportClientName #-}
          lens_Report_reportContractDate :: forall . Lens' c Markup
          lens_Report_reportContractDate = (.) lens_report lens_Report_reportContractDate
          {-# INLINE lens_Report_reportContractDate #-}
          lens_Report_reportCreated :: forall . Lens' c EpochMilli
          lens_Report_reportCreated = (.) lens_report lens_Report_reportCreated
          {-# INLINE lens_Report_reportCreated #-}
          lens_Report_reportDate :: forall . Lens' c Markup
          lens_Report_reportDate = (.) lens_report lens_Report_reportDate
          {-# INLINE lens_Report_reportDate #-}
          lens_Report_reportDisplayItemName :: forall . Lens' c Bool
          lens_Report_reportDisplayItemName = (.) lens_report lens_Report_reportDisplayItemName
          {-# INLINE lens_Report_reportDisplayItemName #-}
          lens_Report_reportEffectiveDate :: forall . Lens' c Markup
          lens_Report_reportEffectiveDate = (.) lens_report lens_Report_reportEffectiveDate
          {-# INLINE lens_Report_reportEffectiveDate #-}
          lens_Report_reportFlags :: forall . Lens' c ReportFlags
          lens_Report_reportFlags = (.) lens_report lens_Report_reportFlags
          {-# INLINE lens_Report_reportFlags #-}
          lens_Report_reportFolder :: forall . Lens' c FilePath
          lens_Report_reportFolder = (.) lens_report lens_Report_reportFolder
          {-# INLINE lens_Report_reportFolder #-}
          lens_Report_reportFooter :: forall . Lens' c Markup
          lens_Report_reportFooter = (.) lens_report lens_Report_reportFooter
          {-# INLINE lens_Report_reportFooter #-}
          lens_Report_reportGlossary :: forall . Lens' c MarkupPairs
          lens_Report_reportGlossary = (.) lens_report lens_Report_reportGlossary
          {-# INLINE lens_Report_reportGlossary #-}
          lens_Report_reportHeader :: forall . Lens' c Markup
          lens_Report_reportHeader = (.) lens_report lens_Report_reportHeader
          {-# INLINE lens_Report_reportHeader #-}
          lens_Report_reportInspectionDate :: forall . Lens' c Markup
          lens_Report_reportInspectionDate = (.) lens_report lens_Report_reportInspectionDate
          {-# INLINE lens_Report_reportInspectionDate #-}
          lens_Report_reportInspectionLocation :: forall . Lens' c Markup
          lens_Report_reportInspectionLocation = (.) lens_report lens_Report_reportInspectionLocation
          {-# INLINE lens_Report_reportInspectionLocation #-}
          lens_Report_reportIntendedUse :: forall . Lens' c MaybeReportIntendedUse
          lens_Report_reportIntendedUse = (.) lens_report lens_Report_reportIntendedUse
          {-# INLINE lens_Report_reportIntendedUse #-}
          lens_Report_reportItemsOwner :: forall . Lens' c Markup
          lens_Report_reportItemsOwner = (.) lens_report lens_Report_reportItemsOwner
          {-# INLINE lens_Report_reportItemsOwner #-}
          lens_Report_reportItemsOwnerFull :: forall . Lens' c Markup
          lens_Report_reportItemsOwnerFull = (.) lens_report lens_Report_reportItemsOwnerFull
          {-# INLINE lens_Report_reportItemsOwnerFull #-}
          lens_Report_reportLetterOfTransmittal :: forall . Lens' c Markup
          lens_Report_reportLetterOfTransmittal = (.) lens_report lens_Report_reportLetterOfTransmittal
          {-# INLINE lens_Report_reportLetterOfTransmittal #-}
          lens_Report_reportLimitingConditions :: forall . Lens' c Markups
          lens_Report_reportLimitingConditions = (.) lens_report lens_Report_reportLimitingConditions
          {-# INLINE lens_Report_reportLimitingConditions #-}
          lens_Report_reportName :: forall . Lens' c Markup
          lens_Report_reportName = (.) lens_report lens_Report_reportName
          {-# INLINE lens_Report_reportName #-}
          lens_Report_reportOrderByItemName :: forall . Lens' c Bool
          lens_Report_reportOrderByItemName = (.) lens_report lens_Report_reportOrderByItemName
          {-# INLINE lens_Report_reportOrderByItemName #-}
          lens_Report_reportPerms :: forall . Lens' c Permissions
          lens_Report_reportPerms = (.) lens_report lens_Report_reportPerms
          {-# INLINE lens_Report_reportPerms #-}
          lens_Report_reportPreparer :: forall . Lens' c Markup
          lens_Report_reportPreparer = (.) lens_report lens_Report_reportPreparer
          {-# INLINE lens_Report_reportPreparer #-}
          lens_Report_reportPreparerAddress :: forall . Lens' c Markup
          lens_Report_reportPreparerAddress = (.) lens_report lens_Report_reportPreparerAddress
          {-# INLINE lens_Report_reportPreparerAddress #-}
          lens_Report_reportPreparerEIN :: forall . Lens' c Markup
          lens_Report_reportPreparerEIN = (.) lens_report lens_Report_reportPreparerEIN
          {-# INLINE lens_Report_reportPreparerEIN #-}
          lens_Report_reportPreparerEMail :: forall . Lens' c Markup
          lens_Report_reportPreparerEMail = (.) lens_report lens_Report_reportPreparerEMail
          {-# INLINE lens_Report_reportPreparerEMail #-}
          lens_Report_reportPreparerWebsite :: forall . Lens' c Markup
          lens_Report_reportPreparerWebsite = (.) lens_report lens_Report_reportPreparerWebsite
          {-# INLINE lens_Report_reportPreparerWebsite #-}
          lens_Report_reportPrivacyPolicy :: forall . Lens' c Markup
          lens_Report_reportPrivacyPolicy = (.) lens_report lens_Report_reportPrivacyPolicy
          {-# INLINE lens_Report_reportPrivacyPolicy #-}
          lens_Report_reportRedacted :: forall . Lens' c Bool
          lens_Report_reportRedacted = (.) lens_report lens_Report_reportRedacted
          {-# INLINE lens_Report_reportRedacted #-}
          lens_Report_reportRevision :: forall . Lens' c Integer
          lens_Report_reportRevision = (.) lens_report lens_Report_reportRevision
          {-# INLINE lens_Report_reportRevision #-}
          lens_Report_reportScopeOfWork :: forall . Lens' c Markup
          lens_Report_reportScopeOfWork = (.) lens_report lens_Report_reportScopeOfWork
          {-# INLINE lens_Report_reportScopeOfWork #-}
          lens_Report_reportSources :: forall . Lens' c MarkupPairs
          lens_Report_reportSources = (.) lens_report lens_Report_reportSources
          {-# INLINE lens_Report_reportSources #-}
          lens_Report_reportStandardsVersion :: forall . Lens' c ReportStandard
          lens_Report_reportStandardsVersion = (.) lens_report lens_Report_reportStandardsVersion
          {-# INLINE lens_Report_reportStandardsVersion #-}
          lens_Report_reportStatus :: forall . Lens' c ReportStatus
          lens_Report_reportStatus = (.) lens_report lens_Report_reportStatus
          {-# INLINE lens_Report_reportStatus #-}
          lens_Report_reportTitle :: forall . Lens' c Markup
          lens_Report_reportTitle = (.) lens_report lens_Report_reportTitle
          {-# INLINE lens_Report_reportTitle #-}
          lens_Report_reportUUID :: forall . Lens' c UUID
          lens_Report_reportUUID = (.) lens_report lens_Report_reportUUID
          {-# INLINE lens_Report_reportUUID #-}
          lens_Report_reportValueApproachInfo :: forall . Lens' c ReportValueApproachInfo
          lens_Report_reportValueApproachInfo = (.) lens_report lens_Report_reportValueApproachInfo
          {-# INLINE lens_Report_reportValueApproachInfo #-}
          lens_Report_reportValueTypeInfo :: forall . Lens' c ReportValueTypeInfo
          lens_Report_reportValueTypeInfo = (.) lens_report lens_Report_reportValueTypeInfo
          {-# INLINE lens_Report_reportValueTypeInfo #-}
class HasReportElem c
    where lens_reportElem :: Lens' c ReportElem
          lens_ReportElem_elemItem :: forall . Traversal' c Item
          lens_ReportElem_elemItem = (.) lens_reportElem lens_ReportElem_elemItem
          {-# INLINE lens_ReportElem_elemItem #-}
          lens_ReportElem_elemText :: forall . Traversal' c Markup
          lens_ReportElem_elemText = (.) lens_reportElem lens_ReportElem_elemText
          {-# INLINE lens_ReportElem_elemText #-}
class HasReportFlags c
    where lens_reportFlags :: Lens' c ReportFlags
          lens_ReportFlags_hideEmptyItemFields :: forall . Lens' c Bool
          lens_ReportFlags_hideEmptyItemFields = (.) lens_reportFlags lens_ReportFlags_hideEmptyItemFields
          {-# INLINE lens_ReportFlags_hideEmptyItemFields #-}
class HasReportImage c
    where lens_reportImage :: Lens' c ReportImage
          lens_ReportImage_picCaption :: forall . Lens' c Markup
          lens_ReportImage_picCaption = (.) lens_reportImage lens_ReportImage_picCaption
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop :: forall . Lens' c ImageCrop
          lens_ReportImage_picCrop = (.) lens_reportImage lens_ReportImage_picCrop
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImage_picMustEnlarge = (.) lens_reportImage lens_ReportImage_picMustEnlarge
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal :: forall . Lens' c MEUI
          lens_ReportImage_picOriginal = (.) lens_reportImage lens_ReportImage_picOriginal
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picSize :: forall . Lens' c ImageSize
          lens_ReportImage_picSize = (.) lens_reportImage lens_ReportImage_picSize
          {-# INLINE lens_ReportImage_picSize #-}
class HasReportImageView c
    where lens_reportImageView :: Lens' c ReportImageView
          lens_ReportImageView__picCaption :: forall . Lens' c Markup
          lens_ReportImageView__picCaption = (.) lens_reportImageView lens_ReportImageView__picCaption
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop :: forall . Lens' c ImageCrop
          lens_ReportImageView__picCrop = (.) lens_reportImageView lens_ReportImageView__picCrop
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImageView__picMustEnlarge = (.) lens_reportImageView lens_ReportImageView__picMustEnlarge
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal :: forall . Lens' c (Maybe (Either URI ImageFile))
          lens_ReportImageView__picOriginal = (.) lens_reportImageView lens_ReportImageView__picOriginal
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picSize :: forall . Lens' c SaneSizeImageSize
          lens_ReportImageView__picSize = (.) lens_reportImageView lens_ReportImageView__picSize
          {-# INLINE lens_ReportImageView__picSize #-}
class HasReportIntendedUse c
    where lens_reportIntendedUse :: Lens' c ReportIntendedUse
class HasReportMap c
    where lens_reportMap :: Lens' c ReportMap
          lens_ReportMap_unReportMap :: forall . Lens' c MRR
          lens_ReportMap_unReportMap = (.) lens_reportMap lens_ReportMap_unReportMap
          {-# INLINE lens_ReportMap_unReportMap #-}
class HasReportStandard c
    where lens_reportStandard :: Lens' c ReportStandard
          lens_ReportStandard_unReportStandard :: forall . Lens' c Int
          lens_ReportStandard_unReportStandard = (.) lens_reportStandard lens_ReportStandard_unReportStandard
          {-# INLINE lens_ReportStandard_unReportStandard #-}
class HasReportStatus c
    where lens_reportStatus :: Lens' c ReportStatus
class HasReportValueApproachInfo c
    where lens_reportValueApproachInfo :: Lens' c ReportValueApproachInfo
          lens_ReportValueApproachInfo_reportValueApproachDescription :: forall . Lens' c Markup
          lens_ReportValueApproachInfo_reportValueApproachDescription = (.) lens_reportValueApproachInfo lens_ReportValueApproachInfo_reportValueApproachDescription
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName :: forall . Lens' c Markup
          lens_ReportValueApproachInfo_reportValueApproachName = (.) lens_reportValueApproachInfo lens_ReportValueApproachInfo_reportValueApproachName
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
class HasReportValueTypeInfo c
    where lens_reportValueTypeInfo :: Lens' c ReportValueTypeInfo
          lens_ReportValueTypeInfo_reportValueTypeDefinition :: forall . Lens' c Markup
          lens_ReportValueTypeInfo_reportValueTypeDefinition = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeDefinition
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription :: forall . Lens' c Markup
          lens_ReportValueTypeInfo_reportValueTypeDescription = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeDescription
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName :: forall . Lens' c Markup
          lens_ReportValueTypeInfo_reportValueTypeName = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeName
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeName #-}
class HasReportView c
    where lens_reportView :: Lens' c ReportView
          lens_ReportView__reportAbbrevs :: forall . Lens' c AbbrevPairs
          lens_ReportView__reportAbbrevs = (.) lens_reportView lens_ReportView__reportAbbrevs
          {-# INLINE lens_ReportView__reportAbbrevs #-}
          lens_ReportView__reportAuthors :: forall . Lens' c Authors
          lens_ReportView__reportAuthors = (.) lens_reportView lens_ReportView__reportAuthors
          {-# INLINE lens_ReportView__reportAuthors #-}
          lens_ReportView__reportBody :: forall . Lens' c ReportElems
          lens_ReportView__reportBody = (.) lens_reportView lens_ReportView__reportBody
          {-# INLINE lens_ReportView__reportBody #-}
          lens_ReportView__reportBranding :: forall . Lens' c Branding
          lens_ReportView__reportBranding = (.) lens_reportView lens_ReportView__reportBranding
          {-# INLINE lens_ReportView__reportBranding #-}
          lens_ReportView__reportBriefItems :: forall . Lens' c Markup
          lens_ReportView__reportBriefItems = (.) lens_reportView lens_ReportView__reportBriefItems
          {-# INLINE lens_ReportView__reportBriefItems #-}
          lens_ReportView__reportCertification :: forall . Lens' c Markups
          lens_ReportView__reportCertification = (.) lens_reportView lens_ReportView__reportCertification
          {-# INLINE lens_ReportView__reportCertification #-}
          lens_ReportView__reportClientAddress :: forall . Lens' c Markup
          lens_ReportView__reportClientAddress = (.) lens_reportView lens_ReportView__reportClientAddress
          {-# INLINE lens_ReportView__reportClientAddress #-}
          lens_ReportView__reportClientGreeting :: forall . Lens' c Markup
          lens_ReportView__reportClientGreeting = (.) lens_reportView lens_ReportView__reportClientGreeting
          {-# INLINE lens_ReportView__reportClientGreeting #-}
          lens_ReportView__reportClientName :: forall . Lens' c Markup
          lens_ReportView__reportClientName = (.) lens_reportView lens_ReportView__reportClientName
          {-# INLINE lens_ReportView__reportClientName #-}
          lens_ReportView__reportContractDate :: forall . Lens' c Markup
          lens_ReportView__reportContractDate = (.) lens_reportView lens_ReportView__reportContractDate
          {-# INLINE lens_ReportView__reportContractDate #-}
          lens_ReportView__reportCreated :: forall . Lens' c EpochMilli
          lens_ReportView__reportCreated = (.) lens_reportView lens_ReportView__reportCreated
          {-# INLINE lens_ReportView__reportCreated #-}
          lens_ReportView__reportDate :: forall . Lens' c Markup
          lens_ReportView__reportDate = (.) lens_reportView lens_ReportView__reportDate
          {-# INLINE lens_ReportView__reportDate #-}
          lens_ReportView__reportDisplayItemName :: forall . Lens' c Bool
          lens_ReportView__reportDisplayItemName = (.) lens_reportView lens_ReportView__reportDisplayItemName
          {-# INLINE lens_ReportView__reportDisplayItemName #-}
          lens_ReportView__reportEffectiveDate :: forall . Lens' c Markup
          lens_ReportView__reportEffectiveDate = (.) lens_reportView lens_ReportView__reportEffectiveDate
          {-# INLINE lens_ReportView__reportEffectiveDate #-}
          lens_ReportView__reportFlags :: forall . Lens' c ReportFlags
          lens_ReportView__reportFlags = (.) lens_reportView lens_ReportView__reportFlags
          {-# INLINE lens_ReportView__reportFlags #-}
          lens_ReportView__reportFolder :: forall . Lens' c ReadOnlyFilePath
          lens_ReportView__reportFolder = (.) lens_reportView lens_ReportView__reportFolder
          {-# INLINE lens_ReportView__reportFolder #-}
          lens_ReportView__reportFooter :: forall . Lens' c Markup
          lens_ReportView__reportFooter = (.) lens_reportView lens_ReportView__reportFooter
          {-# INLINE lens_ReportView__reportFooter #-}
          lens_ReportView__reportGlossary :: forall . Lens' c MarkupPairs
          lens_ReportView__reportGlossary = (.) lens_reportView lens_ReportView__reportGlossary
          {-# INLINE lens_ReportView__reportGlossary #-}
          lens_ReportView__reportHeader :: forall . Lens' c Markup
          lens_ReportView__reportHeader = (.) lens_reportView lens_ReportView__reportHeader
          {-# INLINE lens_ReportView__reportHeader #-}
          lens_ReportView__reportInspectionDate :: forall . Lens' c Markup
          lens_ReportView__reportInspectionDate = (.) lens_reportView lens_ReportView__reportInspectionDate
          {-# INLINE lens_ReportView__reportInspectionDate #-}
          lens_ReportView__reportInspectionLocation :: forall . Lens' c Markup
          lens_ReportView__reportInspectionLocation = (.) lens_reportView lens_ReportView__reportInspectionLocation
          {-# INLINE lens_ReportView__reportInspectionLocation #-}
          lens_ReportView__reportIntendedUse :: forall . Lens' c MaybeReportIntendedUse
          lens_ReportView__reportIntendedUse = (.) lens_reportView lens_ReportView__reportIntendedUse
          {-# INLINE lens_ReportView__reportIntendedUse #-}
          lens_ReportView__reportItemsOwner :: forall . Lens' c Markup
          lens_ReportView__reportItemsOwner = (.) lens_reportView lens_ReportView__reportItemsOwner
          {-# INLINE lens_ReportView__reportItemsOwner #-}
          lens_ReportView__reportItemsOwnerFull :: forall . Lens' c Markup
          lens_ReportView__reportItemsOwnerFull = (.) lens_reportView lens_ReportView__reportItemsOwnerFull
          {-# INLINE lens_ReportView__reportItemsOwnerFull #-}
          lens_ReportView__reportLetterOfTransmittal :: forall . Lens' c Markup
          lens_ReportView__reportLetterOfTransmittal = (.) lens_reportView lens_ReportView__reportLetterOfTransmittal
          {-# INLINE lens_ReportView__reportLetterOfTransmittal #-}
          lens_ReportView__reportLimitingConditions :: forall . Lens' c Markups
          lens_ReportView__reportLimitingConditions = (.) lens_reportView lens_ReportView__reportLimitingConditions
          {-# INLINE lens_ReportView__reportLimitingConditions #-}
          lens_ReportView__reportName :: forall . Lens' c Markup
          lens_ReportView__reportName = (.) lens_reportView lens_ReportView__reportName
          {-# INLINE lens_ReportView__reportName #-}
          lens_ReportView__reportOrderByItemName :: forall . Lens' c Bool
          lens_ReportView__reportOrderByItemName = (.) lens_reportView lens_ReportView__reportOrderByItemName
          {-# INLINE lens_ReportView__reportOrderByItemName #-}
          lens_ReportView__reportPerms :: forall . Lens' c Permissions
          lens_ReportView__reportPerms = (.) lens_reportView lens_ReportView__reportPerms
          {-# INLINE lens_ReportView__reportPerms #-}
          lens_ReportView__reportPreparer :: forall . Lens' c Markup
          lens_ReportView__reportPreparer = (.) lens_reportView lens_ReportView__reportPreparer
          {-# INLINE lens_ReportView__reportPreparer #-}
          lens_ReportView__reportPreparerAddress :: forall . Lens' c Markup
          lens_ReportView__reportPreparerAddress = (.) lens_reportView lens_ReportView__reportPreparerAddress
          {-# INLINE lens_ReportView__reportPreparerAddress #-}
          lens_ReportView__reportPreparerEIN :: forall . Lens' c Markup
          lens_ReportView__reportPreparerEIN = (.) lens_reportView lens_ReportView__reportPreparerEIN
          {-# INLINE lens_ReportView__reportPreparerEIN #-}
          lens_ReportView__reportPreparerEMail :: forall . Lens' c Markup
          lens_ReportView__reportPreparerEMail = (.) lens_reportView lens_ReportView__reportPreparerEMail
          {-# INLINE lens_ReportView__reportPreparerEMail #-}
          lens_ReportView__reportPreparerWebsite :: forall . Lens' c Markup
          lens_ReportView__reportPreparerWebsite = (.) lens_reportView lens_ReportView__reportPreparerWebsite
          {-# INLINE lens_ReportView__reportPreparerWebsite #-}
          lens_ReportView__reportPrivacyPolicy :: forall . Lens' c Markup
          lens_ReportView__reportPrivacyPolicy = (.) lens_reportView lens_ReportView__reportPrivacyPolicy
          {-# INLINE lens_ReportView__reportPrivacyPolicy #-}
          lens_ReportView__reportRedacted :: forall . Lens' c Bool
          lens_ReportView__reportRedacted = (.) lens_reportView lens_ReportView__reportRedacted
          {-# INLINE lens_ReportView__reportRedacted #-}
          lens_ReportView__reportRevision :: forall . Lens' c Integer
          lens_ReportView__reportRevision = (.) lens_reportView lens_ReportView__reportRevision
          {-# INLINE lens_ReportView__reportRevision #-}
          lens_ReportView__reportScopeOfWork :: forall . Lens' c Markup
          lens_ReportView__reportScopeOfWork = (.) lens_reportView lens_ReportView__reportScopeOfWork
          {-# INLINE lens_ReportView__reportScopeOfWork #-}
          lens_ReportView__reportSources :: forall . Lens' c MarkupPairs
          lens_ReportView__reportSources = (.) lens_reportView lens_ReportView__reportSources
          {-# INLINE lens_ReportView__reportSources #-}
          lens_ReportView__reportStandardsVersion :: forall . Lens' c ReportStandard
          lens_ReportView__reportStandardsVersion = (.) lens_reportView lens_ReportView__reportStandardsVersion
          {-# INLINE lens_ReportView__reportStandardsVersion #-}
          lens_ReportView__reportStatus :: forall . Lens' c ReportStatus
          lens_ReportView__reportStatus = (.) lens_reportView lens_ReportView__reportStatus
          {-# INLINE lens_ReportView__reportStatus #-}
          lens_ReportView__reportTitle :: forall . Lens' c Markup
          lens_ReportView__reportTitle = (.) lens_reportView lens_ReportView__reportTitle
          {-# INLINE lens_ReportView__reportTitle #-}
          lens_ReportView__reportUUID :: forall . Lens' c UUID
          lens_ReportView__reportUUID = (.) lens_reportView lens_ReportView__reportUUID
          {-# INLINE lens_ReportView__reportUUID #-}
          lens_ReportView__reportValueApproachInfo :: forall . Lens' c ReportValueApproachInfo
          lens_ReportView__reportValueApproachInfo = (.) lens_reportView lens_ReportView__reportValueApproachInfo
          {-# INLINE lens_ReportView__reportValueApproachInfo #-}
          lens_ReportView__reportValueTypeInfo :: forall . Lens' c ReportValueTypeInfo
          lens_ReportView__reportValueTypeInfo = (.) lens_reportView lens_ReportView__reportValueTypeInfo
          {-# INLINE lens_ReportView__reportValueTypeInfo #-}
class HasText c
    where lens_text :: Lens' c Text
class HasURI c
    where lens_uRI :: Lens' c URI
          lens_URI_uriAuthority :: forall . Lens' c (Maybe URIAuth)
          lens_URI_uriAuthority = (.) lens_uRI lens_URI_uriAuthority
          {-# INLINE lens_URI_uriAuthority #-}
          lens_URI_uriFragment :: forall . Lens' c String
          lens_URI_uriFragment = (.) lens_uRI lens_URI_uriFragment
          {-# INLINE lens_URI_uriFragment #-}
          lens_URI_uriPath :: forall . Lens' c String
          lens_URI_uriPath = (.) lens_uRI lens_URI_uriPath
          {-# INLINE lens_URI_uriPath #-}
          lens_URI_uriQuery :: forall . Lens' c String
          lens_URI_uriQuery = (.) lens_uRI lens_URI_uriQuery
          {-# INLINE lens_URI_uriQuery #-}
          lens_URI_uriScheme :: forall . Lens' c String
          lens_URI_uriScheme = (.) lens_uRI lens_URI_uriScheme
          {-# INLINE lens_URI_uriScheme #-}
class HasUUID c
    where lens_uUID :: Lens' c UUID
class HasUnits c
    where lens_units :: Lens' c Units
class HasUserId c
    where lens_userId :: Lens' c UserId
          lens_UserId__unUserId :: forall . Lens' c Integer
          lens_UserId__unUserId = (.) lens_userId lens_UserId__unUserId
          {-# INLINE lens_UserId__unUserId #-}
instance PathStart Univ String
    where data UPeek Univ String = UPeek_String (UPath Univ String) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_String
          upeekPath (UPeek_String p _) = p
          upeekValue (UPeek_String _ x) = x
          type UPath Univ String = Path_View String UPath_JSONText
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                   JSONText)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                            JSONText)])) [Path_To Proxy] ++ [])
instance PathStart Univ Int64
    where data UPeek Univ Int64 = UPeek_Int64 (UPath Univ Int64) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Int64
          upeekPath (UPeek_Int64 p _) = p
          upeekValue (UPeek_Int64 _ x) = x
          type UPath Univ Int64 = UPath_Int64
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Bool
    where data UPeek Univ Bool = UPeek_Bool (UPath Univ Bool) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Bool
          upeekPath (UPeek_Bool p _) = p
          upeekValue (UPeek_Bool _ x) = x
          type UPath Univ Bool = Path_View Bool (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ Double
    where data UPeek Univ Double = UPeek_Double (UPath Univ Double) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Double
          upeekPath (UPeek_Double p _) = p
          upeekValue (UPeek_Double _ x) = x
          type UPath Univ Double = Path_View Double (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ Int
    where data UPeek Univ Int = UPeek_Int (UPath Univ Int) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Int
          upeekPath (UPeek_Int p _) = p
          upeekValue (UPeek_Int _ x) = x
          type UPath Univ Int = UPath_Int
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Dimension
    where data UPeek Univ Dimension = UPeek_Dimension (UPath Univ Dimension) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Dimension
          upeekPath (UPeek_Dimension p _) = p
          upeekValue (UPeek_Dimension _ x) = x
          type UPath Univ Dimension = Path_View Dimension UPath_JSONText
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                   JSONText)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                            JSONText)])) [Path_To Proxy] ++ [])
instance PathStart Univ ImageCrop
    where data UPeek Univ ImageCrop = UPeek_ImageCrop (UPath Univ ImageCrop) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ImageCrop
          upeekPath (UPeek_ImageCrop p _) = p
          upeekValue (UPeek_ImageCrop _ x) = x
          type UPath Univ ImageCrop = UPath_ImageCrop
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ ImageSize
    where data UPeek Univ ImageSize = UPeek_ImageSize (UPath Univ ImageSize) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ImageSize
          upeekPath (UPeek_ImageSize p _) = p
          upeekValue (UPeek_ImageSize _ x) = x
          type UPath Univ ImageSize = UPath_ImageSize
          upeekRow _unv (_xconc@(ImageSize {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Dimension]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                     Dimension)])) [UPath_ImageSize_dim] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Double]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Double)])) [UPath_ImageSize_size] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Units]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Units)])) [UPath_ImageSize_units] ++ [])))
          upeekTree _unv d (_xconc@(ImageSize {})) = case d of
                                                         Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                         _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Dimension]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                              Dimension)])) [UPath_ImageSize_dim] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Double]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Double)])) [UPath_ImageSize_size] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Units]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Units)])) [UPath_ImageSize_units] ++ [])))
instance PathStart Univ Units
    where data UPeek Univ Units = UPeek_Units (UPath Univ Units) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Units
          upeekPath (UPeek_Units p _) = p
          upeekValue (UPeek_Units _ x) = x
          type UPath Univ Units = Path_View Units UPath_JSONText
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                   JSONText)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                            JSONText)])) [Path_To Proxy] ++ [])
instance PathStart Univ ImageFile
    where data UPeek Univ ImageFile = UPeek_ImageFile (UPath Univ ImageFile) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ImageFile
          upeekPath (UPeek_ImageFile p _) = p
          upeekValue (UPeek_ImageFile _ x) = x
          type UPath Univ ImageFile = UPath_ImageFile
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Integer
    where data UPeek Univ Integer = UPeek_Integer (UPath Univ Integer) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Integer
          upeekPath (UPeek_Integer p _) = p
          upeekValue (UPeek_Integer _ x) = x
          type UPath Univ Integer = UPath_Integer
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ JSONText
    where data UPeek Univ JSONText = UPeek_JSONText (UPath Univ JSONText) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_JSONText
          upeekPath (UPeek_JSONText p _) = p
          upeekValue (UPeek_JSONText _ x) = x
          type UPath Univ JSONText = UPath_JSONText
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Markup
    where data UPeek Univ Markup = UPeek_Markup (UPath Univ Markup) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Markup
          upeekPath (UPeek_Markup p _) = p
          upeekValue (UPeek_Markup _ x) = x
          type UPath Univ Markup = UPath_Markup
          upeekRow _unv (_xconc@(Markdown {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                               Text)])) [UPath_Markup_markdownText] ++ [])
          upeekRow _unv (_xconc@(Html {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                           Text)])) [UPath_Markup_htmlText] ++ [])
          upeekRow _unv (_xconc@(LaTeX {})) = Node (upeekCons idPath Nothing) []
          upeekRow _unv (_xconc@(Pandoc {})) = Node (upeekCons idPath Nothing) []
          upeekRow _unv (_xconc@(Markup {})) = Node (upeekCons idPath Nothing) []
          upeekTree _unv d (_xconc@(Markdown {})) = case d of
                                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        Text)])) [UPath_Markup_markdownText] ++ [])
          upeekTree _unv d (_xconc@(Html {})) = case d of
                                                    Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                    _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                    Text)])) [UPath_Markup_htmlText] ++ [])
          upeekTree _unv _ (_xconc@(LaTeX {})) = Node (upeekCons idPath (Just (u _xconc))) []
          upeekTree _unv _ (_xconc@(Pandoc {})) = Node (upeekCons idPath (Just (u _xconc))) []
          upeekTree _unv _ (_xconc@(Markup {})) = Node (upeekCons idPath (Just (u _xconc))) []
instance PathStart Univ Permissions
    where data UPeek Univ Permissions = UPeek_Permissions (UPath Univ Permissions) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Permissions
          upeekPath (UPeek_Permissions p _) = p
          upeekValue (UPeek_Permissions _ x) = x
          type UPath Univ Permissions = UPath_Permissions
          upeekRow _unv (_xconc@(Permissions {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UserId]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                    UserId)])) [UPath_Permissions_owner] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UserIds]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             UserIds)])) [UPath_Permissions_writers] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UserIds]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         UserIds)])) [UPath_Permissions_readers] ++ [])))
          upeekTree _unv d (_xconc@(Permissions {})) = case d of
                                                           Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                           _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UserId]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                             UserId)])) [UPath_Permissions_owner] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UserIds]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   UserIds)])) [UPath_Permissions_writers] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UserIds]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            UserIds)])) [UPath_Permissions_readers] ++ [])))
instance PathStart Univ UserIds
    where data UPeek Univ UserIds = UPeek_UserIds (UPath Univ UserIds) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_UserIds
          upeekPath (UPeek_UserIds p _) = p
          upeekValue (UPeek_UserIds _ x) = x
          type UPath Univ UserIds = Path_View UserIds (Path_View Text UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                               Text)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                        Text)])) [Path_To Proxy] ++ [])
instance PathStart Univ AbbrevPair
    where data UPeek Univ AbbrevPair = UPeek_AbbrevPair (UPath Univ AbbrevPair) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_AbbrevPair
          upeekPath (UPeek_AbbrevPair p _) = p
          upeekValue (UPeek_AbbrevPair _ x) = x
          type UPath Univ AbbrevPair = Path_Pair (Path_View CIString (Path_View Text UPath_JSONText)) UPath_Markup
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [CIString]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                   CIString)])) [Path_First] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [Path_Second] ++ []))
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [CIString]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                            CIString)])) [Path_First] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [Path_Second] ++ []))
instance PathStart Univ AbbrevPairs
    where data UPeek Univ AbbrevPairs = UPeek_AbbrevPairs (UPath Univ AbbrevPairs) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_AbbrevPairs
          upeekPath (UPeek_AbbrevPairs p _) = p
          upeekValue (UPeek_AbbrevPairs _ x) = x
          type UPath Univ AbbrevPairs = Path_OMap AbbrevPairID (Path_Pair (Path_View CIString (Path_View Text UPath_JSONText)) UPath_Markup)
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [AbbrevPair]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                            AbbrevPair)])) (map (\(_k,
                                                                                                                                                                                                                                                                                                   _) -> Path_At _k) (toPairs _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [AbbrevPair]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                     AbbrevPair)])) (map (\(_k,
                                                                                                                                                                                                                                                                                            _) -> Path_At _k) (toPairs _xyz)) ++ [])
instance PathStart Univ Author
    where data UPeek Univ Author = UPeek_Author (UPath Univ Author) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Author
          upeekPath (UPeek_Author p _) = p
          upeekValue (UPeek_Author _ x) = x
          type UPath Univ Author = UPath_Author
          upeekRow _unv (_xconc@(Author {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                               Markup)])) [UPath_Author_authorName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Markup)])) [UPath_Author_authorCredentials] ++ []))
          upeekTree _unv d (_xconc@(Author {})) = case d of
                                                      Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                      _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        Markup)])) [UPath_Author_authorName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup)])) [UPath_Author_authorCredentials] ++ []))
instance PathStart Univ Authors
    where data UPeek Univ Authors = UPeek_Authors (UPath Univ Authors) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Authors
          upeekPath (UPeek_Authors p _) = p
          upeekValue (UPeek_Authors _ x) = x
          type UPath Univ Authors = Path_OMap AuthorID UPath_Author
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Author]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        Author)])) (map (\(_k,
                                                                                                                                                                                                                                                                                           _) -> Path_At _k) (toPairs _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Author]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 Author)])) (map (\(_k,
                                                                                                                                                                                                                                                                                    _) -> Path_At _k) (toPairs _xyz)) ++ [])
instance PathStart Univ Branding
    where data UPeek Univ Branding = UPeek_Branding (UPath Univ Branding) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Branding
          upeekPath (UPeek_Branding p _) = p
          upeekValue (UPeek_Branding _ x) = x
          type UPath Univ Branding = Path_View Branding (Path_View Text UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                               Text)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                        Text)])) [Path_To Proxy] ++ [])
instance PathStart Univ MarkupPair
    where data UPeek Univ MarkupPair = UPeek_MarkupPair (UPath Univ MarkupPair) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MarkupPair
          upeekPath (UPeek_MarkupPair p _) = p
          upeekValue (UPeek_MarkupPair _ x) = x
          type UPath Univ MarkupPair = Path_Pair UPath_Markup UPath_Markup
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 Markup)])) [Path_First] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [Path_Second] ++ []))
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          Markup)])) [Path_First] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup)])) [Path_Second] ++ []))
instance PathStart Univ MarkupPairs
    where data UPeek Univ MarkupPairs = UPeek_MarkupPairs (UPath Univ MarkupPairs) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MarkupPairs
          upeekPath (UPeek_MarkupPairs p _) = p
          upeekValue (UPeek_MarkupPairs _ x) = x
          type UPath Univ MarkupPairs = Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup)
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MarkupPair]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                            MarkupPair)])) (map (\(_k,
                                                                                                                                                                                                                                                                                                   _) -> Path_At _k) (toPairs _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MarkupPair]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                     MarkupPair)])) (map (\(_k,
                                                                                                                                                                                                                                                                                            _) -> Path_At _k) (toPairs _xyz)) ++ [])
instance PathStart Univ Markups
    where data UPeek Univ Markups = UPeek_Markups (UPath Univ Markups) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Markups
          upeekPath (UPeek_Markups p _) = p
          upeekValue (UPeek_Markups _ x) = x
          type UPath Univ Markups = Path_OMap MarkupID UPath_Markup
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        Markup)])) (map (\(_k,
                                                                                                                                                                                                                                                                                           _) -> Path_At _k) (toPairs _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 Markup)])) (map (\(_k,
                                                                                                                                                                                                                                                                                    _) -> Path_At _k) (toPairs _xyz)) ++ [])
instance PathStart Univ MaybeReportIntendedUse
    where data UPeek Univ MaybeReportIntendedUse
              = UPeek_MaybeReportIntendedUse (UPath Univ MaybeReportIntendedUse) (Maybe Univ)
              deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MaybeReportIntendedUse
          upeekPath (UPeek_MaybeReportIntendedUse p _) = p
          upeekValue (UPeek_MaybeReportIntendedUse _ x) = x
          type UPath Univ MaybeReportIntendedUse = Path_View MaybeReportIntendedUse (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ Report
    where data UPeek Univ Report = UPeek_Report (UPath Univ Report) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Report
          upeekPath (UPeek_Report p _) = p
          upeekValue (UPeek_Report _ x) = x
          type UPath Univ Report = Path_View Report UPath_ReportView
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportView]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                     ReportView)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportView]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                              ReportView)])) [Path_To Proxy] ++ [])
instance PathStart Univ ReportElem
    where data UPeek Univ ReportElem = UPeek_ReportElem (UPath Univ ReportElem) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportElem
          upeekPath (UPeek_ReportElem p _) = p
          upeekValue (UPeek_ReportElem _ x) = x
          type UPath Univ ReportElem = UPath_ReportElem
          upeekRow _unv (_xconc@(ReportItem {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Item]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                 Item)])) [UPath_ReportElem_elemItem] ++ [])
          upeekRow _unv (_xconc@(ReportParagraph {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                        Markup)])) [UPath_ReportElem_elemText] ++ [])
          upeekRow _unv (_xconc@(ReportUndecided {})) = Node (upeekCons idPath Nothing) []
          upeekTree _unv d (_xconc@(ReportItem {})) = case d of
                                                          Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                          _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Item]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                          Item)])) [UPath_ReportElem_elemItem] ++ [])
          upeekTree _unv d (_xconc@(ReportParagraph {})) = case d of
                                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                 Markup)])) [UPath_ReportElem_elemText] ++ [])
          upeekTree _unv _ (_xconc@(ReportUndecided {})) = Node (upeekCons idPath (Just (u _xconc))) []
instance PathStart Univ ReportElems
    where data UPeek Univ ReportElems = UPeek_ReportElems (UPath Univ ReportElems) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportElems
          upeekPath (UPeek_ReportElems p _) = p
          upeekValue (UPeek_ReportElems _ x) = x
          type UPath Univ ReportElems = Path_OMap ReportElemID UPath_ReportElem
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportElem]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                            ReportElem)])) (map (\(_k,
                                                                                                                                                                                                                                                                                                   _) -> Path_At _k) (toPairs _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportElem]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                     ReportElem)])) (map (\(_k,
                                                                                                                                                                                                                                                                                            _) -> Path_At _k) (toPairs _xyz)) ++ [])
instance PathStart Univ ReportFlags
    where data UPeek Univ ReportFlags = UPeek_ReportFlags (UPath Univ ReportFlags) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportFlags
          upeekPath (UPeek_ReportFlags p _) = p
          upeekValue (UPeek_ReportFlags _ x) = x
          type UPath Univ ReportFlags = UPath_ReportFlags
          upeekRow _unv (_xconc@(ReportFlags {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                  Bool)])) [UPath_ReportFlags_hideEmptyItemFields] ++ [])
          upeekTree _unv d (_xconc@(ReportFlags {})) = case d of
                                                           Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                           _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                           Bool)])) [UPath_ReportFlags_hideEmptyItemFields] ++ [])
instance PathStart Univ ReportIntendedUse
    where data UPeek Univ ReportIntendedUse = UPeek_ReportIntendedUse (UPath Univ ReportIntendedUse) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportIntendedUse
          upeekPath (UPeek_ReportIntendedUse p _) = p
          upeekValue (UPeek_ReportIntendedUse _ x) = x
          type UPath Univ ReportIntendedUse = Path_View ReportIntendedUse (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ ReportStandard
    where data UPeek Univ ReportStandard = UPeek_ReportStandard (UPath Univ ReportStandard) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportStandard
          upeekPath (UPeek_ReportStandard p _) = p
          upeekValue (UPeek_ReportStandard _ x) = x
          type UPath Univ ReportStandard = UPath_ReportStandard
          upeekRow _unv (_xconc@(ReportStandard {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Int]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                    Int)])) [UPath_ReportStandard_unReportStandard] ++ [])
          upeekTree _unv d (_xconc@(ReportStandard {})) = case d of
                                                              Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                              _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Int]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                             Int)])) [UPath_ReportStandard_unReportStandard] ++ [])
instance PathStart Univ ReportStatus
    where data UPeek Univ ReportStatus = UPeek_ReportStatus (UPath Univ ReportStatus) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportStatus
          upeekPath (UPeek_ReportStatus p _) = p
          upeekValue (UPeek_ReportStatus _ x) = x
          type UPath Univ ReportStatus = Path_View ReportStatus (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ ReportValueApproachInfo
    where data UPeek Univ ReportValueApproachInfo
              = UPeek_ReportValueApproachInfo (UPath Univ ReportValueApproachInfo) (Maybe Univ)
              deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportValueApproachInfo
          upeekPath (UPeek_ReportValueApproachInfo p _) = p
          upeekValue (UPeek_ReportValueApproachInfo _ x) = x
          type UPath Univ ReportValueApproachInfo = UPath_ReportValueApproachInfo
          upeekRow _unv (_xconc@(ReportValueApproachInfo {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportValueApproachInfo_reportValueApproachName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportValueApproachInfo_reportValueApproachDescription] ++ []))
          upeekTree _unv d (_xconc@(ReportValueApproachInfo {})) = case d of
                                                                       Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                                       _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                         Markup)])) [UPath_ReportValueApproachInfo_reportValueApproachName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportValueApproachInfo_reportValueApproachDescription] ++ []))
instance PathStart Univ ReportValueTypeInfo
    where data UPeek Univ ReportValueTypeInfo = UPeek_ReportValueTypeInfo (UPath Univ ReportValueTypeInfo) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportValueTypeInfo
          upeekPath (UPeek_ReportValueTypeInfo p _) = p
          upeekValue (UPeek_ReportValueTypeInfo _ x) = x
          type UPath Univ ReportValueTypeInfo = UPath_ReportValueTypeInfo
          upeekRow _unv (_xconc@(ReportValueTypeInfo {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportValueTypeInfo_reportValueTypeName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Markup)])) [UPath_ReportValueTypeInfo_reportValueTypeDescription] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Markup)])) [UPath_ReportValueTypeInfo_reportValueTypeDefinition] ++ [])))
          upeekTree _unv d (_xconc@(ReportValueTypeInfo {})) = case d of
                                                                   Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                                   _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportValueTypeInfo_reportValueTypeName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportValueTypeInfo_reportValueTypeDescription] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup)])) [UPath_ReportValueTypeInfo_reportValueTypeDefinition] ++ [])))
instance PathStart Univ EUI
    where data UPeek Univ EUI = UPeek_EUI (UPath Univ EUI) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_EUI
          upeekPath (UPeek_EUI p _) = p
          upeekValue (UPeek_EUI _ x) = x
          type UPath Univ EUI = Path_Either UPath_URI UPath_ImageFile
          upeekRow _unv (_xconc@(Left _)) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [URI]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                         URI)])) [Path_Left] ++ [])
          upeekRow _unv (_xconc@(Right _)) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ImageFile]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                ImageFile)])) [Path_Right] ++ [])
          upeekTree _unv d (_xconc@(Left _)) = case d of
                                                   Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                   _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [URI]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                  URI)])) [Path_Left] ++ [])
          upeekTree _unv d (_xconc@(Right _)) = case d of
                                                    Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                    _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ImageFile]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                         ImageFile)])) [Path_Right] ++ [])
instance PathStart Univ MEUI
    where data UPeek Univ MEUI = UPeek_MEUI (UPath Univ MEUI) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MEUI
          upeekPath (UPeek_MEUI p _) = p
          upeekValue (UPeek_MEUI _ x) = x
          type UPath Univ MEUI = Path_Maybe (Path_Either UPath_URI UPath_ImageFile)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [EUI]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                              EUI)])) [Path_Just] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [EUI]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                       EUI)])) [Path_Just] ++ [])
instance PathStart Univ MaybeImageFile
    where data UPeek Univ MaybeImageFile = UPeek_MaybeImageFile (UPath Univ MaybeImageFile) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MaybeImageFile
          upeekPath (UPeek_MaybeImageFile p _) = p
          upeekValue (UPeek_MaybeImageFile _ x) = x
          type UPath Univ MaybeImageFile = Path_View MaybeImageFile (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ ReportImage
    where data UPeek Univ ReportImage = UPeek_ReportImage (UPath Univ ReportImage) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportImage
          upeekPath (UPeek_ReportImage p _) = p
          upeekValue (UPeek_ReportImage _ x) = x
          type UPath Univ ReportImage = Path_View ReportImage UPath_ReportImageView
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportImageView]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                          ReportImageView)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportImageView]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                   ReportImageView)])) [Path_To Proxy] ++ [])
instance PathStart Univ ReportImages
    where data UPeek Univ ReportImages = UPeek_ReportImages (UPath Univ ReportImages) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportImages
          upeekPath (UPeek_ReportImages p _) = p
          upeekValue (UPeek_ReportImages _ x) = x
          type UPath Univ ReportImages = Path_OMap ReportImageID (Path_View ReportImage UPath_ReportImageView)
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportImage]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                             ReportImage)])) (map (\(_k,
                                                                                                                                                                                                                                                                                                     _) -> Path_At _k) (toPairs _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportImage]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                      ReportImage)])) (map (\(_k,
                                                                                                                                                                                                                                                                                              _) -> Path_At _k) (toPairs _xyz)) ++ [])
instance PathStart Univ ReadOnlyFilePath
    where data UPeek Univ ReadOnlyFilePath = UPeek_ReadOnlyFilePath (UPath Univ ReadOnlyFilePath) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReadOnlyFilePath
          upeekPath (UPeek_ReadOnlyFilePath p _) = p
          upeekValue (UPeek_ReadOnlyFilePath _ x) = x
          type UPath Univ ReadOnlyFilePath = Path_View ReadOnlyFilePath (Path_View String UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 String)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [String]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                          String)])) [Path_To Proxy] ++ [])
instance PathStart Univ ReportImageView
    where data UPeek Univ ReportImageView = UPeek_ReportImageView (UPath Univ ReportImageView) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportImageView
          upeekPath (UPeek_ReportImageView p _) = p
          upeekValue (UPeek_ReportImageView _ x) = x
          type UPath Univ ReportImageView = UPath_ReportImageView
          upeekRow _unv (_xconc@(ReportImageView {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [SaneSizeImageSize]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                   SaneSizeImageSize)])) [UPath_ReportImageView__picSize] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ImageCrop]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ImageCrop)])) [UPath_ReportImageView__picCrop] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup)])) [UPath_ReportImageView__picCaption] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MEUI]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  MEUI)])) [UPath_ReportImageView__picOriginal] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Bool)])) [UPath_ReportImageView__picMustEnlarge] ++ [])))))
          upeekTree _unv d (_xconc@(ReportImageView {})) = case d of
                                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [SaneSizeImageSize]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                            SaneSizeImageSize)])) [UPath_ReportImageView__picSize] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ImageCrop]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ImageCrop)])) [UPath_ReportImageView__picCrop] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportImageView__picCaption] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MEUI]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  MEUI)])) [UPath_ReportImageView__picOriginal] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Bool)])) [UPath_ReportImageView__picMustEnlarge] ++ [])))))
instance PathStart Univ ReportView
    where data UPeek Univ ReportView = UPeek_ReportView (UPath Univ ReportView) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportView
          upeekPath (UPeek_ReportView p _) = p
          upeekValue (UPeek_ReportView _ x) = x
          type UPath Univ ReportView = UPath_ReportView
          upeekRow _unv (_xconc@(ReportView {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReadOnlyFilePath]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                             ReadOnlyFilePath)])) [UPath_ReportView__reportFolder] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportView__reportName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Markup)])) [UPath_ReportView__reportDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportView__reportContractDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportInspectionDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportView__reportEffectiveDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Authors]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Authors)])) [UPath_ReportView__reportAuthors] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Markup)])) [UPath_ReportView__reportPreparer] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportPreparerEIN] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Markup)])) [UPath_ReportView__reportPreparerAddress] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Markup)])) [UPath_ReportView__reportPreparerEMail] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Markup)])) [UPath_ReportView__reportPreparerWebsite] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [AbbrevPairs]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AbbrevPairs)])) [UPath_ReportView__reportAbbrevs] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Markup)])) [UPath_ReportView__reportTitle] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Markup)])) [UPath_ReportView__reportHeader] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportView__reportFooter] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MaybeReportIntendedUse]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     MaybeReportIntendedUse)])) [UPath_ReportView__reportIntendedUse] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportValueTypeInfo]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ReportValueTypeInfo)])) [UPath_ReportView__reportValueTypeInfo] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportValueApproachInfo]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ReportValueApproachInfo)])) [UPath_ReportView__reportValueApproachInfo] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportClientName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Markup)])) [UPath_ReportView__reportClientAddress] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Markup)])) [UPath_ReportView__reportClientGreeting] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportItemsOwnerFull] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportView__reportItemsOwner] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Markup)])) [UPath_ReportView__reportBriefItems] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup)])) [UPath_ReportView__reportInspectionLocation] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportElems]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ReportElems)])) [UPath_ReportView__reportBody] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MarkupPairs]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         MarkupPairs)])) [UPath_ReportView__reportGlossary] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MarkupPairs]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    MarkupPairs)])) [UPath_ReportView__reportSources] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Markup)])) [UPath_ReportView__reportLetterOfTransmittal] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportScopeOfWork] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markups]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Markups)])) [UPath_ReportView__reportCertification] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markups]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markups)])) [UPath_ReportView__reportLimitingConditions] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Markup)])) [UPath_ReportView__reportPrivacyPolicy] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Permissions]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Permissions)])) [UPath_ReportView__reportPerms] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Integer]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Integer)])) [UPath_ReportView__reportRevision] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Int64]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Int64)])) [UPath_ReportView__reportCreated] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Branding]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Branding)])) [UPath_ReportView__reportBranding] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportStatus]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ReportStatus)])) [UPath_ReportView__reportStatus] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Bool)])) [UPath_ReportView__reportRedacted] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportFlags]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ReportFlags)])) [UPath_ReportView__reportFlags] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UUID]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                UUID)])) [UPath_ReportView__reportUUID] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Bool)])) [UPath_ReportView__reportOrderByItemName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Bool)])) [UPath_ReportView__reportDisplayItemName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportStandard]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ReportStandard)])) [UPath_ReportView__reportStandardsVersion] ++ [])))))))))))))))))))))))))))))))))))))))))))))
          upeekTree _unv d (_xconc@(ReportView {})) = case d of
                                                          Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                          _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReadOnlyFilePath]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                      ReadOnlyFilePath)])) [UPath_ReportView__reportFolder] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportView__reportName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportView__reportDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportView__reportContractDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup)])) [UPath_ReportView__reportInspectionDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportView__reportEffectiveDate] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Authors]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Authors)])) [UPath_ReportView__reportAuthors] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Markup)])) [UPath_ReportView__reportPreparer] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportView__reportPreparerEIN] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Markup)])) [UPath_ReportView__reportPreparerAddress] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportView__reportPreparerEMail] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Markup)])) [UPath_ReportView__reportPreparerWebsite] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [AbbrevPairs]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         AbbrevPairs)])) [UPath_ReportView__reportAbbrevs] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Markup)])) [UPath_ReportView__reportTitle] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markup)])) [UPath_ReportView__reportHeader] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup)])) [UPath_ReportView__reportFooter] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MaybeReportIntendedUse]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              MaybeReportIntendedUse)])) [UPath_ReportView__reportIntendedUse] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportValueTypeInfo]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ReportValueTypeInfo)])) [UPath_ReportView__reportValueTypeInfo] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportValueApproachInfo]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ReportValueApproachInfo)])) [UPath_ReportView__reportValueApproachInfo] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportClientName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup)])) [UPath_ReportView__reportClientAddress] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Markup)])) [UPath_ReportView__reportClientGreeting] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup)])) [UPath_ReportView__reportItemsOwnerFull] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportView__reportItemsOwner] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportView__reportBriefItems] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup)])) [UPath_ReportView__reportInspectionLocation] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportElems]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ReportElems)])) [UPath_ReportView__reportBody] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MarkupPairs]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 MarkupPairs)])) [UPath_ReportView__reportGlossary] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MarkupPairs]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         MarkupPairs)])) [UPath_ReportView__reportSources] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Markup)])) [UPath_ReportView__reportLetterOfTransmittal] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Markup)])) [UPath_ReportView__reportScopeOfWork] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markups]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Markups)])) [UPath_ReportView__reportCertification] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markups]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Markups)])) [UPath_ReportView__reportLimitingConditions] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Markup)])) [UPath_ReportView__reportPrivacyPolicy] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Permissions]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Permissions)])) [UPath_ReportView__reportPerms] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Integer]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Integer)])) [UPath_ReportView__reportRevision] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Int64]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Int64)])) [UPath_ReportView__reportCreated] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Branding]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Branding)])) [UPath_ReportView__reportBranding] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportStatus]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ReportStatus)])) [UPath_ReportView__reportStatus] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Bool)])) [UPath_ReportView__reportRedacted] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportFlags]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ReportFlags)])) [UPath_ReportView__reportFlags] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [UUID]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              UUID)])) [UPath_ReportView__reportUUID] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Bool)])) [UPath_ReportView__reportOrderByItemName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Bool]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Bool)])) [UPath_ReportView__reportDisplayItemName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportStandard]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ReportStandard)])) [UPath_ReportView__reportStandardsVersion] ++ [])))))))))))))))))))))))))))))))))))))))))))))
instance PathStart Univ SaneSizeImageSize
    where data UPeek Univ SaneSizeImageSize = UPeek_SaneSizeImageSize (UPath Univ SaneSizeImageSize) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_SaneSizeImageSize
          upeekPath (UPeek_SaneSizeImageSize p _) = p
          upeekValue (UPeek_SaneSizeImageSize _ x) = x
          type UPath Univ SaneSizeImageSize = Path_View SaneSizeImageSize UPath_ImageSize
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ImageSize]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                    ImageSize)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ImageSize]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                             ImageSize)])) [Path_To Proxy] ++ [])
instance PathStart Univ Item
    where data UPeek Univ Item = UPeek_Item (UPath Univ Item) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Item
          upeekPath (UPeek_Item p _) = p
          upeekValue (UPeek_Item _ x) = x
          type UPath Univ Item = UPath_Item
          upeekRow _unv (_xconc@(Item {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                           Text)])) [UPath_Item_itemName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MIM]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          MIM)])) [UPath_Item_fields] ++ (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportImages]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ReportImages)])) [UPath_Item_images] ++ [])))
          upeekTree _unv d (_xconc@(Item {})) = case d of
                                                    Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                    _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                    Text)])) [UPath_Item_itemName] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MIM]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                MIM)])) [UPath_Item_fields] ++ (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [ReportImages]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ReportImages)])) [UPath_Item_images] ++ [])))
instance PathStart Univ MIM
    where data UPeek Univ MIM = UPeek_MIM (UPath Univ MIM) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MIM
          upeekPath (UPeek_MIM p _) = p
          upeekValue (UPeek_MIM _ x) = x
          type UPath Univ MIM = Path_Map ItemFieldName UPath_Markup
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        Markup)])) (map (\(_k,
                                                                                                                                                                                                                                                                                           _) -> Path_Look _k) (toList _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Markup]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 Markup)])) (map (\(_k,
                                                                                                                                                                                                                                                                                    _) -> Path_Look _k) (toList _xyz)) ++ [])
instance PathStart Univ MRR
    where data UPeek Univ MRR = UPeek_MRR (UPath Univ MRR) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_MRR
          upeekPath (UPeek_MRR p _) = p
          upeekValue (UPeek_MRR _ x) = x
          type UPath Univ MRR = Path_Map ReportID (Path_View Report UPath_ReportView)
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Report]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        Report)])) (map (\(_k,
                                                                                                                                                                                                                                                                                           _) -> Path_Look _k) (toList _xyz)) ++ [])
          upeekTree _unv d (_xconc@_xyz) = case d of
                                               Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                               _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Report]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                 Report)])) (map (\(_k,
                                                                                                                                                                                                                                                                                    _) -> Path_Look _k) (toList _xyz)) ++ [])
instance PathStart Univ ReportMap
    where data UPeek Univ ReportMap = UPeek_ReportMap (UPath Univ ReportMap) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_ReportMap
          upeekPath (UPeek_ReportMap p _) = p
          upeekValue (UPeek_ReportMap _ x) = x
          type UPath Univ ReportMap = UPath_ReportMap
          upeekRow _unv (_xconc@(ReportMap {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MRR]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                               MRR)])) [UPath_ReportMap_unReportMap] ++ [])
          upeekTree _unv d (_xconc@(ReportMap {})) = case d of
                                                         Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                                         _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [MRR]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                        MRR)])) [UPath_ReportMap_unReportMap] ++ [])
instance PathStart Univ CIString
    where data UPeek Univ CIString = UPeek_CIString (UPath Univ CIString) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_CIString
          upeekPath (UPeek_CIString p _) = p
          upeekValue (UPeek_CIString _ x) = x
          type UPath Univ CIString = Path_View CIString (Path_View Text UPath_JSONText)
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                               Text)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [Text]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                        Text)])) [Path_To Proxy] ++ [])
instance PathStart Univ URI
    where data UPeek Univ URI = UPeek_URI (UPath Univ URI) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_URI
          upeekPath (UPeek_URI p _) = p
          upeekValue (UPeek_URI _ x) = x
          type UPath Univ URI = UPath_URI
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Text
    where data UPeek Univ Text = UPeek_Text (UPath Univ Text) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_Text
          upeekPath (UPeek_Text p _) = p
          upeekValue (UPeek_Text _ x) = x
          type UPath Univ Text = Path_View Text UPath_JSONText
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (\x' -> Node (upeekCons idPath (Just (u x'))) []) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                                   JSONText)])) [Path_To Proxy] ++ [])
          upeekTree _unv d _xconc = case d of
                                        Just 0 -> Node (upeekCons idPath (Just (u _xconc))) []
                                        _ -> Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (mapPeek f) (map (upeekTree _unv (fmap pred d)) (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy Univ)) _xconc :: [JSONText]) :: [Tree (UPeek Univ
                                                                                                                                                                                                                                                            JSONText)])) [Path_To Proxy] ++ [])
instance PathStart Univ UserId
    where data UPeek Univ UserId = UPeek_UserId (UPath Univ UserId) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_UserId
          upeekPath (UPeek_UserId p _) = p
          upeekValue (UPeek_UserId _ x) = x
          type UPath Univ UserId = UPath_UserId
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ UUID
    where data UPeek Univ UUID = UPeek_UUID (UPath Univ UUID) (Maybe Univ) deriving (Eq, Show, Generic, FromJSON, ToJSON)
          upeekCons = UPeek_UUID
          upeekPath (UPeek_UUID p _) = p
          upeekValue (UPeek_UUID _ x) = x
          type UPath Univ UUID = UPath_UUID
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ _ x = Node (upeekCons idPath (Just (u x))) []
instance ToLens Univ String
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Int64
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Bool
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Double
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Int
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Dimension
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ImageCrop
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ImageSize
    where toLens (UPath_ImageSize_dim _p) = lens_ImageSize_dim . toLens _p
          toLens (UPath_ImageSize_size _p) = lens_ImageSize_size . toLens _p
          toLens (UPath_ImageSize_units _p) = lens_ImageSize_units . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Units
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ImageFile
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Integer
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ JSONText
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Markup
    where toLens (UPath_Markup_markdownText _p) = lens_Markup_markdownText . toLens _p
          toLens (UPath_Markup_htmlText _p) = lens_Markup_htmlText . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Permissions
    where toLens (UPath_Permissions_owner _p) = lens_Permissions_owner . toLens _p
          toLens (UPath_Permissions_writers _p) = lens_Permissions_writers . toLens _p
          toLens (UPath_Permissions_readers _p) = lens_Permissions_readers . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ UserIds
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ AbbrevPair
    where toLens (Path_First _p) = _1 . toLens _p
          toLens (Path_Second _p) = _2 . toLens _p
          toLens (Path_Pair) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ AbbrevPairs
    where toLens (Path_At _k _p) = lens_omat _k . toLens _p
          toLens (Path_OMap) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Author
    where toLens (UPath_Author_authorName _p) = lens_Author_authorName . toLens _p
          toLens (UPath_Author_authorCredentials _p) = lens_Author_authorCredentials . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Authors
    where toLens (Path_At _k _p) = lens_omat _k . toLens _p
          toLens (Path_OMap) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Branding
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MarkupPair
    where toLens (Path_First _p) = _1 . toLens _p
          toLens (Path_Second _p) = _2 . toLens _p
          toLens (Path_Pair) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MarkupPairs
    where toLens (Path_At _k _p) = lens_omat _k . toLens _p
          toLens (Path_OMap) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Markups
    where toLens (Path_At _k _p) = lens_omat _k . toLens _p
          toLens (Path_OMap) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MaybeReportIntendedUse
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Report
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportElem
    where toLens (UPath_ReportElem_elemItem _p) = lens_ReportElem_elemItem . toLens _p
          toLens (UPath_ReportElem_elemText _p) = lens_ReportElem_elemText . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportElems
    where toLens (Path_At _k _p) = lens_omat _k . toLens _p
          toLens (Path_OMap) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportFlags
    where toLens (UPath_ReportFlags_hideEmptyItemFields _p) = lens_ReportFlags_hideEmptyItemFields . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportIntendedUse
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportStandard
    where toLens (UPath_ReportStandard_unReportStandard _p) = lens_ReportStandard_unReportStandard . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportStatus
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportValueApproachInfo
    where toLens (UPath_ReportValueApproachInfo_reportValueApproachName _p) = lens_ReportValueApproachInfo_reportValueApproachName . toLens _p
          toLens (UPath_ReportValueApproachInfo_reportValueApproachDescription _p) = lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportValueTypeInfo
    where toLens (UPath_ReportValueTypeInfo_reportValueTypeName _p) = lens_ReportValueTypeInfo_reportValueTypeName . toLens _p
          toLens (UPath_ReportValueTypeInfo_reportValueTypeDescription _p) = lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _p
          toLens (UPath_ReportValueTypeInfo_reportValueTypeDefinition _p) = lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ EUI
    where toLens (Path_Left _p) = _Left . toLens _p
          toLens (Path_Right _p) = _Right . toLens _p
          toLens (Path_Either) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MEUI
    where toLens (Path_Just _p) = _Just . toLens _p
          toLens (Path_Maybe) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MaybeImageFile
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportImage
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportImages
    where toLens (Path_At _k _p) = lens_omat _k . toLens _p
          toLens (Path_OMap) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReadOnlyFilePath
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportImageView
    where toLens (UPath_ReportImageView__picSize _p) = lens_ReportImageView__picSize . toLens _p
          toLens (UPath_ReportImageView__picCrop _p) = lens_ReportImageView__picCrop . toLens _p
          toLens (UPath_ReportImageView__picCaption _p) = lens_ReportImageView__picCaption . toLens _p
          toLens (UPath_ReportImageView__picOriginal _p) = lens_ReportImageView__picOriginal . toLens _p
          toLens (UPath_ReportImageView__picMustEnlarge _p) = lens_ReportImageView__picMustEnlarge . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportView
    where toLens (UPath_ReportView__reportFolder _p) = lens_ReportView__reportFolder . toLens _p
          toLens (UPath_ReportView__reportName _p) = lens_ReportView__reportName . toLens _p
          toLens (UPath_ReportView__reportDate _p) = lens_ReportView__reportDate . toLens _p
          toLens (UPath_ReportView__reportContractDate _p) = lens_ReportView__reportContractDate . toLens _p
          toLens (UPath_ReportView__reportInspectionDate _p) = lens_ReportView__reportInspectionDate . toLens _p
          toLens (UPath_ReportView__reportEffectiveDate _p) = lens_ReportView__reportEffectiveDate . toLens _p
          toLens (UPath_ReportView__reportAuthors _p) = lens_ReportView__reportAuthors . toLens _p
          toLens (UPath_ReportView__reportPreparer _p) = lens_ReportView__reportPreparer . toLens _p
          toLens (UPath_ReportView__reportPreparerEIN _p) = lens_ReportView__reportPreparerEIN . toLens _p
          toLens (UPath_ReportView__reportPreparerAddress _p) = lens_ReportView__reportPreparerAddress . toLens _p
          toLens (UPath_ReportView__reportPreparerEMail _p) = lens_ReportView__reportPreparerEMail . toLens _p
          toLens (UPath_ReportView__reportPreparerWebsite _p) = lens_ReportView__reportPreparerWebsite . toLens _p
          toLens (UPath_ReportView__reportAbbrevs _p) = lens_ReportView__reportAbbrevs . toLens _p
          toLens (UPath_ReportView__reportTitle _p) = lens_ReportView__reportTitle . toLens _p
          toLens (UPath_ReportView__reportHeader _p) = lens_ReportView__reportHeader . toLens _p
          toLens (UPath_ReportView__reportFooter _p) = lens_ReportView__reportFooter . toLens _p
          toLens (UPath_ReportView__reportIntendedUse _p) = lens_ReportView__reportIntendedUse . toLens _p
          toLens (UPath_ReportView__reportValueTypeInfo _p) = lens_ReportView__reportValueTypeInfo . toLens _p
          toLens (UPath_ReportView__reportValueApproachInfo _p) = lens_ReportView__reportValueApproachInfo . toLens _p
          toLens (UPath_ReportView__reportClientName _p) = lens_ReportView__reportClientName . toLens _p
          toLens (UPath_ReportView__reportClientAddress _p) = lens_ReportView__reportClientAddress . toLens _p
          toLens (UPath_ReportView__reportClientGreeting _p) = lens_ReportView__reportClientGreeting . toLens _p
          toLens (UPath_ReportView__reportItemsOwnerFull _p) = lens_ReportView__reportItemsOwnerFull . toLens _p
          toLens (UPath_ReportView__reportItemsOwner _p) = lens_ReportView__reportItemsOwner . toLens _p
          toLens (UPath_ReportView__reportBriefItems _p) = lens_ReportView__reportBriefItems . toLens _p
          toLens (UPath_ReportView__reportInspectionLocation _p) = lens_ReportView__reportInspectionLocation . toLens _p
          toLens (UPath_ReportView__reportBody _p) = lens_ReportView__reportBody . toLens _p
          toLens (UPath_ReportView__reportGlossary _p) = lens_ReportView__reportGlossary . toLens _p
          toLens (UPath_ReportView__reportSources _p) = lens_ReportView__reportSources . toLens _p
          toLens (UPath_ReportView__reportLetterOfTransmittal _p) = lens_ReportView__reportLetterOfTransmittal . toLens _p
          toLens (UPath_ReportView__reportScopeOfWork _p) = lens_ReportView__reportScopeOfWork . toLens _p
          toLens (UPath_ReportView__reportCertification _p) = lens_ReportView__reportCertification . toLens _p
          toLens (UPath_ReportView__reportLimitingConditions _p) = lens_ReportView__reportLimitingConditions . toLens _p
          toLens (UPath_ReportView__reportPrivacyPolicy _p) = lens_ReportView__reportPrivacyPolicy . toLens _p
          toLens (UPath_ReportView__reportPerms _p) = lens_ReportView__reportPerms . toLens _p
          toLens (UPath_ReportView__reportRevision _p) = lens_ReportView__reportRevision . toLens _p
          toLens (UPath_ReportView__reportCreated _p) = lens_ReportView__reportCreated . toLens _p
          toLens (UPath_ReportView__reportBranding _p) = lens_ReportView__reportBranding . toLens _p
          toLens (UPath_ReportView__reportStatus _p) = lens_ReportView__reportStatus . toLens _p
          toLens (UPath_ReportView__reportRedacted _p) = lens_ReportView__reportRedacted . toLens _p
          toLens (UPath_ReportView__reportFlags _p) = lens_ReportView__reportFlags . toLens _p
          toLens (UPath_ReportView__reportUUID _p) = lens_ReportView__reportUUID . toLens _p
          toLens (UPath_ReportView__reportOrderByItemName _p) = lens_ReportView__reportOrderByItemName . toLens _p
          toLens (UPath_ReportView__reportDisplayItemName _p) = lens_ReportView__reportDisplayItemName . toLens _p
          toLens (UPath_ReportView__reportStandardsVersion _p) = lens_ReportView__reportStandardsVersion . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ SaneSizeImageSize
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Item
    where toLens (UPath_Item_itemName _p) = lens_Item_itemName . toLens _p
          toLens (UPath_Item_fields _p) = lens_Item_fields . toLens _p
          toLens (UPath_Item_images _p) = lens_Item_images . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MIM
    where toLens (Path_Look _k _p) = mat _k . toLens _p
          toLens (Path_Map) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ MRR
    where toLens (Path_Look _k _p) = mat _k . toLens _p
          toLens (Path_Map) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ ReportMap
    where toLens (UPath_ReportMap_unReportMap _p) = lens_ReportMap_unReportMap . toLens _p
          toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ CIString
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ URI
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ Text
    where toLens (Path_To _ _p) = viewLens . toLens _p
          toLens (Path_Self) = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ UserId
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance ToLens Univ UUID
    where toLens _ = lens u (\s a -> maybe s id (unU' a))
instance U Univ String
    where u = U1
          unU' (U1 a) = Just a
          unU' _ = Nothing
instance U Univ Int64
    where u = U2
          unU' (U2 a) = Just a
          unU' _ = Nothing
instance U Univ Bool
    where u = U3
          unU' (U3 a) = Just a
          unU' _ = Nothing
instance U Univ Double
    where u = U4
          unU' (U4 a) = Just a
          unU' _ = Nothing
instance U Univ Int
    where u = U5
          unU' (U5 a) = Just a
          unU' _ = Nothing
instance U Univ Dimension
    where u = U6
          unU' (U6 a) = Just a
          unU' _ = Nothing
instance U Univ ImageCrop
    where u = U7
          unU' (U7 a) = Just a
          unU' _ = Nothing
instance U Univ ImageSize
    where u = U8
          unU' (U8 a) = Just a
          unU' _ = Nothing
instance U Univ Units
    where u = U9
          unU' (U9 a) = Just a
          unU' _ = Nothing
instance U Univ ImageFile
    where u = U10
          unU' (U10 a) = Just a
          unU' _ = Nothing
instance U Univ Integer
    where u = U11
          unU' (U11 a) = Just a
          unU' _ = Nothing
instance U Univ JSONText
    where u = U12
          unU' (U12 a) = Just a
          unU' _ = Nothing
instance U Univ Markup
    where u = U13
          unU' (U13 a) = Just a
          unU' _ = Nothing
instance U Univ Permissions
    where u = U14
          unU' (U14 a) = Just a
          unU' _ = Nothing
instance U Univ UserIds
    where u = U15
          unU' (U15 a) = Just a
          unU' _ = Nothing
instance U Univ AbbrevPair
    where u = U16
          unU' (U16 a) = Just a
          unU' _ = Nothing
instance U Univ AbbrevPairs
    where u = U17
          unU' (U17 a) = Just a
          unU' _ = Nothing
instance U Univ Author
    where u = U18
          unU' (U18 a) = Just a
          unU' _ = Nothing
instance U Univ Authors
    where u = U19
          unU' (U19 a) = Just a
          unU' _ = Nothing
instance U Univ Branding
    where u = U20
          unU' (U20 a) = Just a
          unU' _ = Nothing
instance U Univ MarkupPair
    where u = U21
          unU' (U21 a) = Just a
          unU' _ = Nothing
instance U Univ MarkupPairs
    where u = U22
          unU' (U22 a) = Just a
          unU' _ = Nothing
instance U Univ Markups
    where u = U23
          unU' (U23 a) = Just a
          unU' _ = Nothing
instance U Univ MaybeReportIntendedUse
    where u = U24
          unU' (U24 a) = Just a
          unU' _ = Nothing
instance U Univ Report
    where u = U25
          unU' (U25 a) = Just a
          unU' _ = Nothing
instance U Univ ReportElem
    where u = U26
          unU' (U26 a) = Just a
          unU' _ = Nothing
instance U Univ ReportElems
    where u = U27
          unU' (U27 a) = Just a
          unU' _ = Nothing
instance U Univ ReportFlags
    where u = U28
          unU' (U28 a) = Just a
          unU' _ = Nothing
instance U Univ ReportIntendedUse
    where u = U29
          unU' (U29 a) = Just a
          unU' _ = Nothing
instance U Univ ReportStandard
    where u = U30
          unU' (U30 a) = Just a
          unU' _ = Nothing
instance U Univ ReportStatus
    where u = U31
          unU' (U31 a) = Just a
          unU' _ = Nothing
instance U Univ ReportValueApproachInfo
    where u = U32
          unU' (U32 a) = Just a
          unU' _ = Nothing
instance U Univ ReportValueTypeInfo
    where u = U33
          unU' (U33 a) = Just a
          unU' _ = Nothing
instance U Univ EUI
    where u = U34
          unU' (U34 a) = Just a
          unU' _ = Nothing
instance U Univ MEUI
    where u = U35
          unU' (U35 a) = Just a
          unU' _ = Nothing
instance U Univ MaybeImageFile
    where u = U36
          unU' (U36 a) = Just a
          unU' _ = Nothing
instance U Univ ReportImage
    where u = U37
          unU' (U37 a) = Just a
          unU' _ = Nothing
instance U Univ ReportImages
    where u = U38
          unU' (U38 a) = Just a
          unU' _ = Nothing
instance U Univ ReadOnlyFilePath
    where u = U39
          unU' (U39 a) = Just a
          unU' _ = Nothing
instance U Univ ReportImageView
    where u = U40
          unU' (U40 a) = Just a
          unU' _ = Nothing
instance U Univ ReportView
    where u = U41
          unU' (U41 a) = Just a
          unU' _ = Nothing
instance U Univ SaneSizeImageSize
    where u = U42
          unU' (U42 a) = Just a
          unU' _ = Nothing
instance U Univ Item
    where u = U43
          unU' (U43 a) = Just a
          unU' _ = Nothing
instance U Univ MIM
    where u = U44
          unU' (U44 a) = Just a
          unU' _ = Nothing
instance U Univ MRR
    where u = U45
          unU' (U45 a) = Just a
          unU' _ = Nothing
instance U Univ ReportMap
    where u = U46
          unU' (U46 a) = Just a
          unU' _ = Nothing
instance U Univ CIString
    where u = U47
          unU' (U47 a) = Just a
          unU' _ = Nothing
instance U Univ URI
    where u = U48
          unU' (U48 a) = Just a
          unU' _ = Nothing
instance U Univ Text
    where u = U49
          unU' (U49 a) = Just a
          unU' _ = Nothing
instance U Univ UserId
    where u = U50
          unU' (U50 a) = Just a
          unU' _ = Nothing
instance U Univ UUID
    where u = U51
          unU' (U51 a) = Just a
          unU' _ = Nothing
instance HasAuthor Author
    where lens_author = id
          lens_Author_authorCredentials f (Author x1 x2) = fmap (\y1 -> Author x1 y1) (f x2)
          {-# INLINE lens_Author_authorCredentials #-}
          lens_Author_authorName f (Author x1 x2) = fmap (\y1 -> Author y1 x2) (f x1)
          {-# INLINE lens_Author_authorName #-}
instance HasBool Bool
    where lens_bool = id
instance HasBranding Branding
    where lens_branding = id
instance HasCIString CIString
    where lens_cIString = id
          lens_CIString_unCIString = iso (\(CIString x) -> x) CIString
          {-# INLINE lens_CIString_unCIString #-}
instance HasDimension Dimension
    where lens_dimension = id
instance HasDouble Double
    where lens_double = id
instance HasImageCrop ImageCrop
    where lens_imageCrop = id
          lens_ImageCrop_bottomCrop f (ImageCrop x1 x2 x3 x4 x5) = fmap (\y1 -> ImageCrop x1 y1 x3 x4 x5) (f x2)
          {-# INLINE lens_ImageCrop_bottomCrop #-}
          lens_ImageCrop_leftCrop f (ImageCrop x1 x2 x3 x4 x5) = fmap (\y1 -> ImageCrop x1 x2 y1 x4 x5) (f x3)
          {-# INLINE lens_ImageCrop_leftCrop #-}
          lens_ImageCrop_rightCrop f (ImageCrop x1 x2 x3 x4 x5) = fmap (\y1 -> ImageCrop x1 x2 x3 y1 x5) (f x4)
          {-# INLINE lens_ImageCrop_rightCrop #-}
          lens_ImageCrop_rotation f (ImageCrop x1 x2 x3 x4 x5) = fmap (\y1 -> ImageCrop x1 x2 x3 x4 y1) (f x5)
          {-# INLINE lens_ImageCrop_rotation #-}
          lens_ImageCrop_topCrop f (ImageCrop x1 x2 x3 x4 x5) = fmap (\y1 -> ImageCrop y1 x2 x3 x4 x5) (f x1)
          {-# INLINE lens_ImageCrop_topCrop #-}
instance HasImageFile ImageFile
    where lens_imageFile = id
          lens_ImageFile_imageFile f (ImageFile x1 x2 x3 x4 x5) = fmap (\y1 -> ImageFile y1 x2 x3 x4 x5) (f x1)
          {-# INLINE lens_ImageFile_imageFile #-}
          lens_ImageFile_imageFileHeight f (ImageFile x1 x2 x3 x4 x5) = fmap (\y1 -> ImageFile x1 x2 x3 y1 x5) (f x4)
          {-# INLINE lens_ImageFile_imageFileHeight #-}
          lens_ImageFile_imageFileMaxVal f (ImageFile x1 x2 x3 x4 x5) = fmap (\y1 -> ImageFile x1 x2 x3 x4 y1) (f x5)
          {-# INLINE lens_ImageFile_imageFileMaxVal #-}
          lens_ImageFile_imageFileType f (ImageFile x1 x2 x3 x4 x5) = fmap (\y1 -> ImageFile x1 y1 x3 x4 x5) (f x2)
          {-# INLINE lens_ImageFile_imageFileType #-}
          lens_ImageFile_imageFileWidth f (ImageFile x1 x2 x3 x4 x5) = fmap (\y1 -> ImageFile x1 x2 y1 x4 x5) (f x3)
          {-# INLINE lens_ImageFile_imageFileWidth #-}
instance HasImageSize ImageSize
    where lens_imageSize = id
          lens_ImageSize_dim f (ImageSize x1 x2 x3) = fmap (\y1 -> ImageSize y1 x2 x3) (f x1)
          {-# INLINE lens_ImageSize_dim #-}
          lens_ImageSize_size f (ImageSize x1 x2 x3) = fmap (\y1 -> ImageSize x1 y1 x3) (f x2)
          {-# INLINE lens_ImageSize_size #-}
          lens_ImageSize_units f (ImageSize x1 x2 x3) = fmap (\y1 -> ImageSize x1 x2 y1) (f x3)
          {-# INLINE lens_ImageSize_units #-}
instance HasInt Int
    where lens_int = id
instance HasInt64 Int64
    where lens_int64 = id
instance HasInteger Integer
    where lens_integer = id
instance HasItem Item
    where lens_item = id
          lens_Item_fields f (Item x1 x2 x3) = fmap (\y1 -> Item x1 y1 x3) (f x2)
          {-# INLINE lens_Item_fields #-}
          lens_Item_images f (Item x1 x2 x3) = fmap (\y1 -> Item x1 x2 y1) (f x3)
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName f (Item x1 x2 x3) = fmap (\y1 -> Item y1 x2 x3) (f x1)
          {-# INLINE lens_Item_itemName #-}
instance HasJSONText JSONText
    where lens_jSONText = id
          lens_JSONText_unJSONText = iso (\(JSONText x) -> x) JSONText
          {-# INLINE lens_JSONText_unJSONText #-}
instance HasMarkup Markup
    where lens_markup = id
          lens_Markup_htmlText _ (Markdown x1) = pure (Markdown x1)
          lens_Markup_htmlText f (Html x1) = fmap (\y1 -> Html y1) (f x1)
          lens_Markup_htmlText _ (LaTeX x1) = pure (LaTeX x1)
          lens_Markup_htmlText _ (Pandoc x1) = pure (Pandoc x1)
          lens_Markup_htmlText _ (Markup x1) = pure (Markup x1)
          {-# INLINE lens_Markup_htmlText #-}
          lens_Markup_markdownText f (Markdown x1) = fmap (\y1 -> Markdown y1) (f x1)
          lens_Markup_markdownText _ (Html x1) = pure (Html x1)
          lens_Markup_markdownText _ (LaTeX x1) = pure (LaTeX x1)
          lens_Markup_markdownText _ (Pandoc x1) = pure (Pandoc x1)
          lens_Markup_markdownText _ (Markup x1) = pure (Markup x1)
          {-# INLINE lens_Markup_markdownText #-}
instance HasPermissions Permissions
    where lens_permissions = id
          lens_Permissions_owner f (Permissions x1 x2 x3) = fmap (\y1 -> Permissions y1 x2 x3) (f x1)
          {-# INLINE lens_Permissions_owner #-}
          lens_Permissions_readers f (Permissions x1 x2 x3) = fmap (\y1 -> Permissions x1 x2 y1) (f x3)
          {-# INLINE lens_Permissions_readers #-}
          lens_Permissions_writers f (Permissions x1 x2 x3) = fmap (\y1 -> Permissions x1 y1 x3) (f x2)
          {-# INLINE lens_Permissions_writers #-}
instance HasReport Report
    where lens_report = id
          lens_Report_reportAbbrevs f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44
                                              x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 y1 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x13)
          {-# INLINE lens_Report_reportAbbrevs #-}
          lens_Report_reportAuthors f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44
                                              x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 y1 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x7)
          {-# INLINE lens_Report_reportAuthors #-}
          lens_Report_reportBody f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44
                                           x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 y1 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x27)
          {-# INLINE lens_Report_reportBody #-}
          lens_Report_reportBranding f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44
                                               x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 y1 x39 x40 x41 x42 x43 x44 x45) (f x38)
          {-# INLINE lens_Report_reportBranding #-}
          lens_Report_reportBriefItems f (Report x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9
                                                 x10
                                                 x11
                                                 x12
                                                 x13
                                                 x14
                                                 x15
                                                 x16
                                                 x17
                                                 x18
                                                 x19
                                                 x20
                                                 x21
                                                 x22
                                                 x23
                                                 x24
                                                 x25
                                                 x26
                                                 x27
                                                 x28
                                                 x29
                                                 x30
                                                 x31
                                                 x32
                                                 x33
                                                 x34
                                                 x35
                                                 x36
                                                 x37
                                                 x38
                                                 x39
                                                 x40
                                                 x41
                                                 x42
                                                 x43
                                                 x44
                                                 x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 y1 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x25)
          {-# INLINE lens_Report_reportBriefItems #-}
          lens_Report_reportCertification f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 y1 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x32)
          {-# INLINE lens_Report_reportCertification #-}
          lens_Report_reportClientAddress f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 y1 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x21)
          {-# INLINE lens_Report_reportClientAddress #-}
          lens_Report_reportClientGreeting f (Report x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44
                                                     x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 y1 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x22)
          {-# INLINE lens_Report_reportClientGreeting #-}
          lens_Report_reportClientName f (Report x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9
                                                 x10
                                                 x11
                                                 x12
                                                 x13
                                                 x14
                                                 x15
                                                 x16
                                                 x17
                                                 x18
                                                 x19
                                                 x20
                                                 x21
                                                 x22
                                                 x23
                                                 x24
                                                 x25
                                                 x26
                                                 x27
                                                 x28
                                                 x29
                                                 x30
                                                 x31
                                                 x32
                                                 x33
                                                 x34
                                                 x35
                                                 x36
                                                 x37
                                                 x38
                                                 x39
                                                 x40
                                                 x41
                                                 x42
                                                 x43
                                                 x44
                                                 x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 y1 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x20)
          {-# INLINE lens_Report_reportClientName #-}
          lens_Report_reportContractDate f (Report x1
                                                   x2
                                                   x3
                                                   x4
                                                   x5
                                                   x6
                                                   x7
                                                   x8
                                                   x9
                                                   x10
                                                   x11
                                                   x12
                                                   x13
                                                   x14
                                                   x15
                                                   x16
                                                   x17
                                                   x18
                                                   x19
                                                   x20
                                                   x21
                                                   x22
                                                   x23
                                                   x24
                                                   x25
                                                   x26
                                                   x27
                                                   x28
                                                   x29
                                                   x30
                                                   x31
                                                   x32
                                                   x33
                                                   x34
                                                   x35
                                                   x36
                                                   x37
                                                   x38
                                                   x39
                                                   x40
                                                   x41
                                                   x42
                                                   x43
                                                   x44
                                                   x45) = fmap (\y1 -> Report x1 x2 x3 y1 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x4)
          {-# INLINE lens_Report_reportContractDate #-}
          lens_Report_reportCreated f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44
                                              x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 y1 x38 x39 x40 x41 x42 x43 x44 x45) (f x37)
          {-# INLINE lens_Report_reportCreated #-}
          lens_Report_reportDate f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44
                                           x45) = fmap (\y1 -> Report x1 x2 y1 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x3)
          {-# INLINE lens_Report_reportDate #-}
          lens_Report_reportDisplayItemName f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 y1 x45) (f x44)
          {-# INLINE lens_Report_reportDisplayItemName #-}
          lens_Report_reportEffectiveDate f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 y1 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x6)
          {-# INLINE lens_Report_reportEffectiveDate #-}
          lens_Report_reportFlags f (Report x1
                                            x2
                                            x3
                                            x4
                                            x5
                                            x6
                                            x7
                                            x8
                                            x9
                                            x10
                                            x11
                                            x12
                                            x13
                                            x14
                                            x15
                                            x16
                                            x17
                                            x18
                                            x19
                                            x20
                                            x21
                                            x22
                                            x23
                                            x24
                                            x25
                                            x26
                                            x27
                                            x28
                                            x29
                                            x30
                                            x31
                                            x32
                                            x33
                                            x34
                                            x35
                                            x36
                                            x37
                                            x38
                                            x39
                                            x40
                                            x41
                                            x42
                                            x43
                                            x44
                                            x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 y1 x42 x43 x44 x45) (f x41)
          {-# INLINE lens_Report_reportFlags #-}
          lens_Report_reportFolder f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44
                                             x45) = fmap (\y1 -> Report y1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x1)
          {-# INLINE lens_Report_reportFolder #-}
          lens_Report_reportFooter f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44
                                             x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 y1 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x16)
          {-# INLINE lens_Report_reportFooter #-}
          lens_Report_reportGlossary f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44
                                               x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 y1 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x28)
          {-# INLINE lens_Report_reportGlossary #-}
          lens_Report_reportHeader f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44
                                             x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 y1 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x15)
          {-# INLINE lens_Report_reportHeader #-}
          lens_Report_reportInspectionDate f (Report x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44
                                                     x45) = fmap (\y1 -> Report x1 x2 x3 x4 y1 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x5)
          {-# INLINE lens_Report_reportInspectionDate #-}
          lens_Report_reportInspectionLocation f (Report x1
                                                         x2
                                                         x3
                                                         x4
                                                         x5
                                                         x6
                                                         x7
                                                         x8
                                                         x9
                                                         x10
                                                         x11
                                                         x12
                                                         x13
                                                         x14
                                                         x15
                                                         x16
                                                         x17
                                                         x18
                                                         x19
                                                         x20
                                                         x21
                                                         x22
                                                         x23
                                                         x24
                                                         x25
                                                         x26
                                                         x27
                                                         x28
                                                         x29
                                                         x30
                                                         x31
                                                         x32
                                                         x33
                                                         x34
                                                         x35
                                                         x36
                                                         x37
                                                         x38
                                                         x39
                                                         x40
                                                         x41
                                                         x42
                                                         x43
                                                         x44
                                                         x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 y1 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x26)
          {-# INLINE lens_Report_reportInspectionLocation #-}
          lens_Report_reportIntendedUse f (Report x1
                                                  x2
                                                  x3
                                                  x4
                                                  x5
                                                  x6
                                                  x7
                                                  x8
                                                  x9
                                                  x10
                                                  x11
                                                  x12
                                                  x13
                                                  x14
                                                  x15
                                                  x16
                                                  x17
                                                  x18
                                                  x19
                                                  x20
                                                  x21
                                                  x22
                                                  x23
                                                  x24
                                                  x25
                                                  x26
                                                  x27
                                                  x28
                                                  x29
                                                  x30
                                                  x31
                                                  x32
                                                  x33
                                                  x34
                                                  x35
                                                  x36
                                                  x37
                                                  x38
                                                  x39
                                                  x40
                                                  x41
                                                  x42
                                                  x43
                                                  x44
                                                  x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 y1 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x17)
          {-# INLINE lens_Report_reportIntendedUse #-}
          lens_Report_reportItemsOwner f (Report x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9
                                                 x10
                                                 x11
                                                 x12
                                                 x13
                                                 x14
                                                 x15
                                                 x16
                                                 x17
                                                 x18
                                                 x19
                                                 x20
                                                 x21
                                                 x22
                                                 x23
                                                 x24
                                                 x25
                                                 x26
                                                 x27
                                                 x28
                                                 x29
                                                 x30
                                                 x31
                                                 x32
                                                 x33
                                                 x34
                                                 x35
                                                 x36
                                                 x37
                                                 x38
                                                 x39
                                                 x40
                                                 x41
                                                 x42
                                                 x43
                                                 x44
                                                 x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 y1 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x24)
          {-# INLINE lens_Report_reportItemsOwner #-}
          lens_Report_reportItemsOwnerFull f (Report x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44
                                                     x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 y1 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x23)
          {-# INLINE lens_Report_reportItemsOwnerFull #-}
          lens_Report_reportLetterOfTransmittal f (Report x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44
                                                          x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 y1 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x30)
          {-# INLINE lens_Report_reportLetterOfTransmittal #-}
          lens_Report_reportLimitingConditions f (Report x1
                                                         x2
                                                         x3
                                                         x4
                                                         x5
                                                         x6
                                                         x7
                                                         x8
                                                         x9
                                                         x10
                                                         x11
                                                         x12
                                                         x13
                                                         x14
                                                         x15
                                                         x16
                                                         x17
                                                         x18
                                                         x19
                                                         x20
                                                         x21
                                                         x22
                                                         x23
                                                         x24
                                                         x25
                                                         x26
                                                         x27
                                                         x28
                                                         x29
                                                         x30
                                                         x31
                                                         x32
                                                         x33
                                                         x34
                                                         x35
                                                         x36
                                                         x37
                                                         x38
                                                         x39
                                                         x40
                                                         x41
                                                         x42
                                                         x43
                                                         x44
                                                         x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 y1 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x33)
          {-# INLINE lens_Report_reportLimitingConditions #-}
          lens_Report_reportName f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44
                                           x45) = fmap (\y1 -> Report x1 y1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x2)
          {-# INLINE lens_Report_reportName #-}
          lens_Report_reportOrderByItemName f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 y1 x44 x45) (f x43)
          {-# INLINE lens_Report_reportOrderByItemName #-}
          lens_Report_reportPerms f (Report x1
                                            x2
                                            x3
                                            x4
                                            x5
                                            x6
                                            x7
                                            x8
                                            x9
                                            x10
                                            x11
                                            x12
                                            x13
                                            x14
                                            x15
                                            x16
                                            x17
                                            x18
                                            x19
                                            x20
                                            x21
                                            x22
                                            x23
                                            x24
                                            x25
                                            x26
                                            x27
                                            x28
                                            x29
                                            x30
                                            x31
                                            x32
                                            x33
                                            x34
                                            x35
                                            x36
                                            x37
                                            x38
                                            x39
                                            x40
                                            x41
                                            x42
                                            x43
                                            x44
                                            x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 y1 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x35)
          {-# INLINE lens_Report_reportPerms #-}
          lens_Report_reportPreparer f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44
                                               x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 y1 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x8)
          {-# INLINE lens_Report_reportPreparer #-}
          lens_Report_reportPreparerAddress f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 y1 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x10)
          {-# INLINE lens_Report_reportPreparerAddress #-}
          lens_Report_reportPreparerEIN f (Report x1
                                                  x2
                                                  x3
                                                  x4
                                                  x5
                                                  x6
                                                  x7
                                                  x8
                                                  x9
                                                  x10
                                                  x11
                                                  x12
                                                  x13
                                                  x14
                                                  x15
                                                  x16
                                                  x17
                                                  x18
                                                  x19
                                                  x20
                                                  x21
                                                  x22
                                                  x23
                                                  x24
                                                  x25
                                                  x26
                                                  x27
                                                  x28
                                                  x29
                                                  x30
                                                  x31
                                                  x32
                                                  x33
                                                  x34
                                                  x35
                                                  x36
                                                  x37
                                                  x38
                                                  x39
                                                  x40
                                                  x41
                                                  x42
                                                  x43
                                                  x44
                                                  x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 y1 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x9)
          {-# INLINE lens_Report_reportPreparerEIN #-}
          lens_Report_reportPreparerEMail f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 y1 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x11)
          {-# INLINE lens_Report_reportPreparerEMail #-}
          lens_Report_reportPreparerWebsite f (Report x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 y1 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x12)
          {-# INLINE lens_Report_reportPreparerWebsite #-}
          lens_Report_reportPrivacyPolicy f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 y1 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x34)
          {-# INLINE lens_Report_reportPrivacyPolicy #-}
          lens_Report_reportRedacted f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44
                                               x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 y1 x41 x42 x43 x44 x45) (f x40)
          {-# INLINE lens_Report_reportRedacted #-}
          lens_Report_reportRevision f (Report x1
                                               x2
                                               x3
                                               x4
                                               x5
                                               x6
                                               x7
                                               x8
                                               x9
                                               x10
                                               x11
                                               x12
                                               x13
                                               x14
                                               x15
                                               x16
                                               x17
                                               x18
                                               x19
                                               x20
                                               x21
                                               x22
                                               x23
                                               x24
                                               x25
                                               x26
                                               x27
                                               x28
                                               x29
                                               x30
                                               x31
                                               x32
                                               x33
                                               x34
                                               x35
                                               x36
                                               x37
                                               x38
                                               x39
                                               x40
                                               x41
                                               x42
                                               x43
                                               x44
                                               x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 y1 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x36)
          {-# INLINE lens_Report_reportRevision #-}
          lens_Report_reportScopeOfWork f (Report x1
                                                  x2
                                                  x3
                                                  x4
                                                  x5
                                                  x6
                                                  x7
                                                  x8
                                                  x9
                                                  x10
                                                  x11
                                                  x12
                                                  x13
                                                  x14
                                                  x15
                                                  x16
                                                  x17
                                                  x18
                                                  x19
                                                  x20
                                                  x21
                                                  x22
                                                  x23
                                                  x24
                                                  x25
                                                  x26
                                                  x27
                                                  x28
                                                  x29
                                                  x30
                                                  x31
                                                  x32
                                                  x33
                                                  x34
                                                  x35
                                                  x36
                                                  x37
                                                  x38
                                                  x39
                                                  x40
                                                  x41
                                                  x42
                                                  x43
                                                  x44
                                                  x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 y1 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x31)
          {-# INLINE lens_Report_reportScopeOfWork #-}
          lens_Report_reportSources f (Report x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9
                                              x10
                                              x11
                                              x12
                                              x13
                                              x14
                                              x15
                                              x16
                                              x17
                                              x18
                                              x19
                                              x20
                                              x21
                                              x22
                                              x23
                                              x24
                                              x25
                                              x26
                                              x27
                                              x28
                                              x29
                                              x30
                                              x31
                                              x32
                                              x33
                                              x34
                                              x35
                                              x36
                                              x37
                                              x38
                                              x39
                                              x40
                                              x41
                                              x42
                                              x43
                                              x44
                                              x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 y1 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x29)
          {-# INLINE lens_Report_reportSources #-}
          lens_Report_reportStandardsVersion f (Report x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44
                                                       x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 y1) (f x45)
          {-# INLINE lens_Report_reportStandardsVersion #-}
          lens_Report_reportStatus f (Report x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9
                                             x10
                                             x11
                                             x12
                                             x13
                                             x14
                                             x15
                                             x16
                                             x17
                                             x18
                                             x19
                                             x20
                                             x21
                                             x22
                                             x23
                                             x24
                                             x25
                                             x26
                                             x27
                                             x28
                                             x29
                                             x30
                                             x31
                                             x32
                                             x33
                                             x34
                                             x35
                                             x36
                                             x37
                                             x38
                                             x39
                                             x40
                                             x41
                                             x42
                                             x43
                                             x44
                                             x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 y1 x40 x41 x42 x43 x44 x45) (f x39)
          {-# INLINE lens_Report_reportStatus #-}
          lens_Report_reportTitle f (Report x1
                                            x2
                                            x3
                                            x4
                                            x5
                                            x6
                                            x7
                                            x8
                                            x9
                                            x10
                                            x11
                                            x12
                                            x13
                                            x14
                                            x15
                                            x16
                                            x17
                                            x18
                                            x19
                                            x20
                                            x21
                                            x22
                                            x23
                                            x24
                                            x25
                                            x26
                                            x27
                                            x28
                                            x29
                                            x30
                                            x31
                                            x32
                                            x33
                                            x34
                                            x35
                                            x36
                                            x37
                                            x38
                                            x39
                                            x40
                                            x41
                                            x42
                                            x43
                                            x44
                                            x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 y1 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x14)
          {-# INLINE lens_Report_reportTitle #-}
          lens_Report_reportUUID f (Report x1
                                           x2
                                           x3
                                           x4
                                           x5
                                           x6
                                           x7
                                           x8
                                           x9
                                           x10
                                           x11
                                           x12
                                           x13
                                           x14
                                           x15
                                           x16
                                           x17
                                           x18
                                           x19
                                           x20
                                           x21
                                           x22
                                           x23
                                           x24
                                           x25
                                           x26
                                           x27
                                           x28
                                           x29
                                           x30
                                           x31
                                           x32
                                           x33
                                           x34
                                           x35
                                           x36
                                           x37
                                           x38
                                           x39
                                           x40
                                           x41
                                           x42
                                           x43
                                           x44
                                           x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 y1 x43 x44 x45) (f x42)
          {-# INLINE lens_Report_reportUUID #-}
          lens_Report_reportValueApproachInfo f (Report x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44
                                                        x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 y1 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x19)
          {-# INLINE lens_Report_reportValueApproachInfo #-}
          lens_Report_reportValueTypeInfo f (Report x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> Report x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 y1 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x18)
          {-# INLINE lens_Report_reportValueTypeInfo #-}
instance HasReportElem ReportElem
    where lens_reportElem = id
          lens_ReportElem_elemItem f (ReportItem x1) = fmap (\y1 -> ReportItem y1) (f x1)
          lens_ReportElem_elemItem _ (ReportParagraph x1) = pure (ReportParagraph x1)
          lens_ReportElem_elemItem _ (ReportUndecided) = pure ReportUndecided
          {-# INLINE lens_ReportElem_elemItem #-}
          lens_ReportElem_elemText _ (ReportItem x1) = pure (ReportItem x1)
          lens_ReportElem_elemText f (ReportParagraph x1) = fmap (\y1 -> ReportParagraph y1) (f x1)
          lens_ReportElem_elemText _ (ReportUndecided) = pure ReportUndecided
          {-# INLINE lens_ReportElem_elemText #-}
instance HasReportFlags ReportFlags
    where lens_reportFlags = id
          lens_ReportFlags_hideEmptyItemFields = iso (\(ReportFlags x) -> x) ReportFlags
          {-# INLINE lens_ReportFlags_hideEmptyItemFields #-}
instance HasReportImage ReportImage
    where lens_reportImage = id
          lens_ReportImage_picCaption f (Pic x1 x2 x3 x4 x5) = fmap (\y1 -> Pic x1 x2 y1 x4 x5) (f x3)
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop f (Pic x1 x2 x3 x4 x5) = fmap (\y1 -> Pic x1 y1 x3 x4 x5) (f x2)
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picMustEnlarge f (Pic x1 x2 x3 x4 x5) = fmap (\y1 -> Pic x1 x2 x3 x4 y1) (f x5)
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal f (Pic x1 x2 x3 x4 x5) = fmap (\y1 -> Pic x1 x2 x3 y1 x5) (f x4)
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picSize f (Pic x1 x2 x3 x4 x5) = fmap (\y1 -> Pic y1 x2 x3 x4 x5) (f x1)
          {-# INLINE lens_ReportImage_picSize #-}
instance HasReportImageView ReportImageView
    where lens_reportImageView = id
          lens_ReportImageView__picCaption f (ReportImageView x1 x2 x3 x4 x5) = fmap (\y1 -> ReportImageView x1 x2 y1 x4 x5) (f x3)
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop f (ReportImageView x1 x2 x3 x4 x5) = fmap (\y1 -> ReportImageView x1 y1 x3 x4 x5) (f x2)
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picMustEnlarge f (ReportImageView x1 x2 x3 x4 x5) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 y1) (f x5)
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal f (ReportImageView x1 x2 x3 x4 x5) = fmap (\y1 -> ReportImageView x1 x2 x3 y1 x5) (f x4)
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picSize f (ReportImageView x1 x2 x3 x4 x5) = fmap (\y1 -> ReportImageView y1 x2 x3 x4 x5) (f x1)
          {-# INLINE lens_ReportImageView__picSize #-}
instance HasReportIntendedUse ReportIntendedUse
    where lens_reportIntendedUse = id
instance HasReportMap ReportMap
    where lens_reportMap = id
          lens_ReportMap_unReportMap = iso (\(ReportMap x) -> x) ReportMap
          {-# INLINE lens_ReportMap_unReportMap #-}
instance HasReportStandard ReportStandard
    where lens_reportStandard = id
          lens_ReportStandard_unReportStandard = iso (\(ReportStandard x) -> x) ReportStandard
          {-# INLINE lens_ReportStandard_unReportStandard #-}
instance HasReportStatus ReportStatus
    where lens_reportStatus = id
instance HasReportValueApproachInfo ReportValueApproachInfo
    where lens_reportValueApproachInfo = id
          lens_ReportValueApproachInfo_reportValueApproachDescription f (ReportValueApproachInfo x1 x2) = fmap (\y1 -> ReportValueApproachInfo x1 y1) (f x2)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName f (ReportValueApproachInfo x1 x2) = fmap (\y1 -> ReportValueApproachInfo y1 x2) (f x1)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
instance HasReportValueTypeInfo ReportValueTypeInfo
    where lens_reportValueTypeInfo = id
          lens_ReportValueTypeInfo_reportValueTypeDefinition f (ReportValueTypeInfo x1 x2 x3) = fmap (\y1 -> ReportValueTypeInfo x1 x2 y1) (f x3)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription f (ReportValueTypeInfo x1 x2 x3) = fmap (\y1 -> ReportValueTypeInfo x1 y1 x3) (f x2)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName f (ReportValueTypeInfo x1 x2 x3) = fmap (\y1 -> ReportValueTypeInfo y1 x2 x3) (f x1)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeName #-}
instance HasReportView ReportView
    where lens_reportView = id
          lens_ReportView__reportAbbrevs f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44
                                                       x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 y1 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x13)
          {-# INLINE lens_ReportView__reportAbbrevs #-}
          lens_ReportView__reportAuthors f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44
                                                       x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 y1 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x7)
          {-# INLINE lens_ReportView__reportAuthors #-}
          lens_ReportView__reportBody f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 y1 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x27)
          {-# INLINE lens_ReportView__reportBody #-}
          lens_ReportView__reportBranding f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44
                                                        x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 y1 x39 x40 x41 x42 x43 x44 x45) (f x38)
          {-# INLINE lens_ReportView__reportBranding #-}
          lens_ReportView__reportBriefItems f (ReportView x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44
                                                          x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 y1 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x25)
          {-# INLINE lens_ReportView__reportBriefItems #-}
          lens_ReportView__reportCertification f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44
                                                             x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 y1 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x32)
          {-# INLINE lens_ReportView__reportCertification #-}
          lens_ReportView__reportClientAddress f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44
                                                             x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 y1 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x21)
          {-# INLINE lens_ReportView__reportClientAddress #-}
          lens_ReportView__reportClientGreeting f (ReportView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9
                                                              x10
                                                              x11
                                                              x12
                                                              x13
                                                              x14
                                                              x15
                                                              x16
                                                              x17
                                                              x18
                                                              x19
                                                              x20
                                                              x21
                                                              x22
                                                              x23
                                                              x24
                                                              x25
                                                              x26
                                                              x27
                                                              x28
                                                              x29
                                                              x30
                                                              x31
                                                              x32
                                                              x33
                                                              x34
                                                              x35
                                                              x36
                                                              x37
                                                              x38
                                                              x39
                                                              x40
                                                              x41
                                                              x42
                                                              x43
                                                              x44
                                                              x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 y1 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x22)
          {-# INLINE lens_ReportView__reportClientGreeting #-}
          lens_ReportView__reportClientName f (ReportView x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44
                                                          x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 y1 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x20)
          {-# INLINE lens_ReportView__reportClientName #-}
          lens_ReportView__reportContractDate f (ReportView x1
                                                            x2
                                                            x3
                                                            x4
                                                            x5
                                                            x6
                                                            x7
                                                            x8
                                                            x9
                                                            x10
                                                            x11
                                                            x12
                                                            x13
                                                            x14
                                                            x15
                                                            x16
                                                            x17
                                                            x18
                                                            x19
                                                            x20
                                                            x21
                                                            x22
                                                            x23
                                                            x24
                                                            x25
                                                            x26
                                                            x27
                                                            x28
                                                            x29
                                                            x30
                                                            x31
                                                            x32
                                                            x33
                                                            x34
                                                            x35
                                                            x36
                                                            x37
                                                            x38
                                                            x39
                                                            x40
                                                            x41
                                                            x42
                                                            x43
                                                            x44
                                                            x45) = fmap (\y1 -> ReportView x1 x2 x3 y1 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x4)
          {-# INLINE lens_ReportView__reportContractDate #-}
          lens_ReportView__reportCreated f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44
                                                       x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 y1 x38 x39 x40 x41 x42 x43 x44 x45) (f x37)
          {-# INLINE lens_ReportView__reportCreated #-}
          lens_ReportView__reportDate f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> ReportView x1 x2 y1 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x3)
          {-# INLINE lens_ReportView__reportDate #-}
          lens_ReportView__reportDisplayItemName f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44
                                                               x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 y1 x45) (f x44)
          {-# INLINE lens_ReportView__reportDisplayItemName #-}
          lens_ReportView__reportEffectiveDate f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44
                                                             x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 y1 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x6)
          {-# INLINE lens_ReportView__reportEffectiveDate #-}
          lens_ReportView__reportFlags f (ReportView x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44
                                                     x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 y1 x42 x43 x44 x45) (f x41)
          {-# INLINE lens_ReportView__reportFlags #-}
          lens_ReportView__reportFolder f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> ReportView y1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x1)
          {-# INLINE lens_ReportView__reportFolder #-}
          lens_ReportView__reportFooter f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 y1 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x16)
          {-# INLINE lens_ReportView__reportFooter #-}
          lens_ReportView__reportGlossary f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44
                                                        x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 y1 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x28)
          {-# INLINE lens_ReportView__reportGlossary #-}
          lens_ReportView__reportHeader f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 y1 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x15)
          {-# INLINE lens_ReportView__reportHeader #-}
          lens_ReportView__reportInspectionDate f (ReportView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9
                                                              x10
                                                              x11
                                                              x12
                                                              x13
                                                              x14
                                                              x15
                                                              x16
                                                              x17
                                                              x18
                                                              x19
                                                              x20
                                                              x21
                                                              x22
                                                              x23
                                                              x24
                                                              x25
                                                              x26
                                                              x27
                                                              x28
                                                              x29
                                                              x30
                                                              x31
                                                              x32
                                                              x33
                                                              x34
                                                              x35
                                                              x36
                                                              x37
                                                              x38
                                                              x39
                                                              x40
                                                              x41
                                                              x42
                                                              x43
                                                              x44
                                                              x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 y1 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x5)
          {-# INLINE lens_ReportView__reportInspectionDate #-}
          lens_ReportView__reportInspectionLocation f (ReportView x1
                                                                  x2
                                                                  x3
                                                                  x4
                                                                  x5
                                                                  x6
                                                                  x7
                                                                  x8
                                                                  x9
                                                                  x10
                                                                  x11
                                                                  x12
                                                                  x13
                                                                  x14
                                                                  x15
                                                                  x16
                                                                  x17
                                                                  x18
                                                                  x19
                                                                  x20
                                                                  x21
                                                                  x22
                                                                  x23
                                                                  x24
                                                                  x25
                                                                  x26
                                                                  x27
                                                                  x28
                                                                  x29
                                                                  x30
                                                                  x31
                                                                  x32
                                                                  x33
                                                                  x34
                                                                  x35
                                                                  x36
                                                                  x37
                                                                  x38
                                                                  x39
                                                                  x40
                                                                  x41
                                                                  x42
                                                                  x43
                                                                  x44
                                                                  x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 y1 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x26)
          {-# INLINE lens_ReportView__reportInspectionLocation #-}
          lens_ReportView__reportIntendedUse f (ReportView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9
                                                           x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20
                                                           x21
                                                           x22
                                                           x23
                                                           x24
                                                           x25
                                                           x26
                                                           x27
                                                           x28
                                                           x29
                                                           x30
                                                           x31
                                                           x32
                                                           x33
                                                           x34
                                                           x35
                                                           x36
                                                           x37
                                                           x38
                                                           x39
                                                           x40
                                                           x41
                                                           x42
                                                           x43
                                                           x44
                                                           x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 y1 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x17)
          {-# INLINE lens_ReportView__reportIntendedUse #-}
          lens_ReportView__reportItemsOwner f (ReportView x1
                                                          x2
                                                          x3
                                                          x4
                                                          x5
                                                          x6
                                                          x7
                                                          x8
                                                          x9
                                                          x10
                                                          x11
                                                          x12
                                                          x13
                                                          x14
                                                          x15
                                                          x16
                                                          x17
                                                          x18
                                                          x19
                                                          x20
                                                          x21
                                                          x22
                                                          x23
                                                          x24
                                                          x25
                                                          x26
                                                          x27
                                                          x28
                                                          x29
                                                          x30
                                                          x31
                                                          x32
                                                          x33
                                                          x34
                                                          x35
                                                          x36
                                                          x37
                                                          x38
                                                          x39
                                                          x40
                                                          x41
                                                          x42
                                                          x43
                                                          x44
                                                          x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 y1 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x24)
          {-# INLINE lens_ReportView__reportItemsOwner #-}
          lens_ReportView__reportItemsOwnerFull f (ReportView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9
                                                              x10
                                                              x11
                                                              x12
                                                              x13
                                                              x14
                                                              x15
                                                              x16
                                                              x17
                                                              x18
                                                              x19
                                                              x20
                                                              x21
                                                              x22
                                                              x23
                                                              x24
                                                              x25
                                                              x26
                                                              x27
                                                              x28
                                                              x29
                                                              x30
                                                              x31
                                                              x32
                                                              x33
                                                              x34
                                                              x35
                                                              x36
                                                              x37
                                                              x38
                                                              x39
                                                              x40
                                                              x41
                                                              x42
                                                              x43
                                                              x44
                                                              x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 y1 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x23)
          {-# INLINE lens_ReportView__reportItemsOwnerFull #-}
          lens_ReportView__reportLetterOfTransmittal f (ReportView x1
                                                                   x2
                                                                   x3
                                                                   x4
                                                                   x5
                                                                   x6
                                                                   x7
                                                                   x8
                                                                   x9
                                                                   x10
                                                                   x11
                                                                   x12
                                                                   x13
                                                                   x14
                                                                   x15
                                                                   x16
                                                                   x17
                                                                   x18
                                                                   x19
                                                                   x20
                                                                   x21
                                                                   x22
                                                                   x23
                                                                   x24
                                                                   x25
                                                                   x26
                                                                   x27
                                                                   x28
                                                                   x29
                                                                   x30
                                                                   x31
                                                                   x32
                                                                   x33
                                                                   x34
                                                                   x35
                                                                   x36
                                                                   x37
                                                                   x38
                                                                   x39
                                                                   x40
                                                                   x41
                                                                   x42
                                                                   x43
                                                                   x44
                                                                   x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 y1 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x30)
          {-# INLINE lens_ReportView__reportLetterOfTransmittal #-}
          lens_ReportView__reportLimitingConditions f (ReportView x1
                                                                  x2
                                                                  x3
                                                                  x4
                                                                  x5
                                                                  x6
                                                                  x7
                                                                  x8
                                                                  x9
                                                                  x10
                                                                  x11
                                                                  x12
                                                                  x13
                                                                  x14
                                                                  x15
                                                                  x16
                                                                  x17
                                                                  x18
                                                                  x19
                                                                  x20
                                                                  x21
                                                                  x22
                                                                  x23
                                                                  x24
                                                                  x25
                                                                  x26
                                                                  x27
                                                                  x28
                                                                  x29
                                                                  x30
                                                                  x31
                                                                  x32
                                                                  x33
                                                                  x34
                                                                  x35
                                                                  x36
                                                                  x37
                                                                  x38
                                                                  x39
                                                                  x40
                                                                  x41
                                                                  x42
                                                                  x43
                                                                  x44
                                                                  x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 y1 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x33)
          {-# INLINE lens_ReportView__reportLimitingConditions #-}
          lens_ReportView__reportName f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> ReportView x1 y1 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x2)
          {-# INLINE lens_ReportView__reportName #-}
          lens_ReportView__reportOrderByItemName f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44
                                                               x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 y1 x44 x45) (f x43)
          {-# INLINE lens_ReportView__reportOrderByItemName #-}
          lens_ReportView__reportPerms f (ReportView x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44
                                                     x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 y1 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x35)
          {-# INLINE lens_ReportView__reportPerms #-}
          lens_ReportView__reportPreparer f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44
                                                        x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 y1 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x8)
          {-# INLINE lens_ReportView__reportPreparer #-}
          lens_ReportView__reportPreparerAddress f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44
                                                               x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 y1 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x10)
          {-# INLINE lens_ReportView__reportPreparerAddress #-}
          lens_ReportView__reportPreparerEIN f (ReportView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9
                                                           x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20
                                                           x21
                                                           x22
                                                           x23
                                                           x24
                                                           x25
                                                           x26
                                                           x27
                                                           x28
                                                           x29
                                                           x30
                                                           x31
                                                           x32
                                                           x33
                                                           x34
                                                           x35
                                                           x36
                                                           x37
                                                           x38
                                                           x39
                                                           x40
                                                           x41
                                                           x42
                                                           x43
                                                           x44
                                                           x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 y1 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x9)
          {-# INLINE lens_ReportView__reportPreparerEIN #-}
          lens_ReportView__reportPreparerEMail f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44
                                                             x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 y1 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x11)
          {-# INLINE lens_ReportView__reportPreparerEMail #-}
          lens_ReportView__reportPreparerWebsite f (ReportView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9
                                                               x10
                                                               x11
                                                               x12
                                                               x13
                                                               x14
                                                               x15
                                                               x16
                                                               x17
                                                               x18
                                                               x19
                                                               x20
                                                               x21
                                                               x22
                                                               x23
                                                               x24
                                                               x25
                                                               x26
                                                               x27
                                                               x28
                                                               x29
                                                               x30
                                                               x31
                                                               x32
                                                               x33
                                                               x34
                                                               x35
                                                               x36
                                                               x37
                                                               x38
                                                               x39
                                                               x40
                                                               x41
                                                               x42
                                                               x43
                                                               x44
                                                               x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 y1 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x12)
          {-# INLINE lens_ReportView__reportPreparerWebsite #-}
          lens_ReportView__reportPrivacyPolicy f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44
                                                             x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 y1 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x34)
          {-# INLINE lens_ReportView__reportPrivacyPolicy #-}
          lens_ReportView__reportRedacted f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44
                                                        x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 y1 x41 x42 x43 x44 x45) (f x40)
          {-# INLINE lens_ReportView__reportRedacted #-}
          lens_ReportView__reportRevision f (ReportView x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9
                                                        x10
                                                        x11
                                                        x12
                                                        x13
                                                        x14
                                                        x15
                                                        x16
                                                        x17
                                                        x18
                                                        x19
                                                        x20
                                                        x21
                                                        x22
                                                        x23
                                                        x24
                                                        x25
                                                        x26
                                                        x27
                                                        x28
                                                        x29
                                                        x30
                                                        x31
                                                        x32
                                                        x33
                                                        x34
                                                        x35
                                                        x36
                                                        x37
                                                        x38
                                                        x39
                                                        x40
                                                        x41
                                                        x42
                                                        x43
                                                        x44
                                                        x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 y1 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x36)
          {-# INLINE lens_ReportView__reportRevision #-}
          lens_ReportView__reportScopeOfWork f (ReportView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9
                                                           x10
                                                           x11
                                                           x12
                                                           x13
                                                           x14
                                                           x15
                                                           x16
                                                           x17
                                                           x18
                                                           x19
                                                           x20
                                                           x21
                                                           x22
                                                           x23
                                                           x24
                                                           x25
                                                           x26
                                                           x27
                                                           x28
                                                           x29
                                                           x30
                                                           x31
                                                           x32
                                                           x33
                                                           x34
                                                           x35
                                                           x36
                                                           x37
                                                           x38
                                                           x39
                                                           x40
                                                           x41
                                                           x42
                                                           x43
                                                           x44
                                                           x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 y1 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x31)
          {-# INLINE lens_ReportView__reportScopeOfWork #-}
          lens_ReportView__reportSources f (ReportView x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9
                                                       x10
                                                       x11
                                                       x12
                                                       x13
                                                       x14
                                                       x15
                                                       x16
                                                       x17
                                                       x18
                                                       x19
                                                       x20
                                                       x21
                                                       x22
                                                       x23
                                                       x24
                                                       x25
                                                       x26
                                                       x27
                                                       x28
                                                       x29
                                                       x30
                                                       x31
                                                       x32
                                                       x33
                                                       x34
                                                       x35
                                                       x36
                                                       x37
                                                       x38
                                                       x39
                                                       x40
                                                       x41
                                                       x42
                                                       x43
                                                       x44
                                                       x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 y1 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x29)
          {-# INLINE lens_ReportView__reportSources #-}
          lens_ReportView__reportStandardsVersion f (ReportView x1
                                                                x2
                                                                x3
                                                                x4
                                                                x5
                                                                x6
                                                                x7
                                                                x8
                                                                x9
                                                                x10
                                                                x11
                                                                x12
                                                                x13
                                                                x14
                                                                x15
                                                                x16
                                                                x17
                                                                x18
                                                                x19
                                                                x20
                                                                x21
                                                                x22
                                                                x23
                                                                x24
                                                                x25
                                                                x26
                                                                x27
                                                                x28
                                                                x29
                                                                x30
                                                                x31
                                                                x32
                                                                x33
                                                                x34
                                                                x35
                                                                x36
                                                                x37
                                                                x38
                                                                x39
                                                                x40
                                                                x41
                                                                x42
                                                                x43
                                                                x44
                                                                x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 y1) (f x45)
          {-# INLINE lens_ReportView__reportStandardsVersion #-}
          lens_ReportView__reportStatus f (ReportView x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9
                                                      x10
                                                      x11
                                                      x12
                                                      x13
                                                      x14
                                                      x15
                                                      x16
                                                      x17
                                                      x18
                                                      x19
                                                      x20
                                                      x21
                                                      x22
                                                      x23
                                                      x24
                                                      x25
                                                      x26
                                                      x27
                                                      x28
                                                      x29
                                                      x30
                                                      x31
                                                      x32
                                                      x33
                                                      x34
                                                      x35
                                                      x36
                                                      x37
                                                      x38
                                                      x39
                                                      x40
                                                      x41
                                                      x42
                                                      x43
                                                      x44
                                                      x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 y1 x40 x41 x42 x43 x44 x45) (f x39)
          {-# INLINE lens_ReportView__reportStatus #-}
          lens_ReportView__reportTitle f (ReportView x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9
                                                     x10
                                                     x11
                                                     x12
                                                     x13
                                                     x14
                                                     x15
                                                     x16
                                                     x17
                                                     x18
                                                     x19
                                                     x20
                                                     x21
                                                     x22
                                                     x23
                                                     x24
                                                     x25
                                                     x26
                                                     x27
                                                     x28
                                                     x29
                                                     x30
                                                     x31
                                                     x32
                                                     x33
                                                     x34
                                                     x35
                                                     x36
                                                     x37
                                                     x38
                                                     x39
                                                     x40
                                                     x41
                                                     x42
                                                     x43
                                                     x44
                                                     x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 y1 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x14)
          {-# INLINE lens_ReportView__reportTitle #-}
          lens_ReportView__reportUUID f (ReportView x1
                                                    x2
                                                    x3
                                                    x4
                                                    x5
                                                    x6
                                                    x7
                                                    x8
                                                    x9
                                                    x10
                                                    x11
                                                    x12
                                                    x13
                                                    x14
                                                    x15
                                                    x16
                                                    x17
                                                    x18
                                                    x19
                                                    x20
                                                    x21
                                                    x22
                                                    x23
                                                    x24
                                                    x25
                                                    x26
                                                    x27
                                                    x28
                                                    x29
                                                    x30
                                                    x31
                                                    x32
                                                    x33
                                                    x34
                                                    x35
                                                    x36
                                                    x37
                                                    x38
                                                    x39
                                                    x40
                                                    x41
                                                    x42
                                                    x43
                                                    x44
                                                    x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 y1 x43 x44 x45) (f x42)
          {-# INLINE lens_ReportView__reportUUID #-}
          lens_ReportView__reportValueApproachInfo f (ReportView x1
                                                                 x2
                                                                 x3
                                                                 x4
                                                                 x5
                                                                 x6
                                                                 x7
                                                                 x8
                                                                 x9
                                                                 x10
                                                                 x11
                                                                 x12
                                                                 x13
                                                                 x14
                                                                 x15
                                                                 x16
                                                                 x17
                                                                 x18
                                                                 x19
                                                                 x20
                                                                 x21
                                                                 x22
                                                                 x23
                                                                 x24
                                                                 x25
                                                                 x26
                                                                 x27
                                                                 x28
                                                                 x29
                                                                 x30
                                                                 x31
                                                                 x32
                                                                 x33
                                                                 x34
                                                                 x35
                                                                 x36
                                                                 x37
                                                                 x38
                                                                 x39
                                                                 x40
                                                                 x41
                                                                 x42
                                                                 x43
                                                                 x44
                                                                 x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 y1 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x19)
          {-# INLINE lens_ReportView__reportValueApproachInfo #-}
          lens_ReportView__reportValueTypeInfo f (ReportView x1
                                                             x2
                                                             x3
                                                             x4
                                                             x5
                                                             x6
                                                             x7
                                                             x8
                                                             x9
                                                             x10
                                                             x11
                                                             x12
                                                             x13
                                                             x14
                                                             x15
                                                             x16
                                                             x17
                                                             x18
                                                             x19
                                                             x20
                                                             x21
                                                             x22
                                                             x23
                                                             x24
                                                             x25
                                                             x26
                                                             x27
                                                             x28
                                                             x29
                                                             x30
                                                             x31
                                                             x32
                                                             x33
                                                             x34
                                                             x35
                                                             x36
                                                             x37
                                                             x38
                                                             x39
                                                             x40
                                                             x41
                                                             x42
                                                             x43
                                                             x44
                                                             x45) = fmap (\y1 -> ReportView x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 y1 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45) (f x18)
          {-# INLINE lens_ReportView__reportValueTypeInfo #-}
instance HasText Text
    where lens_text = id
instance HasURI URI
    where lens_uRI = id
          lens_URI_uriAuthority f (URI x1 x2 x3 x4 x5) = fmap (\y1 -> URI x1 y1 x3 x4 x5) (f x2)
          {-# INLINE lens_URI_uriAuthority #-}
          lens_URI_uriFragment f (URI x1 x2 x3 x4 x5) = fmap (\y1 -> URI x1 x2 x3 x4 y1) (f x5)
          {-# INLINE lens_URI_uriFragment #-}
          lens_URI_uriPath f (URI x1 x2 x3 x4 x5) = fmap (\y1 -> URI x1 x2 y1 x4 x5) (f x3)
          {-# INLINE lens_URI_uriPath #-}
          lens_URI_uriQuery f (URI x1 x2 x3 x4 x5) = fmap (\y1 -> URI x1 x2 x3 y1 x5) (f x4)
          {-# INLINE lens_URI_uriQuery #-}
          lens_URI_uriScheme f (URI x1 x2 x3 x4 x5) = fmap (\y1 -> URI y1 x2 x3 x4 x5) (f x1)
          {-# INLINE lens_URI_uriScheme #-}
instance HasUUID UUID
    where lens_uUID = id
instance HasUnits Units
    where lens_units = id
instance HasUserId UserId
    where lens_userId = id
          lens_UserId__unUserId = iso (\(UserId x) -> x) UserId
          {-# INLINE lens_UserId__unUserId #-}
instance Describe (Proxy Int64)
    where describe' _f _ = Just (fromMaybe "Int64" _f)
instance Describe (Proxy Int)
    where describe' _f _ = Just (fromMaybe "Int" _f)
instance Describe (Proxy Dimension)
    where describe' _f _ = Just (fromMaybe "Dimension" _f)
instance Describe (Proxy ImageCrop)
    where describe' _f _ = Just (fromMaybe "Image Crop" _f)
instance Describe (Proxy ImageSize)
    where describe' _f _ = Just (fromMaybe "Image Size" _f)
instance Describe (Proxy Units)
    where describe' _f _ = Just (fromMaybe "Units" _f)
instance Describe (Proxy ImageFile)
    where describe' _f _ = Just (fromMaybe "Image File" _f)
instance Describe (Proxy Integer)
    where describe' _f _ = Just (fromMaybe "Integer" _f)
instance Describe (Proxy Permissions)
    where describe' _f _ = Just (fromMaybe "Permissions" _f)
instance Describe (Proxy UserIds)
    where describe' _f _ = Just (fromMaybe "User Ids" _f)
instance Describe (Proxy AbbrevPair)
    where describe' _f _ = Just (fromMaybe "Abbrev Pair" _f)
instance Describe (Proxy AbbrevPairs)
    where describe' _f _ = Just (fromMaybe "Abbrev Pairs" _f)
instance Describe (Proxy Author)
    where describe' _f _ = Just (fromMaybe "Author" _f)
instance Describe (Proxy Authors)
    where describe' _f _ = Just (fromMaybe "Authors" _f)
instance Describe (Proxy Branding)
    where describe' _f _ = Just (fromMaybe "Branding" _f)
instance Describe (Proxy MarkupPair)
    where describe' _f _ = Just (fromMaybe "Markup Pair" _f)
instance Describe (Proxy MarkupPairs)
    where describe' _f _ = Just (fromMaybe "Markup Pairs" _f)
instance Describe (Proxy Markups)
    where describe' _f _ = Just (fromMaybe "Markups" _f)
instance Describe (Proxy MaybeReportIntendedUse)
    where describe' _f _ = Just (fromMaybe "Maybe Report Intended Use" _f)
instance Describe (Proxy Report)
    where describe' _f _ = Just (fromMaybe "Report" _f)
instance Describe (Proxy ReportElem)
    where describe' _f _ = Just (fromMaybe "Report Elem" _f)
instance Describe (Proxy ReportElems)
    where describe' _f _ = Just (fromMaybe "Report Elems" _f)
instance Describe (Proxy ReportFlags)
    where describe' _f _ = Just (fromMaybe "Report Flags" _f)
instance Describe (Proxy ReportIntendedUse)
    where describe' _f _ = Just (fromMaybe "Report Intended Use" _f)
instance Describe (Proxy ReportStandard)
    where describe' _f _ = Just (fromMaybe "Report Standard" _f)
instance Describe (Proxy ReportStatus)
    where describe' _f _ = Just (fromMaybe "Report Status" _f)
instance Describe (Proxy ReportValueApproachInfo)
    where describe' _f _ = Just (fromMaybe "Report Value Approach Info" _f)
instance Describe (Proxy ReportValueTypeInfo)
    where describe' _f _ = Just (fromMaybe "Report Value Type Info" _f)
instance Describe (Proxy EUI)
    where describe' _f _ = Just (fromMaybe "EUI" _f)
instance Describe (Proxy MEUI)
    where describe' _f _ = Just (fromMaybe "MEUI" _f)
instance Describe (Proxy MaybeImageFile)
    where describe' _f _ = Just (fromMaybe "Maybe Image File" _f)
instance Describe (Proxy ReportImage)
    where describe' _f _ = Just (fromMaybe "Report Image" _f)
instance Describe (Proxy ReportImages)
    where describe' _f _ = Just (fromMaybe "Report Images" _f)
instance Describe (Proxy ReadOnlyFilePath)
    where describe' _f _ = Just (fromMaybe "Read Only File Path" _f)
instance Describe (Proxy ReportImageView)
    where describe' _f _ = Just (fromMaybe "Report Image View" _f)
instance Describe (Proxy ReportView)
    where describe' _f _ = Just (fromMaybe "Report View" _f)
instance Describe (Proxy SaneSizeImageSize)
    where describe' _f _ = Just (fromMaybe "Sane Size Image Size" _f)
instance Describe (Proxy Item)
    where describe' _f _ = Just (fromMaybe "Item" _f)
instance Describe (Proxy MIM)
    where describe' _f _ = Just (fromMaybe "MIM" _f)
instance Describe (Proxy MRR)
    where describe' _f _ = Just (fromMaybe "MRR" _f)
instance Describe (Proxy ReportMap)
    where describe' _f _ = Just (fromMaybe "Report Map" _f)
instance Describe (Proxy CIString)
    where describe' _f _ = Just (fromMaybe "CIString" _f)
instance Describe (Proxy URI)
    where describe' _f _ = Just (fromMaybe "URI" _f)
instance Describe (Proxy UserId)
    where describe' _f _ = Just (fromMaybe "User Id" _f)
instance Describe (Proxy UUID)
    where describe' _f _ = Just (fromMaybe "UUID" _f)
instance Describe UPath_Author
    where describe' _f (UPath_Author_authorName q) = maybe (describe' _f (Proxy :: Proxy Author)) Just (describe' (Just "Author Name") q)
          describe' _f (UPath_Author_authorCredentials q) = maybe (describe' _f (Proxy :: Proxy Author)) Just (describe' (Just "Author Credentials") q)
          describe' f (UPath_Author) = describe' f (Proxy :: Proxy Author)
instance Describe UPath_ImageCrop
    where describe' f _ = describe' f (Proxy :: Proxy ImageCrop)
instance Describe UPath_ImageFile
    where describe' f _ = describe' f (Proxy :: Proxy ImageFile)
instance Describe UPath_ImageSize
    where describe' _f (UPath_ImageSize_dim q) = maybe (describe' _f (Proxy :: Proxy ImageSize)) Just (describe' (Just "Dim") q)
          describe' _f (UPath_ImageSize_size q) = maybe (describe' _f (Proxy :: Proxy ImageSize)) Just (describe' (Just "Size") q)
          describe' _f (UPath_ImageSize_units q) = maybe (describe' _f (Proxy :: Proxy ImageSize)) Just (describe' (Just "Units") q)
          describe' f (UPath_ImageSize) = describe' f (Proxy :: Proxy ImageSize)
instance Describe UPath_Int
    where describe' f _ = describe' f (Proxy :: Proxy Int)
instance Describe UPath_Int64
    where describe' f _ = describe' f (Proxy :: Proxy Int64)
instance Describe UPath_Integer
    where describe' f _ = describe' f (Proxy :: Proxy Integer)
instance Describe UPath_Item
    where describe' _f (UPath_Item_itemName q) = maybe (describe' _f (Proxy :: Proxy Item)) Just (describe' (Just "Item Name") q)
          describe' _f (UPath_Item_fields q) = maybe (describe' _f (Proxy :: Proxy Item)) Just (describe' (Just "Fields") q)
          describe' _f (UPath_Item_images q) = maybe (describe' _f (Proxy :: Proxy Item)) Just (describe' (Just "Images") q)
          describe' f (UPath_Item) = describe' f (Proxy :: Proxy Item)
instance Describe UPath_JSONText
    where describe' f _ = describe' f (Proxy :: Proxy JSONText)
instance Describe UPath_Markup
    where describe' _f (UPath_Markup_markdownText q) = maybe (describe' _f (Proxy :: Proxy Markup)) Just (describe' (Just "Markdown Text") q)
          describe' _f (UPath_Markup_htmlText q) = maybe (describe' _f (Proxy :: Proxy Markup)) Just (describe' (Just "Html Text") q)
          describe' f (UPath_Markup) = describe' f (Proxy :: Proxy Markup)
instance Describe UPath_Permissions
    where describe' _f (UPath_Permissions_owner q) = maybe (describe' _f (Proxy :: Proxy Permissions)) Just (describe' (Just "Owner") q)
          describe' _f (UPath_Permissions_writers q) = maybe (describe' _f (Proxy :: Proxy Permissions)) Just (describe' (Just "Writers") q)
          describe' _f (UPath_Permissions_readers q) = maybe (describe' _f (Proxy :: Proxy Permissions)) Just (describe' (Just "Readers") q)
          describe' f (UPath_Permissions) = describe' f (Proxy :: Proxy Permissions)
instance Describe UPath_ReportElem
    where describe' _f (UPath_ReportElem_elemItem q) = maybe (describe' _f (Proxy :: Proxy ReportElem)) Just (describe' (Just "Elem Item") q)
          describe' _f (UPath_ReportElem_elemText q) = maybe (describe' _f (Proxy :: Proxy ReportElem)) Just (describe' (Just "Elem Text") q)
          describe' f (UPath_ReportElem) = describe' f (Proxy :: Proxy ReportElem)
instance Describe UPath_ReportFlags
    where describe' _f (UPath_ReportFlags_hideEmptyItemFields q) = maybe (describe' _f (Proxy :: Proxy ReportFlags)) Just (describe' (Just "Hide Empty Item Fields") q)
          describe' f (UPath_ReportFlags) = describe' f (Proxy :: Proxy ReportFlags)
instance Describe UPath_ReportImageView
    where describe' _f (UPath_ReportImageView__picSize q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Size") q)
          describe' _f (UPath_ReportImageView__picCrop q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Crop") q)
          describe' _f (UPath_ReportImageView__picCaption q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Caption") q)
          describe' _f (UPath_ReportImageView__picOriginal q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Original") q)
          describe' _f (UPath_ReportImageView__picMustEnlarge q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Must Enlarge") q)
          describe' f (UPath_ReportImageView) = describe' f (Proxy :: Proxy ReportImageView)
instance Describe UPath_ReportMap
    where describe' _f (UPath_ReportMap_unReportMap q) = maybe (describe' _f (Proxy :: Proxy ReportMap)) Just (describe' (Just "Un Report Map") q)
          describe' f (UPath_ReportMap) = describe' f (Proxy :: Proxy ReportMap)
instance Describe UPath_ReportStandard
    where describe' _f (UPath_ReportStandard_unReportStandard q) = maybe (describe' _f (Proxy :: Proxy ReportStandard)) Just (describe' (Just "Un Report Standard") q)
          describe' f (UPath_ReportStandard) = describe' f (Proxy :: Proxy ReportStandard)
instance Describe UPath_ReportValueApproachInfo
    where describe' _f (UPath_ReportValueApproachInfo_reportValueApproachName q) = maybe (describe' _f (Proxy :: Proxy ReportValueApproachInfo)) Just (describe' (Just "Report Value Approach Name") q)
          describe' _f (UPath_ReportValueApproachInfo_reportValueApproachDescription q) = maybe (describe' _f (Proxy :: Proxy ReportValueApproachInfo)) Just (describe' (Just "Report Value Approach Description") q)
          describe' f (UPath_ReportValueApproachInfo) = describe' f (Proxy :: Proxy ReportValueApproachInfo)
instance Describe UPath_ReportValueTypeInfo
    where describe' _f (UPath_ReportValueTypeInfo_reportValueTypeName q) = maybe (describe' _f (Proxy :: Proxy ReportValueTypeInfo)) Just (describe' (Just "Report Value Type Name") q)
          describe' _f (UPath_ReportValueTypeInfo_reportValueTypeDescription q) = maybe (describe' _f (Proxy :: Proxy ReportValueTypeInfo)) Just (describe' (Just "Report Value Type Description") q)
          describe' _f (UPath_ReportValueTypeInfo_reportValueTypeDefinition q) = maybe (describe' _f (Proxy :: Proxy ReportValueTypeInfo)) Just (describe' (Just "Report Value Type Definition") q)
          describe' f (UPath_ReportValueTypeInfo) = describe' f (Proxy :: Proxy ReportValueTypeInfo)
instance Describe UPath_ReportView
    where describe' _f (UPath_ReportView__reportFolder q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Folder") q)
          describe' _f (UPath_ReportView__reportName q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Name") q)
          describe' _f (UPath_ReportView__reportDate q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Date") q)
          describe' _f (UPath_ReportView__reportContractDate q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Contract Date") q)
          describe' _f (UPath_ReportView__reportInspectionDate q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Inspection Date") q)
          describe' _f (UPath_ReportView__reportEffectiveDate q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Effective Date") q)
          describe' _f (UPath_ReportView__reportAuthors q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Authors") q)
          describe' _f (UPath_ReportView__reportPreparer q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer") q)
          describe' _f (UPath_ReportView__reportPreparerEIN q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer EIN") q)
          describe' _f (UPath_ReportView__reportPreparerAddress q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer Address") q)
          describe' _f (UPath_ReportView__reportPreparerEMail q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer EMail") q)
          describe' _f (UPath_ReportView__reportPreparerWebsite q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer Website") q)
          describe' _f (UPath_ReportView__reportAbbrevs q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Abbrevs") q)
          describe' _f (UPath_ReportView__reportTitle q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Title") q)
          describe' _f (UPath_ReportView__reportHeader q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Header") q)
          describe' _f (UPath_ReportView__reportFooter q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Footer") q)
          describe' _f (UPath_ReportView__reportIntendedUse q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Intended Use") q)
          describe' _f (UPath_ReportView__reportValueTypeInfo q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Value Type Info") q)
          describe' _f (UPath_ReportView__reportValueApproachInfo q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Value Approach Info") q)
          describe' _f (UPath_ReportView__reportClientName q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Client Name") q)
          describe' _f (UPath_ReportView__reportClientAddress q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Client Address") q)
          describe' _f (UPath_ReportView__reportClientGreeting q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Client Greeting") q)
          describe' _f (UPath_ReportView__reportItemsOwnerFull q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Items Owner Full") q)
          describe' _f (UPath_ReportView__reportItemsOwner q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Items Owner") q)
          describe' _f (UPath_ReportView__reportBriefItems q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Brief Items") q)
          describe' _f (UPath_ReportView__reportInspectionLocation q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Inspection Location") q)
          describe' _f (UPath_ReportView__reportBody q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Body") q)
          describe' _f (UPath_ReportView__reportGlossary q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Glossary") q)
          describe' _f (UPath_ReportView__reportSources q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Sources") q)
          describe' _f (UPath_ReportView__reportLetterOfTransmittal q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Letter Of Transmittal") q)
          describe' _f (UPath_ReportView__reportScopeOfWork q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Scope Of Work") q)
          describe' _f (UPath_ReportView__reportCertification q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Certification") q)
          describe' _f (UPath_ReportView__reportLimitingConditions q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Limiting Conditions") q)
          describe' _f (UPath_ReportView__reportPrivacyPolicy q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Privacy Policy") q)
          describe' _f (UPath_ReportView__reportPerms q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Perms") q)
          describe' _f (UPath_ReportView__reportRevision q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Revision") q)
          describe' _f (UPath_ReportView__reportCreated q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Created") q)
          describe' _f (UPath_ReportView__reportBranding q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Branding") q)
          describe' _f (UPath_ReportView__reportStatus q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Status") q)
          describe' _f (UPath_ReportView__reportRedacted q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Redacted") q)
          describe' _f (UPath_ReportView__reportFlags q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Flags") q)
          describe' _f (UPath_ReportView__reportUUID q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report UUID") q)
          describe' _f (UPath_ReportView__reportOrderByItemName q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Order By Item Name") q)
          describe' _f (UPath_ReportView__reportDisplayItemName q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Display Item Name") q)
          describe' _f (UPath_ReportView__reportStandardsVersion q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Standards Version") q)
          describe' f (UPath_ReportView) = describe' f (Proxy :: Proxy ReportView)
instance Describe UPath_URI
    where describe' f _ = describe' f (Proxy :: Proxy URI)
instance Describe UPath_UUID
    where describe' f _ = describe' f (Proxy :: Proxy UUID)
instance Describe UPath_UserId
    where describe' f _ = describe' f (Proxy :: Proxy UserId)
instance IsPath UPath_Author
    where type UType UPath_Author = Univ
          type SType UPath_Author = Author
          idPath = UPath_Author
instance IsPath UPath_ImageCrop
    where type UType UPath_ImageCrop = Univ
          type SType UPath_ImageCrop = ImageCrop
          idPath = UPath_ImageCrop
instance IsPath UPath_ImageFile
    where type UType UPath_ImageFile = Univ
          type SType UPath_ImageFile = ImageFile
          idPath = UPath_ImageFile
instance IsPath UPath_ImageSize
    where type UType UPath_ImageSize = Univ
          type SType UPath_ImageSize = ImageSize
          idPath = UPath_ImageSize
instance IsPath UPath_Int
    where type UType UPath_Int = Univ
          type SType UPath_Int = Int
          idPath = UPath_Int
instance IsPath UPath_Int64
    where type UType UPath_Int64 = Univ
          type SType UPath_Int64 = Int64
          idPath = UPath_Int64
instance IsPath UPath_Integer
    where type UType UPath_Integer = Univ
          type SType UPath_Integer = Integer
          idPath = UPath_Integer
instance IsPath UPath_Item
    where type UType UPath_Item = Univ
          type SType UPath_Item = Item
          idPath = UPath_Item
instance IsPath UPath_JSONText
    where type UType UPath_JSONText = Univ
          type SType UPath_JSONText = JSONText
          idPath = UPath_JSONText
instance IsPath UPath_Markup
    where type UType UPath_Markup = Univ
          type SType UPath_Markup = Markup
          idPath = UPath_Markup
instance IsPath UPath_Permissions
    where type UType UPath_Permissions = Univ
          type SType UPath_Permissions = Permissions
          idPath = UPath_Permissions
instance IsPath UPath_ReportElem
    where type UType UPath_ReportElem = Univ
          type SType UPath_ReportElem = ReportElem
          idPath = UPath_ReportElem
instance IsPath UPath_ReportFlags
    where type UType UPath_ReportFlags = Univ
          type SType UPath_ReportFlags = ReportFlags
          idPath = UPath_ReportFlags
instance IsPath UPath_ReportImageView
    where type UType UPath_ReportImageView = Univ
          type SType UPath_ReportImageView = ReportImageView
          idPath = UPath_ReportImageView
instance IsPath UPath_ReportMap
    where type UType UPath_ReportMap = Univ
          type SType UPath_ReportMap = ReportMap
          idPath = UPath_ReportMap
instance IsPath UPath_ReportStandard
    where type UType UPath_ReportStandard = Univ
          type SType UPath_ReportStandard = ReportStandard
          idPath = UPath_ReportStandard
instance IsPath UPath_ReportValueApproachInfo
    where type UType UPath_ReportValueApproachInfo = Univ
          type SType UPath_ReportValueApproachInfo = ReportValueApproachInfo
          idPath = UPath_ReportValueApproachInfo
instance IsPath UPath_ReportValueTypeInfo
    where type UType UPath_ReportValueTypeInfo = Univ
          type SType UPath_ReportValueTypeInfo = ReportValueTypeInfo
          idPath = UPath_ReportValueTypeInfo
instance IsPath UPath_ReportView
    where type UType UPath_ReportView = Univ
          type SType UPath_ReportView = ReportView
          idPath = UPath_ReportView
instance IsPath UPath_URI
    where type UType UPath_URI = Univ
          type SType UPath_URI = URI
          idPath = UPath_URI
instance IsPath UPath_UUID
    where type UType UPath_UUID = Univ
          type SType UPath_UUID = UUID
          idPath = UPath_UUID
instance IsPath UPath_UserId
    where type UType UPath_UserId = Univ
          type SType UPath_UserId = UserId
          idPath = UPath_UserId
ulens :: forall a . U Univ a => Iso' Univ a
