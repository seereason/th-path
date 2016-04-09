-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
import Control.Lens (iso, lens, _Just, _1, _2, _Left, _Right, Lens', toListOf, Traversal')
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (toList)
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node))
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(Path_At), toPairs)
import Language.Haskell.TH.Path.View (View(viewLens))
import Network.URI (URI(URI), URIAuth)

data UPath_Author = UPath_Author_authorName UPath_Markup | UPath_Author_authorCredentials UPath_Markup | UPath_Author deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Bool = UPath_Bool_View UPath_String | UPath_Bool deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Branding = UPath_Branding_View UPath_Text | UPath_Branding deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_CIString = UPath_CIString_View UPath_Text | UPath_CIString deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Dimension = UPath_Dimension_View UPath_JSONText | UPath_Dimension deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Double = UPath_Double_View UPath_String | UPath_Double deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ImageCrop = UPath_ImageCrop deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ImageFile = UPath_ImageFile deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ImageSize
    = UPath_ImageSize_dim UPath_Dimension | UPath_ImageSize_size UPath_Double | UPath_ImageSize_units UPath_Units | UPath_ImageSize
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Int = UPath_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Int64 = UPath_Int64 deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Integer = UPath_Integer deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Item
    = UPath_Item_itemName UPath_Text | UPath_Item_fields UPath_MIM | UPath_Item_images UPath_ReportImages | UPath_Item
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_JSONText = UPath_JSONText deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Markup = UPath_Markup_markdownText UPath_Text | UPath_Markup_htmlText UPath_Text | UPath_Markup deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_MaybeImageFile = UPath_MaybeImageFile_View UPath_String | UPath_MaybeImageFile deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_MaybeReportIntendedUse = UPath_MaybeReportIntendedUse_View UPath_String | UPath_MaybeReportIntendedUse deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Permissions
    = UPath_Permissions_owner UPath_UserId | UPath_Permissions_writers UPath_UserIds | UPath_Permissions_readers UPath_UserIds | UPath_Permissions
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReadOnlyFilePath = UPath_ReadOnlyFilePath_View UPath_String | UPath_ReadOnlyFilePath deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Report = UPath_Report_View UPath_ReportView | UPath_Report deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportElem = UPath_ReportElem_elemItem UPath_Item | UPath_ReportElem_elemText UPath_Markup | UPath_ReportElem deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportFlags = UPath_ReportFlags_hideEmptyItemFields UPath_Bool | UPath_ReportFlags deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportImage = UPath_ReportImage_View UPath_ReportImageView | UPath_ReportImage deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportImageView
    = UPath_ReportImageView__picSize UPath_SaneSizeImageSize
    | UPath_ReportImageView__picCrop UPath_ImageCrop
    | UPath_ReportImageView__picCaption UPath_Markup
    | UPath_ReportImageView__picOriginal (Path_Maybe (Path_Either UPath_URI UPath_ImageFile))
    | UPath_ReportImageView__picEditedDeprecated UPath_MaybeImageFile
    | UPath_ReportImageView__picThumbDeprecated UPath_MaybeImageFile
    | UPath_ReportImageView__picPrinterDeprecated UPath_MaybeImageFile
    | UPath_ReportImageView__picMustEnlarge UPath_Bool
    | UPath_ReportImageView__picEnlargedDeprecated UPath_MaybeImageFile
    | UPath_ReportImageView
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportIntendedUse = UPath_ReportIntendedUse_View UPath_String | UPath_ReportIntendedUse deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportMap = UPath_ReportMap_unReportMap UPath_MRR | UPath_ReportMap deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportStandard = UPath_ReportStandard_unReportStandard UPath_Int | UPath_ReportStandard deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportStatus = UPath_ReportStatus_View UPath_String | UPath_ReportStatus deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportValueApproachInfo
    = UPath_ReportValueApproachInfo_reportValueApproachName UPath_Markup
    | UPath_ReportValueApproachInfo_reportValueApproachDescription UPath_Markup
    | UPath_ReportValueApproachInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportValueTypeInfo
    = UPath_ReportValueTypeInfo_reportValueTypeName UPath_Markup
    | UPath_ReportValueTypeInfo_reportValueTypeDescription UPath_Markup
    | UPath_ReportValueTypeInfo_reportValueTypeDefinition UPath_Markup
    | UPath_ReportValueTypeInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_ReportView
    = UPath_ReportView__reportFolder UPath_ReadOnlyFilePath
    | UPath_ReportView__reportName UPath_Markup
    | UPath_ReportView__reportDate UPath_Markup
    | UPath_ReportView__reportContractDate UPath_Markup
    | UPath_ReportView__reportInspectionDate UPath_Markup
    | UPath_ReportView__reportEffectiveDate UPath_Markup
    | UPath_ReportView__reportAuthors UPath_Authors
    | UPath_ReportView__reportPreparer UPath_Markup
    | UPath_ReportView__reportPreparerEIN UPath_Markup
    | UPath_ReportView__reportPreparerAddress UPath_Markup
    | UPath_ReportView__reportPreparerEMail UPath_Markup
    | UPath_ReportView__reportPreparerWebsite UPath_Markup
    | UPath_ReportView__reportAbbrevs UPath_AbbrevPairs
    | UPath_ReportView__reportTitle UPath_Markup
    | UPath_ReportView__reportHeader UPath_Markup
    | UPath_ReportView__reportFooter UPath_Markup
    | UPath_ReportView__reportIntendedUse UPath_MaybeReportIntendedUse
    | UPath_ReportView__reportValueTypeInfo UPath_ReportValueTypeInfo
    | UPath_ReportView__reportValueApproachInfo UPath_ReportValueApproachInfo
    | UPath_ReportView__reportClientName UPath_Markup
    | UPath_ReportView__reportClientAddress UPath_Markup
    | UPath_ReportView__reportClientGreeting UPath_Markup
    | UPath_ReportView__reportItemsOwnerFull UPath_Markup
    | UPath_ReportView__reportItemsOwner UPath_Markup
    | UPath_ReportView__reportBriefItems UPath_Markup
    | UPath_ReportView__reportInspectionLocation UPath_Markup
    | UPath_ReportView__reportBody UPath_ReportElems
    | UPath_ReportView__reportGlossary UPath_MarkupPairs
    | UPath_ReportView__reportSources UPath_MarkupPairs
    | UPath_ReportView__reportLetterOfTransmittal UPath_Markup
    | UPath_ReportView__reportScopeOfWork UPath_Markup
    | UPath_ReportView__reportCertification UPath_Markups
    | UPath_ReportView__reportLimitingConditions UPath_Markups
    | UPath_ReportView__reportPrivacyPolicy UPath_Markup
    | UPath_ReportView__reportPerms UPath_Permissions
    | UPath_ReportView__reportRevision UPath_Integer
    | UPath_ReportView__reportCreated UPath_EpochMilli
    | UPath_ReportView__reportBranding UPath_Branding
    | UPath_ReportView__reportStatus UPath_ReportStatus
    | UPath_ReportView__reportRedacted UPath_Bool
    | UPath_ReportView__reportFlags UPath_ReportFlags
    | UPath_ReportView__reportUUID UPath_UUID
    | UPath_ReportView__reportOrderByItemName UPath_Bool
    | UPath_ReportView__reportDisplayItemName UPath_Bool
    | UPath_ReportView__reportStandardsVersion UPath_ReportStandard
    | UPath_ReportView
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_SaneSizeImageSize = UPath_SaneSizeImageSize_View UPath_ImageSize | UPath_SaneSizeImageSize deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_String = UPath_String_View UPath_JSONText | UPath_String deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Text = UPath_Text_View UPath_JSONText | UPath_Text deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_URI = UPath_URI deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_UUID = UPath_UUID deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_Units = UPath_Units_View UPath_JSONText | UPath_Units deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_UserId = UPath_UserId deriving (Eq, Ord, Read, Show, Typeable, Data)
data UPath_UserIds = UPath_UserIds_View UPath_Text | UPath_UserIds deriving (Eq, Ord, Read, Show, Typeable, Data)
data Univ
    = U1 String
    | U2 Int64
    | U3 Int
    | U4 Bool
    | U5 Double
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
    deriving (Eq, Show, Data, Typeable)
type UPath_AbbrevPair = Path_Pair UPath_CIString UPath_Markup
type UPath_AbbrevPairs = Path_OMap AbbrevPairID (Path_Pair UPath_CIString UPath_Markup)
type UPath_Authors = Path_OMap AuthorID UPath_Author
type UPath_Checksum = UPath_String
type UPath_EUI = Path_Either UPath_URI UPath_ImageFile
type UPath_EpochMilli = UPath_Int64
type UPath_FilePath = UPath_String
type UPath_MEUI = Path_Maybe (Path_Either UPath_URI UPath_ImageFile)
type UPath_MIM = Path_Map ItemFieldName UPath_Markup
type UPath_MRR = Path_Map ReportID UPath_Report
type UPath_MarkupPair = Path_Pair UPath_Markup UPath_Markup
type UPath_MarkupPairs = Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup)
type UPath_Markups = Path_OMap MarkupID UPath_Markup
type UPath_ReportElems = Path_OMap ReportElemID UPath_ReportElem
type UPath_ReportImages = Path_OMap ReportImageID UPath_ReportImage
type UPath_Size = UPath_Int
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
          lens_ReportImage_picEditedDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImage_picEditedDeprecated = (.) lens_reportImage lens_ReportImage_picEditedDeprecated
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImage_picEnlargedDeprecated = (.) lens_reportImage lens_ReportImage_picEnlargedDeprecated
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImage_picMustEnlarge = (.) lens_reportImage lens_ReportImage_picMustEnlarge
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal :: forall . Lens' c MEUI
          lens_ReportImage_picOriginal = (.) lens_reportImage lens_ReportImage_picOriginal
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImage_picPrinterDeprecated = (.) lens_reportImage lens_ReportImage_picPrinterDeprecated
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize :: forall . Lens' c ImageSize
          lens_ReportImage_picSize = (.) lens_reportImage lens_ReportImage_picSize
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImage_picThumbDeprecated = (.) lens_reportImage lens_ReportImage_picThumbDeprecated
          {-# INLINE lens_ReportImage_picThumbDeprecated #-}
class HasReportImageView c
    where lens_reportImageView :: Lens' c ReportImageView
          lens_ReportImageView__picCaption :: forall . Lens' c Markup
          lens_ReportImageView__picCaption = (.) lens_reportImageView lens_ReportImageView__picCaption
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop :: forall . Lens' c ImageCrop
          lens_ReportImageView__picCrop = (.) lens_reportImageView lens_ReportImageView__picCrop
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picEditedDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImageView__picEditedDeprecated = (.) lens_reportImageView lens_ReportImageView__picEditedDeprecated
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImageView__picEnlargedDeprecated = (.) lens_reportImageView lens_ReportImageView__picEnlargedDeprecated
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImageView__picMustEnlarge = (.) lens_reportImageView lens_ReportImageView__picMustEnlarge
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal :: forall . Lens' c (Maybe (Either URI ImageFile))
          lens_ReportImageView__picOriginal = (.) lens_reportImageView lens_ReportImageView__picOriginal
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImageView__picPrinterDeprecated = (.) lens_reportImageView lens_ReportImageView__picPrinterDeprecated
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize :: forall . Lens' c SaneSizeImageSize
          lens_ReportImageView__picSize = (.) lens_reportImageView lens_ReportImageView__picSize
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated :: forall . Lens' c MaybeImageFile
          lens_ReportImageView__picThumbDeprecated = (.) lens_reportImageView lens_ReportImageView__picThumbDeprecated
          {-# INLINE lens_ReportImageView__picThumbDeprecated #-}
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
    where data UPeek Univ String = UPeek_String (UPath Univ String) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_String
          upeekPath (UPeek_String p _) = p
          upeekValue (UPeek_String _ x) = x
          type UPath Univ String = UPath_String
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_String_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: JSONText))) :: UPeek Univ
                                                                                                                                                                                                                                      JSONText) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_String_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [JSONText]))) [UPath_String_View] ++ [])
instance PathStart Univ Int64
    where data UPeek Univ Int64 = UPeek_Int64 (UPath Univ Int64) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Int64
          upeekPath (UPeek_Int64 p _) = p
          upeekValue (UPeek_Int64 _ x) = x
          type UPath Univ Int64 = UPath_Int64
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Bool
    where data UPeek Univ Bool = UPeek_Bool (UPath Univ Bool) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Bool
          upeekPath (UPeek_Bool p _) = p
          upeekValue (UPeek_Bool _ x) = x
          type UPath Univ Bool = UPath_Bool
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Bool_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Bool_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_Bool_View] ++ [])
instance PathStart Univ Double
    where data UPeek Univ Double = UPeek_Double (UPath Univ Double) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Double
          upeekPath (UPeek_Double p _) = p
          upeekValue (UPeek_Double _ x) = x
          type UPath Univ Double = UPath_Double
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Double_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Double_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_Double_View] ++ [])
instance PathStart Univ Int
    where data UPeek Univ Int = UPeek_Int (UPath Univ Int) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Int
          upeekPath (UPeek_Int p _) = p
          upeekValue (UPeek_Int _ x) = x
          type UPath Univ Int = UPath_Int
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Dimension
    where data UPeek Univ Dimension = UPeek_Dimension (UPath Univ Dimension) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Dimension
          upeekPath (UPeek_Dimension p _) = p
          upeekValue (UPeek_Dimension _ x) = x
          type UPath Univ Dimension = UPath_Dimension
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Dimension_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: JSONText))) :: UPeek Univ
                                                                                                                                                                                                                                      JSONText) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Dimension_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [JSONText]))) [UPath_Dimension_View] ++ [])
instance PathStart Univ ImageCrop
    where data UPeek Univ ImageCrop = UPeek_ImageCrop (UPath Univ ImageCrop) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ImageCrop
          upeekPath (UPeek_ImageCrop p _) = p
          upeekValue (UPeek_ImageCrop _ x) = x
          type UPath Univ ImageCrop = UPath_ImageCrop
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ ImageSize
    where data UPeek Univ ImageSize = UPeek_ImageSize (UPath Univ ImageSize) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ImageSize
          upeekPath (UPeek_ImageSize p _) = p
          upeekValue (UPeek_ImageSize _ x) = x
          type UPath Univ ImageSize = UPath_ImageSize
          upaths _ _f r0 (_xconc@(ImageSize {})) = foldr _f (foldr _f (foldr _f r0 [UPath_ImageSize_units idPath]) [UPath_ImageSize_size idPath]) [UPath_ImageSize_dim idPath]
          upeekRow _unv (_xconc@(ImageSize {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Dimension))) :: UPeek Univ
                                                                                                                                                                                                                                                 Dimension) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ImageSize_dim] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Double))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Double) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ImageSize_size] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Units))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Units) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ImageSize_units] ++ [])))
          upeekTree _unv (_xconc@(ImageSize {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Dimension]))) [UPath_ImageSize_dim] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Double]))) [UPath_ImageSize_size] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Units]))) [UPath_ImageSize_units] ++ [])))
instance PathStart Univ Units
    where data UPeek Univ Units = UPeek_Units (UPath Univ Units) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Units
          upeekPath (UPeek_Units p _) = p
          upeekValue (UPeek_Units _ x) = x
          type UPath Univ Units = UPath_Units
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Units_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: JSONText))) :: UPeek Univ
                                                                                                                                                                                                                                      JSONText) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Units_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [JSONText]))) [UPath_Units_View] ++ [])
instance PathStart Univ ImageFile
    where data UPeek Univ ImageFile = UPeek_ImageFile (UPath Univ ImageFile) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ImageFile
          upeekPath (UPeek_ImageFile p _) = p
          upeekValue (UPeek_ImageFile _ x) = x
          type UPath Univ ImageFile = UPath_ImageFile
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Integer
    where data UPeek Univ Integer = UPeek_Integer (UPath Univ Integer) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Integer
          upeekPath (UPeek_Integer p _) = p
          upeekValue (UPeek_Integer _ x) = x
          type UPath Univ Integer = UPath_Integer
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ JSONText
    where data UPeek Univ JSONText = UPeek_JSONText (UPath Univ JSONText) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_JSONText
          upeekPath (UPeek_JSONText p _) = p
          upeekValue (UPeek_JSONText _ x) = x
          type UPath Univ JSONText = UPath_JSONText
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Markup
    where data UPeek Univ Markup = UPeek_Markup (UPath Univ Markup) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Markup
          upeekPath (UPeek_Markup p _) = p
          upeekValue (UPeek_Markup _ x) = x
          type UPath Univ Markup = UPath_Markup
          upaths _ _f r0 (_xconc@(Markdown {})) = foldr _f r0 [UPath_Markup_markdownText idPath]
          upaths _ _f r0 (_xconc@(Html {})) = foldr _f r0 [UPath_Markup_htmlText idPath]
          upaths _ _f r0 (_xconc@(LaTeX {})) = r0
          upaths _ _f r0 (_xconc@(Pandoc {})) = r0
          upaths _ _f r0 (_xconc@(Markup {})) = r0
          upeekRow _unv (_xconc@(Markdown {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Text))) :: UPeek Univ
                                                                                                                                                                                                                                           Text) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Markup_markdownText] ++ [])
          upeekRow _unv (_xconc@(Html {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Text))) :: UPeek Univ
                                                                                                                                                                                                                                       Text) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Markup_htmlText] ++ [])
          upeekRow _unv (_xconc@(LaTeX {})) = Node (upeekCons idPath Nothing) []
          upeekRow _unv (_xconc@(Pandoc {})) = Node (upeekCons idPath Nothing) []
          upeekRow _unv (_xconc@(Markup {})) = Node (upeekCons idPath Nothing) []
          upeekTree _unv (_xconc@(Markdown {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Text]))) [UPath_Markup_markdownText] ++ [])
          upeekTree _unv (_xconc@(Html {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Text]))) [UPath_Markup_htmlText] ++ [])
          upeekTree _unv (_xconc@(LaTeX {})) = Node (upeekCons idPath Nothing) []
          upeekTree _unv (_xconc@(Pandoc {})) = Node (upeekCons idPath Nothing) []
          upeekTree _unv (_xconc@(Markup {})) = Node (upeekCons idPath Nothing) []
instance PathStart Univ Permissions
    where data UPeek Univ Permissions = UPeek_Permissions (UPath Univ Permissions) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Permissions
          upeekPath (UPeek_Permissions p _) = p
          upeekValue (UPeek_Permissions _ x) = x
          type UPath Univ Permissions = UPath_Permissions
          upaths _ _f r0 (_xconc@(Permissions {})) = foldr _f (foldr _f (foldr _f r0 [UPath_Permissions_readers idPath]) [UPath_Permissions_writers idPath]) [UPath_Permissions_owner idPath]
          upeekRow _unv (_xconc@(Permissions {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: UserId))) :: UPeek Univ
                                                                                                                                                                                                                                                UserId) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Permissions_owner] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: UserIds))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    UserIds) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Permissions_writers] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: UserIds))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           UserIds) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Permissions_readers] ++ [])))
          upeekTree _unv (_xconc@(Permissions {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [UserId]))) [UPath_Permissions_owner] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [UserIds]))) [UPath_Permissions_writers] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [UserIds]))) [UPath_Permissions_readers] ++ [])))
instance PathStart Univ UserIds
    where data UPeek Univ UserIds = UPeek_UserIds (UPath Univ UserIds) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_UserIds
          upeekPath (UPeek_UserIds p _) = p
          upeekValue (UPeek_UserIds _ x) = x
          type UPath Univ UserIds = UPath_UserIds
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_UserIds_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Text))) :: UPeek Univ
                                                                                                                                                                                                                                  Text) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_UserIds_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Text]))) [UPath_UserIds_View] ++ [])
instance PathStart Univ AbbrevPair
    where data UPeek Univ AbbrevPair = UPeek_AbbrevPair (UPath Univ AbbrevPair) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_AbbrevPair
          upeekPath (UPeek_AbbrevPair p _) = p
          upeekValue (UPeek_AbbrevPair _ x) = x
          type UPath Univ AbbrevPair = UPath_AbbrevPair
          upaths _ _f r0 (_xconc@_) = foldr _f (foldr _f r0 [Path_Second idPath]) [Path_First idPath]
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: CIString))) :: UPeek Univ
                                                                                                                                                                                                                               CIString) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_First] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_Second] ++ []))
          upeekTree _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [CIString]))) [Path_First] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [Path_Second] ++ []))
instance PathStart Univ AbbrevPairs
    where data UPeek Univ AbbrevPairs = UPeek_AbbrevPairs (UPath Univ AbbrevPairs) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_AbbrevPairs
          upeekPath (UPeek_AbbrevPairs p _) = p
          upeekValue (UPeek_AbbrevPairs _ x) = x
          type UPath Univ AbbrevPairs = UPath_AbbrevPairs
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_At k idPath) (toPairs _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: AbbrevPair))) :: UPeek Univ
                                                                                                                                                                                                                                        AbbrevPair) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                              _v) -> Path_At k) (toPairs _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [AbbrevPair]))) (map (\(k,
                                                                                                                                                                                                                                                                 _v) -> Path_At k) (toPairs _xyz)) ++ [])
instance PathStart Univ Author
    where data UPeek Univ Author = UPeek_Author (UPath Univ Author) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Author
          upeekPath (UPeek_Author p _) = p
          upeekValue (UPeek_Author _ x) = x
          type UPath Univ Author = UPath_Author
          upaths _ _f r0 (_xconc@(Author {})) = foldr _f (foldr _f r0 [UPath_Author_authorCredentials idPath]) [UPath_Author_authorName idPath]
          upeekRow _unv (_xconc@(Author {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                           Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Author_authorName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Author_authorCredentials] ++ []))
          upeekTree _unv (_xconc@(Author {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_Author_authorName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_Author_authorCredentials] ++ []))
instance PathStart Univ Authors
    where data UPeek Univ Authors = UPeek_Authors (UPath Univ Authors) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Authors
          upeekPath (UPeek_Authors p _) = p
          upeekValue (UPeek_Authors _ x) = x
          type UPath Univ Authors = UPath_Authors
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_At k idPath) (toPairs _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Author))) :: UPeek Univ
                                                                                                                                                                                                                                    Author) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                      _v) -> Path_At k) (toPairs _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Author]))) (map (\(k,
                                                                                                                                                                                                                                                             _v) -> Path_At k) (toPairs _xyz)) ++ [])
instance PathStart Univ Branding
    where data UPeek Univ Branding = UPeek_Branding (UPath Univ Branding) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Branding
          upeekPath (UPeek_Branding p _) = p
          upeekValue (UPeek_Branding _ x) = x
          type UPath Univ Branding = UPath_Branding
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Branding_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Text))) :: UPeek Univ
                                                                                                                                                                                                                                  Text) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Branding_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Text]))) [UPath_Branding_View] ++ [])
instance PathStart Univ MarkupPair
    where data UPeek Univ MarkupPair = UPeek_MarkupPair (UPath Univ MarkupPair) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MarkupPair
          upeekPath (UPeek_MarkupPair p _) = p
          upeekValue (UPeek_MarkupPair _ x) = x
          type UPath Univ MarkupPair = UPath_MarkupPair
          upaths _ _f r0 (_xconc@_) = foldr _f (foldr _f r0 [Path_Second idPath]) [Path_First idPath]
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_First] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_Second] ++ []))
          upeekTree _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [Path_First] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [Path_Second] ++ []))
instance PathStart Univ MarkupPairs
    where data UPeek Univ MarkupPairs = UPeek_MarkupPairs (UPath Univ MarkupPairs) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MarkupPairs
          upeekPath (UPeek_MarkupPairs p _) = p
          upeekValue (UPeek_MarkupPairs _ x) = x
          type UPath Univ MarkupPairs = UPath_MarkupPairs
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_At k idPath) (toPairs _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MarkupPair))) :: UPeek Univ
                                                                                                                                                                                                                                        MarkupPair) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                              _v) -> Path_At k) (toPairs _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MarkupPair]))) (map (\(k,
                                                                                                                                                                                                                                                                 _v) -> Path_At k) (toPairs _xyz)) ++ [])
instance PathStart Univ Markups
    where data UPeek Univ Markups = UPeek_Markups (UPath Univ Markups) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Markups
          upeekPath (UPeek_Markups p _) = p
          upeekValue (UPeek_Markups _ x) = x
          type UPath Univ Markups = UPath_Markups
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_At k idPath) (toPairs _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                    Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                      _v) -> Path_At k) (toPairs _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) (map (\(k,
                                                                                                                                                                                                                                                             _v) -> Path_At k) (toPairs _xyz)) ++ [])
instance PathStart Univ MaybeReportIntendedUse
    where data UPeek Univ MaybeReportIntendedUse = UPeek_MaybeReportIntendedUse (UPath Univ MaybeReportIntendedUse) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MaybeReportIntendedUse
          upeekPath (UPeek_MaybeReportIntendedUse p _) = p
          upeekValue (UPeek_MaybeReportIntendedUse _ x) = x
          type UPath Univ MaybeReportIntendedUse = UPath_MaybeReportIntendedUse
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_MaybeReportIntendedUse_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_MaybeReportIntendedUse_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_MaybeReportIntendedUse_View] ++ [])
instance PathStart Univ Report
    where data UPeek Univ Report = UPeek_Report (UPath Univ Report) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Report
          upeekPath (UPeek_Report p _) = p
          upeekValue (UPeek_Report _ x) = x
          type UPath Univ Report = UPath_Report
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Report_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportView))) :: UPeek Univ
                                                                                                                                                                                                                                        ReportView) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Report_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportView]))) [UPath_Report_View] ++ [])
instance PathStart Univ ReportElem
    where data UPeek Univ ReportElem = UPeek_ReportElem (UPath Univ ReportElem) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportElem
          upeekPath (UPeek_ReportElem p _) = p
          upeekValue (UPeek_ReportElem _ x) = x
          type UPath Univ ReportElem = UPath_ReportElem
          upaths _ _f r0 (_xconc@(ReportItem {})) = foldr _f r0 [UPath_ReportElem_elemItem idPath]
          upaths _ _f r0 (_xconc@(ReportParagraph {})) = foldr _f r0 [UPath_ReportElem_elemText idPath]
          upaths _ _f r0 (_xconc@(ReportUndecided {})) = r0
          upeekRow _unv (_xconc@(ReportItem {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Item))) :: UPeek Univ
                                                                                                                                                                                                                                             Item) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportElem_elemItem] ++ [])
          upeekRow _unv (_xconc@(ReportParagraph {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                    Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportElem_elemText] ++ [])
          upeekRow _unv (_xconc@(ReportUndecided {})) = Node (upeekCons idPath Nothing) []
          upeekTree _unv (_xconc@(ReportItem {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Item]))) [UPath_ReportElem_elemItem] ++ [])
          upeekTree _unv (_xconc@(ReportParagraph {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportElem_elemText] ++ [])
          upeekTree _unv (_xconc@(ReportUndecided {})) = Node (upeekCons idPath Nothing) []
instance PathStart Univ ReportElems
    where data UPeek Univ ReportElems = UPeek_ReportElems (UPath Univ ReportElems) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportElems
          upeekPath (UPeek_ReportElems p _) = p
          upeekValue (UPeek_ReportElems _ x) = x
          type UPath Univ ReportElems = UPath_ReportElems
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_At k idPath) (toPairs _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportElem))) :: UPeek Univ
                                                                                                                                                                                                                                        ReportElem) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                              _v) -> Path_At k) (toPairs _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportElem]))) (map (\(k,
                                                                                                                                                                                                                                                                 _v) -> Path_At k) (toPairs _xyz)) ++ [])
instance PathStart Univ ReportFlags
    where data UPeek Univ ReportFlags = UPeek_ReportFlags (UPath Univ ReportFlags) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportFlags
          upeekPath (UPeek_ReportFlags p _) = p
          upeekValue (UPeek_ReportFlags _ x) = x
          type UPath Univ ReportFlags = UPath_ReportFlags
          upaths _ _f r0 (_xconc@(ReportFlags {})) = foldr _f r0 [UPath_ReportFlags_hideEmptyItemFields idPath]
          upeekRow _unv (_xconc@(ReportFlags {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Bool))) :: UPeek Univ
                                                                                                                                                                                                                                              Bool) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportFlags_hideEmptyItemFields] ++ [])
          upeekTree _unv (_xconc@(ReportFlags {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Bool]))) [UPath_ReportFlags_hideEmptyItemFields] ++ [])
instance PathStart Univ ReportIntendedUse
    where data UPeek Univ ReportIntendedUse = UPeek_ReportIntendedUse (UPath Univ ReportIntendedUse) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportIntendedUse
          upeekPath (UPeek_ReportIntendedUse p _) = p
          upeekValue (UPeek_ReportIntendedUse _ x) = x
          type UPath Univ ReportIntendedUse = UPath_ReportIntendedUse
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_ReportIntendedUse_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportIntendedUse_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_ReportIntendedUse_View] ++ [])
instance PathStart Univ ReportStandard
    where data UPeek Univ ReportStandard = UPeek_ReportStandard (UPath Univ ReportStandard) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportStandard
          upeekPath (UPeek_ReportStandard p _) = p
          upeekValue (UPeek_ReportStandard _ x) = x
          type UPath Univ ReportStandard = UPath_ReportStandard
          upaths _ _f r0 (_xconc@(ReportStandard {})) = foldr _f r0 [UPath_ReportStandard_unReportStandard idPath]
          upeekRow _unv (_xconc@(ReportStandard {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Int))) :: UPeek Univ
                                                                                                                                                                                                                                                Int) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportStandard_unReportStandard] ++ [])
          upeekTree _unv (_xconc@(ReportStandard {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Int]))) [UPath_ReportStandard_unReportStandard] ++ [])
instance PathStart Univ ReportStatus
    where data UPeek Univ ReportStatus = UPeek_ReportStatus (UPath Univ ReportStatus) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportStatus
          upeekPath (UPeek_ReportStatus p _) = p
          upeekValue (UPeek_ReportStatus _ x) = x
          type UPath Univ ReportStatus = UPath_ReportStatus
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_ReportStatus_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportStatus_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_ReportStatus_View] ++ [])
instance PathStart Univ ReportValueApproachInfo
    where data UPeek Univ ReportValueApproachInfo = UPeek_ReportValueApproachInfo (UPath Univ ReportValueApproachInfo) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportValueApproachInfo
          upeekPath (UPeek_ReportValueApproachInfo p _) = p
          upeekValue (UPeek_ReportValueApproachInfo _ x) = x
          type UPath Univ ReportValueApproachInfo = UPath_ReportValueApproachInfo
          upaths _ _f r0 (_xconc@(ReportValueApproachInfo {})) = foldr _f (foldr _f r0 [UPath_ReportValueApproachInfo_reportValueApproachDescription idPath]) [UPath_ReportValueApproachInfo_reportValueApproachName idPath]
          upeekRow _unv (_xconc@(ReportValueApproachInfo {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                            Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportValueApproachInfo_reportValueApproachName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportValueApproachInfo_reportValueApproachDescription] ++ []))
          upeekTree _unv (_xconc@(ReportValueApproachInfo {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportValueApproachInfo_reportValueApproachName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportValueApproachInfo_reportValueApproachDescription] ++ []))
instance PathStart Univ ReportValueTypeInfo
    where data UPeek Univ ReportValueTypeInfo = UPeek_ReportValueTypeInfo (UPath Univ ReportValueTypeInfo) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportValueTypeInfo
          upeekPath (UPeek_ReportValueTypeInfo p _) = p
          upeekValue (UPeek_ReportValueTypeInfo _ x) = x
          type UPath Univ ReportValueTypeInfo = UPath_ReportValueTypeInfo
          upaths _ _f r0 (_xconc@(ReportValueTypeInfo {})) = foldr _f (foldr _f (foldr _f r0 [UPath_ReportValueTypeInfo_reportValueTypeDefinition idPath]) [UPath_ReportValueTypeInfo_reportValueTypeDescription idPath]) [UPath_ReportValueTypeInfo_reportValueTypeName idPath]
          upeekRow _unv (_xconc@(ReportValueTypeInfo {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                        Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportValueTypeInfo_reportValueTypeName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportValueTypeInfo_reportValueTypeDescription] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportValueTypeInfo_reportValueTypeDefinition] ++ [])))
          upeekTree _unv (_xconc@(ReportValueTypeInfo {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportValueTypeInfo_reportValueTypeName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportValueTypeInfo_reportValueTypeDescription] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportValueTypeInfo_reportValueTypeDefinition] ++ [])))
instance PathStart Univ EUI
    where data UPeek Univ EUI = UPeek_EUI (UPath Univ EUI) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_EUI
          upeekPath (UPeek_EUI p _) = p
          upeekValue (UPeek_EUI _ x) = x
          type UPath Univ EUI = UPath_EUI
          upaths _ _f r0 (_xconc@(Left _)) = foldr _f r0 [Path_Left idPath]
          upaths _ _f r0 (_xconc@(Right _)) = foldr _f r0 [Path_Right idPath]
          upeekRow _unv (_xconc@(Left _)) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: URI))) :: UPeek Univ
                                                                                                                                                                                                                                     URI) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_Left] ++ [])
          upeekRow _unv (_xconc@(Right _)) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ImageFile))) :: UPeek Univ
                                                                                                                                                                                                                                            ImageFile) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_Right] ++ [])
          upeekTree _unv (_xconc@(Left _)) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [URI]))) [Path_Left] ++ [])
          upeekTree _unv (_xconc@(Right _)) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ImageFile]))) [Path_Right] ++ [])
instance PathStart Univ MEUI
    where data UPeek Univ MEUI = UPeek_MEUI (UPath Univ MEUI) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MEUI
          upeekPath (UPeek_MEUI p _) = p
          upeekValue (UPeek_MEUI _ x) = x
          type UPath Univ MEUI = UPath_MEUI
          upaths _ _f r0 (_xconc@_) = foldr _f r0 [Path_Just idPath]
          upeekRow _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: EUI))) :: UPeek Univ
                                                                                                                                                                                                                          EUI) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [Path_Just] ++ [])
          upeekTree _unv _xconc = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [EUI]))) [Path_Just] ++ [])
instance PathStart Univ MaybeImageFile
    where data UPeek Univ MaybeImageFile = UPeek_MaybeImageFile (UPath Univ MaybeImageFile) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MaybeImageFile
          upeekPath (UPeek_MaybeImageFile p _) = p
          upeekValue (UPeek_MaybeImageFile _ x) = x
          type UPath Univ MaybeImageFile = UPath_MaybeImageFile
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_MaybeImageFile_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_MaybeImageFile_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_MaybeImageFile_View] ++ [])
instance PathStart Univ ReportImage
    where data UPeek Univ ReportImage = UPeek_ReportImage (UPath Univ ReportImage) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportImage
          upeekPath (UPeek_ReportImage p _) = p
          upeekValue (UPeek_ReportImage _ x) = x
          type UPath Univ ReportImage = UPath_ReportImage
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_ReportImage_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportImageView))) :: UPeek Univ
                                                                                                                                                                                                                                             ReportImageView) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImage_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportImageView]))) [UPath_ReportImage_View] ++ [])
instance PathStart Univ ReportImages
    where data UPeek Univ ReportImages = UPeek_ReportImages (UPath Univ ReportImages) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportImages
          upeekPath (UPeek_ReportImages p _) = p
          upeekValue (UPeek_ReportImages _ x) = x
          type UPath Univ ReportImages = UPath_ReportImages
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_At k idPath) (toPairs _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportImage))) :: UPeek Univ
                                                                                                                                                                                                                                         ReportImage) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                                _v) -> Path_At k) (toPairs _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportImage]))) (map (\(k,
                                                                                                                                                                                                                                                                  _v) -> Path_At k) (toPairs _xyz)) ++ [])
instance PathStart Univ ReadOnlyFilePath
    where data UPeek Univ ReadOnlyFilePath = UPeek_ReadOnlyFilePath (UPath Univ ReadOnlyFilePath) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReadOnlyFilePath
          upeekPath (UPeek_ReadOnlyFilePath p _) = p
          upeekValue (UPeek_ReadOnlyFilePath _ x) = x
          type UPath Univ ReadOnlyFilePath = UPath_ReadOnlyFilePath
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_ReadOnlyFilePath_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: String))) :: UPeek Univ
                                                                                                                                                                                                                                    String) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReadOnlyFilePath_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [String]))) [UPath_ReadOnlyFilePath_View] ++ [])
instance PathStart Univ ReportImageView
    where data UPeek Univ ReportImageView = UPeek_ReportImageView (UPath Univ ReportImageView) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportImageView
          upeekPath (UPeek_ReportImageView p _) = p
          upeekValue (UPeek_ReportImageView _ x) = x
          type UPath Univ ReportImageView = UPath_ReportImageView
          upaths _ _f r0 (_xconc@(ReportImageView {})) = foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f r0 [UPath_ReportImageView__picEnlargedDeprecated idPath]) [UPath_ReportImageView__picMustEnlarge idPath]) [UPath_ReportImageView__picPrinterDeprecated idPath]) [UPath_ReportImageView__picThumbDeprecated idPath]) [UPath_ReportImageView__picEditedDeprecated idPath]) [UPath_ReportImageView__picOriginal idPath]) [UPath_ReportImageView__picCaption idPath]) [UPath_ReportImageView__picCrop idPath]) [UPath_ReportImageView__picSize idPath]
          upeekRow _unv (_xconc@(ReportImageView {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: SaneSizeImageSize))) :: UPeek Univ
                                                                                                                                                                                                                                                               SaneSizeImageSize) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picSize] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ImageCrop))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ImageCrop) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picCrop] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picCaption] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MEUI))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               MEUI) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picOriginal] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MaybeImageFile))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   MaybeImageFile) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picEditedDeprecated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MaybeImageFile))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         MaybeImageFile) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picThumbDeprecated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MaybeImageFile))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              MaybeImageFile) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picPrinterDeprecated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Bool))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Bool) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picMustEnlarge] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MaybeImageFile))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  MaybeImageFile) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportImageView__picEnlargedDeprecated] ++ [])))))))))
          upeekTree _unv (_xconc@(ReportImageView {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [SaneSizeImageSize]))) [UPath_ReportImageView__picSize] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ImageCrop]))) [UPath_ReportImageView__picCrop] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportImageView__picCaption] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MEUI]))) [UPath_ReportImageView__picOriginal] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MaybeImageFile]))) [UPath_ReportImageView__picEditedDeprecated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MaybeImageFile]))) [UPath_ReportImageView__picThumbDeprecated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MaybeImageFile]))) [UPath_ReportImageView__picPrinterDeprecated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Bool]))) [UPath_ReportImageView__picMustEnlarge] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MaybeImageFile]))) [UPath_ReportImageView__picEnlargedDeprecated] ++ [])))))))))
instance PathStart Univ ReportView
    where data UPeek Univ ReportView = UPeek_ReportView (UPath Univ ReportView) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportView
          upeekPath (UPeek_ReportView p _) = p
          upeekValue (UPeek_ReportView _ x) = x
          type UPath Univ ReportView = UPath_ReportView
          upaths _ _f r0 (_xconc@(ReportView {})) = foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f (foldr _f r0 [UPath_ReportView__reportStandardsVersion idPath]) [UPath_ReportView__reportDisplayItemName idPath]) [UPath_ReportView__reportOrderByItemName idPath]) [UPath_ReportView__reportUUID idPath]) [UPath_ReportView__reportFlags idPath]) [UPath_ReportView__reportRedacted idPath]) [UPath_ReportView__reportStatus idPath]) [UPath_ReportView__reportBranding idPath]) [UPath_ReportView__reportCreated idPath]) [UPath_ReportView__reportRevision idPath]) [UPath_ReportView__reportPerms idPath]) [UPath_ReportView__reportPrivacyPolicy idPath]) [UPath_ReportView__reportLimitingConditions idPath]) [UPath_ReportView__reportCertification idPath]) [UPath_ReportView__reportScopeOfWork idPath]) [UPath_ReportView__reportLetterOfTransmittal idPath]) [UPath_ReportView__reportSources idPath]) [UPath_ReportView__reportGlossary idPath]) [UPath_ReportView__reportBody idPath]) [UPath_ReportView__reportInspectionLocation idPath]) [UPath_ReportView__reportBriefItems idPath]) [UPath_ReportView__reportItemsOwner idPath]) [UPath_ReportView__reportItemsOwnerFull idPath]) [UPath_ReportView__reportClientGreeting idPath]) [UPath_ReportView__reportClientAddress idPath]) [UPath_ReportView__reportClientName idPath]) [UPath_ReportView__reportValueApproachInfo idPath]) [UPath_ReportView__reportValueTypeInfo idPath]) [UPath_ReportView__reportIntendedUse idPath]) [UPath_ReportView__reportFooter idPath]) [UPath_ReportView__reportHeader idPath]) [UPath_ReportView__reportTitle idPath]) [UPath_ReportView__reportAbbrevs idPath]) [UPath_ReportView__reportPreparerWebsite idPath]) [UPath_ReportView__reportPreparerEMail idPath]) [UPath_ReportView__reportPreparerAddress idPath]) [UPath_ReportView__reportPreparerEIN idPath]) [UPath_ReportView__reportPreparer idPath]) [UPath_ReportView__reportAuthors idPath]) [UPath_ReportView__reportEffectiveDate idPath]) [UPath_ReportView__reportInspectionDate idPath]) [UPath_ReportView__reportContractDate idPath]) [UPath_ReportView__reportDate idPath]) [UPath_ReportView__reportName idPath]) [UPath_ReportView__reportFolder idPath]
          upeekRow _unv (_xconc@(ReportView {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReadOnlyFilePath))) :: UPeek Univ
                                                                                                                                                                                                                                                         ReadOnlyFilePath) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportFolder] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportContractDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportInspectionDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportEffectiveDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Authors))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Authors) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportAuthors] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPreparer] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPreparerEIN] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPreparerAddress] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPreparerEMail] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPreparerWebsite] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: AbbrevPairs))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    AbbrevPairs) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportAbbrevs] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportTitle] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportHeader] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportFooter] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MaybeReportIntendedUse))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 MaybeReportIntendedUse) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportIntendedUse] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportValueTypeInfo))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ReportValueTypeInfo) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportValueTypeInfo] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportValueApproachInfo))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ReportValueApproachInfo) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportValueApproachInfo] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportClientName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportClientAddress] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportClientGreeting] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportItemsOwnerFull] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportItemsOwner] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportBriefItems] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportInspectionLocation] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportElems))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ReportElems) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportBody] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MarkupPairs))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              MarkupPairs) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportGlossary] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MarkupPairs))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    MarkupPairs) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportSources] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportLetterOfTransmittal] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportScopeOfWork] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markups))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Markups) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportCertification] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markups))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Markups) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportLimitingConditions] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPrivacyPolicy] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Permissions))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Permissions) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportPerms] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Integer))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Integer) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportRevision] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Int64))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Int64) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportCreated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Branding))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Branding) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportBranding] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportStatus))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ReportStatus) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportStatus] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Bool))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Bool) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportRedacted] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportFlags))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ReportFlags) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportFlags] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: UUID))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               UUID) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportUUID] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Bool))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Bool) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportOrderByItemName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Bool))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Bool) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportDisplayItemName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportStandard))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           ReportStandard) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportView__reportStandardsVersion] ++ [])))))))))))))))))))))))))))))))))))))))))))))
          upeekTree _unv (_xconc@(ReportView {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReadOnlyFilePath]))) [UPath_ReportView__reportFolder] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportContractDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportInspectionDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportEffectiveDate] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Authors]))) [UPath_ReportView__reportAuthors] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportPreparer] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportPreparerEIN] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportPreparerAddress] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportPreparerEMail] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportPreparerWebsite] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [AbbrevPairs]))) [UPath_ReportView__reportAbbrevs] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportTitle] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportHeader] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportFooter] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MaybeReportIntendedUse]))) [UPath_ReportView__reportIntendedUse] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportValueTypeInfo]))) [UPath_ReportView__reportValueTypeInfo] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportValueApproachInfo]))) [UPath_ReportView__reportValueApproachInfo] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportClientName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportClientAddress] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportClientGreeting] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportItemsOwnerFull] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportItemsOwner] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportBriefItems] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportInspectionLocation] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportElems]))) [UPath_ReportView__reportBody] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MarkupPairs]))) [UPath_ReportView__reportGlossary] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MarkupPairs]))) [UPath_ReportView__reportSources] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportLetterOfTransmittal] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportScopeOfWork] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markups]))) [UPath_ReportView__reportCertification] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markups]))) [UPath_ReportView__reportLimitingConditions] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) [UPath_ReportView__reportPrivacyPolicy] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Permissions]))) [UPath_ReportView__reportPerms] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Integer]))) [UPath_ReportView__reportRevision] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Int64]))) [UPath_ReportView__reportCreated] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Branding]))) [UPath_ReportView__reportBranding] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportStatus]))) [UPath_ReportView__reportStatus] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Bool]))) [UPath_ReportView__reportRedacted] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportFlags]))) [UPath_ReportView__reportFlags] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [UUID]))) [UPath_ReportView__reportUUID] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Bool]))) [UPath_ReportView__reportOrderByItemName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Bool]))) [UPath_ReportView__reportDisplayItemName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportStandard]))) [UPath_ReportView__reportStandardsVersion] ++ [])))))))))))))))))))))))))))))))))))))))))))))
instance PathStart Univ SaneSizeImageSize
    where data UPeek Univ SaneSizeImageSize = UPeek_SaneSizeImageSize (UPath Univ SaneSizeImageSize) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_SaneSizeImageSize
          upeekPath (UPeek_SaneSizeImageSize p _) = p
          upeekValue (UPeek_SaneSizeImageSize _ x) = x
          type UPath Univ SaneSizeImageSize = UPath_SaneSizeImageSize
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_SaneSizeImageSize_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ImageSize))) :: UPeek Univ
                                                                                                                                                                                                                                       ImageSize) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_SaneSizeImageSize_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ImageSize]))) [UPath_SaneSizeImageSize_View] ++ [])
instance PathStart Univ Item
    where data UPeek Univ Item = UPeek_Item (UPath Univ Item) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Item
          upeekPath (UPeek_Item p _) = p
          upeekValue (UPeek_Item _ x) = x
          type UPath Univ Item = UPath_Item
          upaths _ _f r0 (_xconc@(Item {})) = foldr _f (foldr _f (foldr _f r0 [UPath_Item_images idPath]) [UPath_Item_fields idPath]) [UPath_Item_itemName idPath]
          upeekRow _unv (_xconc@(Item {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Text))) :: UPeek Univ
                                                                                                                                                                                                                                       Text) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Item_itemName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MIM))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 MIM) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Item_fields] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: ReportImages))) :: UPeek Univ
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ReportImages) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Item_images] ++ [])))
          upeekTree _unv (_xconc@(Item {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Text]))) [UPath_Item_itemName] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MIM]))) [UPath_Item_fields] ++ (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [ReportImages]))) [UPath_Item_images] ++ [])))
instance PathStart Univ MIM
    where data UPeek Univ MIM = UPeek_MIM (UPath Univ MIM) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MIM
          upeekPath (UPeek_MIM p _) = p
          upeekValue (UPeek_MIM _ x) = x
          type UPath Univ MIM = UPath_MIM
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_Look k idPath) (toList _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Markup))) :: UPeek Univ
                                                                                                                                                                                                                                    Markup) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                      _v) -> Path_Look k) (toList _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Markup]))) (map (\(k,
                                                                                                                                                                                                                                                             _v) -> Path_Look k) (toList _xyz)) ++ [])
instance PathStart Univ MRR
    where data UPeek Univ MRR = UPeek_MRR (UPath Univ MRR) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_MRR
          upeekPath (UPeek_MRR p _) = p
          upeekValue (UPeek_MRR _ x) = x
          type UPath Univ MRR = UPath_MRR
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 (map (\(k, _v) -> Path_Look k idPath) (toList _xyz))
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Report))) :: UPeek Univ
                                                                                                                                                                                                                                    Report) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) (map (\(k,
                                                                                                                                                                                                                                                                                                                      _v) -> Path_Look k) (toList _xyz)) ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Report]))) (map (\(k,
                                                                                                                                                                                                                                                             _v) -> Path_Look k) (toList _xyz)) ++ [])
instance PathStart Univ ReportMap
    where data UPeek Univ ReportMap = UPeek_ReportMap (UPath Univ ReportMap) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_ReportMap
          upeekPath (UPeek_ReportMap p _) = p
          upeekValue (UPeek_ReportMap _ x) = x
          type UPath Univ ReportMap = UPath_ReportMap
          upaths _ _f r0 (_xconc@(ReportMap {})) = foldr _f r0 [UPath_ReportMap_unReportMap idPath]
          upeekRow _unv (_xconc@(ReportMap {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: MRR))) :: UPeek Univ
                                                                                                                                                                                                                                           MRR) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_ReportMap_unReportMap] ++ [])
          upeekTree _unv (_xconc@(ReportMap {})) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [MRR]))) [UPath_ReportMap_unReportMap] ++ [])
instance PathStart Univ CIString
    where data UPeek Univ CIString = UPeek_CIString (UPath Univ CIString) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_CIString
          upeekPath (UPeek_CIString p _) = p
          upeekValue (UPeek_CIString _ x) = x
          type UPath Univ CIString = UPath_CIString
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_CIString_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: Text))) :: UPeek Univ
                                                                                                                                                                                                                                  Text) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_CIString_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [Text]))) [UPath_CIString_View] ++ [])
instance PathStart Univ URI
    where data UPeek Univ URI = UPeek_URI (UPath Univ URI) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_URI
          upeekPath (UPeek_URI p _) = p
          upeekValue (UPeek_URI _ x) = x
          type UPath Univ URI = UPath_URI
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ Text
    where data UPeek Univ Text = UPeek_Text (UPath Univ Text) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_Text
          upeekPath (UPeek_Text p _) = p
          upeekValue (UPeek_Text _ x) = x
          type UPath Univ Text = UPath_Text
          upaths _ _f r0 (_xconc@_xyz) = foldr _f r0 [UPath_Text_View idPath]
          upeekRow _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map ((\_ x' -> Node (upeekCons idPath (Just (u (x' :: JSONText))) :: UPeek Univ
                                                                                                                                                                                                                                      JSONText) []) _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc)))) [UPath_Text_View] ++ [])
          upeekTree _unv (_xconc@_xyz) = Node (upeekCons idPath Nothing) (concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk)) (map (upeekTree _unv) (mapMaybe unU' (toListOf (toLens (f idPath)) _xconc) :: [JSONText]))) [UPath_Text_View] ++ [])
instance PathStart Univ UserId
    where data UPeek Univ UserId = UPeek_UserId (UPath Univ UserId) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_UserId
          upeekPath (UPeek_UserId p _) = p
          upeekValue (UPeek_UserId _ x) = x
          type UPath Univ UserId = UPath_UserId
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance PathStart Univ UUID
    where data UPeek Univ UUID = UPeek_UUID (UPath Univ UUID) (Maybe Univ) deriving (Eq, Show)
          upeekCons = UPeek_UUID
          upeekPath (UPeek_UUID p _) = p
          upeekValue (UPeek_UUID _ x) = x
          type UPath Univ UUID = UPath_UUID
          upaths _ _ r _ = r
          upeekRow _ _ = Node (upeekCons idPath Nothing) []
          upeekTree _ x = Node (upeekCons idPath (Just (u x))) []
instance U Univ String
    where u = U1
          unU' (U1 a) = Just a
          unU' _ = Nothing
instance U Univ Int64
    where u = U2
          unU' (U2 a) = Just a
          unU' _ = Nothing
instance U Univ Bool
    where u = U4
          unU' (U4 a) = Just a
          unU' _ = Nothing
instance U Univ Double
    where u = U5
          unU' (U5 a) = Just a
          unU' _ = Nothing
instance U Univ Int
    where u = U3
          unU' (U3 a) = Just a
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
          lens_ReportImage_picCaption f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 y1 x4 x5 x6 x7 x8 x9) (f x3)
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 y1 x3 x4 x5 x6 x7 x8 x9) (f x2)
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picEditedDeprecated f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 y1 x6 x7 x8 x9) (f x5)
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 x7 x8 y1) (f x9)
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 x7 y1 x9) (f x8)
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 x3 y1 x5 x6 x7 x8 x9) (f x4)
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 y1 x8 x9) (f x7)
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic y1 x2 x3 x4 x5 x6 x7 x8 x9) (f x1)
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated f (Pic x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 y1 x7 x8 x9) (f x6)
          {-# INLINE lens_ReportImage_picThumbDeprecated #-}
instance HasReportImageView ReportImageView
    where lens_reportImageView = id
          lens_ReportImageView__picCaption f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 y1 x4 x5 x6 x7 x8 x9) (f x3)
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 y1 x3 x4 x5 x6 x7 x8 x9) (f x2)
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picEditedDeprecated f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 y1 x6 x7 x8 x9) (f x5)
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 y1) (f x9)
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 x7 y1 x9) (f x8)
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 x3 y1 x5 x6 x7 x8 x9) (f x4)
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 y1 x8 x9) (f x7)
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView y1 x2 x3 x4 x5 x6 x7 x8 x9) (f x1)
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated f (ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 y1 x7 x8 x9) (f x6)
          {-# INLINE lens_ReportImageView__picThumbDeprecated #-}
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
instance Describe (Path_Either UPath_URI UPath_ImageFile)
    where describe' _f (_p@(Path_Left _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                   wfld = Nothing;
                                                   next = describe' wfld _wp;
                                                   top = describe' _f (Proxy :: Proxy EUI)}
                                               in maybe top Just next
          describe' _f (_p@(Path_Right _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy EUI)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy EUI)
          describe' _ p = error ("Unexpected " ++ ("EUI" ++ (" path: " ++ show p)))
instance Describe (Path_Map ItemFieldName UPath_Markup)
    where describe' _f (_p@(Path_Look _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                      wfld = Nothing;
                                                      next = describe' wfld _wp;
                                                      top = describe' _f (Proxy :: Proxy MIM)}
                                                  in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MIM)
          describe' _ p = error ("Unexpected " ++ ("MIM" ++ (" path: " ++ show p)))
instance Describe (Path_Map ReportID UPath_Report)
    where describe' _f (_p@(Path_Look _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                      wfld = Nothing;
                                                      next = describe' wfld _wp;
                                                      top = describe' _f (Proxy :: Proxy MRR)}
                                                  in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MRR)
          describe' _ p = error ("Unexpected " ++ ("MRR" ++ (" path: " ++ show p)))
instance Describe (Path_Pair UPath_CIString UPath_Markup)
    where describe' _f (_p@(Path_First _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy AbbrevPair)}
                                                in maybe top Just next
          describe' _f (_p@(Path_Second _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                     wfld = Nothing;
                                                     next = describe' wfld _wp;
                                                     top = describe' _f (Proxy :: Proxy AbbrevPair)}
                                                 in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy AbbrevPair)
          describe' _ p = error ("Unexpected " ++ ("AbbrevPair" ++ (" path: " ++ show p)))
instance Describe (Path_Pair UPath_Markup UPath_Markup)
    where describe' _f (_p@(Path_First _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy MarkupPair)}
                                                in maybe top Just next
          describe' _f (_p@(Path_Second _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                     wfld = Nothing;
                                                     next = describe' wfld _wp;
                                                     top = describe' _f (Proxy :: Proxy MarkupPair)}
                                                 in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MarkupPair)
          describe' _ p = error ("Unexpected " ++ ("MarkupPair" ++ (" path: " ++ show p)))
instance Describe (Path_OMap AbbrevPairID (Path_Pair UPath_CIString UPath_Markup))
    where describe' _f (_p@(Path_At _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy AbbrevPairs)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy AbbrevPairs)
          describe' _ p = error ("Unexpected " ++ ("AbbrevPairs" ++ (" path: " ++ show p)))
instance Describe (Path_OMap AuthorID UPath_Author)
    where describe' _f (_p@(Path_At _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy Authors)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Authors)
          describe' _ p = error ("Unexpected " ++ ("Authors" ++ (" path: " ++ show p)))
instance Describe (Path_OMap MarkupID UPath_Markup)
    where describe' _f (_p@(Path_At _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy Markups)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Markups)
          describe' _ p = error ("Unexpected " ++ ("Markups" ++ (" path: " ++ show p)))
instance Describe (Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup))
    where describe' _f (_p@(Path_At _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy MarkupPairs)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MarkupPairs)
          describe' _ p = error ("Unexpected " ++ ("MarkupPairs" ++ (" path: " ++ show p)))
instance Describe (Path_OMap ReportElemID UPath_ReportElem)
    where describe' _f (_p@(Path_At _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy ReportElems)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportElems)
          describe' _ p = error ("Unexpected " ++ ("ReportElems" ++ (" path: " ++ show p)))
instance Describe (Path_OMap ReportImageID UPath_ReportImage)
    where describe' _f (_p@(Path_At _k _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                    wfld = Nothing;
                                                    next = describe' wfld _wp;
                                                    top = describe' _f (Proxy :: Proxy ReportImages)}
                                                in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportImages)
          describe' _ p = error ("Unexpected " ++ ("ReportImages" ++ (" path: " ++ show p)))
instance Describe (Proxy Int64)
    where describe' _f _ = case _f of
                               Nothing -> Just "Int64"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Int)
    where describe' _f _ = case _f of
                               Nothing -> Just "Int"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Dimension)
    where describe' _f _ = case _f of
                               Nothing -> Just "Dimension"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ImageCrop)
    where describe' _f _ = case _f of
                               Nothing -> Just "Image Crop"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ImageSize)
    where describe' _f _ = case _f of
                               Nothing -> Just "Image Size"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Units)
    where describe' _f _ = case _f of
                               Nothing -> Just "Units"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ImageFile)
    where describe' _f _ = case _f of
                               Nothing -> Just "Image File"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Integer)
    where describe' _f _ = case _f of
                               Nothing -> Just "Integer"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Permissions)
    where describe' _f _ = case _f of
                               Nothing -> Just "Permissions"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy UserIds)
    where describe' _f _ = case _f of
                               Nothing -> Just "User Ids"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy AbbrevPair)
    where describe' _f _ = case _f of
                               Nothing -> Just "Abbrev Pair"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy AbbrevPairs)
    where describe' _f _ = case _f of
                               Nothing -> Just "Abbrev Pairs"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Author)
    where describe' _f _ = case _f of
                               Nothing -> Just "Author"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Authors)
    where describe' _f _ = case _f of
                               Nothing -> Just "Authors"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Branding)
    where describe' _f _ = case _f of
                               Nothing -> Just "Branding"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MarkupPair)
    where describe' _f _ = case _f of
                               Nothing -> Just "Markup Pair"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MarkupPairs)
    where describe' _f _ = case _f of
                               Nothing -> Just "Markup Pairs"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Markups)
    where describe' _f _ = case _f of
                               Nothing -> Just "Markups"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MaybeReportIntendedUse)
    where describe' _f _ = case _f of
                               Nothing -> Just "Maybe Report Intended Use"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Report)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportElem)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Elem"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportElems)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Elems"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportFlags)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Flags"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportIntendedUse)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Intended Use"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportStandard)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Standard"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportStatus)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Status"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportValueApproachInfo)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Value Approach Info"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportValueTypeInfo)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Value Type Info"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy EUI)
    where describe' _f _ = case _f of
                               Nothing -> Just "EUI"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MEUI)
    where describe' _f _ = case _f of
                               Nothing -> Just "MEUI"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MaybeImageFile)
    where describe' _f _ = case _f of
                               Nothing -> Just "Maybe Image File"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportImage)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Image"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportImages)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Images"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReadOnlyFilePath)
    where describe' _f _ = case _f of
                               Nothing -> Just "Read Only File Path"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportImageView)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Image View"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportView)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report View"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy SaneSizeImageSize)
    where describe' _f _ = case _f of
                               Nothing -> Just "Sane Size Image Size"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy Item)
    where describe' _f _ = case _f of
                               Nothing -> Just "Item"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MIM)
    where describe' _f _ = case _f of
                               Nothing -> Just "MIM"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy MRR)
    where describe' _f _ = case _f of
                               Nothing -> Just "MRR"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy ReportMap)
    where describe' _f _ = case _f of
                               Nothing -> Just "Report Map"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy CIString)
    where describe' _f _ = case _f of
                               Nothing -> Just "CIString"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy URI)
    where describe' _f _ = case _f of
                               Nothing -> Just "URI"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy UserId)
    where describe' _f _ = case _f of
                               Nothing -> Just "User Id"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Proxy UUID)
    where describe' _f _ = case _f of
                               Nothing -> Just "UUID"
                               Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                               Just (_tname, cname, Left fpos) -> Just (camelWords $ (cname ++ ("[" ++ (show fpos ++ "]"))))
instance Describe (Path_Maybe (Path_Either UPath_URI UPath_ImageFile))
    where describe' _f (_p@(Path_Just _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                   wfld = Nothing;
                                                   next = describe' wfld _wp;
                                                   top = describe' _f (Proxy :: Proxy MEUI)}
                                               in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MEUI)
          describe' _ p = error ("Unexpected " ++ ("MEUI" ++ (" path: " ++ show p)))
instance Describe UPath_Author
    where describe' _f (_p@(UPath_Author_authorName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                 wfld = Just ("Author", "Author", Right "authorName");
                                                                 next = describe' wfld _wp;
                                                                 top = describe' _f (Proxy :: Proxy Author)}
                                                             in maybe top Just next
          describe' _f (_p@(UPath_Author_authorCredentials _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("Author", "Author", Right "authorCredentials");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy Author)}
                                                                    in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Author)
          describe' _ p = error ("Unexpected " ++ ("Author" ++ (" path: " ++ show p)))
instance Describe UPath_Bool
    where describe' _f (_p@(UPath_Bool_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                         wfld = Nothing;
                                                         next = describe' wfld _wp;
                                                         top = describe' _f (Proxy :: Proxy Bool)}
                                                     in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Bool)
          describe' _ p = error ("Unexpected " ++ ("Bool" ++ (" path: " ++ show p)))
instance Describe UPath_Branding
    where describe' _f (_p@(UPath_Branding_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                             wfld = Nothing;
                                                             next = describe' wfld _wp;
                                                             top = describe' _f (Proxy :: Proxy Branding)}
                                                         in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Branding)
          describe' _ p = error ("Unexpected " ++ ("Branding" ++ (" path: " ++ show p)))
instance Describe UPath_CIString
    where describe' _f (_p@(UPath_CIString_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                             wfld = Nothing;
                                                             next = describe' wfld _wp;
                                                             top = describe' _f (Proxy :: Proxy CIString)}
                                                         in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy CIString)
          describe' _ p = error ("Unexpected " ++ ("CIString" ++ (" path: " ++ show p)))
instance Describe UPath_Dimension
    where describe' _f (_p@(UPath_Dimension_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                              wfld = Nothing;
                                                              next = describe' wfld _wp;
                                                              top = describe' _f (Proxy :: Proxy Dimension)}
                                                          in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Dimension)
          describe' _ p = error ("Unexpected " ++ ("Dimension" ++ (" path: " ++ show p)))
instance Describe UPath_Double
    where describe' _f (_p@(UPath_Double_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                           wfld = Nothing;
                                                           next = describe' wfld _wp;
                                                           top = describe' _f (Proxy :: Proxy Double)}
                                                       in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Double)
          describe' _ p = error ("Unexpected " ++ ("Double" ++ (" path: " ++ show p)))
instance Describe UPath_ImageCrop
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy ImageCrop)
          describe' _ p = error ("Unexpected " ++ ("ImageCrop" ++ (" path: " ++ show p)))
instance Describe UPath_ImageFile
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy ImageFile)
          describe' _ p = error ("Unexpected " ++ ("ImageFile" ++ (" path: " ++ show p)))
instance Describe UPath_ImageSize
    where describe' _f (_p@(UPath_ImageSize_dim _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                             wfld = Just ("ImageSize", "ImageSize", Right "dim");
                                                             next = describe' wfld _wp;
                                                             top = describe' _f (Proxy :: Proxy ImageSize)}
                                                         in maybe top Just next
          describe' _f (_p@(UPath_ImageSize_size _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                              wfld = Just ("ImageSize", "ImageSize", Right "size");
                                                              next = describe' wfld _wp;
                                                              top = describe' _f (Proxy :: Proxy ImageSize)}
                                                          in maybe top Just next
          describe' _f (_p@(UPath_ImageSize_units _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                               wfld = Just ("ImageSize", "ImageSize", Right "units");
                                                               next = describe' wfld _wp;
                                                               top = describe' _f (Proxy :: Proxy ImageSize)}
                                                           in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ImageSize)
          describe' _ p = error ("Unexpected " ++ ("ImageSize" ++ (" path: " ++ show p)))
instance Describe UPath_Int
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy Int)
          describe' _ p = error ("Unexpected " ++ ("Int" ++ (" path: " ++ show p)))
instance Describe UPath_Int64
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy Int64)
          describe' _ p = error ("Unexpected " ++ ("Int64" ++ (" path: " ++ show p)))
instance Describe UPath_Integer
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy Integer)
          describe' _ p = error ("Unexpected " ++ ("Integer" ++ (" path: " ++ show p)))
instance Describe UPath_Item
    where describe' _f (_p@(UPath_Item_itemName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                             wfld = Just ("Item", "Item", Right "itemName");
                                                             next = describe' wfld _wp;
                                                             top = describe' _f (Proxy :: Proxy Item)}
                                                         in maybe top Just next
          describe' _f (_p@(UPath_Item_fields _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                           wfld = Just ("Item", "Item", Right "fields");
                                                           next = describe' wfld _wp;
                                                           top = describe' _f (Proxy :: Proxy Item)}
                                                       in maybe top Just next
          describe' _f (_p@(UPath_Item_images _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                           wfld = Just ("Item", "Item", Right "images");
                                                           next = describe' wfld _wp;
                                                           top = describe' _f (Proxy :: Proxy Item)}
                                                       in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Item)
          describe' _ p = error ("Unexpected " ++ ("Item" ++ (" path: " ++ show p)))
instance Describe UPath_JSONText
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy JSONText)
          describe' _ p = error ("Unexpected " ++ ("JSONText" ++ (" path: " ++ show p)))
instance Describe UPath_Markup
    where describe' _f (_p@(UPath_Markup_markdownText _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                   wfld = Just ("Markup", "Markdown", Right "markdownText");
                                                                   next = describe' wfld _wp;
                                                                   top = describe' _f (Proxy :: Proxy Markup)}
                                                               in maybe top Just next
          describe' _f (_p@(UPath_Markup_htmlText _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                               wfld = Just ("Markup", "Html", Right "htmlText");
                                                               next = describe' wfld _wp;
                                                               top = describe' _f (Proxy :: Proxy Markup)}
                                                           in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Markup)
          describe' _ p = error ("Unexpected " ++ ("Markup" ++ (" path: " ++ show p)))
instance Describe UPath_MaybeImageFile
    where describe' _f (_p@(UPath_MaybeImageFile_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                   wfld = Nothing;
                                                                   next = describe' wfld _wp;
                                                                   top = describe' _f (Proxy :: Proxy MaybeImageFile)}
                                                               in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MaybeImageFile)
          describe' _ p = error ("Unexpected " ++ ("MaybeImageFile" ++ (" path: " ++ show p)))
instance Describe UPath_MaybeReportIntendedUse
    where describe' _f (_p@(UPath_MaybeReportIntendedUse_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                           wfld = Nothing;
                                                                           next = describe' wfld _wp;
                                                                           top = describe' _f (Proxy :: Proxy MaybeReportIntendedUse)}
                                                                       in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy MaybeReportIntendedUse)
          describe' _ p = error ("Unexpected " ++ ("MaybeReportIntendedUse" ++ (" path: " ++ show p)))
instance Describe UPath_Permissions
    where describe' _f (_p@(UPath_Permissions_owner _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                 wfld = Just ("Permissions", "Permissions", Right "owner");
                                                                 next = describe' wfld _wp;
                                                                 top = describe' _f (Proxy :: Proxy Permissions)}
                                                             in maybe top Just next
          describe' _f (_p@(UPath_Permissions_writers _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                   wfld = Just ("Permissions", "Permissions", Right "writers");
                                                                   next = describe' wfld _wp;
                                                                   top = describe' _f (Proxy :: Proxy Permissions)}
                                                               in maybe top Just next
          describe' _f (_p@(UPath_Permissions_readers _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                   wfld = Just ("Permissions", "Permissions", Right "readers");
                                                                   next = describe' wfld _wp;
                                                                   top = describe' _f (Proxy :: Proxy Permissions)}
                                                               in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Permissions)
          describe' _ p = error ("Unexpected " ++ ("Permissions" ++ (" path: " ++ show p)))
instance Describe UPath_ReadOnlyFilePath
    where describe' _f (_p@(UPath_ReadOnlyFilePath_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                     wfld = Nothing;
                                                                     next = describe' wfld _wp;
                                                                     top = describe' _f (Proxy :: Proxy ReadOnlyFilePath)}
                                                                 in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReadOnlyFilePath)
          describe' _ p = error ("Unexpected " ++ ("ReadOnlyFilePath" ++ (" path: " ++ show p)))
instance Describe UPath_Report
    where describe' _f (_p@(UPath_Report_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                           wfld = Nothing;
                                                           next = describe' wfld _wp;
                                                           top = describe' _f (Proxy :: Proxy Report)}
                                                       in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Report)
          describe' _ p = error ("Unexpected " ++ ("Report" ++ (" path: " ++ show p)))
instance Describe UPath_ReportElem
    where describe' _f (_p@(UPath_ReportElem_elemItem _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                   wfld = Just ("ReportElem", "ReportItem", Right "elemItem");
                                                                   next = describe' wfld _wp;
                                                                   top = describe' _f (Proxy :: Proxy ReportElem)}
                                                               in maybe top Just next
          describe' _f (_p@(UPath_ReportElem_elemText _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                   wfld = Just ("ReportElem", "ReportParagraph", Right "elemText");
                                                                   next = describe' wfld _wp;
                                                                   top = describe' _f (Proxy :: Proxy ReportElem)}
                                                               in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportElem)
          describe' _ p = error ("Unexpected " ++ ("ReportElem" ++ (" path: " ++ show p)))
instance Describe UPath_ReportFlags
    where describe' _f (_p@(UPath_ReportFlags_hideEmptyItemFields _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportFlags", "ReportFlags", Right "hideEmptyItemFields");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportFlags)}
                                                                           in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportFlags)
          describe' _ p = error ("Unexpected " ++ ("ReportFlags" ++ (" path: " ++ show p)))
instance Describe UPath_ReportImage
    where describe' _f (_p@(UPath_ReportImage_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                wfld = Nothing;
                                                                next = describe' wfld _wp;
                                                                top = describe' _f (Proxy :: Proxy ReportImage)}
                                                            in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportImage)
          describe' _ p = error ("Unexpected " ++ ("ReportImage" ++ (" path: " ++ show p)))
instance Describe UPath_ReportImageView
    where describe' _f (_p@(UPath_ReportImageView__picSize _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("ReportImageView", "ReportImageView", Right "_picSize");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                    in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picCrop _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("ReportImageView", "ReportImageView", Right "_picCrop");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                    in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picCaption _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                           wfld = Just ("ReportImageView", "ReportImageView", Right "_picCaption");
                                                                           next = describe' wfld _wp;
                                                                           top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                       in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picOriginal _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                            wfld = Just ("ReportImageView", "ReportImageView", Right "_picOriginal");
                                                                            next = describe' wfld _wp;
                                                                            top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                        in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picEditedDeprecated _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                    wfld = Just ("ReportImageView", "ReportImageView", Right "_picEditedDeprecated");
                                                                                    next = describe' wfld _wp;
                                                                                    top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                                in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picThumbDeprecated _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                   wfld = Just ("ReportImageView", "ReportImageView", Right "_picThumbDeprecated");
                                                                                   next = describe' wfld _wp;
                                                                                   top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                               in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picPrinterDeprecated _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                     wfld = Just ("ReportImageView", "ReportImageView", Right "_picPrinterDeprecated");
                                                                                     next = describe' wfld _wp;
                                                                                     top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                                 in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picMustEnlarge _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportImageView", "ReportImageView", Right "_picMustEnlarge");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportImageView__picEnlargedDeprecated _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                      wfld = Just ("ReportImageView", "ReportImageView", Right "_picEnlargedDeprecated");
                                                                                      next = describe' wfld _wp;
                                                                                      top = describe' _f (Proxy :: Proxy ReportImageView)}
                                                                                  in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportImageView)
          describe' _ p = error ("Unexpected " ++ ("ReportImageView" ++ (" path: " ++ show p)))
instance Describe UPath_ReportIntendedUse
    where describe' _f (_p@(UPath_ReportIntendedUse_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                      wfld = Nothing;
                                                                      next = describe' wfld _wp;
                                                                      top = describe' _f (Proxy :: Proxy ReportIntendedUse)}
                                                                  in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportIntendedUse)
          describe' _ p = error ("Unexpected " ++ ("ReportIntendedUse" ++ (" path: " ++ show p)))
instance Describe UPath_ReportMap
    where describe' _f (_p@(UPath_ReportMap_unReportMap _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                     wfld = Just ("ReportMap", "ReportMap", Right "unReportMap");
                                                                     next = describe' wfld _wp;
                                                                     top = describe' _f (Proxy :: Proxy ReportMap)}
                                                                 in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportMap)
          describe' _ p = error ("Unexpected " ++ ("ReportMap" ++ (" path: " ++ show p)))
instance Describe UPath_ReportStandard
    where describe' _f (_p@(UPath_ReportStandard_unReportStandard _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportStandard", "ReportStandard", Right "unReportStandard");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportStandard)}
                                                                           in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportStandard)
          describe' _ p = error ("Unexpected " ++ ("ReportStandard" ++ (" path: " ++ show p)))
instance Describe UPath_ReportStatus
    where describe' _f (_p@(UPath_ReportStatus_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                 wfld = Nothing;
                                                                 next = describe' wfld _wp;
                                                                 top = describe' _f (Proxy :: Proxy ReportStatus)}
                                                             in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportStatus)
          describe' _ p = error ("Unexpected " ++ ("ReportStatus" ++ (" path: " ++ show p)))
instance Describe UPath_ReportValueApproachInfo
    where describe' _f (_p@(UPath_ReportValueApproachInfo_reportValueApproachName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                               wfld = Just ("ReportValueApproachInfo", "ReportValueApproachInfo", Right "reportValueApproachName");
                                                                                               next = describe' wfld _wp;
                                                                                               top = describe' _f (Proxy :: Proxy ReportValueApproachInfo)}
                                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportValueApproachInfo_reportValueApproachDescription _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                                      wfld = Just ("ReportValueApproachInfo", "ReportValueApproachInfo", Right "reportValueApproachDescription");
                                                                                                      next = describe' wfld _wp;
                                                                                                      top = describe' _f (Proxy :: Proxy ReportValueApproachInfo)}
                                                                                                  in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportValueApproachInfo)
          describe' _ p = error ("Unexpected " ++ ("ReportValueApproachInfo" ++ (" path: " ++ show p)))
instance Describe UPath_ReportValueTypeInfo
    where describe' _f (_p@(UPath_ReportValueTypeInfo_reportValueTypeName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                       wfld = Just ("ReportValueTypeInfo", "ReportValueTypeInfo", Right "reportValueTypeName");
                                                                                       next = describe' wfld _wp;
                                                                                       top = describe' _f (Proxy :: Proxy ReportValueTypeInfo)}
                                                                                   in maybe top Just next
          describe' _f (_p@(UPath_ReportValueTypeInfo_reportValueTypeDescription _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                              wfld = Just ("ReportValueTypeInfo", "ReportValueTypeInfo", Right "reportValueTypeDescription");
                                                                                              next = describe' wfld _wp;
                                                                                              top = describe' _f (Proxy :: Proxy ReportValueTypeInfo)}
                                                                                          in maybe top Just next
          describe' _f (_p@(UPath_ReportValueTypeInfo_reportValueTypeDefinition _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                             wfld = Just ("ReportValueTypeInfo", "ReportValueTypeInfo", Right "reportValueTypeDefinition");
                                                                                             next = describe' wfld _wp;
                                                                                             top = describe' _f (Proxy :: Proxy ReportValueTypeInfo)}
                                                                                         in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportValueTypeInfo)
          describe' _ p = error ("Unexpected " ++ ("ReportValueTypeInfo" ++ (" path: " ++ show p)))
instance Describe UPath_ReportView
    where describe' _f (_p@(UPath_ReportView__reportFolder _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("ReportView", "ReportView", Right "_reportFolder");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy ReportView)}
                                                                    in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                      wfld = Just ("ReportView", "ReportView", Right "_reportName");
                                                                      next = describe' wfld _wp;
                                                                      top = describe' _f (Proxy :: Proxy ReportView)}
                                                                  in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportDate _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                      wfld = Just ("ReportView", "ReportView", Right "_reportDate");
                                                                      next = describe' wfld _wp;
                                                                      top = describe' _f (Proxy :: Proxy ReportView)}
                                                                  in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportContractDate _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                              wfld = Just ("ReportView", "ReportView", Right "_reportContractDate");
                                                                              next = describe' wfld _wp;
                                                                              top = describe' _f (Proxy :: Proxy ReportView)}
                                                                          in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportInspectionDate _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                wfld = Just ("ReportView", "ReportView", Right "_reportInspectionDate");
                                                                                next = describe' wfld _wp;
                                                                                top = describe' _f (Proxy :: Proxy ReportView)}
                                                                            in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportEffectiveDate _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportView", "ReportView", Right "_reportEffectiveDate");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportAuthors _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                         wfld = Just ("ReportView", "ReportView", Right "_reportAuthors");
                                                                         next = describe' wfld _wp;
                                                                         top = describe' _f (Proxy :: Proxy ReportView)}
                                                                     in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPreparer _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                          wfld = Just ("ReportView", "ReportView", Right "_reportPreparer");
                                                                          next = describe' wfld _wp;
                                                                          top = describe' _f (Proxy :: Proxy ReportView)}
                                                                      in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPreparerEIN _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                             wfld = Just ("ReportView", "ReportView", Right "_reportPreparerEIN");
                                                                             next = describe' wfld _wp;
                                                                             top = describe' _f (Proxy :: Proxy ReportView)}
                                                                         in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPreparerAddress _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                 wfld = Just ("ReportView", "ReportView", Right "_reportPreparerAddress");
                                                                                 next = describe' wfld _wp;
                                                                                 top = describe' _f (Proxy :: Proxy ReportView)}
                                                                             in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPreparerEMail _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportView", "ReportView", Right "_reportPreparerEMail");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPreparerWebsite _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                 wfld = Just ("ReportView", "ReportView", Right "_reportPreparerWebsite");
                                                                                 next = describe' wfld _wp;
                                                                                 top = describe' _f (Proxy :: Proxy ReportView)}
                                                                             in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportAbbrevs _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                         wfld = Just ("ReportView", "ReportView", Right "_reportAbbrevs");
                                                                         next = describe' wfld _wp;
                                                                         top = describe' _f (Proxy :: Proxy ReportView)}
                                                                     in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportTitle _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                       wfld = Just ("ReportView", "ReportView", Right "_reportTitle");
                                                                       next = describe' wfld _wp;
                                                                       top = describe' _f (Proxy :: Proxy ReportView)}
                                                                   in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportHeader _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("ReportView", "ReportView", Right "_reportHeader");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy ReportView)}
                                                                    in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportFooter _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("ReportView", "ReportView", Right "_reportFooter");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy ReportView)}
                                                                    in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportIntendedUse _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                             wfld = Just ("ReportView", "ReportView", Right "_reportIntendedUse");
                                                                             next = describe' wfld _wp;
                                                                             top = describe' _f (Proxy :: Proxy ReportView)}
                                                                         in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportValueTypeInfo _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportView", "ReportView", Right "_reportValueTypeInfo");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportValueApproachInfo _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                   wfld = Just ("ReportView", "ReportView", Right "_reportValueApproachInfo");
                                                                                   next = describe' wfld _wp;
                                                                                   top = describe' _f (Proxy :: Proxy ReportView)}
                                                                               in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportClientName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                            wfld = Just ("ReportView", "ReportView", Right "_reportClientName");
                                                                            next = describe' wfld _wp;
                                                                            top = describe' _f (Proxy :: Proxy ReportView)}
                                                                        in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportClientAddress _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportView", "ReportView", Right "_reportClientAddress");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportClientGreeting _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                wfld = Just ("ReportView", "ReportView", Right "_reportClientGreeting");
                                                                                next = describe' wfld _wp;
                                                                                top = describe' _f (Proxy :: Proxy ReportView)}
                                                                            in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportItemsOwnerFull _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                wfld = Just ("ReportView", "ReportView", Right "_reportItemsOwnerFull");
                                                                                next = describe' wfld _wp;
                                                                                top = describe' _f (Proxy :: Proxy ReportView)}
                                                                            in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportItemsOwner _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                            wfld = Just ("ReportView", "ReportView", Right "_reportItemsOwner");
                                                                            next = describe' wfld _wp;
                                                                            top = describe' _f (Proxy :: Proxy ReportView)}
                                                                        in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportBriefItems _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                            wfld = Just ("ReportView", "ReportView", Right "_reportBriefItems");
                                                                            next = describe' wfld _wp;
                                                                            top = describe' _f (Proxy :: Proxy ReportView)}
                                                                        in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportInspectionLocation _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                    wfld = Just ("ReportView", "ReportView", Right "_reportInspectionLocation");
                                                                                    next = describe' wfld _wp;
                                                                                    top = describe' _f (Proxy :: Proxy ReportView)}
                                                                                in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportBody _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                      wfld = Just ("ReportView", "ReportView", Right "_reportBody");
                                                                      next = describe' wfld _wp;
                                                                      top = describe' _f (Proxy :: Proxy ReportView)}
                                                                  in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportGlossary _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                          wfld = Just ("ReportView", "ReportView", Right "_reportGlossary");
                                                                          next = describe' wfld _wp;
                                                                          top = describe' _f (Proxy :: Proxy ReportView)}
                                                                      in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportSources _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                         wfld = Just ("ReportView", "ReportView", Right "_reportSources");
                                                                         next = describe' wfld _wp;
                                                                         top = describe' _f (Proxy :: Proxy ReportView)}
                                                                     in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportLetterOfTransmittal _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                     wfld = Just ("ReportView", "ReportView", Right "_reportLetterOfTransmittal");
                                                                                     next = describe' wfld _wp;
                                                                                     top = describe' _f (Proxy :: Proxy ReportView)}
                                                                                 in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportScopeOfWork _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                             wfld = Just ("ReportView", "ReportView", Right "_reportScopeOfWork");
                                                                             next = describe' wfld _wp;
                                                                             top = describe' _f (Proxy :: Proxy ReportView)}
                                                                         in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportCertification _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportView", "ReportView", Right "_reportCertification");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportLimitingConditions _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                    wfld = Just ("ReportView", "ReportView", Right "_reportLimitingConditions");
                                                                                    next = describe' wfld _wp;
                                                                                    top = describe' _f (Proxy :: Proxy ReportView)}
                                                                                in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPrivacyPolicy _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                               wfld = Just ("ReportView", "ReportView", Right "_reportPrivacyPolicy");
                                                                               next = describe' wfld _wp;
                                                                               top = describe' _f (Proxy :: Proxy ReportView)}
                                                                           in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportPerms _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                       wfld = Just ("ReportView", "ReportView", Right "_reportPerms");
                                                                       next = describe' wfld _wp;
                                                                       top = describe' _f (Proxy :: Proxy ReportView)}
                                                                   in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportRevision _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                          wfld = Just ("ReportView", "ReportView", Right "_reportRevision");
                                                                          next = describe' wfld _wp;
                                                                          top = describe' _f (Proxy :: Proxy ReportView)}
                                                                      in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportCreated _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                         wfld = Just ("ReportView", "ReportView", Right "_reportCreated");
                                                                         next = describe' wfld _wp;
                                                                         top = describe' _f (Proxy :: Proxy ReportView)}
                                                                     in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportBranding _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                          wfld = Just ("ReportView", "ReportView", Right "_reportBranding");
                                                                          next = describe' wfld _wp;
                                                                          top = describe' _f (Proxy :: Proxy ReportView)}
                                                                      in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportStatus _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                        wfld = Just ("ReportView", "ReportView", Right "_reportStatus");
                                                                        next = describe' wfld _wp;
                                                                        top = describe' _f (Proxy :: Proxy ReportView)}
                                                                    in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportRedacted _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                          wfld = Just ("ReportView", "ReportView", Right "_reportRedacted");
                                                                          next = describe' wfld _wp;
                                                                          top = describe' _f (Proxy :: Proxy ReportView)}
                                                                      in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportFlags _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                       wfld = Just ("ReportView", "ReportView", Right "_reportFlags");
                                                                       next = describe' wfld _wp;
                                                                       top = describe' _f (Proxy :: Proxy ReportView)}
                                                                   in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportUUID _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                      wfld = Just ("ReportView", "ReportView", Right "_reportUUID");
                                                                      next = describe' wfld _wp;
                                                                      top = describe' _f (Proxy :: Proxy ReportView)}
                                                                  in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportOrderByItemName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                 wfld = Just ("ReportView", "ReportView", Right "_reportOrderByItemName");
                                                                                 next = describe' wfld _wp;
                                                                                 top = describe' _f (Proxy :: Proxy ReportView)}
                                                                             in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportDisplayItemName _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                 wfld = Just ("ReportView", "ReportView", Right "_reportDisplayItemName");
                                                                                 next = describe' wfld _wp;
                                                                                 top = describe' _f (Proxy :: Proxy ReportView)}
                                                                             in maybe top Just next
          describe' _f (_p@(UPath_ReportView__reportStandardsVersion _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                                  wfld = Just ("ReportView", "ReportView", Right "_reportStandardsVersion");
                                                                                  next = describe' wfld _wp;
                                                                                  top = describe' _f (Proxy :: Proxy ReportView)}
                                                                              in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy ReportView)
          describe' _ p = error ("Unexpected " ++ ("ReportView" ++ (" path: " ++ show p)))
instance Describe UPath_SaneSizeImageSize
    where describe' _f (_p@(UPath_SaneSizeImageSize_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                                      wfld = Nothing;
                                                                      next = describe' wfld _wp;
                                                                      top = describe' _f (Proxy :: Proxy SaneSizeImageSize)}
                                                                  in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy SaneSizeImageSize)
          describe' _ p = error ("Unexpected " ++ ("SaneSizeImageSize" ++ (" path: " ++ show p)))
instance Describe UPath_String
    where describe' _f (_p@(UPath_String_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                           wfld = Nothing;
                                                           next = describe' wfld _wp;
                                                           top = describe' _f (Proxy :: Proxy String)}
                                                       in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy String)
          describe' _ p = error ("Unexpected " ++ ("String" ++ (" path: " ++ show p)))
instance Describe UPath_Text
    where describe' _f (_p@(UPath_Text_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                         wfld = Nothing;
                                                         next = describe' wfld _wp;
                                                         top = describe' _f (Proxy :: Proxy Text)}
                                                     in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Text)
          describe' _ p = error ("Unexpected " ++ ("Text" ++ (" path: " ++ show p)))
instance Describe UPath_URI
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy URI)
          describe' _ p = error ("Unexpected " ++ ("URI" ++ (" path: " ++ show p)))
instance Describe UPath_UUID
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy UUID)
          describe' _ p = error ("Unexpected " ++ ("UUID" ++ (" path: " ++ show p)))
instance Describe UPath_Units
    where describe' _f (_p@(UPath_Units_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                          wfld = Nothing;
                                                          next = describe' wfld _wp;
                                                          top = describe' _f (Proxy :: Proxy Units)}
                                                      in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy Units)
          describe' _ p = error ("Unexpected " ++ ("Units" ++ (" path: " ++ show p)))
instance Describe UPath_UserId
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy UserId)
          describe' _ p = error ("Unexpected " ++ ("UserId" ++ (" path: " ++ show p)))
instance Describe UPath_UserIds
    where describe' _f (_p@(UPath_UserIds_View _wp)) = let {wfld :: Maybe ((String, String, Either Int String));
                                                            wfld = Nothing;
                                                            next = describe' wfld _wp;
                                                            top = describe' _f (Proxy :: Proxy UserIds)}
                                                        in maybe top Just next
          describe' f p | p == idPath = describe' f (Proxy :: Proxy UserIds)
          describe' _ p = error ("Unexpected " ++ ("UserIds" ++ (" path: " ++ show p)))
instance IdPath UPath_Author
    where idPath = UPath_Author
instance IdPath UPath_Bool
    where idPath = UPath_Bool
instance IdPath UPath_Branding
    where idPath = UPath_Branding
instance IdPath UPath_CIString
    where idPath = UPath_CIString
instance IdPath UPath_Dimension
    where idPath = UPath_Dimension
instance IdPath UPath_Double
    where idPath = UPath_Double
instance IdPath UPath_ImageCrop
    where idPath = UPath_ImageCrop
instance IdPath UPath_ImageFile
    where idPath = UPath_ImageFile
instance IdPath UPath_ImageSize
    where idPath = UPath_ImageSize
instance IdPath UPath_Int
    where idPath = UPath_Int
instance IdPath UPath_Int64
    where idPath = UPath_Int64
instance IdPath UPath_Integer
    where idPath = UPath_Integer
instance IdPath UPath_Item
    where idPath = UPath_Item
instance IdPath UPath_JSONText
    where idPath = UPath_JSONText
instance IdPath UPath_Markup
    where idPath = UPath_Markup
instance IdPath UPath_MaybeImageFile
    where idPath = UPath_MaybeImageFile
instance IdPath UPath_MaybeReportIntendedUse
    where idPath = UPath_MaybeReportIntendedUse
instance IdPath UPath_Permissions
    where idPath = UPath_Permissions
instance IdPath UPath_ReadOnlyFilePath
    where idPath = UPath_ReadOnlyFilePath
instance IdPath UPath_Report
    where idPath = UPath_Report
instance IdPath UPath_ReportElem
    where idPath = UPath_ReportElem
instance IdPath UPath_ReportFlags
    where idPath = UPath_ReportFlags
instance IdPath UPath_ReportImage
    where idPath = UPath_ReportImage
instance IdPath UPath_ReportImageView
    where idPath = UPath_ReportImageView
instance IdPath UPath_ReportIntendedUse
    where idPath = UPath_ReportIntendedUse
instance IdPath UPath_ReportMap
    where idPath = UPath_ReportMap
instance IdPath UPath_ReportStandard
    where idPath = UPath_ReportStandard
instance IdPath UPath_ReportStatus
    where idPath = UPath_ReportStatus
instance IdPath UPath_ReportValueApproachInfo
    where idPath = UPath_ReportValueApproachInfo
instance IdPath UPath_ReportValueTypeInfo
    where idPath = UPath_ReportValueTypeInfo
instance IdPath UPath_ReportView
    where idPath = UPath_ReportView
instance IdPath UPath_SaneSizeImageSize
    where idPath = UPath_SaneSizeImageSize
instance IdPath UPath_String
    where idPath = UPath_String
instance IdPath UPath_Text
    where idPath = UPath_Text
instance IdPath UPath_URI
    where idPath = UPath_URI
instance IdPath UPath_UUID
    where idPath = UPath_UUID
instance IdPath UPath_Units
    where idPath = UPath_Units
instance IdPath UPath_UserId
    where idPath = UPath_UserId
instance IdPath UPath_UserIds
    where idPath = UPath_UserIds
instance ToLens (Path_Either UPath_URI UPath_ImageFile)
    where type S (Path_Either UPath_URI UPath_ImageFile) = EUI
          type A (Path_Either UPath_URI UPath_ImageFile) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_Left v) = _Left . toLens v
          toLens (Path_Right v) = _Right . toLens v
instance ToLens (Path_Map ItemFieldName UPath_Markup)
    where type S (Path_Map ItemFieldName UPath_Markup) = MIM
          type A (Path_Map ItemFieldName UPath_Markup) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID UPath_Report)
    where type S (Path_Map ReportID UPath_Report) = MRR
          type A (Path_Map ReportID UPath_Report) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Pair UPath_CIString UPath_Markup)
    where type S (Path_Pair UPath_CIString UPath_Markup) = AbbrevPair
          type A (Path_Pair UPath_CIString UPath_Markup) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
instance ToLens (Path_Pair UPath_Markup UPath_Markup)
    where type S (Path_Pair UPath_Markup UPath_Markup) = MarkupPair
          type A (Path_Pair UPath_Markup UPath_Markup) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
instance ToLens (Path_OMap AbbrevPairID (Path_Pair UPath_CIString UPath_Markup))
    where type S (Path_OMap AbbrevPairID (Path_Pair UPath_CIString UPath_Markup)) = AbbrevPairs
          type A (Path_OMap AbbrevPairID (Path_Pair UPath_CIString UPath_Markup)) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AuthorID UPath_Author)
    where type S (Path_OMap AuthorID UPath_Author) = Authors
          type A (Path_OMap AuthorID UPath_Author) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupID UPath_Markup)
    where type S (Path_OMap MarkupID UPath_Markup) = Markups
          type A (Path_OMap MarkupID UPath_Markup) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup))
    where type S (Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup)) = MarkupPairs
          type A (Path_OMap MarkupPairID (Path_Pair UPath_Markup UPath_Markup)) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID UPath_ReportElem)
    where type S (Path_OMap ReportElemID UPath_ReportElem) = ReportElems
          type A (Path_OMap ReportElemID UPath_ReportElem) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID UPath_ReportImage)
    where type S (Path_OMap ReportImageID UPath_ReportImage) = ReportImages
          type A (Path_OMap ReportImageID UPath_ReportImage) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_Maybe (Path_Either UPath_URI UPath_ImageFile))
    where type S (Path_Maybe (Path_Either UPath_URI UPath_ImageFile)) = MEUI
          type A (Path_Maybe (Path_Either UPath_URI UPath_ImageFile)) = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (Path_Just v) = _Just . toLens v
instance ToLens UPath_Author
    where type S UPath_Author = Author
          type A UPath_Author = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Author_authorName _p) = lens_Author_authorName . toLens _p
          toLens (UPath_Author_authorCredentials _p) = lens_Author_authorCredentials . toLens _p
instance ToLens UPath_Bool
    where type S UPath_Bool = Bool
          type A UPath_Bool = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Bool_View v) = (viewLens :: Lens' Bool String) . toLens v
instance ToLens UPath_Branding
    where type S UPath_Branding = Branding
          type A UPath_Branding = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Branding_View v) = (viewLens :: Lens' Branding Text) . toLens v
instance ToLens UPath_CIString
    where type S UPath_CIString = CIString
          type A UPath_CIString = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_CIString_View v) = (viewLens :: Lens' CIString Text) . toLens v
instance ToLens UPath_Dimension
    where type S UPath_Dimension = Dimension
          type A UPath_Dimension = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Dimension_View v) = (viewLens :: Lens' Dimension JSONText) . toLens v
instance ToLens UPath_Double
    where type S UPath_Double = Double
          type A UPath_Double = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Double_View v) = (viewLens :: Lens' Double String) . toLens v
instance ToLens UPath_ImageCrop
    where type S UPath_ImageCrop = ImageCrop
          type A UPath_ImageCrop = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_ImageFile
    where type S UPath_ImageFile = ImageFile
          type A UPath_ImageFile = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_ImageSize
    where type S UPath_ImageSize = ImageSize
          type A UPath_ImageSize = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ImageSize_dim _p) = lens_ImageSize_dim . toLens _p
          toLens (UPath_ImageSize_size _p) = lens_ImageSize_size . toLens _p
          toLens (UPath_ImageSize_units _p) = lens_ImageSize_units . toLens _p
instance ToLens UPath_Int
    where type S UPath_Int = Int
          type A UPath_Int = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_Int64
    where type S UPath_Int64 = Int64
          type A UPath_Int64 = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_Integer
    where type S UPath_Integer = Integer
          type A UPath_Integer = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_Item
    where type S UPath_Item = Item
          type A UPath_Item = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Item_itemName _p) = lens_Item_itemName . toLens _p
          toLens (UPath_Item_fields _p) = lens_Item_fields . toLens _p
          toLens (UPath_Item_images _p) = lens_Item_images . toLens _p
instance ToLens UPath_JSONText
    where type S UPath_JSONText = JSONText
          type A UPath_JSONText = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_Markup
    where type S UPath_Markup = Markup
          type A UPath_Markup = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Markup_markdownText _p) = lens_Markup_markdownText . toLens _p
          toLens (UPath_Markup_htmlText _p) = lens_Markup_htmlText . toLens _p
instance ToLens UPath_MaybeImageFile
    where type S UPath_MaybeImageFile = MaybeImageFile
          type A UPath_MaybeImageFile = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_MaybeImageFile_View v) = (viewLens :: Lens' MaybeImageFile String) . toLens v
instance ToLens UPath_MaybeReportIntendedUse
    where type S UPath_MaybeReportIntendedUse = MaybeReportIntendedUse
          type A UPath_MaybeReportIntendedUse = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_MaybeReportIntendedUse_View v) = (viewLens :: Lens' MaybeReportIntendedUse String) . toLens v
instance ToLens UPath_Permissions
    where type S UPath_Permissions = Permissions
          type A UPath_Permissions = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Permissions_owner _p) = lens_Permissions_owner . toLens _p
          toLens (UPath_Permissions_writers _p) = lens_Permissions_writers . toLens _p
          toLens (UPath_Permissions_readers _p) = lens_Permissions_readers . toLens _p
instance ToLens UPath_ReadOnlyFilePath
    where type S UPath_ReadOnlyFilePath = ReadOnlyFilePath
          type A UPath_ReadOnlyFilePath = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReadOnlyFilePath_View v) = (viewLens :: Lens' ReadOnlyFilePath String) . toLens v
instance ToLens UPath_Report
    where type S UPath_Report = Report
          type A UPath_Report = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Report_View v) = (viewLens :: Lens' Report ReportView) . toLens v
instance ToLens UPath_ReportElem
    where type S UPath_ReportElem = ReportElem
          type A UPath_ReportElem = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportElem_elemItem _p) = lens_ReportElem_elemItem . toLens _p
          toLens (UPath_ReportElem_elemText _p) = lens_ReportElem_elemText . toLens _p
instance ToLens UPath_ReportFlags
    where type S UPath_ReportFlags = ReportFlags
          type A UPath_ReportFlags = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportFlags_hideEmptyItemFields _p) = lens_ReportFlags_hideEmptyItemFields . toLens _p
instance ToLens UPath_ReportImage
    where type S UPath_ReportImage = ReportImage
          type A UPath_ReportImage = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportImage_View v) = (viewLens :: Lens' ReportImage ReportImageView) . toLens v
instance ToLens UPath_ReportImageView
    where type S UPath_ReportImageView = ReportImageView
          type A UPath_ReportImageView = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportImageView__picSize _p) = lens_ReportImageView__picSize . toLens _p
          toLens (UPath_ReportImageView__picCrop _p) = lens_ReportImageView__picCrop . toLens _p
          toLens (UPath_ReportImageView__picCaption _p) = lens_ReportImageView__picCaption . toLens _p
          toLens (UPath_ReportImageView__picOriginal _p) = lens_ReportImageView__picOriginal . toLens _p
          toLens (UPath_ReportImageView__picEditedDeprecated _p) = lens_ReportImageView__picEditedDeprecated . toLens _p
          toLens (UPath_ReportImageView__picThumbDeprecated _p) = lens_ReportImageView__picThumbDeprecated . toLens _p
          toLens (UPath_ReportImageView__picPrinterDeprecated _p) = lens_ReportImageView__picPrinterDeprecated . toLens _p
          toLens (UPath_ReportImageView__picMustEnlarge _p) = lens_ReportImageView__picMustEnlarge . toLens _p
          toLens (UPath_ReportImageView__picEnlargedDeprecated _p) = lens_ReportImageView__picEnlargedDeprecated . toLens _p
instance ToLens UPath_ReportIntendedUse
    where type S UPath_ReportIntendedUse = ReportIntendedUse
          type A UPath_ReportIntendedUse = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportIntendedUse_View v) = (viewLens :: Lens' ReportIntendedUse String) . toLens v
instance ToLens UPath_ReportMap
    where type S UPath_ReportMap = ReportMap
          type A UPath_ReportMap = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportMap_unReportMap _p) = lens_ReportMap_unReportMap . toLens _p
instance ToLens UPath_ReportStandard
    where type S UPath_ReportStandard = ReportStandard
          type A UPath_ReportStandard = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportStandard_unReportStandard _p) = lens_ReportStandard_unReportStandard . toLens _p
instance ToLens UPath_ReportStatus
    where type S UPath_ReportStatus = ReportStatus
          type A UPath_ReportStatus = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportStatus_View v) = (viewLens :: Lens' ReportStatus String) . toLens v
instance ToLens UPath_ReportValueApproachInfo
    where type S UPath_ReportValueApproachInfo = ReportValueApproachInfo
          type A UPath_ReportValueApproachInfo = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportValueApproachInfo_reportValueApproachName _p) = lens_ReportValueApproachInfo_reportValueApproachName . toLens _p
          toLens (UPath_ReportValueApproachInfo_reportValueApproachDescription _p) = lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _p
instance ToLens UPath_ReportValueTypeInfo
    where type S UPath_ReportValueTypeInfo = ReportValueTypeInfo
          type A UPath_ReportValueTypeInfo = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportValueTypeInfo_reportValueTypeName _p) = lens_ReportValueTypeInfo_reportValueTypeName . toLens _p
          toLens (UPath_ReportValueTypeInfo_reportValueTypeDescription _p) = lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _p
          toLens (UPath_ReportValueTypeInfo_reportValueTypeDefinition _p) = lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _p
instance ToLens UPath_ReportView
    where type S UPath_ReportView = ReportView
          type A UPath_ReportView = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_ReportView__reportFolder _p) = lens_ReportView__reportFolder . toLens _p
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
instance ToLens UPath_SaneSizeImageSize
    where type S UPath_SaneSizeImageSize = SaneSizeImageSize
          type A UPath_SaneSizeImageSize = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_SaneSizeImageSize_View v) = (viewLens :: Lens' SaneSizeImageSize ImageSize) . toLens v
instance ToLens UPath_String
    where type S UPath_String = String
          type A UPath_String = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_String_View v) = (viewLens :: Lens' String JSONText) . toLens v
instance ToLens UPath_Text
    where type S UPath_Text = Text
          type A UPath_Text = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Text_View v) = (viewLens :: Lens' Text JSONText) . toLens v
instance ToLens UPath_URI
    where type S UPath_URI = URI
          type A UPath_URI = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_UUID
    where type S UPath_UUID = UUID
          type A UPath_UUID = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_Units
    where type S UPath_Units = Units
          type A UPath_Units = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_Units_View v) = (viewLens :: Lens' Units JSONText) . toLens v
instance ToLens UPath_UserId
    where type S UPath_UserId = UserId
          type A UPath_UserId = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
instance ToLens UPath_UserIds
    where type S UPath_UserIds = UserIds
          type A UPath_UserIds = Univ
          toLens p | p == idPath = lens u (\s a -> maybe s id (unU' a))
          toLens (UPath_UserIds_View v) = (viewLens :: Lens' UserIds Text) . toLens v
