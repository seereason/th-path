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
import Appraisal.Maybe
import Appraisal.Permissions
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.ReportInstances
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID(..), ReportMap(..), MRR)
import Appraisal.Utils.CIString (CIString(..))
import Control.Lens (Iso', iso, lens, Lens', Traversal')
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (toList)
import Data.Maybe (fromMaybe)
import Data.Order (Path_OMap(Path_At), toPairs)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node))
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans (showUUID)
import GHC.Generics (Generic)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.View (View(viewLens))
import Network.URI (URI(URI), URIAuth)

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
    = U1 AbbrevPair
    | U2 AbbrevPairs
    | U3 Author
    | U4 Authors
    | U5 Bool
    | U6 Branding
    | U7 CIString
    | U8 Dimension
    | U9 Double
    | U10 EUI
    | U11 ImageCrop
    | U12 ImageFile
    | U13 ImageSize
    | U14 Int
    | U15 Int64
    | U16 Integer
    | U17 Item
    | U18 JSONText
    | U19 MEUI
    | U20 MIM
    | U21 MRR
    | U22 Markup
    | U23 MarkupPair
    | U24 MarkupPairs
    | U25 Markups
    | U26 MaybeImageFile
    | U27 MaybeReportIntendedUse
    | U28 Permissions
    | U29 ReadOnlyFilePath
    | U30 Report
    | U31 ReportElem
    | U32 ReportElems
    | U33 ReportFlags
    | U34 ReportImage
    | U35 ReportImageView
    | U36 ReportImages
    | U37 ReportIntendedUse
    | U38 ReportMap
    | U39 ReportStandard
    | U40 ReportStatus
    | U41 ReportValueApproachInfo
    | U42 ReportValueTypeInfo
    | U43 ReportView
    | U44 SaneSizeImageSize
    | U45 String
    | U46 Text
    | U47 URI
    | U48 UUID
    | U49 Units
    | U50 UserId
    | U51 UserIds
    deriving (Eq, Ord, Read, Data, Typeable, Generic, FromJSON, ToJSON)
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
instance Describe (Proxy CIString)
    where describe' _f _ = Just (fromMaybe "CIString" _f)
instance Describe (Proxy Dimension)
    where describe' _f _ = Just (fromMaybe "Dimension" _f)
instance Describe (Proxy EUI)
    where describe' _f _ = Just (fromMaybe "EUI" _f)
instance Describe (Proxy ImageCrop)
    where describe' _f _ = Just (fromMaybe "Image Crop" _f)
instance Describe (Proxy ImageFile)
    where describe' _f _ = Just (fromMaybe "Image File" _f)
instance Describe (Proxy ImageSize)
    where describe' _f _ = Just (fromMaybe "Image Size" _f)
instance Describe (Proxy Int)
    where describe' _f _ = Just (fromMaybe "Int" _f)
instance Describe (Proxy Int64)
    where describe' _f _ = Just (fromMaybe "Int64" _f)
instance Describe (Proxy Integer)
    where describe' _f _ = Just (fromMaybe "Integer" _f)
instance Describe (Proxy Item)
    where describe' _f _ = Just (fromMaybe "Item" _f)
instance Describe (Proxy MEUI)
    where describe' _f _ = Just (fromMaybe "MEUI" _f)
instance Describe (Proxy MIM)
    where describe' _f _ = Just (fromMaybe "MIM" _f)
instance Describe (Proxy MRR)
    where describe' _f _ = Just (fromMaybe "MRR" _f)
instance Describe (Proxy MarkupPair)
    where describe' _f _ = Just (fromMaybe "Markup Pair" _f)
instance Describe (Proxy MarkupPairs)
    where describe' _f _ = Just (fromMaybe "Markup Pairs" _f)
instance Describe (Proxy Markups)
    where describe' _f _ = Just (fromMaybe "Markups" _f)
instance Describe (Proxy MaybeImageFile)
    where describe' _f _ = Just (fromMaybe "Maybe Image File" _f)
instance Describe (Proxy MaybeReportIntendedUse)
    where describe' _f _ = Just (fromMaybe "Maybe Report Intended Use" _f)
instance Describe (Proxy Permissions)
    where describe' _f _ = Just (fromMaybe "Permissions" _f)
instance Describe (Proxy ReadOnlyFilePath)
    where describe' _f _ = Just (fromMaybe "Read Only File Path" _f)
instance Describe (Proxy Report)
    where describe' _f _ = Just (fromMaybe "Report" _f)
instance Describe (Proxy ReportElem)
    where describe' _f _ = Just (fromMaybe "Report Elem" _f)
instance Describe (Proxy ReportElems)
    where describe' _f _ = Just (fromMaybe "Report Elems" _f)
instance Describe (Proxy ReportFlags)
    where describe' _f _ = Just (fromMaybe "Report Flags" _f)
instance Describe (Proxy ReportImage)
    where describe' _f _ = Just (fromMaybe "Report Image" _f)
instance Describe (Proxy ReportImageView)
    where describe' _f _ = Just (fromMaybe "Report Image View" _f)
instance Describe (Proxy ReportImages)
    where describe' _f _ = Just (fromMaybe "Report Images" _f)
instance Describe (Proxy ReportIntendedUse)
    where describe' _f _ = Just (fromMaybe "Report Intended Use" _f)
instance Describe (Proxy ReportMap)
    where describe' _f _ = Just (fromMaybe "Report Map" _f)
instance Describe (Proxy ReportStandard)
    where describe' _f _ = Just (fromMaybe "Report Standard" _f)
instance Describe (Proxy ReportStatus)
    where describe' _f _ = Just (fromMaybe "Report Status" _f)
instance Describe (Proxy ReportValueApproachInfo)
    where describe' _f _ = Just (fromMaybe "Report Value Approach Info" _f)
instance Describe (Proxy ReportValueTypeInfo)
    where describe' _f _ = Just (fromMaybe "Report Value Type Info" _f)
instance Describe (Proxy ReportView)
    where describe' _f _ = Just (fromMaybe "Report View" _f)
instance Describe (Proxy SaneSizeImageSize)
    where describe' _f _ = Just (fromMaybe "Sane Size Image Size" _f)
instance Describe (Proxy URI)
    where describe' _f _ = Just (fromMaybe "URI" _f)
instance Describe (Proxy UUID)
    where describe' _f _ = Just (fromMaybe "UUID" _f)
instance Describe (Proxy Units)
    where describe' _f _ = Just (fromMaybe "Units" _f)
instance Describe (Proxy UserId)
    where describe' _f _ = Just (fromMaybe "User Id" _f)
instance Describe (Proxy UserIds)
    where describe' _f _ = Just (fromMaybe "User Ids" _f)
instance Describe UPath_Author
    where describe' _f (UPath_Author_authorName _q) = maybe (describe' _f (Proxy :: Proxy Author)) Just (describe' (Just "Author Name") _q)
          describe' _f (UPath_Author_authorCredentials _q) = maybe (describe' _f (Proxy :: Proxy Author)) Just (describe' (Just "Author Credentials") _q)
          describe' f (UPath_Author) = describe' f (Proxy :: Proxy Author)
instance Describe UPath_ImageCrop
    where describe' f _ = describe' f (Proxy :: Proxy ImageCrop)
instance Describe UPath_ImageFile
    where describe' f _ = describe' f (Proxy :: Proxy ImageFile)
instance Describe UPath_ImageSize
    where describe' _f (UPath_ImageSize_dim _q) = maybe (describe' _f (Proxy :: Proxy ImageSize)) Just (describe' (Just "Dim") _q)
          describe' _f (UPath_ImageSize_size _q) = maybe (describe' _f (Proxy :: Proxy ImageSize)) Just (describe' (Just "Size") _q)
          describe' _f (UPath_ImageSize_units _q) = maybe (describe' _f (Proxy :: Proxy ImageSize)) Just (describe' (Just "Units") _q)
          describe' f (UPath_ImageSize) = describe' f (Proxy :: Proxy ImageSize)
instance Describe UPath_Int
    where describe' f _ = describe' f (Proxy :: Proxy Int)
instance Describe UPath_Int64
    where describe' f _ = describe' f (Proxy :: Proxy Int64)
instance Describe UPath_Integer
    where describe' f _ = describe' f (Proxy :: Proxy Integer)
instance Describe UPath_Item
    where describe' _f (UPath_Item_itemName _q) = maybe (describe' _f (Proxy :: Proxy Item)) Just (describe' (Just "Item Name") _q)
          describe' _f (UPath_Item_fields _q) = maybe (describe' _f (Proxy :: Proxy Item)) Just (describe' (Just "Fields") _q)
          describe' _f (UPath_Item_images _q) = maybe (describe' _f (Proxy :: Proxy Item)) Just (describe' (Just "Images") _q)
          describe' f (UPath_Item) = describe' f (Proxy :: Proxy Item)
instance Describe UPath_JSONText
    where describe' f _ = describe' f (Proxy :: Proxy JSONText)
instance Describe UPath_Markup
    where describe' _f (UPath_Markup_markdownText _q) = maybe (describe' _f (Proxy :: Proxy Markup)) Just (describe' (Just "Markdown Text") _q)
          describe' _f (UPath_Markup_htmlText _q) = maybe (describe' _f (Proxy :: Proxy Markup)) Just (describe' (Just "Html Text") _q)
          describe' f (UPath_Markup) = describe' f (Proxy :: Proxy Markup)
instance Describe UPath_Permissions
    where describe' _f (UPath_Permissions_owner _q) = maybe (describe' _f (Proxy :: Proxy Permissions)) Just (describe' (Just "Owner") _q)
          describe' _f (UPath_Permissions_writers _q) = maybe (describe' _f (Proxy :: Proxy Permissions)) Just (describe' (Just "Writers") _q)
          describe' _f (UPath_Permissions_readers _q) = maybe (describe' _f (Proxy :: Proxy Permissions)) Just (describe' (Just "Readers") _q)
          describe' f (UPath_Permissions) = describe' f (Proxy :: Proxy Permissions)
instance Describe UPath_ReportElem
    where describe' _f (UPath_ReportElem_elemItem _q) = maybe (describe' _f (Proxy :: Proxy ReportElem)) Just (describe' (Just "Elem Item") _q)
          describe' _f (UPath_ReportElem_elemText _q) = maybe (describe' _f (Proxy :: Proxy ReportElem)) Just (describe' (Just "Elem Text") _q)
          describe' f (UPath_ReportElem) = describe' f (Proxy :: Proxy ReportElem)
instance Describe UPath_ReportFlags
    where describe' _f (UPath_ReportFlags_hideEmptyItemFields _q) = maybe (describe' _f (Proxy :: Proxy ReportFlags)) Just (describe' (Just "Hide Empty Item Fields") _q)
          describe' f (UPath_ReportFlags) = describe' f (Proxy :: Proxy ReportFlags)
instance Describe UPath_ReportImageView
    where describe' _f (UPath_ReportImageView__picSize _q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Size") _q)
          describe' _f (UPath_ReportImageView__picCrop _q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Crop") _q)
          describe' _f (UPath_ReportImageView__picCaption _q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Caption") _q)
          describe' _f (UPath_ReportImageView__picOriginal _q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Original") _q)
          describe' _f (UPath_ReportImageView__picMustEnlarge _q) = maybe (describe' _f (Proxy :: Proxy ReportImageView)) Just (describe' (Just "Pic Must Enlarge") _q)
          describe' f (UPath_ReportImageView) = describe' f (Proxy :: Proxy ReportImageView)
instance Describe UPath_ReportMap
    where describe' _f (UPath_ReportMap_unReportMap _q) = maybe (describe' _f (Proxy :: Proxy ReportMap)) Just (describe' (Just "Un Report Map") _q)
          describe' f (UPath_ReportMap) = describe' f (Proxy :: Proxy ReportMap)
instance Describe UPath_ReportStandard
    where describe' _f (UPath_ReportStandard_unReportStandard _q) = maybe (describe' _f (Proxy :: Proxy ReportStandard)) Just (describe' (Just "Un Report Standard") _q)
          describe' f (UPath_ReportStandard) = describe' f (Proxy :: Proxy ReportStandard)
instance Describe UPath_ReportValueApproachInfo
    where describe' _f (UPath_ReportValueApproachInfo_reportValueApproachName _q) = maybe (describe' _f (Proxy :: Proxy ReportValueApproachInfo)) Just (describe' (Just "Report Value Approach Name") _q)
          describe' _f (UPath_ReportValueApproachInfo_reportValueApproachDescription _q) = maybe (describe' _f (Proxy :: Proxy ReportValueApproachInfo)) Just (describe' (Just "Report Value Approach Description") _q)
          describe' f (UPath_ReportValueApproachInfo) = describe' f (Proxy :: Proxy ReportValueApproachInfo)
instance Describe UPath_ReportValueTypeInfo
    where describe' _f (UPath_ReportValueTypeInfo_reportValueTypeName _q) = maybe (describe' _f (Proxy :: Proxy ReportValueTypeInfo)) Just (describe' (Just "Report Value Type Name") _q)
          describe' _f (UPath_ReportValueTypeInfo_reportValueTypeDescription _q) = maybe (describe' _f (Proxy :: Proxy ReportValueTypeInfo)) Just (describe' (Just "Report Value Type Description") _q)
          describe' _f (UPath_ReportValueTypeInfo_reportValueTypeDefinition _q) = maybe (describe' _f (Proxy :: Proxy ReportValueTypeInfo)) Just (describe' (Just "Report Value Type Definition") _q)
          describe' f (UPath_ReportValueTypeInfo) = describe' f (Proxy :: Proxy ReportValueTypeInfo)
instance Describe UPath_ReportView
    where describe' _f (UPath_ReportView__reportFolder _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Folder") _q)
          describe' _f (UPath_ReportView__reportName _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Name") _q)
          describe' _f (UPath_ReportView__reportDate _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Date") _q)
          describe' _f (UPath_ReportView__reportContractDate _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Contract Date") _q)
          describe' _f (UPath_ReportView__reportInspectionDate _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Inspection Date") _q)
          describe' _f (UPath_ReportView__reportEffectiveDate _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Effective Date") _q)
          describe' _f (UPath_ReportView__reportAuthors _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Authors") _q)
          describe' _f (UPath_ReportView__reportPreparer _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer") _q)
          describe' _f (UPath_ReportView__reportPreparerEIN _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer EIN") _q)
          describe' _f (UPath_ReportView__reportPreparerAddress _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer Address") _q)
          describe' _f (UPath_ReportView__reportPreparerEMail _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer EMail") _q)
          describe' _f (UPath_ReportView__reportPreparerWebsite _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Preparer Website") _q)
          describe' _f (UPath_ReportView__reportAbbrevs _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Abbrevs") _q)
          describe' _f (UPath_ReportView__reportTitle _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Title") _q)
          describe' _f (UPath_ReportView__reportHeader _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Header") _q)
          describe' _f (UPath_ReportView__reportFooter _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Footer") _q)
          describe' _f (UPath_ReportView__reportIntendedUse _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Intended Use") _q)
          describe' _f (UPath_ReportView__reportValueTypeInfo _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Value Type Info") _q)
          describe' _f (UPath_ReportView__reportValueApproachInfo _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Value Approach Info") _q)
          describe' _f (UPath_ReportView__reportClientName _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Client Name") _q)
          describe' _f (UPath_ReportView__reportClientAddress _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Client Address") _q)
          describe' _f (UPath_ReportView__reportClientGreeting _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Client Greeting") _q)
          describe' _f (UPath_ReportView__reportItemsOwnerFull _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Items Owner Full") _q)
          describe' _f (UPath_ReportView__reportItemsOwner _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Items Owner") _q)
          describe' _f (UPath_ReportView__reportBriefItems _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Brief Items") _q)
          describe' _f (UPath_ReportView__reportInspectionLocation _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Inspection Location") _q)
          describe' _f (UPath_ReportView__reportBody _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Body") _q)
          describe' _f (UPath_ReportView__reportGlossary _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Glossary") _q)
          describe' _f (UPath_ReportView__reportSources _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Sources") _q)
          describe' _f (UPath_ReportView__reportLetterOfTransmittal _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Letter Of Transmittal") _q)
          describe' _f (UPath_ReportView__reportScopeOfWork _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Scope Of Work") _q)
          describe' _f (UPath_ReportView__reportCertification _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Certification") _q)
          describe' _f (UPath_ReportView__reportLimitingConditions _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Limiting Conditions") _q)
          describe' _f (UPath_ReportView__reportPrivacyPolicy _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Privacy Policy") _q)
          describe' _f (UPath_ReportView__reportPerms _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Perms") _q)
          describe' _f (UPath_ReportView__reportRevision _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Revision") _q)
          describe' _f (UPath_ReportView__reportCreated _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Created") _q)
          describe' _f (UPath_ReportView__reportBranding _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Branding") _q)
          describe' _f (UPath_ReportView__reportStatus _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Status") _q)
          describe' _f (UPath_ReportView__reportRedacted _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Redacted") _q)
          describe' _f (UPath_ReportView__reportFlags _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Flags") _q)
          describe' _f (UPath_ReportView__reportUUID _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report UUID") _q)
          describe' _f (UPath_ReportView__reportOrderByItemName _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Order By Item Name") _q)
          describe' _f (UPath_ReportView__reportDisplayItemName _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Display Item Name") _q)
          describe' _f (UPath_ReportView__reportStandardsVersion _q) = maybe (describe' _f (Proxy :: Proxy ReportView)) Just (describe' (Just "Report Standards Version") _q)
          describe' f (UPath_ReportView) = describe' f (Proxy :: Proxy ReportView)
instance Describe UPath_URI
    where describe' f _ = describe' f (Proxy :: Proxy URI)
instance Describe UPath_UUID
    where describe' f _ = describe' f (Proxy :: Proxy UUID)
instance Describe UPath_UserId
    where describe' f _ = describe' f (Proxy :: Proxy UserId)
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
instance IsPath UPath_Author
    where type UType UPath_Author = Univ
          type SType UPath_Author = Author
          idPath = UPath_Author
          toLens p = case p of
                         UPath_Author_authorName _p -> lens_Author_authorName . toLens _p
                         UPath_Author_authorCredentials _p -> lens_Author_authorCredentials . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ImageCrop
    where type UType UPath_ImageCrop = Univ
          type SType UPath_ImageCrop = ImageCrop
          idPath = UPath_ImageCrop
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ImageFile
    where type UType UPath_ImageFile = Univ
          type SType UPath_ImageFile = ImageFile
          idPath = UPath_ImageFile
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ImageSize
    where type UType UPath_ImageSize = Univ
          type SType UPath_ImageSize = ImageSize
          idPath = UPath_ImageSize
          toLens p = case p of
                         UPath_ImageSize_dim _p -> lens_ImageSize_dim . toLens _p
                         UPath_ImageSize_size _p -> lens_ImageSize_size . toLens _p
                         UPath_ImageSize_units _p -> lens_ImageSize_units . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_Int
    where type UType UPath_Int = Univ
          type SType UPath_Int = Int
          idPath = UPath_Int
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_Int64
    where type UType UPath_Int64 = Univ
          type SType UPath_Int64 = Int64
          idPath = UPath_Int64
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_Integer
    where type UType UPath_Integer = Univ
          type SType UPath_Integer = Integer
          idPath = UPath_Integer
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_Item
    where type UType UPath_Item = Univ
          type SType UPath_Item = Item
          idPath = UPath_Item
          toLens p = case p of
                         UPath_Item_itemName _p -> lens_Item_itemName . toLens _p
                         UPath_Item_fields _p -> lens_Item_fields . toLens _p
                         UPath_Item_images _p -> lens_Item_images . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_JSONText
    where type UType UPath_JSONText = Univ
          type SType UPath_JSONText = JSONText
          idPath = UPath_JSONText
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_Markup
    where type UType UPath_Markup = Univ
          type SType UPath_Markup = Markup
          idPath = UPath_Markup
          toLens p = case p of
                         UPath_Markup_markdownText _p -> lens_Markup_markdownText . toLens _p
                         UPath_Markup_htmlText _p -> lens_Markup_htmlText . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_Permissions
    where type UType UPath_Permissions = Univ
          type SType UPath_Permissions = Permissions
          idPath = UPath_Permissions
          toLens p = case p of
                         UPath_Permissions_owner _p -> lens_Permissions_owner . toLens _p
                         UPath_Permissions_writers _p -> lens_Permissions_writers . toLens _p
                         UPath_Permissions_readers _p -> lens_Permissions_readers . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportElem
    where type UType UPath_ReportElem = Univ
          type SType UPath_ReportElem = ReportElem
          idPath = UPath_ReportElem
          toLens p = case p of
                         UPath_ReportElem_elemItem _p -> lens_ReportElem_elemItem . toLens _p
                         UPath_ReportElem_elemText _p -> lens_ReportElem_elemText . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportFlags
    where type UType UPath_ReportFlags = Univ
          type SType UPath_ReportFlags = ReportFlags
          idPath = UPath_ReportFlags
          toLens p = case p of
                         UPath_ReportFlags_hideEmptyItemFields _p -> lens_ReportFlags_hideEmptyItemFields . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportImageView
    where type UType UPath_ReportImageView = Univ
          type SType UPath_ReportImageView = ReportImageView
          idPath = UPath_ReportImageView
          toLens p = case p of
                         UPath_ReportImageView__picSize _p -> lens_ReportImageView__picSize . toLens _p
                         UPath_ReportImageView__picCrop _p -> lens_ReportImageView__picCrop . toLens _p
                         UPath_ReportImageView__picCaption _p -> lens_ReportImageView__picCaption . toLens _p
                         UPath_ReportImageView__picOriginal _p -> lens_ReportImageView__picOriginal . toLens _p
                         UPath_ReportImageView__picMustEnlarge _p -> lens_ReportImageView__picMustEnlarge . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportMap
    where type UType UPath_ReportMap = Univ
          type SType UPath_ReportMap = ReportMap
          idPath = UPath_ReportMap
          toLens p = case p of
                         UPath_ReportMap_unReportMap _p -> lens_ReportMap_unReportMap . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportStandard
    where type UType UPath_ReportStandard = Univ
          type SType UPath_ReportStandard = ReportStandard
          idPath = UPath_ReportStandard
          toLens p = case p of
                         UPath_ReportStandard_unReportStandard _p -> lens_ReportStandard_unReportStandard . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportValueApproachInfo
    where type UType UPath_ReportValueApproachInfo = Univ
          type SType UPath_ReportValueApproachInfo = ReportValueApproachInfo
          idPath = UPath_ReportValueApproachInfo
          toLens p = case p of
                         UPath_ReportValueApproachInfo_reportValueApproachName _p -> lens_ReportValueApproachInfo_reportValueApproachName . toLens _p
                         UPath_ReportValueApproachInfo_reportValueApproachDescription _p -> lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportValueTypeInfo
    where type UType UPath_ReportValueTypeInfo = Univ
          type SType UPath_ReportValueTypeInfo = ReportValueTypeInfo
          idPath = UPath_ReportValueTypeInfo
          toLens p = case p of
                         UPath_ReportValueTypeInfo_reportValueTypeName _p -> lens_ReportValueTypeInfo_reportValueTypeName . toLens _p
                         UPath_ReportValueTypeInfo_reportValueTypeDescription _p -> lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _p
                         UPath_ReportValueTypeInfo_reportValueTypeDefinition _p -> lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_ReportView
    where type UType UPath_ReportView = Univ
          type SType UPath_ReportView = ReportView
          idPath = UPath_ReportView
          toLens p = case p of
                         UPath_ReportView__reportFolder _p -> lens_ReportView__reportFolder . toLens _p
                         UPath_ReportView__reportName _p -> lens_ReportView__reportName . toLens _p
                         UPath_ReportView__reportDate _p -> lens_ReportView__reportDate . toLens _p
                         UPath_ReportView__reportContractDate _p -> lens_ReportView__reportContractDate . toLens _p
                         UPath_ReportView__reportInspectionDate _p -> lens_ReportView__reportInspectionDate . toLens _p
                         UPath_ReportView__reportEffectiveDate _p -> lens_ReportView__reportEffectiveDate . toLens _p
                         UPath_ReportView__reportAuthors _p -> lens_ReportView__reportAuthors . toLens _p
                         UPath_ReportView__reportPreparer _p -> lens_ReportView__reportPreparer . toLens _p
                         UPath_ReportView__reportPreparerEIN _p -> lens_ReportView__reportPreparerEIN . toLens _p
                         UPath_ReportView__reportPreparerAddress _p -> lens_ReportView__reportPreparerAddress . toLens _p
                         UPath_ReportView__reportPreparerEMail _p -> lens_ReportView__reportPreparerEMail . toLens _p
                         UPath_ReportView__reportPreparerWebsite _p -> lens_ReportView__reportPreparerWebsite . toLens _p
                         UPath_ReportView__reportAbbrevs _p -> lens_ReportView__reportAbbrevs . toLens _p
                         UPath_ReportView__reportTitle _p -> lens_ReportView__reportTitle . toLens _p
                         UPath_ReportView__reportHeader _p -> lens_ReportView__reportHeader . toLens _p
                         UPath_ReportView__reportFooter _p -> lens_ReportView__reportFooter . toLens _p
                         UPath_ReportView__reportIntendedUse _p -> lens_ReportView__reportIntendedUse . toLens _p
                         UPath_ReportView__reportValueTypeInfo _p -> lens_ReportView__reportValueTypeInfo . toLens _p
                         UPath_ReportView__reportValueApproachInfo _p -> lens_ReportView__reportValueApproachInfo . toLens _p
                         UPath_ReportView__reportClientName _p -> lens_ReportView__reportClientName . toLens _p
                         UPath_ReportView__reportClientAddress _p -> lens_ReportView__reportClientAddress . toLens _p
                         UPath_ReportView__reportClientGreeting _p -> lens_ReportView__reportClientGreeting . toLens _p
                         UPath_ReportView__reportItemsOwnerFull _p -> lens_ReportView__reportItemsOwnerFull . toLens _p
                         UPath_ReportView__reportItemsOwner _p -> lens_ReportView__reportItemsOwner . toLens _p
                         UPath_ReportView__reportBriefItems _p -> lens_ReportView__reportBriefItems . toLens _p
                         UPath_ReportView__reportInspectionLocation _p -> lens_ReportView__reportInspectionLocation . toLens _p
                         UPath_ReportView__reportBody _p -> lens_ReportView__reportBody . toLens _p
                         UPath_ReportView__reportGlossary _p -> lens_ReportView__reportGlossary . toLens _p
                         UPath_ReportView__reportSources _p -> lens_ReportView__reportSources . toLens _p
                         UPath_ReportView__reportLetterOfTransmittal _p -> lens_ReportView__reportLetterOfTransmittal . toLens _p
                         UPath_ReportView__reportScopeOfWork _p -> lens_ReportView__reportScopeOfWork . toLens _p
                         UPath_ReportView__reportCertification _p -> lens_ReportView__reportCertification . toLens _p
                         UPath_ReportView__reportLimitingConditions _p -> lens_ReportView__reportLimitingConditions . toLens _p
                         UPath_ReportView__reportPrivacyPolicy _p -> lens_ReportView__reportPrivacyPolicy . toLens _p
                         UPath_ReportView__reportPerms _p -> lens_ReportView__reportPerms . toLens _p
                         UPath_ReportView__reportRevision _p -> lens_ReportView__reportRevision . toLens _p
                         UPath_ReportView__reportCreated _p -> lens_ReportView__reportCreated . toLens _p
                         UPath_ReportView__reportBranding _p -> lens_ReportView__reportBranding . toLens _p
                         UPath_ReportView__reportStatus _p -> lens_ReportView__reportStatus . toLens _p
                         UPath_ReportView__reportRedacted _p -> lens_ReportView__reportRedacted . toLens _p
                         UPath_ReportView__reportFlags _p -> lens_ReportView__reportFlags . toLens _p
                         UPath_ReportView__reportUUID _p -> lens_ReportView__reportUUID . toLens _p
                         UPath_ReportView__reportOrderByItemName _p -> lens_ReportView__reportOrderByItemName . toLens _p
                         UPath_ReportView__reportDisplayItemName _p -> lens_ReportView__reportDisplayItemName . toLens _p
                         UPath_ReportView__reportStandardsVersion _p -> lens_ReportView__reportStandardsVersion . toLens _p
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_URI
    where type UType UPath_URI = Univ
          type SType UPath_URI = URI
          idPath = UPath_URI
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_UUID
    where type UType UPath_UUID = Univ
          type SType UPath_UUID = UUID
          idPath = UPath_UUID
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance IsPath UPath_UserId
    where type UType UPath_UserId = Univ
          type SType UPath_UserId = UserId
          idPath = UPath_UserId
          toLens p = case p of
                         _ -> lens u (\s a -> maybe s id (unU' a))
instance PathStart Univ Author
    where type UPath Univ Author = UPath_Author
          upeekRow _ (x@(Author {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_Author_authorName],
                                                                           concatMap (makeRow x) [UPath_Author_authorCredentials]])
          upeekTree _ d (x@(Author {})) = case d of
                                              Just 0 -> Node (Peek idPath (Just (u x))) []
                                              _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_Author_authorName], concatMap (makeTrees d x) [UPath_Author_authorCredentials]])
          upeekCol _ (_p@(UPath_Author_authorName _q)) (x@(Author {})) = Node (Peek idPath Nothing) (makeCol x UPath_Author_authorName (\(UPath_Author_authorName p) -> p) _p)
          upeekCol _ (_p@(UPath_Author_authorCredentials _q)) (x@(Author {})) = Node (Peek idPath Nothing) (makeCol x UPath_Author_authorCredentials (\(UPath_Author_authorCredentials p) -> p) _p)
          upeekCol _ _p (x@(Author {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Bool
    where type UPath Univ Bool = Path_View Bool (Path_View String UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Branding
    where type UPath Univ Branding = Path_View Branding (Path_View Text UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ CIString
    where type UPath Univ CIString = Path_View CIString (Path_View Text UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Dimension
    where type UPath Univ Dimension = Path_View Dimension UPath_JSONText
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Double
    where type UPath Univ Double = Path_View Double (Path_View String UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ImageCrop
    where type UPath Univ ImageCrop = UPath_ImageCrop
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ImageFile
    where type UPath Univ ImageFile = UPath_ImageFile
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ImageSize
    where type UPath Univ ImageSize = UPath_ImageSize
          upeekRow _ (x@(ImageSize {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ImageSize_dim],
                                                                              concatMap (makeRow x) [UPath_ImageSize_size],
                                                                              concatMap (makeRow x) [UPath_ImageSize_units]])
          upeekTree _ d (x@(ImageSize {})) = case d of
                                                 Just 0 -> Node (Peek idPath (Just (u x))) []
                                                 _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ImageSize_dim],
                                                                                          concatMap (makeTrees d x) [UPath_ImageSize_size],
                                                                                          concatMap (makeTrees d x) [UPath_ImageSize_units]])
          upeekCol _ (_p@(UPath_ImageSize_dim _q)) (x@(ImageSize {})) = Node (Peek idPath Nothing) (makeCol x UPath_ImageSize_dim (\(UPath_ImageSize_dim p) -> p) _p)
          upeekCol _ (_p@(UPath_ImageSize_size _q)) (x@(ImageSize {})) = Node (Peek idPath Nothing) (makeCol x UPath_ImageSize_size (\(UPath_ImageSize_size p) -> p) _p)
          upeekCol _ (_p@(UPath_ImageSize_units _q)) (x@(ImageSize {})) = Node (Peek idPath Nothing) (makeCol x UPath_ImageSize_units (\(UPath_ImageSize_units p) -> p) _p)
          upeekCol _ _p (x@(ImageSize {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Int
    where type UPath Univ Int = UPath_Int
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Int64
    where type UPath Univ Int64 = UPath_Int64
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Integer
    where type UPath Univ Integer = UPath_Integer
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Item
    where type UPath Univ Item = UPath_Item
          upeekRow _ (x@(Item {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_Item_itemName],
                                                                         concatMap (makeRow x) [UPath_Item_fields],
                                                                         concatMap (makeRow x) [UPath_Item_images]])
          upeekTree _ d (x@(Item {})) = case d of
                                            Just 0 -> Node (Peek idPath (Just (u x))) []
                                            _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_Item_itemName],
                                                                                     concatMap (makeTrees d x) [UPath_Item_fields],
                                                                                     concatMap (makeTrees d x) [UPath_Item_images]])
          upeekCol _ (_p@(UPath_Item_itemName _q)) (x@(Item {})) = Node (Peek idPath Nothing) (makeCol x UPath_Item_itemName (\(UPath_Item_itemName p) -> p) _p)
          upeekCol _ (_p@(UPath_Item_fields _q)) (x@(Item {})) = Node (Peek idPath Nothing) (makeCol x UPath_Item_fields (\(UPath_Item_fields p) -> p) _p)
          upeekCol _ (_p@(UPath_Item_images _q)) (x@(Item {})) = Node (Peek idPath Nothing) (makeCol x UPath_Item_images (\(UPath_Item_images p) -> p) _p)
          upeekCol _ _p (x@(Item {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ JSONText
    where type UPath Univ JSONText = UPath_JSONText
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Markup
    where type UPath Univ Markup = UPath_Markup
          upeekRow _ (x@(Markdown {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_Markup_markdownText]])
          upeekRow _ (x@(Html {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_Markup_htmlText]])
          upeekRow _ (x@(LaTeX {})) = Node (Peek idPath Nothing) (concat [])
          upeekRow _ (x@(Pandoc {})) = Node (Peek idPath Nothing) (concat [])
          upeekRow _ (x@(Markup {})) = Node (Peek idPath Nothing) (concat [])
          upeekTree _ d (x@(Markdown {})) = case d of
                                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_Markup_markdownText]])
          upeekTree _ d (x@(Html {})) = case d of
                                            Just 0 -> Node (Peek idPath (Just (u x))) []
                                            _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_Markup_htmlText]])
          upeekTree _ _ (x@(LaTeX {})) = Node (Peek idPath (Just (u x))) []
          upeekTree _ _ (x@(Pandoc {})) = Node (Peek idPath (Just (u x))) []
          upeekTree _ _ (x@(Markup {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ (_p@(UPath_Markup_markdownText _q)) (x@(Markdown {})) = Node (Peek idPath Nothing) (makeCol x UPath_Markup_markdownText (\(UPath_Markup_markdownText p) -> p) _p)
          upeekCol _ _p (x@(Markdown {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ (_p@(UPath_Markup_htmlText _q)) (x@(Html {})) = Node (Peek idPath Nothing) (makeCol x UPath_Markup_htmlText (\(UPath_Markup_htmlText p) -> p) _p)
          upeekCol _ _p (x@(Html {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ _p (x@(LaTeX {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ _p (x@(Pandoc {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ _p (x@(Markup {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ MaybeReportIntendedUse
    where type UPath Univ MaybeReportIntendedUse = Path_View MaybeReportIntendedUse (Path_View String UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Permissions
    where type UPath Univ Permissions = UPath_Permissions
          upeekRow _ (x@(Permissions {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_Permissions_owner],
                                                                                concatMap (makeRow x) [UPath_Permissions_writers],
                                                                                concatMap (makeRow x) [UPath_Permissions_readers]])
          upeekTree _ d (x@(Permissions {})) = case d of
                                                   Just 0 -> Node (Peek idPath (Just (u x))) []
                                                   _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_Permissions_owner],
                                                                                            concatMap (makeTrees d x) [UPath_Permissions_writers],
                                                                                            concatMap (makeTrees d x) [UPath_Permissions_readers]])
          upeekCol _ (_p@(UPath_Permissions_owner _q)) (x@(Permissions {})) = Node (Peek idPath Nothing) (makeCol x UPath_Permissions_owner (\(UPath_Permissions_owner p) -> p) _p)
          upeekCol _ (_p@(UPath_Permissions_writers _q)) (x@(Permissions {})) = Node (Peek idPath Nothing) (makeCol x UPath_Permissions_writers (\(UPath_Permissions_writers p) -> p) _p)
          upeekCol _ (_p@(UPath_Permissions_readers _q)) (x@(Permissions {})) = Node (Peek idPath Nothing) (makeCol x UPath_Permissions_readers (\(UPath_Permissions_readers p) -> p) _p)
          upeekCol _ _p (x@(Permissions {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReadOnlyFilePath
    where type UPath Univ ReadOnlyFilePath = Path_View ReadOnlyFilePath (Path_View String UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Report
    where type UPath Univ Report = Path_View Report UPath_ReportView
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportElem
    where type UPath Univ ReportElem = UPath_ReportElem
          upeekRow _ (x@(ReportItem {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportElem_elemItem]])
          upeekRow _ (x@(ReportParagraph {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportElem_elemText]])
          upeekRow _ (x@(ReportUndecided {})) = Node (Peek idPath Nothing) (concat [])
          upeekTree _ d (x@(ReportItem {})) = case d of
                                                  Just 0 -> Node (Peek idPath (Just (u x))) []
                                                  _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportElem_elemItem]])
          upeekTree _ d (x@(ReportParagraph {})) = case d of
                                                       Just 0 -> Node (Peek idPath (Just (u x))) []
                                                       _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportElem_elemText]])
          upeekTree _ _ (x@(ReportUndecided {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ (_p@(UPath_ReportElem_elemItem _q)) (x@(ReportItem {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportElem_elemItem (\(UPath_ReportElem_elemItem p) -> p) _p)
          upeekCol _ _p (x@(ReportItem {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ (_p@(UPath_ReportElem_elemText _q)) (x@(ReportParagraph {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportElem_elemText (\(UPath_ReportElem_elemText p) -> p) _p)
          upeekCol _ _p (x@(ReportParagraph {})) = Node (Peek idPath (Just (u x))) []
          upeekCol _ _p (x@(ReportUndecided {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportFlags
    where type UPath Univ ReportFlags = UPath_ReportFlags
          upeekRow _ (x@(ReportFlags {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportFlags_hideEmptyItemFields]])
          upeekTree _ d (x@(ReportFlags {})) = case d of
                                                   Just 0 -> Node (Peek idPath (Just (u x))) []
                                                   _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportFlags_hideEmptyItemFields]])
          upeekCol _ (_p@(UPath_ReportFlags_hideEmptyItemFields _q)) (x@(ReportFlags {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportFlags_hideEmptyItemFields (\(UPath_ReportFlags_hideEmptyItemFields p) -> p) _p)
          upeekCol _ _p (x@(ReportFlags {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportImage
    where type UPath Univ ReportImage = Path_View ReportImage UPath_ReportImageView
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportImageView
    where type UPath Univ ReportImageView = UPath_ReportImageView
          upeekRow _ (x@(ReportImageView {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportImageView__picSize],
                                                                                    concatMap (makeRow x) [UPath_ReportImageView__picCrop],
                                                                                    concatMap (makeRow x) [UPath_ReportImageView__picCaption],
                                                                                    concatMap (makeRow x) [UPath_ReportImageView__picOriginal],
                                                                                    concatMap (makeRow x) [UPath_ReportImageView__picMustEnlarge]])
          upeekTree _ d (x@(ReportImageView {})) = case d of
                                                       Just 0 -> Node (Peek idPath (Just (u x))) []
                                                       _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportImageView__picSize],
                                                                                                concatMap (makeTrees d x) [UPath_ReportImageView__picCrop],
                                                                                                concatMap (makeTrees d x) [UPath_ReportImageView__picCaption],
                                                                                                concatMap (makeTrees d x) [UPath_ReportImageView__picOriginal],
                                                                                                concatMap (makeTrees d x) [UPath_ReportImageView__picMustEnlarge]])
          upeekCol _ (_p@(UPath_ReportImageView__picSize _q)) (x@(ReportImageView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportImageView__picSize (\(UPath_ReportImageView__picSize p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportImageView__picCrop _q)) (x@(ReportImageView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportImageView__picCrop (\(UPath_ReportImageView__picCrop p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportImageView__picCaption _q)) (x@(ReportImageView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportImageView__picCaption (\(UPath_ReportImageView__picCaption p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportImageView__picOriginal _q)) (x@(ReportImageView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportImageView__picOriginal (\(UPath_ReportImageView__picOriginal p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportImageView__picMustEnlarge _q)) (x@(ReportImageView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportImageView__picMustEnlarge (\(UPath_ReportImageView__picMustEnlarge p) -> p) _p)
          upeekCol _ _p (x@(ReportImageView {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportIntendedUse
    where type UPath Univ ReportIntendedUse = Path_View ReportIntendedUse (Path_View String UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportMap
    where type UPath Univ ReportMap = UPath_ReportMap
          upeekRow _ (x@(ReportMap {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportMap_unReportMap]])
          upeekTree _ d (x@(ReportMap {})) = case d of
                                                 Just 0 -> Node (Peek idPath (Just (u x))) []
                                                 _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportMap_unReportMap]])
          upeekCol _ (_p@(UPath_ReportMap_unReportMap _q)) (x@(ReportMap {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportMap_unReportMap (\(UPath_ReportMap_unReportMap p) -> p) _p)
          upeekCol _ _p (x@(ReportMap {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportStandard
    where type UPath Univ ReportStandard = UPath_ReportStandard
          upeekRow _ (x@(ReportStandard {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportStandard_unReportStandard]])
          upeekTree _ d (x@(ReportStandard {})) = case d of
                                                      Just 0 -> Node (Peek idPath (Just (u x))) []
                                                      _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportStandard_unReportStandard]])
          upeekCol _ (_p@(UPath_ReportStandard_unReportStandard _q)) (x@(ReportStandard {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportStandard_unReportStandard (\(UPath_ReportStandard_unReportStandard p) -> p) _p)
          upeekCol _ _p (x@(ReportStandard {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportStatus
    where type UPath Univ ReportStatus = Path_View ReportStatus (Path_View String UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportValueApproachInfo
    where type UPath Univ ReportValueApproachInfo = UPath_ReportValueApproachInfo
          upeekRow _ (x@(ReportValueApproachInfo {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportValueApproachInfo_reportValueApproachName],
                                                                                            concatMap (makeRow x) [UPath_ReportValueApproachInfo_reportValueApproachDescription]])
          upeekTree _ d (x@(ReportValueApproachInfo {})) = case d of
                                                               Just 0 -> Node (Peek idPath (Just (u x))) []
                                                               _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportValueApproachInfo_reportValueApproachName],
                                                                                                        concatMap (makeTrees d x) [UPath_ReportValueApproachInfo_reportValueApproachDescription]])
          upeekCol _ (_p@(UPath_ReportValueApproachInfo_reportValueApproachName _q)) (x@(ReportValueApproachInfo {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportValueApproachInfo_reportValueApproachName (\(UPath_ReportValueApproachInfo_reportValueApproachName p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportValueApproachInfo_reportValueApproachDescription _q)) (x@(ReportValueApproachInfo {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportValueApproachInfo_reportValueApproachDescription (\(UPath_ReportValueApproachInfo_reportValueApproachDescription p) -> p) _p)
          upeekCol _ _p (x@(ReportValueApproachInfo {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportValueTypeInfo
    where type UPath Univ ReportValueTypeInfo = UPath_ReportValueTypeInfo
          upeekRow _ (x@(ReportValueTypeInfo {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportValueTypeInfo_reportValueTypeName],
                                                                                        concatMap (makeRow x) [UPath_ReportValueTypeInfo_reportValueTypeDescription],
                                                                                        concatMap (makeRow x) [UPath_ReportValueTypeInfo_reportValueTypeDefinition]])
          upeekTree _ d (x@(ReportValueTypeInfo {})) = case d of
                                                           Just 0 -> Node (Peek idPath (Just (u x))) []
                                                           _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportValueTypeInfo_reportValueTypeName],
                                                                                                    concatMap (makeTrees d x) [UPath_ReportValueTypeInfo_reportValueTypeDescription],
                                                                                                    concatMap (makeTrees d x) [UPath_ReportValueTypeInfo_reportValueTypeDefinition]])
          upeekCol _ (_p@(UPath_ReportValueTypeInfo_reportValueTypeName _q)) (x@(ReportValueTypeInfo {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportValueTypeInfo_reportValueTypeName (\(UPath_ReportValueTypeInfo_reportValueTypeName p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportValueTypeInfo_reportValueTypeDescription _q)) (x@(ReportValueTypeInfo {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportValueTypeInfo_reportValueTypeDescription (\(UPath_ReportValueTypeInfo_reportValueTypeDescription p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportValueTypeInfo_reportValueTypeDefinition _q)) (x@(ReportValueTypeInfo {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportValueTypeInfo_reportValueTypeDefinition (\(UPath_ReportValueTypeInfo_reportValueTypeDefinition p) -> p) _p)
          upeekCol _ _p (x@(ReportValueTypeInfo {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ ReportView
    where type UPath Univ ReportView = UPath_ReportView
          upeekRow _ (x@(ReportView {})) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [UPath_ReportView__reportFolder],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportName],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportDate],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportContractDate],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportInspectionDate],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportEffectiveDate],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportAuthors],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPreparer],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPreparerEIN],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPreparerAddress],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPreparerEMail],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPreparerWebsite],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportAbbrevs],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportTitle],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportHeader],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportFooter],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportIntendedUse],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportValueTypeInfo],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportValueApproachInfo],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportClientName],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportClientAddress],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportClientGreeting],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportItemsOwnerFull],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportItemsOwner],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportBriefItems],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportInspectionLocation],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportBody],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportGlossary],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportSources],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportLetterOfTransmittal],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportScopeOfWork],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportCertification],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportLimitingConditions],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPrivacyPolicy],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportPerms],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportRevision],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportCreated],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportBranding],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportStatus],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportRedacted],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportFlags],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportUUID],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportOrderByItemName],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportDisplayItemName],
                                                                               concatMap (makeRow x) [UPath_ReportView__reportStandardsVersion]])
          upeekTree _ d (x@(ReportView {})) = case d of
                                                  Just 0 -> Node (Peek idPath (Just (u x))) []
                                                  _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [UPath_ReportView__reportFolder],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportName],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportDate],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportContractDate],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportInspectionDate],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportEffectiveDate],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportAuthors],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPreparer],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPreparerEIN],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPreparerAddress],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPreparerEMail],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPreparerWebsite],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportAbbrevs],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportTitle],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportHeader],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportFooter],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportIntendedUse],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportValueTypeInfo],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportValueApproachInfo],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportClientName],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportClientAddress],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportClientGreeting],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportItemsOwnerFull],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportItemsOwner],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportBriefItems],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportInspectionLocation],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportBody],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportGlossary],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportSources],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportLetterOfTransmittal],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportScopeOfWork],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportCertification],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportLimitingConditions],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPrivacyPolicy],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportPerms],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportRevision],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportCreated],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportBranding],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportStatus],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportRedacted],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportFlags],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportUUID],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportOrderByItemName],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportDisplayItemName],
                                                                                           concatMap (makeTrees d x) [UPath_ReportView__reportStandardsVersion]])
          upeekCol _ (_p@(UPath_ReportView__reportFolder _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportFolder (\(UPath_ReportView__reportFolder p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportName _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportName (\(UPath_ReportView__reportName p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportDate _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportDate (\(UPath_ReportView__reportDate p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportContractDate _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportContractDate (\(UPath_ReportView__reportContractDate p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportInspectionDate _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportInspectionDate (\(UPath_ReportView__reportInspectionDate p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportEffectiveDate _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportEffectiveDate (\(UPath_ReportView__reportEffectiveDate p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportAuthors _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportAuthors (\(UPath_ReportView__reportAuthors p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPreparer _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPreparer (\(UPath_ReportView__reportPreparer p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPreparerEIN _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPreparerEIN (\(UPath_ReportView__reportPreparerEIN p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPreparerAddress _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPreparerAddress (\(UPath_ReportView__reportPreparerAddress p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPreparerEMail _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPreparerEMail (\(UPath_ReportView__reportPreparerEMail p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPreparerWebsite _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPreparerWebsite (\(UPath_ReportView__reportPreparerWebsite p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportAbbrevs _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportAbbrevs (\(UPath_ReportView__reportAbbrevs p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportTitle _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportTitle (\(UPath_ReportView__reportTitle p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportHeader _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportHeader (\(UPath_ReportView__reportHeader p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportFooter _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportFooter (\(UPath_ReportView__reportFooter p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportIntendedUse _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportIntendedUse (\(UPath_ReportView__reportIntendedUse p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportValueTypeInfo _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportValueTypeInfo (\(UPath_ReportView__reportValueTypeInfo p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportValueApproachInfo _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportValueApproachInfo (\(UPath_ReportView__reportValueApproachInfo p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportClientName _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportClientName (\(UPath_ReportView__reportClientName p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportClientAddress _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportClientAddress (\(UPath_ReportView__reportClientAddress p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportClientGreeting _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportClientGreeting (\(UPath_ReportView__reportClientGreeting p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportItemsOwnerFull _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportItemsOwnerFull (\(UPath_ReportView__reportItemsOwnerFull p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportItemsOwner _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportItemsOwner (\(UPath_ReportView__reportItemsOwner p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportBriefItems _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportBriefItems (\(UPath_ReportView__reportBriefItems p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportInspectionLocation _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportInspectionLocation (\(UPath_ReportView__reportInspectionLocation p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportBody _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportBody (\(UPath_ReportView__reportBody p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportGlossary _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportGlossary (\(UPath_ReportView__reportGlossary p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportSources _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportSources (\(UPath_ReportView__reportSources p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportLetterOfTransmittal _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportLetterOfTransmittal (\(UPath_ReportView__reportLetterOfTransmittal p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportScopeOfWork _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportScopeOfWork (\(UPath_ReportView__reportScopeOfWork p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportCertification _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportCertification (\(UPath_ReportView__reportCertification p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportLimitingConditions _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportLimitingConditions (\(UPath_ReportView__reportLimitingConditions p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPrivacyPolicy _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPrivacyPolicy (\(UPath_ReportView__reportPrivacyPolicy p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportPerms _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportPerms (\(UPath_ReportView__reportPerms p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportRevision _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportRevision (\(UPath_ReportView__reportRevision p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportCreated _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportCreated (\(UPath_ReportView__reportCreated p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportBranding _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportBranding (\(UPath_ReportView__reportBranding p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportStatus _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportStatus (\(UPath_ReportView__reportStatus p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportRedacted _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportRedacted (\(UPath_ReportView__reportRedacted p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportFlags _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportFlags (\(UPath_ReportView__reportFlags p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportUUID _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportUUID (\(UPath_ReportView__reportUUID p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportOrderByItemName _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportOrderByItemName (\(UPath_ReportView__reportOrderByItemName p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportDisplayItemName _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportDisplayItemName (\(UPath_ReportView__reportDisplayItemName p) -> p) _p)
          upeekCol _ (_p@(UPath_ReportView__reportStandardsVersion _q)) (x@(ReportView {})) = Node (Peek idPath Nothing) (makeCol x UPath_ReportView__reportStandardsVersion (\(UPath_ReportView__reportStandardsVersion p) -> p) _p)
          upeekCol _ _p (x@(ReportView {})) = Node (Peek idPath (Just (u x))) []
instance PathStart Univ SaneSizeImageSize
    where type UPath Univ SaneSizeImageSize = Path_View SaneSizeImageSize UPath_ImageSize
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ String
    where type UPath Univ String = Path_View String UPath_JSONText
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Text
    where type UPath Univ Text = Path_View Text UPath_JSONText
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ URI
    where type UPath Univ URI = UPath_URI
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ UUID
    where type UPath Univ UUID = UPath_UUID
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ Units
    where type UPath Univ Units = Path_View Units UPath_JSONText
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ UserId
    where type UPath Univ UserId = UPath_UserId
          upeekRow _ _ = Node (Peek idPath Nothing) []
          upeekTree _ _ x = Node (Peek idPath (Just (u x))) []
          upeekCol _ _ x = Node (Peek idPath (Just (u x))) []
instance PathStart Univ UserIds
    where type UPath Univ UserIds = Path_View UserIds (Path_View Text UPath_JSONText)
          upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
          upeekTree _ d x = case d of
                                Just 0 -> Node (Peek idPath (Just (u x))) []
                                _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
          upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
          upeekCol _ _p x = Node (Peek idPath (Just (u x))) []
instance Show Univ
    where show (U1 x) = "(u (" ++ (show x ++ " :: AbbrevPair) :: Univ)")
          show (U2 x) = "(u (" ++ (show x ++ " :: AbbrevPairs) :: Univ)")
          show (U3 x) = "(u (" ++ (show x ++ " :: Author) :: Univ)")
          show (U4 x) = "(u (" ++ (show x ++ " :: Authors) :: Univ)")
          show (U5 x) = "(u (" ++ (show x ++ " :: Bool) :: Univ)")
          show (U6 x) = "(u (" ++ (show x ++ " :: Branding) :: Univ)")
          show (U7 x) = "(u (" ++ (show x ++ " :: CIString) :: Univ)")
          show (U8 x) = "(u (" ++ (show x ++ " :: Dimension) :: Univ)")
          show (U9 x) = "(u (" ++ (show x ++ " :: Double) :: Univ)")
          show (U10 x) = "(u (" ++ (show x ++ " :: EUI) :: Univ)")
          show (U11 x) = "(u (" ++ (show x ++ " :: ImageCrop) :: Univ)")
          show (U12 x) = "(u (" ++ (show x ++ " :: ImageFile) :: Univ)")
          show (U13 x) = "(u (" ++ (show x ++ " :: ImageSize) :: Univ)")
          show (U14 x) = "(u (" ++ (show x ++ " :: Int) :: Univ)")
          show (U15 x) = "(u (" ++ (show x ++ " :: Int64) :: Univ)")
          show (U16 x) = "(u (" ++ (show x ++ " :: Integer) :: Univ)")
          show (U17 x) = "(u (" ++ (show x ++ " :: Item) :: Univ)")
          show (U18 x) = "(u (" ++ (show x ++ " :: JSONText) :: Univ)")
          show (U19 x) = "(u (" ++ (show x ++ " :: MEUI) :: Univ)")
          show (U20 x) = "(u (" ++ (show x ++ " :: MIM) :: Univ)")
          show (U21 x) = "(u (" ++ (show x ++ " :: MRR) :: Univ)")
          show (U22 x) = "(u (" ++ (show x ++ " :: Markup) :: Univ)")
          show (U23 x) = "(u (" ++ (show x ++ " :: MarkupPair) :: Univ)")
          show (U24 x) = "(u (" ++ (show x ++ " :: MarkupPairs) :: Univ)")
          show (U25 x) = "(u (" ++ (show x ++ " :: Markups) :: Univ)")
          show (U26 x) = "(u (" ++ (show x ++ " :: MaybeImageFile) :: Univ)")
          show (U27 x) = "(u (" ++ (show x ++ " :: MaybeReportIntendedUse) :: Univ)")
          show (U28 x) = "(u (" ++ (show x ++ " :: Permissions) :: Univ)")
          show (U29 x) = "(u (" ++ (show x ++ " :: ReadOnlyFilePath) :: Univ)")
          show (U30 x) = "(u (" ++ (show x ++ " :: Report) :: Univ)")
          show (U31 x) = "(u (" ++ (show x ++ " :: ReportElem) :: Univ)")
          show (U32 x) = "(u (" ++ (show x ++ " :: ReportElems) :: Univ)")
          show (U33 x) = "(u (" ++ (show x ++ " :: ReportFlags) :: Univ)")
          show (U34 x) = "(u (" ++ (show x ++ " :: ReportImage) :: Univ)")
          show (U35 x) = "(u (" ++ (show x ++ " :: ReportImageView) :: Univ)")
          show (U36 x) = "(u (" ++ (show x ++ " :: ReportImages) :: Univ)")
          show (U37 x) = "(u (" ++ (show x ++ " :: ReportIntendedUse) :: Univ)")
          show (U38 x) = "(u (" ++ (show x ++ " :: ReportMap) :: Univ)")
          show (U39 x) = "(u (" ++ (show x ++ " :: ReportStandard) :: Univ)")
          show (U40 x) = "(u (" ++ (show x ++ " :: ReportStatus) :: Univ)")
          show (U41 x) = "(u (" ++ (show x ++ " :: ReportValueApproachInfo) :: Univ)")
          show (U42 x) = "(u (" ++ (show x ++ " :: ReportValueTypeInfo) :: Univ)")
          show (U43 x) = "(u (" ++ (show x ++ " :: ReportView) :: Univ)")
          show (U44 x) = "(u (" ++ (show x ++ " :: SaneSizeImageSize) :: Univ)")
          show (U45 x) = "(u (" ++ (show x ++ " :: String) :: Univ)")
          show (U46 x) = "(u (" ++ (show x ++ " :: Text) :: Univ)")
          show (U47 x) = "(u (" ++ (show x ++ " :: URI) :: Univ)")
          show (U48 x) = "(u (" ++ (showUUID x ++ " :: UUID) :: Univ)")
          show (U49 x) = "(u (" ++ (show x ++ " :: Units) :: Univ)")
          show (U50 x) = "(u (" ++ (show x ++ " :: UserId) :: Univ)")
          show (U51 x) = "(u (" ++ (show x ++ " :: UserIds) :: Univ)")
instance U Univ AbbrevPair
    where u = U1
          unU' (U1 a) = Just a
          unU' _ = Nothing
instance U Univ AbbrevPairs
    where u = U2
          unU' (U2 a) = Just a
          unU' _ = Nothing
instance U Univ Author
    where u = U3
          unU' (U3 a) = Just a
          unU' _ = Nothing
instance U Univ Authors
    where u = U4
          unU' (U4 a) = Just a
          unU' _ = Nothing
instance U Univ Bool
    where u = U5
          unU' (U5 a) = Just a
          unU' _ = Nothing
instance U Univ Branding
    where u = U6
          unU' (U6 a) = Just a
          unU' _ = Nothing
instance U Univ CIString
    where u = U7
          unU' (U7 a) = Just a
          unU' _ = Nothing
instance U Univ Dimension
    where u = U8
          unU' (U8 a) = Just a
          unU' _ = Nothing
instance U Univ Double
    where u = U9
          unU' (U9 a) = Just a
          unU' _ = Nothing
instance U Univ EUI
    where u = U10
          unU' (U10 a) = Just a
          unU' _ = Nothing
instance U Univ ImageCrop
    where u = U11
          unU' (U11 a) = Just a
          unU' _ = Nothing
instance U Univ ImageFile
    where u = U12
          unU' (U12 a) = Just a
          unU' _ = Nothing
instance U Univ ImageSize
    where u = U13
          unU' (U13 a) = Just a
          unU' _ = Nothing
instance U Univ Int
    where u = U14
          unU' (U14 a) = Just a
          unU' _ = Nothing
instance U Univ Int64
    where u = U15
          unU' (U15 a) = Just a
          unU' _ = Nothing
instance U Univ Integer
    where u = U16
          unU' (U16 a) = Just a
          unU' _ = Nothing
instance U Univ Item
    where u = U17
          unU' (U17 a) = Just a
          unU' _ = Nothing
instance U Univ JSONText
    where u = U18
          unU' (U18 a) = Just a
          unU' _ = Nothing
instance U Univ MEUI
    where u = U19
          unU' (U19 a) = Just a
          unU' _ = Nothing
instance U Univ MIM
    where u = U20
          unU' (U20 a) = Just a
          unU' _ = Nothing
instance U Univ MRR
    where u = U21
          unU' (U21 a) = Just a
          unU' _ = Nothing
instance U Univ Markup
    where u = U22
          unU' (U22 a) = Just a
          unU' _ = Nothing
instance U Univ MarkupPair
    where u = U23
          unU' (U23 a) = Just a
          unU' _ = Nothing
instance U Univ MarkupPairs
    where u = U24
          unU' (U24 a) = Just a
          unU' _ = Nothing
instance U Univ Markups
    where u = U25
          unU' (U25 a) = Just a
          unU' _ = Nothing
instance U Univ MaybeImageFile
    where u = U26
          unU' (U26 a) = Just a
          unU' _ = Nothing
instance U Univ MaybeReportIntendedUse
    where u = U27
          unU' (U27 a) = Just a
          unU' _ = Nothing
instance U Univ Permissions
    where u = U28
          unU' (U28 a) = Just a
          unU' _ = Nothing
instance U Univ ReadOnlyFilePath
    where u = U29
          unU' (U29 a) = Just a
          unU' _ = Nothing
instance U Univ Report
    where u = U30
          unU' (U30 a) = Just a
          unU' _ = Nothing
instance U Univ ReportElem
    where u = U31
          unU' (U31 a) = Just a
          unU' _ = Nothing
instance U Univ ReportElems
    where u = U32
          unU' (U32 a) = Just a
          unU' _ = Nothing
instance U Univ ReportFlags
    where u = U33
          unU' (U33 a) = Just a
          unU' _ = Nothing
instance U Univ ReportImage
    where u = U34
          unU' (U34 a) = Just a
          unU' _ = Nothing
instance U Univ ReportImageView
    where u = U35
          unU' (U35 a) = Just a
          unU' _ = Nothing
instance U Univ ReportImages
    where u = U36
          unU' (U36 a) = Just a
          unU' _ = Nothing
instance U Univ ReportIntendedUse
    where u = U37
          unU' (U37 a) = Just a
          unU' _ = Nothing
instance U Univ ReportMap
    where u = U38
          unU' (U38 a) = Just a
          unU' _ = Nothing
instance U Univ ReportStandard
    where u = U39
          unU' (U39 a) = Just a
          unU' _ = Nothing
instance U Univ ReportStatus
    where u = U40
          unU' (U40 a) = Just a
          unU' _ = Nothing
instance U Univ ReportValueApproachInfo
    where u = U41
          unU' (U41 a) = Just a
          unU' _ = Nothing
instance U Univ ReportValueTypeInfo
    where u = U42
          unU' (U42 a) = Just a
          unU' _ = Nothing
instance U Univ ReportView
    where u = U43
          unU' (U43 a) = Just a
          unU' _ = Nothing
instance U Univ SaneSizeImageSize
    where u = U44
          unU' (U44 a) = Just a
          unU' _ = Nothing
instance U Univ String
    where u = U45
          unU' (U45 a) = Just a
          unU' _ = Nothing
instance U Univ Text
    where u = U46
          unU' (U46 a) = Just a
          unU' _ = Nothing
instance U Univ URI
    where u = U47
          unU' (U47 a) = Just a
          unU' _ = Nothing
instance U Univ UUID
    where u = U48
          unU' (U48 a) = Just a
          unU' _ = Nothing
instance U Univ Units
    where u = U49
          unU' (U49 a) = Just a
          unU' _ = Nothing
instance U Univ UserId
    where u = U50
          unU' (U50 a) = Just a
          unU' _ = Nothing
instance U Univ UserIds
    where u = U51
          unU' (U51 a) = Just a
          unU' _ = Nothing
uMatch (U1 _) (U1 _) = True
uMatch (U2 _) (U2 _) = True
uMatch (U3 _) (U3 _) = True
uMatch (U4 _) (U4 _) = True
uMatch (U5 _) (U5 _) = True
uMatch (U6 _) (U6 _) = True
uMatch (U7 _) (U7 _) = True
uMatch (U8 _) (U8 _) = True
uMatch (U9 _) (U9 _) = True
uMatch (U10 _) (U10 _) = True
uMatch (U11 _) (U11 _) = True
uMatch (U12 _) (U12 _) = True
uMatch (U13 _) (U13 _) = True
uMatch (U14 _) (U14 _) = True
uMatch (U15 _) (U15 _) = True
uMatch (U16 _) (U16 _) = True
uMatch (U17 _) (U17 _) = True
uMatch (U18 _) (U18 _) = True
uMatch (U19 _) (U19 _) = True
uMatch (U20 _) (U20 _) = True
uMatch (U21 _) (U21 _) = True
uMatch (U22 _) (U22 _) = True
uMatch (U23 _) (U23 _) = True
uMatch (U24 _) (U24 _) = True
uMatch (U25 _) (U25 _) = True
uMatch (U26 _) (U26 _) = True
uMatch (U27 _) (U27 _) = True
uMatch (U28 _) (U28 _) = True
uMatch (U29 _) (U29 _) = True
uMatch (U30 _) (U30 _) = True
uMatch (U31 _) (U31 _) = True
uMatch (U32 _) (U32 _) = True
uMatch (U33 _) (U33 _) = True
uMatch (U34 _) (U34 _) = True
uMatch (U35 _) (U35 _) = True
uMatch (U36 _) (U36 _) = True
uMatch (U37 _) (U37 _) = True
uMatch (U38 _) (U38 _) = True
uMatch (U39 _) (U39 _) = True
uMatch (U40 _) (U40 _) = True
uMatch (U41 _) (U41 _) = True
uMatch (U42 _) (U42 _) = True
uMatch (U43 _) (U43 _) = True
uMatch (U44 _) (U44 _) = True
uMatch (U45 _) (U45 _) = True
uMatch (U46 _) (U46 _) = True
uMatch (U47 _) (U47 _) = True
uMatch (U48 _) (U48 _) = True
uMatch (U49 _) (U49 _) = True
uMatch (U50 _) (U50 _) = True
uMatch (U51 _) (U51 _) = True
uMatch _ _ = False
uSimple (U1 _) = False
uSimple (U2 _) = False
uSimple (U3 _) = False
uSimple (U4 _) = False
uSimple (U5 _) = False
uSimple (U6 _) = False
uSimple (U7 _) = False
uSimple (U8 _) = False
uSimple (U9 _) = False
uSimple (U10 _) = False
uSimple (U11 _) = True
uSimple (U12 _) = True
uSimple (U13 _) = False
uSimple (U14 _) = True
uSimple (U15 _) = True
uSimple (U16 _) = True
uSimple (U17 _) = False
uSimple (U18 _) = True
uSimple (U19 _) = False
uSimple (U20 _) = False
uSimple (U21 _) = False
uSimple (U22 _) = False
uSimple (U23 _) = False
uSimple (U24 _) = False
uSimple (U25 _) = False
uSimple (U26 _) = False
uSimple (U27 _) = False
uSimple (U28 _) = False
uSimple (U29 _) = False
uSimple (U30 _) = False
uSimple (U31 _) = False
uSimple (U32 _) = False
uSimple (U33 _) = False
uSimple (U34 _) = False
uSimple (U35 _) = False
uSimple (U36 _) = False
uSimple (U37 _) = False
uSimple (U38 _) = False
uSimple (U39 _) = False
uSimple (U40 _) = False
uSimple (U41 _) = False
uSimple (U42 _) = False
uSimple (U43 _) = False
uSimple (U44 _) = False
uSimple (U45 _) = False
uSimple (U46 _) = False
uSimple (U47 _) = True
uSimple (U48 _) = True
uSimple (U49 _) = False
uSimple (U50 _) = True
uSimple (U51 _) = False
uView (U1 _) = False
uView (U2 _) = False
uView (U3 _) = False
uView (U4 _) = False
uView (U5 _) = True
uView (U6 _) = True
uView (U7 _) = True
uView (U8 _) = True
uView (U9 _) = True
uView (U10 _) = False
uView (U11 _) = False
uView (U12 _) = False
uView (U13 _) = False
uView (U14 _) = False
uView (U15 _) = False
uView (U16 _) = False
uView (U17 _) = False
uView (U18 _) = False
uView (U19 _) = False
uView (U20 _) = False
uView (U21 _) = False
uView (U22 _) = False
uView (U23 _) = False
uView (U24 _) = False
uView (U25 _) = False
uView (U26 _) = True
uView (U27 _) = True
uView (U28 _) = False
uView (U29 _) = True
uView (U30 _) = True
uView (U31 _) = False
uView (U32 _) = False
uView (U33 _) = False
uView (U34 _) = True
uView (U35 _) = False
uView (U36 _) = False
uView (U37 _) = True
uView (U38 _) = False
uView (U39 _) = False
uView (U40 _) = True
uView (U41 _) = False
uView (U42 _) = False
uView (U43 _) = False
uView (U44 _) = True
uView (U45 _) = True
uView (U46 _) = True
uView (U47 _) = False
uView (U48 _) = False
uView (U49 _) = True
uView (U50 _) = False
uView (U51 _) = True
ulens :: forall a . U Univ a => Iso' Univ a
ulens = ulens' Proxy
