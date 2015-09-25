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
    where cIString :: Lens' c CIString
          lens_CIString_unCIString :: forall . Lens' c String
          lens_CIString_unCIString = (.) cIString lens_CIString_unCIString
          {-# INLINE lens_CIString_unCIString #-}
class HasDimension c
    where lens_dimension :: Lens' c Dimension
class HasDouble c
    where lens_double :: Lens' c Double
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
class HasItem c
    where lens_item :: Lens' c Item
          lens_Item_fields :: forall . Lens' c (Map ItemFieldName Markup)
          lens_Item_fields = (.) lens_item lens_Item_fields
          {-# INLINE lens_Item_fields #-}
          lens_Item_images :: forall . Lens' c ReportImages
          lens_Item_images = (.) lens_item lens_Item_images
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName :: forall . Lens' c Text
          lens_Item_itemName = (.) lens_item lens_Item_itemName
          {-# INLINE lens_Item_itemName #-}
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
          lens_Report_reportIntendedUse :: forall . Lens' c
                                                          MaybeReportIntendedUse
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
          lens_Report_reportStatus :: forall . Lens' c ReportStatus
          lens_Report_reportStatus = (.) lens_report lens_Report_reportStatus
          {-# INLINE lens_Report_reportStatus #-}
          lens_Report_reportTitle :: forall . Lens' c Markup
          lens_Report_reportTitle = (.) lens_report lens_Report_reportTitle
          {-# INLINE lens_Report_reportTitle #-}
          lens_Report_reportUUID :: forall . Lens' c UUID
          lens_Report_reportUUID = (.) lens_report lens_Report_reportUUID
          {-# INLINE lens_Report_reportUUID #-}
          lens_Report_reportValueApproachInfo :: forall . Lens' c
                                                                ReportValueApproachInfo
          lens_Report_reportValueApproachInfo = (.) lens_report lens_Report_reportValueApproachInfo
          {-# INLINE lens_Report_reportValueApproachInfo #-}
          lens_Report_reportValueTypeInfo :: forall . Lens' c
                                                            ReportValueTypeInfo
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
          lens_ReportImage_picEditedDeprecated :: forall . Lens' c
                                                                 MaybeImageFile
          lens_ReportImage_picEditedDeprecated = (.) lens_reportImage lens_ReportImage_picEditedDeprecated
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated :: forall . Lens' c
                                                                   MaybeImageFile
          lens_ReportImage_picEnlargedDeprecated = (.) lens_reportImage lens_ReportImage_picEnlargedDeprecated
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImage_picMustEnlarge = (.) lens_reportImage lens_ReportImage_picMustEnlarge
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal :: forall . Lens' c
                                                         (Maybe (Either URI ImageFile))
          lens_ReportImage_picOriginal = (.) lens_reportImage lens_ReportImage_picOriginal
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated :: forall . Lens' c
                                                                  MaybeImageFile
          lens_ReportImage_picPrinterDeprecated = (.) lens_reportImage lens_ReportImage_picPrinterDeprecated
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize :: forall . Lens' c ImageSize
          lens_ReportImage_picSize = (.) lens_reportImage lens_ReportImage_picSize
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated :: forall . Lens' c
                                                                MaybeImageFile
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
          lens_ReportImageView__picEditedDeprecated :: forall . Lens' c
                                                                      MaybeImageFile
          lens_ReportImageView__picEditedDeprecated = (.) lens_reportImageView lens_ReportImageView__picEditedDeprecated
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated :: forall . Lens' c
                                                                        MaybeImageFile
          lens_ReportImageView__picEnlargedDeprecated = (.) lens_reportImageView lens_ReportImageView__picEnlargedDeprecated
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge :: forall . Lens' c Bool
          lens_ReportImageView__picMustEnlarge = (.) lens_reportImageView lens_ReportImageView__picMustEnlarge
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal :: forall . Lens' c
                                                              (Maybe (Either URI ImageFile))
          lens_ReportImageView__picOriginal = (.) lens_reportImageView lens_ReportImageView__picOriginal
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated :: forall . Lens' c
                                                                       MaybeImageFile
          lens_ReportImageView__picPrinterDeprecated = (.) lens_reportImageView lens_ReportImageView__picPrinterDeprecated
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize :: forall . Lens' c SaneSizeImageSize
          lens_ReportImageView__picSize = (.) lens_reportImageView lens_ReportImageView__picSize
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated :: forall . Lens' c
                                                                     MaybeImageFile
          lens_ReportImageView__picThumbDeprecated = (.) lens_reportImageView lens_ReportImageView__picThumbDeprecated
          {-# INLINE lens_ReportImageView__picThumbDeprecated #-}
class HasReportIntendedUse c
    where lens_reportIntendedUse :: Lens' c ReportIntendedUse
class HasReportMap c
    where reportMap :: Lens' c ReportMap
          lens_ReportMap_unReportMap :: forall . Lens' c
                                                       (Map ReportID Report)
          lens_ReportMap_unReportMap = (.) reportMap lens_ReportMap_unReportMap
          {-# INLINE lens_ReportMap_unReportMap #-}
class HasReportStatus c
    where lens_reportStatus :: Lens' c ReportStatus
class HasReportValueApproachInfo c
    where lens_reportValueApproachInfo :: Lens' c
                                                ReportValueApproachInfo
          lens_ReportValueApproachInfo_reportValueApproachDescription :: forall . Lens' c
                                                                                        Markup
          lens_ReportValueApproachInfo_reportValueApproachDescription = (.) lens_reportValueApproachInfo lens_ReportValueApproachInfo_reportValueApproachDescription
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName :: forall . Lens' c
                                                                                 Markup
          lens_ReportValueApproachInfo_reportValueApproachName = (.) lens_reportValueApproachInfo lens_ReportValueApproachInfo_reportValueApproachName
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
class HasReportValueTypeInfo c
    where lens_reportValueTypeInfo :: Lens' c ReportValueTypeInfo
          lens_ReportValueTypeInfo_reportValueTypeDefinition :: forall . Lens' c
                                                                               Markup
          lens_ReportValueTypeInfo_reportValueTypeDefinition = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeDefinition
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription :: forall . Lens' c
                                                                                Markup
          lens_ReportValueTypeInfo_reportValueTypeDescription = (.) lens_reportValueTypeInfo lens_ReportValueTypeInfo_reportValueTypeDescription
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName :: forall . Lens' c
                                                                         Markup
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
          lens_ReportView__reportInspectionLocation :: forall . Lens' c
                                                                      Markup
          lens_ReportView__reportInspectionLocation = (.) lens_reportView lens_ReportView__reportInspectionLocation
          {-# INLINE lens_ReportView__reportInspectionLocation #-}
          lens_ReportView__reportIntendedUse :: forall . Lens' c
                                                               MaybeReportIntendedUse
          lens_ReportView__reportIntendedUse = (.) lens_reportView lens_ReportView__reportIntendedUse
          {-# INLINE lens_ReportView__reportIntendedUse #-}
          lens_ReportView__reportItemsOwner :: forall . Lens' c Markup
          lens_ReportView__reportItemsOwner = (.) lens_reportView lens_ReportView__reportItemsOwner
          {-# INLINE lens_ReportView__reportItemsOwner #-}
          lens_ReportView__reportItemsOwnerFull :: forall . Lens' c Markup
          lens_ReportView__reportItemsOwnerFull = (.) lens_reportView lens_ReportView__reportItemsOwnerFull
          {-# INLINE lens_ReportView__reportItemsOwnerFull #-}
          lens_ReportView__reportLetterOfTransmittal :: forall . Lens' c
                                                                       Markup
          lens_ReportView__reportLetterOfTransmittal = (.) lens_reportView lens_ReportView__reportLetterOfTransmittal
          {-# INLINE lens_ReportView__reportLetterOfTransmittal #-}
          lens_ReportView__reportLimitingConditions :: forall . Lens' c
                                                                      Markups
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
          lens_ReportView__reportStatus :: forall . Lens' c ReportStatus
          lens_ReportView__reportStatus = (.) lens_reportView lens_ReportView__reportStatus
          {-# INLINE lens_ReportView__reportStatus #-}
          lens_ReportView__reportTitle :: forall . Lens' c Markup
          lens_ReportView__reportTitle = (.) lens_reportView lens_ReportView__reportTitle
          {-# INLINE lens_ReportView__reportTitle #-}
          lens_ReportView__reportUUID :: forall . Lens' c UUID
          lens_ReportView__reportUUID = (.) lens_reportView lens_ReportView__reportUUID
          {-# INLINE lens_ReportView__reportUUID #-}
          lens_ReportView__reportValueApproachInfo :: forall . Lens' c
                                                                     ReportValueApproachInfo
          lens_ReportView__reportValueApproachInfo = (.) lens_reportView lens_ReportView__reportValueApproachInfo
          {-# INLINE lens_ReportView__reportValueApproachInfo #-}
          lens_ReportView__reportValueTypeInfo :: forall . Lens' c
                                                                 ReportValueTypeInfo
          lens_ReportView__reportValueTypeInfo = (.) lens_reportView lens_ReportView__reportValueTypeInfo
          {-# INLINE lens_ReportView__reportValueTypeInfo #-}
class HasText c
    where lens_text :: Lens' c Text
class HasUnits c
    where lens_units :: Lens' c Units
instance HasAuthor Author
    where lens_author = id
          lens_Author_authorCredentials f (Author x
                                                  x) = fmap (\y -> Author x y) (f x)
          {-# INLINE lens_Author_authorCredentials #-}
          lens_Author_authorName f (Author x
                                           x) = fmap (\y -> Author y x) (f x)
          {-# INLINE lens_Author_authorName #-}
instance HasBool Bool
    where lens_bool = id
instance HasBranding Branding
    where lens_branding = id
instance HasCIString CIString
    where cIString = id
          lens_CIString_unCIString = iso (\(CIString x) -> x) CIString
          {-# INLINE lens_CIString_unCIString #-}
instance HasDimension Dimension
    where lens_dimension = id
instance HasDouble Double
    where lens_double = id
instance HasImageSize ImageSize
    where lens_imageSize = id
          lens_ImageSize_dim f (ImageSize x
                                          x
                                          x) = fmap (\y -> ImageSize y x x) (f x)
          {-# INLINE lens_ImageSize_dim #-}
          lens_ImageSize_size f (ImageSize x
                                           x
                                           x) = fmap (\y -> ImageSize x y x) (f x)
          {-# INLINE lens_ImageSize_size #-}
          lens_ImageSize_units f (ImageSize x
                                            x
                                            x) = fmap (\y -> ImageSize x x y) (f x)
          {-# INLINE lens_ImageSize_units #-}
instance HasItem Item
    where lens_item = id
          lens_Item_fields f (Item x x x) = fmap (\y -> Item x y x) (f x)
          {-# INLINE lens_Item_fields #-}
          lens_Item_images f (Item x x x) = fmap (\y -> Item x x y) (f x)
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName f (Item x x x) = fmap (\y -> Item y x x) (f x)
          {-# INLINE lens_Item_itemName #-}
instance HasMarkup Markup
    where lens_markup = id
          lens_Markup_htmlText _ (Markdown x) = pure (Markdown x)
          lens_Markup_htmlText f (Html x) = fmap (\y -> Html y) (f x)
          lens_Markup_htmlText _ (LaTeX x) = pure (LaTeX x)
          lens_Markup_htmlText _ (Pandoc x) = pure (Pandoc x)
          lens_Markup_htmlText _ (Markup x) = pure (Markup x)
          {-# INLINE lens_Markup_htmlText #-}
          lens_Markup_markdownText f (Markdown x) = fmap (\y -> Markdown y) (f x)
          lens_Markup_markdownText _ (Html x) = pure (Html x)
          lens_Markup_markdownText _ (LaTeX x) = pure (LaTeX x)
          lens_Markup_markdownText _ (Pandoc x) = pure (Pandoc x)
          lens_Markup_markdownText _ (Markup x) = pure (Markup x)
          {-# INLINE lens_Markup_markdownText #-}
instance HasPermissions Permissions
    where lens_permissions = id
          lens_Permissions_owner f (Permissions x
                                                x
                                                x) = fmap (\y -> Permissions y x x) (f x)
          {-# INLINE lens_Permissions_owner #-}
          lens_Permissions_readers f (Permissions x
                                                  x
                                                  x) = fmap (\y -> Permissions x x y) (f x)
          {-# INLINE lens_Permissions_readers #-}
          lens_Permissions_writers f (Permissions x
                                                  x
                                                  x) = fmap (\y -> Permissions x y x) (f x)
          {-# INLINE lens_Permissions_writers #-}
instance HasReport Report
    where lens_report = id
          lens_Report_reportAbbrevs f (Report x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x) = fmap (\y -> Report x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportAbbrevs #-}
          lens_Report_reportAuthors f (Report x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x) = fmap (\y -> Report x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportAuthors #-}
          lens_Report_reportBody f (Report x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportBody #-}
          lens_Report_reportBranding f (Report x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x) (f x)
          {-# INLINE lens_Report_reportBranding #-}
          lens_Report_reportBriefItems f (Report x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportBriefItems #-}
          lens_Report_reportCertification f (Report x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportCertification #-}
          lens_Report_reportClientAddress f (Report x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportClientAddress #-}
          lens_Report_reportClientGreeting f (Report x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportClientGreeting #-}
          lens_Report_reportClientName f (Report x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportClientName #-}
          lens_Report_reportContractDate f (Report x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x
                                                   x) = fmap (\y -> Report x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportContractDate #-}
          lens_Report_reportCreated f (Report x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x) (f x)
          {-# INLINE lens_Report_reportCreated #-}
          lens_Report_reportDate f (Report x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x) = fmap (\y -> Report x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportDate #-}
          lens_Report_reportDisplayItemName f (Report x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y) (f x)
          {-# INLINE lens_Report_reportDisplayItemName #-}
          lens_Report_reportEffectiveDate f (Report x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> Report x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportEffectiveDate #-}
          lens_Report_reportFlags f (Report x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x) (f x)
          {-# INLINE lens_Report_reportFlags #-}
          lens_Report_reportFolder f (Report x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x) = fmap (\y -> Report y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportFolder #-}
          lens_Report_reportFooter f (Report x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x) = fmap (\y -> Report x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportFooter #-}
          lens_Report_reportGlossary f (Report x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportGlossary #-}
          lens_Report_reportHeader f (Report x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x) = fmap (\y -> Report x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportHeader #-}
          lens_Report_reportInspectionDate f (Report x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> Report x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportInspectionDate #-}
          lens_Report_reportInspectionLocation f (Report x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportInspectionLocation #-}
          lens_Report_reportIntendedUse f (Report x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x) = fmap (\y -> Report x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportIntendedUse #-}
          lens_Report_reportItemsOwner f (Report x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportItemsOwner #-}
          lens_Report_reportItemsOwnerFull f (Report x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportItemsOwnerFull #-}
          lens_Report_reportLetterOfTransmittal f (Report x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportLetterOfTransmittal #-}
          lens_Report_reportLimitingConditions f (Report x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x
                                                         x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportLimitingConditions #-}
          lens_Report_reportName f (Report x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x) = fmap (\y -> Report x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportName #-}
          lens_Report_reportOrderByItemName f (Report x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x) (f x)
          {-# INLINE lens_Report_reportOrderByItemName #-}
          lens_Report_reportPerms f (Report x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPerms #-}
          lens_Report_reportPreparer f (Report x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x) = fmap (\y -> Report x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPreparer #-}
          lens_Report_reportPreparerAddress f (Report x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> Report x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPreparerAddress #-}
          lens_Report_reportPreparerEIN f (Report x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x) = fmap (\y -> Report x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPreparerEIN #-}
          lens_Report_reportPreparerEMail f (Report x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> Report x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPreparerEMail #-}
          lens_Report_reportPreparerWebsite f (Report x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> Report x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPreparerWebsite #-}
          lens_Report_reportPrivacyPolicy f (Report x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportPrivacyPolicy #-}
          lens_Report_reportRedacted f (Report x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x) (f x)
          {-# INLINE lens_Report_reportRedacted #-}
          lens_Report_reportRevision f (Report x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x
                                               x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportRevision #-}
          lens_Report_reportScopeOfWork f (Report x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x
                                                  x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportScopeOfWork #-}
          lens_Report_reportSources f (Report x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportSources #-}
          lens_Report_reportStatus f (Report x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x) (f x)
          {-# INLINE lens_Report_reportStatus #-}
          lens_Report_reportTitle f (Report x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x
                                            x) = fmap (\y -> Report x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportTitle #-}
          lens_Report_reportUUID f (Report x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x
                                           x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x) (f x)
          {-# INLINE lens_Report_reportUUID #-}
          lens_Report_reportValueApproachInfo f (Report x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportValueApproachInfo #-}
          lens_Report_reportValueTypeInfo f (Report x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> Report x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_Report_reportValueTypeInfo #-}
instance HasReportElem ReportElem
    where lens_reportElem = id
          lens_ReportElem_elemItem f (ReportItem x) = fmap (\y -> ReportItem y) (f x)
          lens_ReportElem_elemItem _ (ReportParagraph x) = pure (ReportParagraph x)
          lens_ReportElem_elemItem _ (ReportUndecided) = pure ReportUndecided
          {-# INLINE lens_ReportElem_elemItem #-}
          lens_ReportElem_elemText _ (ReportItem x) = pure (ReportItem x)
          lens_ReportElem_elemText f (ReportParagraph x) = fmap (\y -> ReportParagraph y) (f x)
          lens_ReportElem_elemText _ (ReportUndecided) = pure ReportUndecided
          {-# INLINE lens_ReportElem_elemText #-}
instance HasReportFlags ReportFlags
    where lens_reportFlags = id
          lens_ReportFlags_hideEmptyItemFields = iso (\(ReportFlags x) -> x) ReportFlags
          {-# INLINE lens_ReportFlags_hideEmptyItemFields #-}
instance HasReportImage ReportImage
    where lens_reportImage = id
          lens_ReportImage_picCaption f (Pic x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x
                                             x) = fmap (\y -> Pic x x y x x x x x x) (f x)
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop f (Pic x
                                          x
                                          x
                                          x
                                          x
                                          x
                                          x
                                          x
                                          x) = fmap (\y -> Pic x y x x x x x x x) (f x)
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picEditedDeprecated f (Pic x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> Pic x x x x y x x x x) (f x)
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated f (Pic x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> Pic x x x x x x x x y) (f x)
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge f (Pic x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x
                                                 x) = fmap (\y -> Pic x x x x x x x y x) (f x)
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal f (Pic x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x
                                              x) = fmap (\y -> Pic x x x y x x x x x) (f x)
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated f (Pic x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x) = fmap (\y -> Pic x x x x x x y x x) (f x)
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize f (Pic x
                                          x
                                          x
                                          x
                                          x
                                          x
                                          x
                                          x
                                          x) = fmap (\y -> Pic y x x x x x x x x) (f x)
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated f (Pic x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> Pic x x x x x y x x x) (f x)
          {-# INLINE lens_ReportImage_picThumbDeprecated #-}
instance HasReportImageView ReportImageView
    where lens_reportImageView = id
          lens_ReportImageView__picCaption f (ReportImageView x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x) = fmap (\y -> ReportImageView x x y x x x x x x) (f x)
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop f (ReportImageView x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x) = fmap (\y -> ReportImageView x y x x x x x x x) (f x)
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picEditedDeprecated f (ReportImageView x
                                                                       x
                                                                       x
                                                                       x
                                                                       x
                                                                       x
                                                                       x
                                                                       x
                                                                       x) = fmap (\y -> ReportImageView x x x x y x x x x) (f x)
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated f (ReportImageView x
                                                                         x
                                                                         x
                                                                         x
                                                                         x
                                                                         x
                                                                         x
                                                                         x
                                                                         x) = fmap (\y -> ReportImageView x x x x x x x x y) (f x)
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge f (ReportImageView x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x) = fmap (\y -> ReportImageView x x x x x x x y x) (f x)
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal f (ReportImageView x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x) = fmap (\y -> ReportImageView x x x y x x x x x) (f x)
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated f (ReportImageView x
                                                                        x
                                                                        x
                                                                        x
                                                                        x
                                                                        x
                                                                        x
                                                                        x
                                                                        x) = fmap (\y -> ReportImageView x x x x x x y x x) (f x)
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize f (ReportImageView x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x) = fmap (\y -> ReportImageView y x x x x x x x x) (f x)
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated f (ReportImageView x
                                                                      x
                                                                      x
                                                                      x
                                                                      x
                                                                      x
                                                                      x
                                                                      x
                                                                      x) = fmap (\y -> ReportImageView x x x x x y x x x) (f x)
          {-# INLINE lens_ReportImageView__picThumbDeprecated #-}
instance HasReportIntendedUse ReportIntendedUse
    where lens_reportIntendedUse = id
instance HasReportMap ReportMap
    where reportMap = id
          lens_ReportMap_unReportMap = iso (\(ReportMap x) -> x) ReportMap
          {-# INLINE lens_ReportMap_unReportMap #-}
instance HasReportStatus ReportStatus
    where lens_reportStatus = id
instance HasReportValueApproachInfo ReportValueApproachInfo
    where lens_reportValueApproachInfo = id
          lens_ReportValueApproachInfo_reportValueApproachDescription f (ReportValueApproachInfo x
                                                                                                 x) = fmap (\y -> ReportValueApproachInfo x y) (f x)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName f (ReportValueApproachInfo x
                                                                                          x) = fmap (\y -> ReportValueApproachInfo y x) (f x)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
instance HasReportValueTypeInfo ReportValueTypeInfo
    where lens_reportValueTypeInfo = id
          lens_ReportValueTypeInfo_reportValueTypeDefinition f (ReportValueTypeInfo x
                                                                                    x
                                                                                    x) = fmap (\y -> ReportValueTypeInfo x x y) (f x)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription f (ReportValueTypeInfo x
                                                                                     x
                                                                                     x) = fmap (\y -> ReportValueTypeInfo x y x) (f x)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName f (ReportValueTypeInfo x
                                                                              x
                                                                              x) = fmap (\y -> ReportValueTypeInfo y x x) (f x)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeName #-}
instance HasReportView ReportView
    where lens_reportView = id
          lens_ReportView__reportAbbrevs f (ReportView x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x) = fmap (\y -> ReportView x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportAbbrevs #-}
          lens_ReportView__reportAuthors f (ReportView x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x) = fmap (\y -> ReportView x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportAuthors #-}
          lens_ReportView__reportBody f (ReportView x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportBody #-}
          lens_ReportView__reportBranding f (ReportView x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportBranding #-}
          lens_ReportView__reportBriefItems f (ReportView x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportBriefItems #-}
          lens_ReportView__reportCertification f (ReportView x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportCertification #-}
          lens_ReportView__reportClientAddress f (ReportView x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportClientAddress #-}
          lens_ReportView__reportClientGreeting f (ReportView x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportClientGreeting #-}
          lens_ReportView__reportClientName f (ReportView x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportClientName #-}
          lens_ReportView__reportContractDate f (ReportView x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x
                                                            x) = fmap (\y -> ReportView x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportContractDate #-}
          lens_ReportView__reportCreated f (ReportView x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportCreated #-}
          lens_ReportView__reportDate f (ReportView x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> ReportView x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportDate #-}
          lens_ReportView__reportDisplayItemName f (ReportView x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y) (f x)
          {-# INLINE lens_ReportView__reportDisplayItemName #-}
          lens_ReportView__reportEffectiveDate f (ReportView x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x) = fmap (\y -> ReportView x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportEffectiveDate #-}
          lens_ReportView__reportFlags f (ReportView x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x) (f x)
          {-# INLINE lens_ReportView__reportFlags #-}
          lens_ReportView__reportFolder f (ReportView x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> ReportView y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportFolder #-}
          lens_ReportView__reportFooter f (ReportView x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportFooter #-}
          lens_ReportView__reportGlossary f (ReportView x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportGlossary #-}
          lens_ReportView__reportHeader f (ReportView x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> ReportView x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportHeader #-}
          lens_ReportView__reportInspectionDate f (ReportView x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x) = fmap (\y -> ReportView x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportInspectionDate #-}
          lens_ReportView__reportInspectionLocation f (ReportView x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportInspectionLocation #-}
          lens_ReportView__reportIntendedUse f (ReportView x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportIntendedUse #-}
          lens_ReportView__reportItemsOwner f (ReportView x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x
                                                          x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportItemsOwner #-}
          lens_ReportView__reportItemsOwnerFull f (ReportView x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x
                                                              x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportItemsOwnerFull #-}
          lens_ReportView__reportLetterOfTransmittal f (ReportView x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x
                                                                   x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportLetterOfTransmittal #-}
          lens_ReportView__reportLimitingConditions f (ReportView x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x
                                                                  x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportLimitingConditions #-}
          lens_ReportView__reportName f (ReportView x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> ReportView x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportName #-}
          lens_ReportView__reportOrderByItemName f (ReportView x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x) (f x)
          {-# INLINE lens_ReportView__reportOrderByItemName #-}
          lens_ReportView__reportPerms f (ReportView x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPerms #-}
          lens_ReportView__reportPreparer f (ReportView x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> ReportView x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPreparer #-}
          lens_ReportView__reportPreparerAddress f (ReportView x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x) = fmap (\y -> ReportView x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPreparerAddress #-}
          lens_ReportView__reportPreparerEIN f (ReportView x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x) = fmap (\y -> ReportView x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPreparerEIN #-}
          lens_ReportView__reportPreparerEMail f (ReportView x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x) = fmap (\y -> ReportView x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPreparerEMail #-}
          lens_ReportView__reportPreparerWebsite f (ReportView x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x
                                                               x) = fmap (\y -> ReportView x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPreparerWebsite #-}
          lens_ReportView__reportPrivacyPolicy f (ReportView x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportPrivacyPolicy #-}
          lens_ReportView__reportRedacted f (ReportView x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x) (f x)
          {-# INLINE lens_ReportView__reportRedacted #-}
          lens_ReportView__reportRevision f (ReportView x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x
                                                        x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportRevision #-}
          lens_ReportView__reportScopeOfWork f (ReportView x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x
                                                           x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportScopeOfWork #-}
          lens_ReportView__reportSources f (ReportView x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x
                                                       x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportSources #-}
          lens_ReportView__reportStatus f (ReportView x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x
                                                      x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x x x x) (f x)
          {-# INLINE lens_ReportView__reportStatus #-}
          lens_ReportView__reportTitle f (ReportView x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x
                                                     x) = fmap (\y -> ReportView x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportTitle #-}
          lens_ReportView__reportUUID f (ReportView x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x
                                                    x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x y x x) (f x)
          {-# INLINE lens_ReportView__reportUUID #-}
          lens_ReportView__reportValueApproachInfo f (ReportView x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x
                                                                 x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportValueApproachInfo #-}
          lens_ReportView__reportValueTypeInfo f (ReportView x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x
                                                             x) = fmap (\y -> ReportView x x x x x x x x x x x x x x x x x y x x x x x x x x x x x x x x x x x x x x x x x x x x) (f x)
          {-# INLINE lens_ReportView__reportValueTypeInfo #-}
instance HasText Text
    where lens_text = id
instance HasUnits Units
    where lens_units = id
