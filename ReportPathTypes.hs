data Path_Author a
    = Path_Author_authorName (Path_Markup a)
    | Path_Author_authorCredentials (Path_Markup a)
    | Path_Author
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Bool a
    = Path_Bool_View (Path_String a) | Path_Bool
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Branding a
    = Path_Branding_View (Path_Text a) | Path_Branding
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_CIString a
    = Path_CIString_View (Path_Text a) | Path_CIString
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Dimension a
    = Path_Dimension_View (Path_JSONText a) | Path_Dimension
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Double a
    = Path_Double_View (Path_String a) | Path_Double
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ImageCrop a
    = Path_ImageCrop
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ImageFile a
    = Path_ImageFile
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ImageSize a
    = Path_ImageSize_dim (Path_Dimension a)
    | Path_ImageSize_size (Path_Double a)
    | Path_ImageSize_units (Path_Units a)
    | Path_ImageSize
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Int64 a
    = Path_Int64
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Integer a
    = Path_Integer
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Item a
    = Path_Item_itemName (Path_Text a)
    | Path_Item_fields (Path_Map ItemFieldName (Path_Markup a))
    | Path_Item_images (Path_ReportImages a)
    | Path_Item
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_JSONText a
    = Path_JSONText
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Markup a
    = Path_Markup_markdownText (Path_Text a)
    | Path_Markup_htmlText (Path_Text a)
    | Path_Markup
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_MaybeImageFile a
    = Path_MaybeImageFile_View (Path_String a) | Path_MaybeImageFile
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_MaybeReportIntendedUse a
    = Path_MaybeReportIntendedUse_View (Path_String a)
    | Path_MaybeReportIntendedUse
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Permissions a
    = Path_Permissions_owner (Path_UserId a)
    | Path_Permissions_writers (Path_UserIds a)
    | Path_Permissions_readers (Path_UserIds a)
    | Path_Permissions
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReadOnlyFilePath a
    = Path_ReadOnlyFilePath_View a | Path_ReadOnlyFilePath
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Report a
    = Path_Report_View (Path_ReportView a) | Path_Report
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportElem a
    = Path_ReportElem_elemItem (Path_Item a)
    | Path_ReportElem_elemText (Path_Markup a)
    | Path_ReportElem
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportFlags a
    = Path_ReportFlags_hideEmptyItemFields (Path_Bool a)
    | Path_ReportFlags
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportImage a
    = Path_ReportImage_View (Path_ReportImageView a) | Path_ReportImage
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportImageView a
    = Path_ReportImageView__picSize (Path_SaneSizeImageSize a)
    | Path_ReportImageView__picCrop (Path_ImageCrop a)
    | Path_ReportImageView__picCaption (Path_Markup a)
    | Path_ReportImageView__picOriginal (Path_Maybe (Path_Either (Path_URI a)
                                                                 (Path_ImageFile a)))
    | Path_ReportImageView__picEditedDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView__picThumbDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView__picPrinterDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView__picMustEnlarge (Path_Bool a)
    | Path_ReportImageView__picEnlargedDeprecated (Path_MaybeImageFile a)
    | Path_ReportImageView
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportIntendedUse a
    = Path_ReportIntendedUse_View (Path_String a)
    | Path_ReportIntendedUse
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportMap a
    = Path_ReportMap_unReportMap (Path_Map ReportID (Path_Report a))
    | Path_ReportMap
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportStatus a
    = Path_ReportStatus_View (Path_String a) | Path_ReportStatus
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportValueApproachInfo a
    = Path_ReportValueApproachInfo_reportValueApproachName (Path_Markup a)
    | Path_ReportValueApproachInfo_reportValueApproachDescription (Path_Markup a)
    | Path_ReportValueApproachInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportValueTypeInfo a
    = Path_ReportValueTypeInfo_reportValueTypeName (Path_Markup a)
    | Path_ReportValueTypeInfo_reportValueTypeDescription (Path_Markup a)
    | Path_ReportValueTypeInfo_reportValueTypeDefinition (Path_Markup a)
    | Path_ReportValueTypeInfo
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_ReportView a
    = Path_ReportView__reportFolder (Path_ReadOnlyFilePath a)
    | Path_ReportView__reportName (Path_Markup a)
    | Path_ReportView__reportDate (Path_Markup a)
    | Path_ReportView__reportContractDate (Path_Markup a)
    | Path_ReportView__reportInspectionDate (Path_Markup a)
    | Path_ReportView__reportEffectiveDate (Path_Markup a)
    | Path_ReportView__reportAuthors (Path_Authors a)
    | Path_ReportView__reportPreparer (Path_Markup a)
    | Path_ReportView__reportPreparerEIN (Path_Markup a)
    | Path_ReportView__reportPreparerAddress (Path_Markup a)
    | Path_ReportView__reportPreparerEMail (Path_Markup a)
    | Path_ReportView__reportPreparerWebsite (Path_Markup a)
    | Path_ReportView__reportAbbrevs (Path_AbbrevPairs a)
    | Path_ReportView__reportTitle (Path_Markup a)
    | Path_ReportView__reportHeader (Path_Markup a)
    | Path_ReportView__reportFooter (Path_Markup a)
    | Path_ReportView__reportIntendedUse (Path_MaybeReportIntendedUse a)
    | Path_ReportView__reportValueTypeInfo (Path_ReportValueTypeInfo a)
    | Path_ReportView__reportValueApproachInfo (Path_ReportValueApproachInfo a)
    | Path_ReportView__reportClientName (Path_Markup a)
    | Path_ReportView__reportClientAddress (Path_Markup a)
    | Path_ReportView__reportClientGreeting (Path_Markup a)
    | Path_ReportView__reportItemsOwnerFull (Path_Markup a)
    | Path_ReportView__reportItemsOwner (Path_Markup a)
    | Path_ReportView__reportBriefItems (Path_Markup a)
    | Path_ReportView__reportInspectionLocation (Path_Markup a)
    | Path_ReportView__reportBody (Path_ReportElems a)
    | Path_ReportView__reportGlossary (Path_MarkupPairs a)
    | Path_ReportView__reportSources (Path_MarkupPairs a)
    | Path_ReportView__reportLetterOfTransmittal (Path_Markup a)
    | Path_ReportView__reportScopeOfWork (Path_Markup a)
    | Path_ReportView__reportCertification (Path_Markups a)
    | Path_ReportView__reportLimitingConditions (Path_Markups a)
    | Path_ReportView__reportPrivacyPolicy (Path_Markup a)
    | Path_ReportView__reportPerms (Path_Permissions a)
    | Path_ReportView__reportRevision (Path_Integer a)
    | Path_ReportView__reportCreated (Path_EpochMilli a)
    | Path_ReportView__reportBranding (Path_Branding a)
    | Path_ReportView__reportStatus (Path_ReportStatus a)
    | Path_ReportView__reportRedacted (Path_Bool a)
    | Path_ReportView__reportFlags (Path_ReportFlags a)
    | Path_ReportView__reportUUID (Path_UUID a)
    | Path_ReportView__reportOrderByItemName (Path_Bool a)
    | Path_ReportView__reportDisplayItemName (Path_Bool a)
    | Path_ReportView
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_SaneSizeImageSize a
    = Path_SaneSizeImageSize_View (Path_ImageSize a)
    | Path_SaneSizeImageSize
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_String a
    = Path_String_View (Path_JSONText a) | Path_String
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Text a
    = Path_Text_View (Path_JSONText a) | Path_Text
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_URI a
    = Path_URI
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_UUID a
    = Path_UUID
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Units a
    = Path_Units_View (Path_JSONText a) | Path_Units
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_UserId a
    = Path_UserId
    deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_UserIds a
    = Path_UserIds_View (Path_Text a) | Path_UserIds
    deriving (Eq, Ord, Read, Show, Typeable, Data)
instance IdPath (Path_Author a)
    where idPath = Path_Author
instance IdPath (Path_Bool a)
    where idPath = Path_Bool
instance IdPath (Path_Branding a)
    where idPath = Path_Branding
instance IdPath (Path_CIString a)
    where idPath = Path_CIString
instance IdPath (Path_Dimension a)
    where idPath = Path_Dimension
instance IdPath (Path_Double a)
    where idPath = Path_Double
instance IdPath (Path_ImageCrop a)
    where idPath = Path_ImageCrop
instance IdPath (Path_ImageFile a)
    where idPath = Path_ImageFile
instance IdPath (Path_ImageSize a)
    where idPath = Path_ImageSize
instance IdPath (Path_Int64 a)
    where idPath = Path_Int64
instance IdPath (Path_Integer a)
    where idPath = Path_Integer
instance IdPath (Path_Item a)
    where idPath = Path_Item
instance IdPath (Path_JSONText a)
    where idPath = Path_JSONText
instance IdPath (Path_Markup a)
    where idPath = Path_Markup
instance IdPath (Path_MaybeImageFile a)
    where idPath = Path_MaybeImageFile
instance IdPath (Path_MaybeReportIntendedUse a)
    where idPath = Path_MaybeReportIntendedUse
instance IdPath (Path_Permissions a)
    where idPath = Path_Permissions
instance IdPath (Path_ReadOnlyFilePath a)
    where idPath = Path_ReadOnlyFilePath
instance IdPath (Path_Report a)
    where idPath = Path_Report
instance IdPath (Path_ReportElem a)
    where idPath = Path_ReportElem
instance IdPath (Path_ReportFlags a)
    where idPath = Path_ReportFlags
instance IdPath (Path_ReportImage a)
    where idPath = Path_ReportImage
instance IdPath (Path_ReportImageView a)
    where idPath = Path_ReportImageView
instance IdPath (Path_ReportIntendedUse a)
    where idPath = Path_ReportIntendedUse
instance IdPath (Path_ReportMap a)
    where idPath = Path_ReportMap
instance IdPath (Path_ReportStatus a)
    where idPath = Path_ReportStatus
instance IdPath (Path_ReportValueApproachInfo a)
    where idPath = Path_ReportValueApproachInfo
instance IdPath (Path_ReportValueTypeInfo a)
    where idPath = Path_ReportValueTypeInfo
instance IdPath (Path_ReportView a)
    where idPath = Path_ReportView
instance IdPath (Path_SaneSizeImageSize a)
    where idPath = Path_SaneSizeImageSize
instance IdPath (Path_String a)
    where idPath = Path_String
instance IdPath (Path_Text a)
    where idPath = Path_Text
instance IdPath (Path_URI a)
    where idPath = Path_URI
instance IdPath (Path_UUID a)
    where idPath = Path_UUID
instance IdPath (Path_Units a)
    where idPath = Path_Units
instance IdPath (Path_UserId a)
    where idPath = Path_UserId
instance IdPath (Path_UserIds a)
    where idPath = Path_UserIds
type Path_AbbrevPair a = Path_Pair (Path_CIString a)
                                   (Path_Markup a)
type Path_AbbrevPairs a = Path_OMap AbbrevPairID
                                    (Path_Pair (Path_CIString a) (Path_Markup a))
type Path_Authors a = Path_OMap AuthorID (Path_Author a)
type Path_Checksum a = Path_String a
type Path_EpochMilli a = Path_Int64 a
type Path_FilePath a = Path_String a
type Path_MarkupPair a = Path_Pair (Path_Markup a) (Path_Markup a)
type Path_MarkupPairs a = Path_OMap MarkupPairID
                                    (Path_Pair (Path_Markup a) (Path_Markup a))
type Path_Markups a = Path_OMap MarkupID (Path_Markup a)
type Path_ReportElems a = Path_OMap ReportElemID
                                    (Path_ReportElem a)
type Path_ReportImages a = Path_OMap ReportImageID
                                     (Path_ReportImage a)
