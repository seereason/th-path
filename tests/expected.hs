data LE_AbbrevPair
    = LE_AbbrevPair_JSONText (Path_AbbrevPair JSONText) JSONText
    | LE_AbbrevPair_Markup (Path_AbbrevPair Markup) Markup
    | LE_AbbrevPair_AbbrevPair (Path_AbbrevPair ((CIString, Markup)))
                               ((CIString, Markup))
    | LE_AbbrevPair_CIString (Path_AbbrevPair CIString) CIString
    | LE_AbbrevPair_Text (Path_AbbrevPair Text) Text
data LE_AbbrevPairs
    = LE_AbbrevPairs_JSONText (Path_AbbrevPairs JSONText) JSONText
    | LE_AbbrevPairs_Markup (Path_AbbrevPairs Markup) Markup
    | LE_AbbrevPairs_AbbrevPair (Path_AbbrevPairs ((CIString, Markup)))
                                ((CIString, Markup))
    | LE_AbbrevPairs_AbbrevPairs (Path_AbbrevPairs (Order AbbrevPairID
                                                          ((CIString, Markup))))
                                 (Order AbbrevPairID ((CIString, Markup)))
    | LE_AbbrevPairs_CIString (Path_AbbrevPairs CIString) CIString
    | LE_AbbrevPairs_Text (Path_AbbrevPairs Text) Text
data LE_Author
    = LE_Author_JSONText (Path_Author JSONText) JSONText
    | LE_Author_Markup (Path_Author Markup) Markup
    | LE_Author_Author (Path_Author Author) Author
    | LE_Author_Text (Path_Author Text) Text
data LE_Authors
    = LE_Authors_JSONText (Path_Authors JSONText) JSONText
    | LE_Authors_Markup (Path_Authors Markup) Markup
    | LE_Authors_Author (Path_Authors Author) Author
    | LE_Authors_Authors (Path_Authors (Order AuthorID Author))
                         (Order AuthorID Author)
    | LE_Authors_Text (Path_Authors Text) Text
data LE_Bool
    = LE_Bool_String (Path_Bool ([Char])) ([Char])
    | LE_Bool_Bool (Path_Bool Bool) Bool
    | LE_Bool_JSONText (Path_Bool JSONText) JSONText
data LE_Branding
    = LE_Branding_JSONText (Path_Branding JSONText) JSONText
    | LE_Branding_Branding (Path_Branding Branding) Branding
    | LE_Branding_Text (Path_Branding Text) Text
data LE_CIString
    = LE_CIString_JSONText (Path_CIString JSONText) JSONText
    | LE_CIString_CIString (Path_CIString CIString) CIString
    | LE_CIString_Text (Path_CIString Text) Text
data LE_Dimension
    = LE_Dimension_Dimension (Path_Dimension Dimension) Dimension
    | LE_Dimension_JSONText (Path_Dimension JSONText) JSONText
data LE_Double
    = LE_Double_String (Path_Double ([Char])) ([Char])
    | LE_Double_Double (Path_Double Double) Double
    | LE_Double_JSONText (Path_Double JSONText) JSONText
data LE_ImageCrop
    = LE_ImageCrop_ImageCrop (Path_ImageCrop ImageCrop) ImageCrop
data LE_ImageFile
    = LE_ImageFile_ImageFile (Path_ImageFile ImageFile) ImageFile
data LE_ImageSize
    = LE_ImageSize_String (Path_ImageSize ([Char])) ([Char])
    | LE_ImageSize_Double (Path_ImageSize Double) Double
    | LE_ImageSize_Dimension (Path_ImageSize Dimension) Dimension
    | LE_ImageSize_ImageSize (Path_ImageSize ImageSize) ImageSize
    | LE_ImageSize_Units (Path_ImageSize Units) Units
    | LE_ImageSize_JSONText (Path_ImageSize JSONText) JSONText
data LE_Int64 = LE_Int64_Int64 (Path_Int64 Int64) Int64
data LE_Integer = LE_Integer_Integer (Path_Integer Integer) Integer
data LE_Item
    = LE_Item_String (Path_Item ([Char])) ([Char])
    | LE_Item_Bool (Path_Item Bool) Bool
    | LE_Item_Double (Path_Item Double) Double
    | LE_Item_Dimension (Path_Item Dimension) Dimension
    | LE_Item_ImageCrop (Path_Item ImageCrop) ImageCrop
    | LE_Item_ImageSize (Path_Item ImageSize) ImageSize
    | LE_Item_Units (Path_Item Units) Units
    | LE_Item_ImageFile (Path_Item ImageFile) ImageFile
    | LE_Item_JSONText (Path_Item JSONText) JSONText
    | LE_Item_Markup (Path_Item Markup) Markup
    | LE_Item_MaybeImageFile (Path_Item (Maybe ImageFile))
                             (Maybe ImageFile)
    | LE_Item_ReportImage (Path_Item ReportImage) ReportImage
    | LE_Item_ReportImages (Path_Item (Order ReportImageID
                                             ReportImage))
                           (Order ReportImageID ReportImage)
    | LE_Item_ReportImageView (Path_Item ReportImageView)
                              ReportImageView
    | LE_Item_SaneSizeImageSize (Path_Item (SaneSize ImageSize))
                                (SaneSize ImageSize)
    | LE_Item_Item (Path_Item Item) Item
    | LE_Item_URI (Path_Item URI) URI
    | LE_Item_Text (Path_Item Text) Text
data LE_JSONText
    = LE_JSONText_JSONText (Path_JSONText JSONText) JSONText
data LE_Markup
    = LE_Markup_JSONText (Path_Markup JSONText) JSONText
    | LE_Markup_Markup (Path_Markup Markup) Markup
    | LE_Markup_Text (Path_Markup Text) Text
data LE_MarkupPair
    = LE_MarkupPair_JSONText (Path_MarkupPair JSONText) JSONText
    | LE_MarkupPair_Markup (Path_MarkupPair Markup) Markup
    | LE_MarkupPair_MarkupPair (Path_MarkupPair ((Markup, Markup)))
                               ((Markup, Markup))
    | LE_MarkupPair_Text (Path_MarkupPair Text) Text
data LE_MarkupPairs
    = LE_MarkupPairs_JSONText (Path_MarkupPairs JSONText) JSONText
    | LE_MarkupPairs_Markup (Path_MarkupPairs Markup) Markup
    | LE_MarkupPairs_MarkupPair (Path_MarkupPairs ((Markup, Markup)))
                                ((Markup, Markup))
    | LE_MarkupPairs_MarkupPairs (Path_MarkupPairs (Order MarkupPairID
                                                          ((Markup, Markup))))
                                 (Order MarkupPairID ((Markup, Markup)))
    | LE_MarkupPairs_Text (Path_MarkupPairs Text) Text
data LE_Markups
    = LE_Markups_JSONText (Path_Markups JSONText) JSONText
    | LE_Markups_Markup (Path_Markups Markup) Markup
    | LE_Markups_Markups (Path_Markups (Order MarkupID Markup))
                         (Order MarkupID Markup)
    | LE_Markups_Text (Path_Markups Text) Text
data LE_MaybeImageFile
    = LE_MaybeImageFile_String (Path_MaybeImageFile ([Char])) ([Char])
    | LE_MaybeImageFile_JSONText (Path_MaybeImageFile JSONText)
                                 JSONText
    | LE_MaybeImageFile_MaybeImageFile (Path_MaybeImageFile (Maybe ImageFile))
                                       (Maybe ImageFile)
data LE_MaybeReportIntendedUse
    = LE_MaybeReportIntendedUse_String (Path_MaybeReportIntendedUse ([Char]))
                                       ([Char])
    | LE_MaybeReportIntendedUse_JSONText (Path_MaybeReportIntendedUse JSONText)
                                         JSONText
    | LE_MaybeReportIntendedUse_MaybeReportIntendedUse (Path_MaybeReportIntendedUse (Maybe ReportIntendedUse))
                                                       (Maybe ReportIntendedUse)
data LE_Permissions
    = LE_Permissions_JSONText (Path_Permissions JSONText) JSONText
    | LE_Permissions_Permissions (Path_Permissions Permissions)
                                 Permissions
    | LE_Permissions_UserIds (Path_Permissions ([UserId])) ([UserId])
    | LE_Permissions_Text (Path_Permissions Text) Text
    | LE_Permissions_UserId (Path_Permissions UserId) UserId
data LE_ReadOnlyFilePath
    = LE_ReadOnlyFilePath_ReadOnlyFilePath (Path_ReadOnlyFilePath (ReadOnly ([Char])))
                                           (ReadOnly ([Char]))
data LE_Report
    = LE_Report_String (Path_Report ([Char])) ([Char])
    | LE_Report_Int64 (Path_Report Int64) Int64
    | LE_Report_Size (Path_Report Int) Int
    | LE_Report_Bool (Path_Report Bool) Bool
    | LE_Report_Double (Path_Report Double) Double
    | LE_Report_Dimension (Path_Report Dimension) Dimension
    | LE_Report_ImageCrop (Path_Report ImageCrop) ImageCrop
    | LE_Report_ImageSize (Path_Report ImageSize) ImageSize
    | LE_Report_Units (Path_Report Units) Units
    | LE_Report_ImageFile (Path_Report ImageFile) ImageFile
    | LE_Report_Integer (Path_Report Integer) Integer
    | LE_Report_JSONText (Path_Report JSONText) JSONText
    | LE_Report_Markup (Path_Report Markup) Markup
    | LE_Report_Permissions (Path_Report Permissions) Permissions
    | LE_Report_UserIds (Path_Report ([UserId])) ([UserId])
    | LE_Report_AbbrevPair (Path_Report ((CIString, Markup)))
                           ((CIString, Markup))
    | LE_Report_AbbrevPairs (Path_Report (Order AbbrevPairID
                                                ((CIString, Markup))))
                            (Order AbbrevPairID ((CIString, Markup)))
    | LE_Report_Author (Path_Report Author) Author
    | LE_Report_Authors (Path_Report (Order AuthorID Author))
                        (Order AuthorID Author)
    | LE_Report_Branding (Path_Report Branding) Branding
    | LE_Report_MarkupPair (Path_Report ((Markup, Markup)))
                           ((Markup, Markup))
    | LE_Report_MarkupPairs (Path_Report (Order MarkupPairID
                                                ((Markup, Markup))))
                            (Order MarkupPairID ((Markup, Markup)))
    | LE_Report_Markups (Path_Report (Order MarkupID Markup))
                        (Order MarkupID Markup)
    | LE_Report_MaybeReportIntendedUse (Path_Report (Maybe ReportIntendedUse))
                                       (Maybe ReportIntendedUse)
    | LE_Report_Report (Path_Report Report) Report
    | LE_Report_ReportElem (Path_Report ReportElem) ReportElem
    | LE_Report_ReportElems (Path_Report (Order ReportElemID
                                                ReportElem))
                            (Order ReportElemID ReportElem)
    | LE_Report_ReportFlags (Path_Report ReportFlags) ReportFlags
    | LE_Report_ReportStandard (Path_Report ReportStandard)
                               ReportStandard
    | LE_Report_ReportStatus (Path_Report ReportStatus) ReportStatus
    | LE_Report_ReportValueApproachInfo (Path_Report ReportValueApproachInfo)
                                        ReportValueApproachInfo
    | LE_Report_ReportValueTypeInfo (Path_Report ReportValueTypeInfo)
                                    ReportValueTypeInfo
    | LE_Report_MaybeImageFile (Path_Report (Maybe ImageFile))
                               (Maybe ImageFile)
    | LE_Report_ReportImage (Path_Report ReportImage) ReportImage
    | LE_Report_ReportImages (Path_Report (Order ReportImageID
                                                 ReportImage))
                             (Order ReportImageID ReportImage)
    | LE_Report_ReadOnlyFilePath (Path_Report (ReadOnly ([Char])))
                                 (ReadOnly ([Char]))
    | LE_Report_ReportImageView (Path_Report ReportImageView)
                                ReportImageView
    | LE_Report_ReportView (Path_Report ReportView) ReportView
    | LE_Report_SaneSizeImageSize (Path_Report (SaneSize ImageSize))
                                  (SaneSize ImageSize)
    | LE_Report_Item (Path_Report Item) Item
    | LE_Report_CIString (Path_Report CIString) CIString
    | LE_Report_URI (Path_Report URI) URI
    | LE_Report_Text (Path_Report Text) Text
    | LE_Report_UserId (Path_Report UserId) UserId
    | LE_Report_UUID (Path_Report UUID) UUID
data LE_ReportElem
    = LE_ReportElem_String (Path_ReportElem ([Char])) ([Char])
    | LE_ReportElem_Bool (Path_ReportElem Bool) Bool
    | LE_ReportElem_Double (Path_ReportElem Double) Double
    | LE_ReportElem_Dimension (Path_ReportElem Dimension) Dimension
    | LE_ReportElem_ImageCrop (Path_ReportElem ImageCrop) ImageCrop
    | LE_ReportElem_ImageSize (Path_ReportElem ImageSize) ImageSize
    | LE_ReportElem_Units (Path_ReportElem Units) Units
    | LE_ReportElem_ImageFile (Path_ReportElem ImageFile) ImageFile
    | LE_ReportElem_JSONText (Path_ReportElem JSONText) JSONText
    | LE_ReportElem_Markup (Path_ReportElem Markup) Markup
    | LE_ReportElem_ReportElem (Path_ReportElem ReportElem) ReportElem
    | LE_ReportElem_MaybeImageFile (Path_ReportElem (Maybe ImageFile))
                                   (Maybe ImageFile)
    | LE_ReportElem_ReportImage (Path_ReportElem ReportImage)
                                ReportImage
    | LE_ReportElem_ReportImages (Path_ReportElem (Order ReportImageID
                                                         ReportImage))
                                 (Order ReportImageID ReportImage)
    | LE_ReportElem_ReportImageView (Path_ReportElem ReportImageView)
                                    ReportImageView
    | LE_ReportElem_SaneSizeImageSize (Path_ReportElem (SaneSize ImageSize))
                                      (SaneSize ImageSize)
    | LE_ReportElem_Item (Path_ReportElem Item) Item
    | LE_ReportElem_URI (Path_ReportElem URI) URI
    | LE_ReportElem_Text (Path_ReportElem Text) Text
data LE_ReportElems
    = LE_ReportElems_String (Path_ReportElems ([Char])) ([Char])
    | LE_ReportElems_Bool (Path_ReportElems Bool) Bool
    | LE_ReportElems_Double (Path_ReportElems Double) Double
    | LE_ReportElems_Dimension (Path_ReportElems Dimension) Dimension
    | LE_ReportElems_ImageCrop (Path_ReportElems ImageCrop) ImageCrop
    | LE_ReportElems_ImageSize (Path_ReportElems ImageSize) ImageSize
    | LE_ReportElems_Units (Path_ReportElems Units) Units
    | LE_ReportElems_ImageFile (Path_ReportElems ImageFile) ImageFile
    | LE_ReportElems_JSONText (Path_ReportElems JSONText) JSONText
    | LE_ReportElems_Markup (Path_ReportElems Markup) Markup
    | LE_ReportElems_ReportElem (Path_ReportElems ReportElem)
                                ReportElem
    | LE_ReportElems_ReportElems (Path_ReportElems (Order ReportElemID
                                                          ReportElem))
                                 (Order ReportElemID ReportElem)
    | LE_ReportElems_MaybeImageFile (Path_ReportElems (Maybe ImageFile))
                                    (Maybe ImageFile)
    | LE_ReportElems_ReportImage (Path_ReportElems ReportImage)
                                 ReportImage
    | LE_ReportElems_ReportImages (Path_ReportElems (Order ReportImageID
                                                           ReportImage))
                                  (Order ReportImageID ReportImage)
    | LE_ReportElems_ReportImageView (Path_ReportElems ReportImageView)
                                     ReportImageView
    | LE_ReportElems_SaneSizeImageSize (Path_ReportElems (SaneSize ImageSize))
                                       (SaneSize ImageSize)
    | LE_ReportElems_Item (Path_ReportElems Item) Item
    | LE_ReportElems_URI (Path_ReportElems URI) URI
    | LE_ReportElems_Text (Path_ReportElems Text) Text
data LE_ReportFlags
    = LE_ReportFlags_String (Path_ReportFlags ([Char])) ([Char])
    | LE_ReportFlags_Bool (Path_ReportFlags Bool) Bool
    | LE_ReportFlags_JSONText (Path_ReportFlags JSONText) JSONText
    | LE_ReportFlags_ReportFlags (Path_ReportFlags ReportFlags)
                                 ReportFlags
data LE_ReportImage
    = LE_ReportImage_String (Path_ReportImage ([Char])) ([Char])
    | LE_ReportImage_Bool (Path_ReportImage Bool) Bool
    | LE_ReportImage_Double (Path_ReportImage Double) Double
    | LE_ReportImage_Dimension (Path_ReportImage Dimension) Dimension
    | LE_ReportImage_ImageCrop (Path_ReportImage ImageCrop) ImageCrop
    | LE_ReportImage_ImageSize (Path_ReportImage ImageSize) ImageSize
    | LE_ReportImage_Units (Path_ReportImage Units) Units
    | LE_ReportImage_ImageFile (Path_ReportImage ImageFile) ImageFile
    | LE_ReportImage_JSONText (Path_ReportImage JSONText) JSONText
    | LE_ReportImage_Markup (Path_ReportImage Markup) Markup
    | LE_ReportImage_MaybeImageFile (Path_ReportImage (Maybe ImageFile))
                                    (Maybe ImageFile)
    | LE_ReportImage_ReportImage (Path_ReportImage ReportImage)
                                 ReportImage
    | LE_ReportImage_ReportImageView (Path_ReportImage ReportImageView)
                                     ReportImageView
    | LE_ReportImage_SaneSizeImageSize (Path_ReportImage (SaneSize ImageSize))
                                       (SaneSize ImageSize)
    | LE_ReportImage_URI (Path_ReportImage URI) URI
    | LE_ReportImage_Text (Path_ReportImage Text) Text
data LE_ReportImageView
    = LE_ReportImageView_String (Path_ReportImageView ([Char]))
                                ([Char])
    | LE_ReportImageView_Bool (Path_ReportImageView Bool) Bool
    | LE_ReportImageView_Double (Path_ReportImageView Double) Double
    | LE_ReportImageView_Dimension (Path_ReportImageView Dimension)
                                   Dimension
    | LE_ReportImageView_ImageCrop (Path_ReportImageView ImageCrop)
                                   ImageCrop
    | LE_ReportImageView_ImageSize (Path_ReportImageView ImageSize)
                                   ImageSize
    | LE_ReportImageView_Units (Path_ReportImageView Units) Units
    | LE_ReportImageView_ImageFile (Path_ReportImageView ImageFile)
                                   ImageFile
    | LE_ReportImageView_JSONText (Path_ReportImageView JSONText)
                                  JSONText
    | LE_ReportImageView_Markup (Path_ReportImageView Markup) Markup
    | LE_ReportImageView_MaybeImageFile (Path_ReportImageView (Maybe ImageFile))
                                        (Maybe ImageFile)
    | LE_ReportImageView_ReportImageView (Path_ReportImageView ReportImageView)
                                         ReportImageView
    | LE_ReportImageView_SaneSizeImageSize (Path_ReportImageView (SaneSize ImageSize))
                                           (SaneSize ImageSize)
    | LE_ReportImageView_URI (Path_ReportImageView URI) URI
    | LE_ReportImageView_Text (Path_ReportImageView Text) Text
data LE_ReportImages
    = LE_ReportImages_String (Path_ReportImages ([Char])) ([Char])
    | LE_ReportImages_Bool (Path_ReportImages Bool) Bool
    | LE_ReportImages_Double (Path_ReportImages Double) Double
    | LE_ReportImages_Dimension (Path_ReportImages Dimension) Dimension
    | LE_ReportImages_ImageCrop (Path_ReportImages ImageCrop) ImageCrop
    | LE_ReportImages_ImageSize (Path_ReportImages ImageSize) ImageSize
    | LE_ReportImages_Units (Path_ReportImages Units) Units
    | LE_ReportImages_ImageFile (Path_ReportImages ImageFile) ImageFile
    | LE_ReportImages_JSONText (Path_ReportImages JSONText) JSONText
    | LE_ReportImages_Markup (Path_ReportImages Markup) Markup
    | LE_ReportImages_MaybeImageFile (Path_ReportImages (Maybe ImageFile))
                                     (Maybe ImageFile)
    | LE_ReportImages_ReportImage (Path_ReportImages ReportImage)
                                  ReportImage
    | LE_ReportImages_ReportImages (Path_ReportImages (Order ReportImageID
                                                             ReportImage))
                                   (Order ReportImageID ReportImage)
    | LE_ReportImages_ReportImageView (Path_ReportImages ReportImageView)
                                      ReportImageView
    | LE_ReportImages_SaneSizeImageSize (Path_ReportImages (SaneSize ImageSize))
                                        (SaneSize ImageSize)
    | LE_ReportImages_URI (Path_ReportImages URI) URI
    | LE_ReportImages_Text (Path_ReportImages Text) Text
data LE_ReportIntendedUse
    = LE_ReportIntendedUse_String (Path_ReportIntendedUse ([Char]))
                                  ([Char])
    | LE_ReportIntendedUse_JSONText (Path_ReportIntendedUse JSONText)
                                    JSONText
    | LE_ReportIntendedUse_ReportIntendedUse (Path_ReportIntendedUse ReportIntendedUse)
                                             ReportIntendedUse
data LE_ReportMap
    = LE_ReportMap_String (Path_ReportMap ([Char])) ([Char])
    | LE_ReportMap_Int64 (Path_ReportMap Int64) Int64
    | LE_ReportMap_Size (Path_ReportMap Int) Int
    | LE_ReportMap_Bool (Path_ReportMap Bool) Bool
    | LE_ReportMap_Double (Path_ReportMap Double) Double
    | LE_ReportMap_Dimension (Path_ReportMap Dimension) Dimension
    | LE_ReportMap_ImageCrop (Path_ReportMap ImageCrop) ImageCrop
    | LE_ReportMap_ImageSize (Path_ReportMap ImageSize) ImageSize
    | LE_ReportMap_Units (Path_ReportMap Units) Units
    | LE_ReportMap_ImageFile (Path_ReportMap ImageFile) ImageFile
    | LE_ReportMap_Integer (Path_ReportMap Integer) Integer
    | LE_ReportMap_JSONText (Path_ReportMap JSONText) JSONText
    | LE_ReportMap_Markup (Path_ReportMap Markup) Markup
    | LE_ReportMap_Permissions (Path_ReportMap Permissions) Permissions
    | LE_ReportMap_UserIds (Path_ReportMap ([UserId])) ([UserId])
    | LE_ReportMap_AbbrevPair (Path_ReportMap ((CIString, Markup)))
                              ((CIString, Markup))
    | LE_ReportMap_AbbrevPairs (Path_ReportMap (Order AbbrevPairID
                                                      ((CIString, Markup))))
                               (Order AbbrevPairID ((CIString, Markup)))
    | LE_ReportMap_Author (Path_ReportMap Author) Author
    | LE_ReportMap_Authors (Path_ReportMap (Order AuthorID Author))
                           (Order AuthorID Author)
    | LE_ReportMap_Branding (Path_ReportMap Branding) Branding
    | LE_ReportMap_MarkupPair (Path_ReportMap ((Markup, Markup)))
                              ((Markup, Markup))
    | LE_ReportMap_MarkupPairs (Path_ReportMap (Order MarkupPairID
                                                      ((Markup, Markup))))
                               (Order MarkupPairID ((Markup, Markup)))
    | LE_ReportMap_Markups (Path_ReportMap (Order MarkupID Markup))
                           (Order MarkupID Markup)
    | LE_ReportMap_MaybeReportIntendedUse (Path_ReportMap (Maybe ReportIntendedUse))
                                          (Maybe ReportIntendedUse)
    | LE_ReportMap_Report (Path_ReportMap Report) Report
    | LE_ReportMap_ReportElem (Path_ReportMap ReportElem) ReportElem
    | LE_ReportMap_ReportElems (Path_ReportMap (Order ReportElemID
                                                      ReportElem))
                               (Order ReportElemID ReportElem)
    | LE_ReportMap_ReportFlags (Path_ReportMap ReportFlags) ReportFlags
    | LE_ReportMap_ReportStandard (Path_ReportMap ReportStandard)
                                  ReportStandard
    | LE_ReportMap_ReportStatus (Path_ReportMap ReportStatus)
                                ReportStatus
    | LE_ReportMap_ReportValueApproachInfo (Path_ReportMap ReportValueApproachInfo)
                                           ReportValueApproachInfo
    | LE_ReportMap_ReportValueTypeInfo (Path_ReportMap ReportValueTypeInfo)
                                       ReportValueTypeInfo
    | LE_ReportMap_MaybeImageFile (Path_ReportMap (Maybe ImageFile))
                                  (Maybe ImageFile)
    | LE_ReportMap_ReportImage (Path_ReportMap ReportImage) ReportImage
    | LE_ReportMap_ReportImages (Path_ReportMap (Order ReportImageID
                                                       ReportImage))
                                (Order ReportImageID ReportImage)
    | LE_ReportMap_ReadOnlyFilePath (Path_ReportMap (ReadOnly ([Char])))
                                    (ReadOnly ([Char]))
    | LE_ReportMap_ReportImageView (Path_ReportMap ReportImageView)
                                   ReportImageView
    | LE_ReportMap_ReportView (Path_ReportMap ReportView) ReportView
    | LE_ReportMap_SaneSizeImageSize (Path_ReportMap (SaneSize ImageSize))
                                     (SaneSize ImageSize)
    | LE_ReportMap_Item (Path_ReportMap Item) Item
    | LE_ReportMap_ReportMap (Path_ReportMap ReportMap) ReportMap
    | LE_ReportMap_CIString (Path_ReportMap CIString) CIString
    | LE_ReportMap_URI (Path_ReportMap URI) URI
    | LE_ReportMap_Text (Path_ReportMap Text) Text
    | LE_ReportMap_UserId (Path_ReportMap UserId) UserId
    | LE_ReportMap_UUID (Path_ReportMap UUID) UUID
data LE_ReportStandard
    = LE_ReportStandard_Size (Path_ReportStandard Int) Int
    | LE_ReportStandard_ReportStandard (Path_ReportStandard ReportStandard)
                                       ReportStandard
data LE_ReportStatus
    = LE_ReportStatus_String (Path_ReportStatus ([Char])) ([Char])
    | LE_ReportStatus_JSONText (Path_ReportStatus JSONText) JSONText
    | LE_ReportStatus_ReportStatus (Path_ReportStatus ReportStatus)
                                   ReportStatus
data LE_ReportValueApproachInfo
    = LE_ReportValueApproachInfo_JSONText (Path_ReportValueApproachInfo JSONText)
                                          JSONText
    | LE_ReportValueApproachInfo_Markup (Path_ReportValueApproachInfo Markup)
                                        Markup
    | LE_ReportValueApproachInfo_ReportValueApproachInfo (Path_ReportValueApproachInfo ReportValueApproachInfo)
                                                         ReportValueApproachInfo
    | LE_ReportValueApproachInfo_Text (Path_ReportValueApproachInfo Text)
                                      Text
data LE_ReportValueTypeInfo
    = LE_ReportValueTypeInfo_JSONText (Path_ReportValueTypeInfo JSONText)
                                      JSONText
    | LE_ReportValueTypeInfo_Markup (Path_ReportValueTypeInfo Markup)
                                    Markup
    | LE_ReportValueTypeInfo_ReportValueTypeInfo (Path_ReportValueTypeInfo ReportValueTypeInfo)
                                                 ReportValueTypeInfo
    | LE_ReportValueTypeInfo_Text (Path_ReportValueTypeInfo Text) Text
data LE_ReportView
    = LE_ReportView_String (Path_ReportView ([Char])) ([Char])
    | LE_ReportView_Int64 (Path_ReportView Int64) Int64
    | LE_ReportView_Size (Path_ReportView Int) Int
    | LE_ReportView_Bool (Path_ReportView Bool) Bool
    | LE_ReportView_Double (Path_ReportView Double) Double
    | LE_ReportView_Dimension (Path_ReportView Dimension) Dimension
    | LE_ReportView_ImageCrop (Path_ReportView ImageCrop) ImageCrop
    | LE_ReportView_ImageSize (Path_ReportView ImageSize) ImageSize
    | LE_ReportView_Units (Path_ReportView Units) Units
    | LE_ReportView_ImageFile (Path_ReportView ImageFile) ImageFile
    | LE_ReportView_Integer (Path_ReportView Integer) Integer
    | LE_ReportView_JSONText (Path_ReportView JSONText) JSONText
    | LE_ReportView_Markup (Path_ReportView Markup) Markup
    | LE_ReportView_Permissions (Path_ReportView Permissions)
                                Permissions
    | LE_ReportView_UserIds (Path_ReportView ([UserId])) ([UserId])
    | LE_ReportView_AbbrevPair (Path_ReportView ((CIString, Markup)))
                               ((CIString, Markup))
    | LE_ReportView_AbbrevPairs (Path_ReportView (Order AbbrevPairID
                                                        ((CIString, Markup))))
                                (Order AbbrevPairID ((CIString, Markup)))
    | LE_ReportView_Author (Path_ReportView Author) Author
    | LE_ReportView_Authors (Path_ReportView (Order AuthorID Author))
                            (Order AuthorID Author)
    | LE_ReportView_Branding (Path_ReportView Branding) Branding
    | LE_ReportView_MarkupPair (Path_ReportView ((Markup, Markup)))
                               ((Markup, Markup))
    | LE_ReportView_MarkupPairs (Path_ReportView (Order MarkupPairID
                                                        ((Markup, Markup))))
                                (Order MarkupPairID ((Markup, Markup)))
    | LE_ReportView_Markups (Path_ReportView (Order MarkupID Markup))
                            (Order MarkupID Markup)
    | LE_ReportView_MaybeReportIntendedUse (Path_ReportView (Maybe ReportIntendedUse))
                                           (Maybe ReportIntendedUse)
    | LE_ReportView_ReportElem (Path_ReportView ReportElem) ReportElem
    | LE_ReportView_ReportElems (Path_ReportView (Order ReportElemID
                                                        ReportElem))
                                (Order ReportElemID ReportElem)
    | LE_ReportView_ReportFlags (Path_ReportView ReportFlags)
                                ReportFlags
    | LE_ReportView_ReportStandard (Path_ReportView ReportStandard)
                                   ReportStandard
    | LE_ReportView_ReportStatus (Path_ReportView ReportStatus)
                                 ReportStatus
    | LE_ReportView_ReportValueApproachInfo (Path_ReportView ReportValueApproachInfo)
                                            ReportValueApproachInfo
    | LE_ReportView_ReportValueTypeInfo (Path_ReportView ReportValueTypeInfo)
                                        ReportValueTypeInfo
    | LE_ReportView_MaybeImageFile (Path_ReportView (Maybe ImageFile))
                                   (Maybe ImageFile)
    | LE_ReportView_ReportImage (Path_ReportView ReportImage)
                                ReportImage
    | LE_ReportView_ReportImages (Path_ReportView (Order ReportImageID
                                                         ReportImage))
                                 (Order ReportImageID ReportImage)
    | LE_ReportView_ReadOnlyFilePath (Path_ReportView (ReadOnly ([Char])))
                                     (ReadOnly ([Char]))
    | LE_ReportView_ReportImageView (Path_ReportView ReportImageView)
                                    ReportImageView
    | LE_ReportView_ReportView (Path_ReportView ReportView) ReportView
    | LE_ReportView_SaneSizeImageSize (Path_ReportView (SaneSize ImageSize))
                                      (SaneSize ImageSize)
    | LE_ReportView_Item (Path_ReportView Item) Item
    | LE_ReportView_CIString (Path_ReportView CIString) CIString
    | LE_ReportView_URI (Path_ReportView URI) URI
    | LE_ReportView_Text (Path_ReportView Text) Text
    | LE_ReportView_UserId (Path_ReportView UserId) UserId
    | LE_ReportView_UUID (Path_ReportView UUID) UUID
data LE_SaneSizeImageSize
    = LE_SaneSizeImageSize_String (Path_SaneSizeImageSize ([Char]))
                                  ([Char])
    | LE_SaneSizeImageSize_Double (Path_SaneSizeImageSize Double)
                                  Double
    | LE_SaneSizeImageSize_Dimension (Path_SaneSizeImageSize Dimension)
                                     Dimension
    | LE_SaneSizeImageSize_ImageSize (Path_SaneSizeImageSize ImageSize)
                                     ImageSize
    | LE_SaneSizeImageSize_Units (Path_SaneSizeImageSize Units) Units
    | LE_SaneSizeImageSize_JSONText (Path_SaneSizeImageSize JSONText)
                                    JSONText
    | LE_SaneSizeImageSize_SaneSizeImageSize (Path_SaneSizeImageSize (SaneSize ImageSize))
                                             (SaneSize ImageSize)
data LE_Size = LE_Size_Size (Path_Int Int) Int
data LE_String
    = LE_String_String (Path_String ([Char])) ([Char])
    | LE_String_JSONText (Path_String JSONText) JSONText
data LE_Text
    = LE_Text_JSONText (Path_Text JSONText) JSONText
    | LE_Text_Text (Path_Text Text) Text
data LE_URI = LE_URI_URI (Path_URI URI) URI
data LE_UUID = LE_UUID_UUID (Path_UUID UUID) UUID
data LE_Units
    = LE_Units_Units (Path_Units Units) Units
    | LE_Units_JSONText (Path_Units JSONText) JSONText
data LE_UserId = LE_UserId_UserId (Path_UserId UserId) UserId
data LE_UserIds
    = LE_UserIds_JSONText (Path_UserIds JSONText) JSONText
    | LE_UserIds_UserIds (Path_UserIds ([UserId])) ([UserId])
    | LE_UserIds_Text (Path_UserIds Text) Text
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
data Path_Int a
    = Path_Int
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
data Path_ReportStandard a
    = Path_ReportStandard_unReportStandard (Path_Int a)
    | Path_ReportStandard
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
    | Path_ReportView__reportStandardsVersion (Path_ReportStandard a)
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
type Path_Size a = Path_Int a
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
          lens_Report_reportStandardsVersion :: forall . Lens' c
                                                               ReportStandard
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
    where lens_reportMap :: Lens' c ReportMap
          lens_ReportMap_unReportMap :: forall . Lens' c
                                                       (Map ReportID Report)
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
          lens_ReportView__reportStandardsVersion :: forall . Lens' c
                                                                    ReportStandard
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
instance IsPath (Either URI ImageFile) (Either URI ImageFile)
    where type PathType (Either URI ImageFile)
                        (Either URI ImageFile) = Path_Either (Path_URI (Either URI
                                                                               ImageFile))
                                                             (Path_ImageFile (Either URI ImageFile))
          toLens _ = iso id id
instance IsPath (Either URI ImageFile) ImageFile
    where type PathType (Either URI ImageFile)
                        ImageFile = Path_Either (Path_URI ImageFile)
                                                (Path_ImageFile ImageFile)
          toLens (Path_Right _) = _Right
          toLens u = error $ ("Unexpected goal ImageFile for Either URI ImageFile: " ++ show u)
instance IsPath (Either URI ImageFile) URI
    where type PathType (Either URI ImageFile)
                        URI = Path_Either (Path_URI URI) (Path_ImageFile URI)
          toLens (Path_Left _) = _Left
          toLens u = error $ ("Unexpected goal URI for Either URI ImageFile: " ++ show u)
instance IsPath (Map ItemFieldName Markup)
                (Map ItemFieldName Markup)
    where type PathType (Map ItemFieldName Markup)
                        (Map ItemFieldName Markup) = Path_Map ItemFieldName
                                                              (Path_Markup (Map ItemFieldName
                                                                                Markup))
          toLens _ = iso id id
instance IsPath (Map ItemFieldName Markup) JSONText
    where type PathType (Map ItemFieldName Markup)
                        JSONText = Path_Map ItemFieldName (Path_Markup JSONText)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Map ItemFieldName Markup: " ++ show u)
instance IsPath (Map ItemFieldName Markup) Markup
    where type PathType (Map ItemFieldName Markup)
                        Markup = Path_Map ItemFieldName (Path_Markup Markup)
          toLens (Path_Look k _) = mat k
          toLens u = error $ ("Unexpected goal Markup for Map ItemFieldName Markup: " ++ show u)
instance IsPath (Map ItemFieldName Markup) Text
    where type PathType (Map ItemFieldName Markup)
                        Text = Path_Map ItemFieldName (Path_Markup Text)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Map ItemFieldName Markup: " ++ show u)
instance IsPath (Map ReportID Report) (Either URI ImageFile)
    where type PathType (Map ReportID Report)
                        (Either URI ImageFile) = Path_Map ReportID
                                                          (Path_Report (Either URI ImageFile))
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) (Map ItemFieldName Markup)
    where type PathType (Map ReportID Report)
                        (Map ItemFieldName Markup) = Path_Map ReportID
                                                              (Path_Report (Map ItemFieldName
                                                                                Markup))
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) (Map ReportID Report)
    where type PathType (Map ReportID Report)
                        (Map ReportID Report) = Path_Map ReportID
                                                         (Path_Report (Map ReportID Report))
          toLens _ = iso id id
instance IsPath (Map ReportID Report)
                (Maybe (Either URI ImageFile))
    where type PathType (Map ReportID Report)
                        (Maybe (Either URI ImageFile)) = Path_Map ReportID
                                                                  (Path_Report (Maybe (Either URI
                                                                                              ImageFile)))
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) String
    where type PathType (Map ReportID Report)
                        String = Path_Map ReportID (Path_Report String)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Int64
    where type PathType (Map ReportID Report) Int64 = Path_Map ReportID
                                                               (Path_Report Int64)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Bool
    where type PathType (Map ReportID Report) Bool = Path_Map ReportID
                                                              (Path_Report Bool)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Bool for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Double
    where type PathType (Map ReportID Report)
                        Double = Path_Map ReportID (Path_Report Double)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Double for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Int
    where type PathType (Map ReportID Report) Int = Path_Map ReportID
                                                             (Path_Report Int)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Int (aka Size) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Dimension
    where type PathType (Map ReportID Report)
                        Dimension = Path_Map ReportID (Path_Report Dimension)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ImageCrop
    where type PathType (Map ReportID Report)
                        ImageCrop = Path_Map ReportID (Path_Report ImageCrop)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ImageSize
    where type PathType (Map ReportID Report)
                        ImageSize = Path_Map ReportID (Path_Report ImageSize)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Units
    where type PathType (Map ReportID Report) Units = Path_Map ReportID
                                                               (Path_Report Units)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Units for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ImageFile
    where type PathType (Map ReportID Report)
                        ImageFile = Path_Map ReportID (Path_Report ImageFile)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Integer
    where type PathType (Map ReportID Report)
                        Integer = Path_Map ReportID (Path_Report Integer)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Integer for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) JSONText
    where type PathType (Map ReportID Report)
                        JSONText = Path_Map ReportID (Path_Report JSONText)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Markup
    where type PathType (Map ReportID Report)
                        Markup = Path_Map ReportID (Path_Report Markup)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Permissions
    where type PathType (Map ReportID Report)
                        Permissions = Path_Map ReportID (Path_Report Permissions)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Permissions for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) UserIds
    where type PathType (Map ReportID Report)
                        UserIds = Path_Map ReportID (Path_Report UserIds)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) AbbrevPair
    where type PathType (Map ReportID Report)
                        AbbrevPair = Path_Map ReportID (Path_Report AbbrevPair)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) AbbrevPairs
    where type PathType (Map ReportID Report)
                        AbbrevPairs = Path_Map ReportID (Path_Report AbbrevPairs)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Author
    where type PathType (Map ReportID Report)
                        Author = Path_Map ReportID (Path_Report Author)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Author for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Authors
    where type PathType (Map ReportID Report)
                        Authors = Path_Map ReportID (Path_Report Authors)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Branding
    where type PathType (Map ReportID Report)
                        Branding = Path_Map ReportID (Path_Report Branding)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Branding for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) MarkupPair
    where type PathType (Map ReportID Report)
                        MarkupPair = Path_Map ReportID (Path_Report MarkupPair)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) MarkupPairs
    where type PathType (Map ReportID Report)
                        MarkupPairs = Path_Map ReportID (Path_Report MarkupPairs)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Markups
    where type PathType (Map ReportID Report)
                        Markups = Path_Map ReportID (Path_Report Markups)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) MaybeReportIntendedUse
    where type PathType (Map ReportID Report)
                        MaybeReportIntendedUse = Path_Map ReportID
                                                          (Path_Report MaybeReportIntendedUse)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Report
    where type PathType (Map ReportID Report)
                        Report = Path_Map ReportID (Path_Report Report)
          toLens (Path_Look k _) = mat k
          toLens u = error $ ("Unexpected goal Report for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportElem
    where type PathType (Map ReportID Report)
                        ReportElem = Path_Map ReportID (Path_Report ReportElem)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportElem for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportElems
    where type PathType (Map ReportID Report)
                        ReportElems = Path_Map ReportID (Path_Report ReportElems)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportFlags
    where type PathType (Map ReportID Report)
                        ReportFlags = Path_Map ReportID (Path_Report ReportFlags)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportFlags for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportStandard
    where type PathType (Map ReportID Report)
                        ReportStandard = Path_Map ReportID (Path_Report ReportStandard)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportStandard for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportStatus
    where type PathType (Map ReportID Report)
                        ReportStatus = Path_Map ReportID (Path_Report ReportStatus)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportStatus for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportValueApproachInfo
    where type PathType (Map ReportID Report)
                        ReportValueApproachInfo = Path_Map ReportID
                                                           (Path_Report ReportValueApproachInfo)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportValueTypeInfo
    where type PathType (Map ReportID Report)
                        ReportValueTypeInfo = Path_Map ReportID
                                                       (Path_Report ReportValueTypeInfo)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) MaybeImageFile
    where type PathType (Map ReportID Report)
                        MaybeImageFile = Path_Map ReportID (Path_Report MaybeImageFile)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportImage
    where type PathType (Map ReportID Report)
                        ReportImage = Path_Map ReportID (Path_Report ReportImage)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImage for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportImages
    where type PathType (Map ReportID Report)
                        ReportImages = Path_Map ReportID (Path_Report ReportImages)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReadOnlyFilePath
    where type PathType (Map ReportID Report)
                        ReadOnlyFilePath = Path_Map ReportID (Path_Report ReadOnlyFilePath)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportImageView
    where type PathType (Map ReportID Report)
                        ReportImageView = Path_Map ReportID (Path_Report ReportImageView)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) ReportView
    where type PathType (Map ReportID Report)
                        ReportView = Path_Map ReportID (Path_Report ReportView)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal ReportView for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) SaneSizeImageSize
    where type PathType (Map ReportID Report)
                        SaneSizeImageSize = Path_Map ReportID
                                                     (Path_Report SaneSizeImageSize)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Item
    where type PathType (Map ReportID Report) Item = Path_Map ReportID
                                                              (Path_Report Item)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Item for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) CIString
    where type PathType (Map ReportID Report)
                        CIString = Path_Map ReportID (Path_Report CIString)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal CIString for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) URI
    where type PathType (Map ReportID Report) URI = Path_Map ReportID
                                                             (Path_Report URI)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal URI for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) Text
    where type PathType (Map ReportID Report) Text = Path_Map ReportID
                                                              (Path_Report Text)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) UserId
    where type PathType (Map ReportID Report)
                        UserId = Path_Map ReportID (Path_Report UserId)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal UserId for Map ReportID Report: " ++ show u)
instance IsPath (Map ReportID Report) UUID
    where type PathType (Map ReportID Report) UUID = Path_Map ReportID
                                                              (Path_Report UUID)
          toLens (Path_Look k v) = mat k . toLens v
          toLens u = error $ ("Unexpected goal UUID for Map ReportID Report: " ++ show u)
instance IsPath (Maybe (Either URI ImageFile))
                (Either URI ImageFile)
    where type PathType (Maybe (Either URI ImageFile))
                        (Either URI
                                ImageFile) = Path_Maybe (Path_Either (Path_URI (Either URI
                                                                                       ImageFile))
                                                                     (Path_ImageFile (Either URI
                                                                                             ImageFile)))
          toLens (Path_Just _) = _Just
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Maybe (Either URI ImageFile): " ++ show u)
instance IsPath (Maybe (Either URI ImageFile))
                (Maybe (Either URI ImageFile))
    where type PathType (Maybe (Either URI ImageFile))
                        (Maybe (Either URI
                                       ImageFile)) = Path_Maybe (Path_Either (Path_URI (Maybe (Either URI
                                                                                                      ImageFile)))
                                                                             (Path_ImageFile (Maybe (Either URI
                                                                                                            ImageFile))))
          toLens _ = iso id id
instance IsPath (Maybe (Either URI ImageFile)) ImageFile
    where type PathType (Maybe (Either URI ImageFile))
                        ImageFile = Path_Maybe (Path_Either (Path_URI ImageFile)
                                                            (Path_ImageFile ImageFile))
          toLens (Path_Just v) = _Just . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Maybe (Either URI ImageFile): " ++ show u)
instance IsPath (Maybe (Either URI ImageFile)) URI
    where type PathType (Maybe (Either URI ImageFile))
                        URI = Path_Maybe (Path_Either (Path_URI URI) (Path_ImageFile URI))
          toLens (Path_Just v) = _Just . toLens v
          toLens u = error $ ("Unexpected goal URI for Maybe (Either URI ImageFile): " ++ show u)
instance IsPath String String
    where type PathType String String = Path_String String
          toLens _ = iso id id
instance IsPath String JSONText
    where type PathType String JSONText = Path_String JSONText
          toLens (Path_String_View _) = viewLens :: Lens' ([Char]) JSONText
          toLens u = error $ ("Unexpected goal JSONText for [Char] (aka Checksum, aka FilePath, aka String): " ++ show u)
instance IsPath Int64 Int64
    where type PathType Int64 Int64 = Path_Int64 Int64
          toLens _ = iso id id
instance IsPath Bool String
    where type PathType Bool String = Path_Bool String
          toLens (Path_Bool_View _) = viewLens :: Lens' Bool String
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Bool: " ++ show u)
instance IsPath Bool Bool
    where type PathType Bool Bool = Path_Bool Bool
          toLens _ = iso id id
instance IsPath Bool JSONText
    where type PathType Bool JSONText = Path_Bool JSONText
          toLens (Path_Bool_View v) = (viewLens :: Lens' Bool
                                                         String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Bool: " ++ show u)
instance IsPath Double String
    where type PathType Double String = Path_Double String
          toLens (Path_Double_View _) = viewLens :: Lens' Double String
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Double: " ++ show u)
instance IsPath Double Double
    where type PathType Double Double = Path_Double Double
          toLens _ = iso id id
instance IsPath Double JSONText
    where type PathType Double JSONText = Path_Double JSONText
          toLens (Path_Double_View v) = (viewLens :: Lens' Double
                                                           String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Double: " ++ show u)
instance IsPath Int Int
    where type PathType Int Int = Path_Int Int
          toLens _ = iso id id
instance IsPath Dimension Dimension
    where type PathType Dimension Dimension = Path_Dimension Dimension
          toLens _ = iso id id
instance IsPath Dimension JSONText
    where type PathType Dimension JSONText = Path_Dimension JSONText
          toLens (Path_Dimension_View _) = viewLens :: Lens' Dimension
                                                             JSONText
          toLens u = error $ ("Unexpected goal JSONText for Dimension: " ++ show u)
instance IsPath ImageCrop ImageCrop
    where type PathType ImageCrop ImageCrop = Path_ImageCrop ImageCrop
          toLens _ = iso id id
instance IsPath ImageSize String
    where type PathType ImageSize String = Path_ImageSize String
          toLens (Path_ImageSize_size _x) = lens_ImageSize_size . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ImageSize: " ++ show u)
instance IsPath ImageSize Double
    where type PathType ImageSize Double = Path_ImageSize Double
          toLens (Path_ImageSize_size _x) = lens_ImageSize_size
          toLens u = error $ ("Unexpected goal Double for ImageSize: " ++ show u)
instance IsPath ImageSize Dimension
    where type PathType ImageSize Dimension = Path_ImageSize Dimension
          toLens (Path_ImageSize_dim _x) = lens_ImageSize_dim
          toLens u = error $ ("Unexpected goal Dimension for ImageSize: " ++ show u)
instance IsPath ImageSize ImageSize
    where type PathType ImageSize ImageSize = Path_ImageSize ImageSize
          toLens _ = iso id id
instance IsPath ImageSize Units
    where type PathType ImageSize Units = Path_ImageSize Units
          toLens (Path_ImageSize_units _x) = lens_ImageSize_units
          toLens u = error $ ("Unexpected goal Units for ImageSize: " ++ show u)
instance IsPath ImageSize JSONText
    where type PathType ImageSize JSONText = Path_ImageSize JSONText
          toLens (Path_ImageSize_dim _x) = lens_ImageSize_dim . toLens _x
          toLens (Path_ImageSize_size _x) = lens_ImageSize_size . toLens _x
          toLens (Path_ImageSize_units _x) = lens_ImageSize_units . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ImageSize: " ++ show u)
instance IsPath Units Units
    where type PathType Units Units = Path_Units Units
          toLens _ = iso id id
instance IsPath Units JSONText
    where type PathType Units JSONText = Path_Units JSONText
          toLens (Path_Units_View _) = viewLens :: Lens' Units JSONText
          toLens u = error $ ("Unexpected goal JSONText for Units: " ++ show u)
instance IsPath ImageFile ImageFile
    where type PathType ImageFile ImageFile = Path_ImageFile ImageFile
          toLens _ = iso id id
instance IsPath Integer Integer
    where type PathType Integer Integer = Path_Integer Integer
          toLens _ = iso id id
instance IsPath JSONText JSONText
    where type PathType JSONText JSONText = Path_JSONText JSONText
          toLens _ = iso id id
instance IsPath Markup JSONText
    where type PathType Markup JSONText = Path_Markup JSONText
          toLens (Path_Markup_markdownText _x) = lens_Markup_markdownText . toLens _x
          toLens (Path_Markup_htmlText _x) = lens_Markup_htmlText . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Markup: " ++ show u)
instance IsPath Markup Markup
    where type PathType Markup Markup = Path_Markup Markup
          toLens _ = iso id id
instance IsPath Markup Text
    where type PathType Markup Text = Path_Markup Text
          toLens (Path_Markup_markdownText _x) = lens_Markup_markdownText
          toLens (Path_Markup_htmlText _x) = lens_Markup_htmlText
          toLens u = error $ ("Unexpected goal Text for Markup: " ++ show u)
instance IsPath Permissions JSONText
    where type PathType Permissions
                        JSONText = Path_Permissions JSONText
          toLens (Path_Permissions_writers _x) = lens_Permissions_writers . toLens _x
          toLens (Path_Permissions_readers _x) = lens_Permissions_readers . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Permissions: " ++ show u)
instance IsPath Permissions Permissions
    where type PathType Permissions
                        Permissions = Path_Permissions Permissions
          toLens _ = iso id id
instance IsPath Permissions UserIds
    where type PathType Permissions UserIds = Path_Permissions UserIds
          toLens (Path_Permissions_writers _x) = lens_Permissions_writers
          toLens (Path_Permissions_readers _x) = lens_Permissions_readers
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for Permissions: " ++ show u)
instance IsPath Permissions Text
    where type PathType Permissions Text = Path_Permissions Text
          toLens (Path_Permissions_writers _x) = lens_Permissions_writers . toLens _x
          toLens (Path_Permissions_readers _x) = lens_Permissions_readers . toLens _x
          toLens u = error $ ("Unexpected goal Text for Permissions: " ++ show u)
instance IsPath Permissions UserId
    where type PathType Permissions UserId = Path_Permissions UserId
          toLens (Path_Permissions_owner _x) = lens_Permissions_owner
          toLens u = error $ ("Unexpected goal UserId for Permissions: " ++ show u)
instance IsPath UserIds JSONText
    where type PathType UserIds JSONText = Path_UserIds JSONText
          toLens (Path_UserIds_View v) = (viewLens :: Lens' ([UserId])
                                                            Text) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for [UserId] (aka UserIds): " ++ show u)
instance IsPath UserIds UserIds
    where type PathType UserIds UserIds = Path_UserIds UserIds
          toLens _ = iso id id
instance IsPath UserIds Text
    where type PathType UserIds Text = Path_UserIds Text
          toLens (Path_UserIds_View _) = viewLens :: Lens' ([UserId]) Text
          toLens u = error $ ("Unexpected goal Text for [UserId] (aka UserIds): " ++ show u)
instance IsPath AbbrevPair JSONText
    where type PathType AbbrevPair
                        JSONText = Path_Pair (Path_CIString JSONText)
                                             (Path_Markup JSONText)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal JSONText for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance IsPath AbbrevPair Markup
    where type PathType AbbrevPair
                        Markup = Path_Pair (Path_CIString Markup) (Path_Markup Markup)
          toLens (Path_Second _) = _2
          toLens u = error $ ("Unexpected goal Markup for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance IsPath AbbrevPair AbbrevPair
    where type PathType AbbrevPair
                        AbbrevPair = Path_Pair (Path_CIString AbbrevPair)
                                               (Path_Markup AbbrevPair)
          toLens _ = iso id id
instance IsPath AbbrevPair CIString
    where type PathType AbbrevPair
                        CIString = Path_Pair (Path_CIString CIString)
                                             (Path_Markup CIString)
          toLens (Path_First _) = _1
          toLens u = error $ ("Unexpected goal CIString for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance IsPath AbbrevPair Text
    where type PathType AbbrevPair
                        Text = Path_Pair (Path_CIString Text) (Path_Markup Text)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal Text for (CIString, Markup) (aka AbbrevPair): " ++ show u)
instance IsPath AbbrevPairs JSONText
    where type PathType AbbrevPairs JSONText = Path_OMap AbbrevPairID
                                                         (Path_Pair (Path_CIString JSONText)
                                                                    (Path_Markup JSONText))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance IsPath AbbrevPairs Markup
    where type PathType AbbrevPairs Markup = Path_OMap AbbrevPairID
                                                       (Path_Pair (Path_CIString Markup)
                                                                  (Path_Markup Markup))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance IsPath AbbrevPairs AbbrevPair
    where type PathType AbbrevPairs AbbrevPair = Path_OMap AbbrevPairID
                                                           (Path_Pair (Path_CIString AbbrevPair)
                                                                      (Path_Markup AbbrevPair))
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance IsPath AbbrevPairs AbbrevPairs
    where type PathType AbbrevPairs
                        AbbrevPairs = Path_OMap AbbrevPairID
                                                (Path_Pair (Path_CIString AbbrevPairs)
                                                           (Path_Markup AbbrevPairs))
          toLens _ = iso id id
instance IsPath AbbrevPairs CIString
    where type PathType AbbrevPairs CIString = Path_OMap AbbrevPairID
                                                         (Path_Pair (Path_CIString CIString)
                                                                    (Path_Markup CIString))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal CIString for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance IsPath AbbrevPairs Text
    where type PathType AbbrevPairs Text = Path_OMap AbbrevPairID
                                                     (Path_Pair (Path_CIString Text)
                                                                (Path_Markup Text))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs): " ++ show u)
instance IsPath Author JSONText
    where type PathType Author JSONText = Path_Author JSONText
          toLens (Path_Author_authorName _x) = lens_Author_authorName . toLens _x
          toLens (Path_Author_authorCredentials _x) = lens_Author_authorCredentials . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Author: " ++ show u)
instance IsPath Author Markup
    where type PathType Author Markup = Path_Author Markup
          toLens (Path_Author_authorName _x) = lens_Author_authorName
          toLens (Path_Author_authorCredentials _x) = lens_Author_authorCredentials
          toLens u = error $ ("Unexpected goal Markup for Author: " ++ show u)
instance IsPath Author Author
    where type PathType Author Author = Path_Author Author
          toLens _ = iso id id
instance IsPath Author Text
    where type PathType Author Text = Path_Author Text
          toLens (Path_Author_authorName _x) = lens_Author_authorName . toLens _x
          toLens (Path_Author_authorCredentials _x) = lens_Author_authorCredentials . toLens _x
          toLens u = error $ ("Unexpected goal Text for Author: " ++ show u)
instance IsPath Authors JSONText
    where type PathType Authors JSONText = Path_OMap AuthorID
                                                     (Path_Author JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order AuthorID Author (aka Authors): " ++ show u)
instance IsPath Authors Markup
    where type PathType Authors Markup = Path_OMap AuthorID
                                                   (Path_Author Markup)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order AuthorID Author (aka Authors): " ++ show u)
instance IsPath Authors Author
    where type PathType Authors Author = Path_OMap AuthorID
                                                   (Path_Author Author)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal Author for Order AuthorID Author (aka Authors): " ++ show u)
instance IsPath Authors Authors
    where type PathType Authors Authors = Path_OMap AuthorID
                                                    (Path_Author Authors)
          toLens _ = iso id id
instance IsPath Authors Text
    where type PathType Authors Text = Path_OMap AuthorID
                                                 (Path_Author Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order AuthorID Author (aka Authors): " ++ show u)
instance IsPath Branding JSONText
    where type PathType Branding JSONText = Path_Branding JSONText
          toLens (Path_Branding_View v) = (viewLens :: Lens' Branding
                                                             Text) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Branding: " ++ show u)
instance IsPath Branding Branding
    where type PathType Branding Branding = Path_Branding Branding
          toLens _ = iso id id
instance IsPath Branding Text
    where type PathType Branding Text = Path_Branding Text
          toLens (Path_Branding_View _) = viewLens :: Lens' Branding Text
          toLens u = error $ ("Unexpected goal Text for Branding: " ++ show u)
instance IsPath MarkupPair JSONText
    where type PathType MarkupPair
                        JSONText = Path_Pair (Path_Markup JSONText) (Path_Markup JSONText)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal JSONText for (Markup, Markup) (aka MarkupPair): " ++ show u)
instance IsPath MarkupPair Markup
    where type PathType MarkupPair
                        Markup = Path_Pair (Path_Markup Markup) (Path_Markup Markup)
          toLens (Path_First _) = _1
          toLens (Path_Second _) = _2
          toLens u = error $ ("Unexpected goal Markup for (Markup, Markup) (aka MarkupPair): " ++ show u)
instance IsPath MarkupPair MarkupPair
    where type PathType MarkupPair
                        MarkupPair = Path_Pair (Path_Markup MarkupPair)
                                               (Path_Markup MarkupPair)
          toLens _ = iso id id
instance IsPath MarkupPair Text
    where type PathType MarkupPair Text = Path_Pair (Path_Markup Text)
                                                    (Path_Markup Text)
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
          toLens u = error $ ("Unexpected goal Text for (Markup, Markup) (aka MarkupPair): " ++ show u)
instance IsPath MarkupPairs JSONText
    where type PathType MarkupPairs JSONText = Path_OMap MarkupPairID
                                                         (Path_Pair (Path_Markup JSONText)
                                                                    (Path_Markup JSONText))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance IsPath MarkupPairs Markup
    where type PathType MarkupPairs Markup = Path_OMap MarkupPairID
                                                       (Path_Pair (Path_Markup Markup)
                                                                  (Path_Markup Markup))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance IsPath MarkupPairs MarkupPair
    where type PathType MarkupPairs MarkupPair = Path_OMap MarkupPairID
                                                           (Path_Pair (Path_Markup MarkupPair)
                                                                      (Path_Markup MarkupPair))
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance IsPath MarkupPairs MarkupPairs
    where type PathType MarkupPairs
                        MarkupPairs = Path_OMap MarkupPairID
                                                (Path_Pair (Path_Markup MarkupPairs)
                                                           (Path_Markup MarkupPairs))
          toLens _ = iso id id
instance IsPath MarkupPairs Text
    where type PathType MarkupPairs Text = Path_OMap MarkupPairID
                                                     (Path_Pair (Path_Markup Text)
                                                                (Path_Markup Text))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs): " ++ show u)
instance IsPath Markups JSONText
    where type PathType Markups JSONText = Path_OMap MarkupID
                                                     (Path_Markup JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order MarkupID Markup (aka Markups): " ++ show u)
instance IsPath Markups Markup
    where type PathType Markups Markup = Path_OMap MarkupID
                                                   (Path_Markup Markup)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal Markup for Order MarkupID Markup (aka Markups): " ++ show u)
instance IsPath Markups Markups
    where type PathType Markups Markups = Path_OMap MarkupID
                                                    (Path_Markup Markups)
          toLens _ = iso id id
instance IsPath Markups Text
    where type PathType Markups Text = Path_OMap MarkupID
                                                 (Path_Markup Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order MarkupID Markup (aka Markups): " ++ show u)
instance IsPath MaybeReportIntendedUse String
    where type PathType MaybeReportIntendedUse
                        String = Path_MaybeReportIntendedUse String
          toLens (Path_MaybeReportIntendedUse_View _) = viewLens :: Lens' (Maybe ReportIntendedUse)
                                                                          String
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Maybe ReportIntendedUse (aka MaybeReportIntendedUse): " ++ show u)
instance IsPath MaybeReportIntendedUse JSONText
    where type PathType MaybeReportIntendedUse
                        JSONText = Path_MaybeReportIntendedUse JSONText
          toLens (Path_MaybeReportIntendedUse_View v) = (viewLens :: Lens' (Maybe ReportIntendedUse)
                                                                           String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Maybe ReportIntendedUse (aka MaybeReportIntendedUse): " ++ show u)
instance IsPath MaybeReportIntendedUse MaybeReportIntendedUse
    where type PathType MaybeReportIntendedUse
                        MaybeReportIntendedUse = Path_MaybeReportIntendedUse MaybeReportIntendedUse
          toLens _ = iso id id
instance IsPath Report (Either URI ImageFile)
    where type PathType Report
                        (Either URI ImageFile) = Path_Report (Either URI ImageFile)
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Report: " ++ show u)
instance IsPath Report (Map ItemFieldName Markup)
    where type PathType Report
                        (Map ItemFieldName Markup) = Path_Report (Map ItemFieldName Markup)
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Report: " ++ show u)
instance IsPath Report (Maybe (Either URI ImageFile))
    where type PathType Report
                        (Maybe (Either URI ImageFile)) = Path_Report (Maybe (Either URI
                                                                                    ImageFile))
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Report: " ++ show u)
instance IsPath Report String
    where type PathType Report String = Path_Report String
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Report: " ++ show u)
instance IsPath Report Int64
    where type PathType Report Int64 = Path_Report Int64
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for Report: " ++ show u)
instance IsPath Report Bool
    where type PathType Report Bool = Path_Report Bool
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Bool for Report: " ++ show u)
instance IsPath Report Double
    where type PathType Report Double = Path_Report Double
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Double for Report: " ++ show u)
instance IsPath Report Int
    where type PathType Report Int = Path_Report Int
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Int (aka Size) for Report: " ++ show u)
instance IsPath Report Dimension
    where type PathType Report Dimension = Path_Report Dimension
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Report: " ++ show u)
instance IsPath Report ImageCrop
    where type PathType Report ImageCrop = Path_Report ImageCrop
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Report: " ++ show u)
instance IsPath Report ImageSize
    where type PathType Report ImageSize = Path_Report ImageSize
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Report: " ++ show u)
instance IsPath Report Units
    where type PathType Report Units = Path_Report Units
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Units for Report: " ++ show u)
instance IsPath Report ImageFile
    where type PathType Report ImageFile = Path_Report ImageFile
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Report: " ++ show u)
instance IsPath Report Integer
    where type PathType Report Integer = Path_Report Integer
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Integer for Report: " ++ show u)
instance IsPath Report JSONText
    where type PathType Report JSONText = Path_Report JSONText
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Report: " ++ show u)
instance IsPath Report Markup
    where type PathType Report Markup = Path_Report Markup
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Markup for Report: " ++ show u)
instance IsPath Report Permissions
    where type PathType Report Permissions = Path_Report Permissions
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Permissions for Report: " ++ show u)
instance IsPath Report UserIds
    where type PathType Report UserIds = Path_Report UserIds
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for Report: " ++ show u)
instance IsPath Report AbbrevPair
    where type PathType Report AbbrevPair = Path_Report AbbrevPair
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for Report: " ++ show u)
instance IsPath Report AbbrevPairs
    where type PathType Report AbbrevPairs = Path_Report AbbrevPairs
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for Report: " ++ show u)
instance IsPath Report Author
    where type PathType Report Author = Path_Report Author
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Author for Report: " ++ show u)
instance IsPath Report Authors
    where type PathType Report Authors = Path_Report Authors
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for Report: " ++ show u)
instance IsPath Report Branding
    where type PathType Report Branding = Path_Report Branding
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Branding for Report: " ++ show u)
instance IsPath Report MarkupPair
    where type PathType Report MarkupPair = Path_Report MarkupPair
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for Report: " ++ show u)
instance IsPath Report MarkupPairs
    where type PathType Report MarkupPairs = Path_Report MarkupPairs
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for Report: " ++ show u)
instance IsPath Report Markups
    where type PathType Report Markups = Path_Report Markups
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for Report: " ++ show u)
instance IsPath Report MaybeReportIntendedUse
    where type PathType Report
                        MaybeReportIntendedUse = Path_Report MaybeReportIntendedUse
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for Report: " ++ show u)
instance IsPath Report Report
    where type PathType Report Report = Path_Report Report
          toLens _ = iso id id
instance IsPath Report ReportElem
    where type PathType Report ReportElem = Path_Report ReportElem
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportElem for Report: " ++ show u)
instance IsPath Report ReportElems
    where type PathType Report ReportElems = Path_Report ReportElems
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for Report: " ++ show u)
instance IsPath Report ReportFlags
    where type PathType Report ReportFlags = Path_Report ReportFlags
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportFlags for Report: " ++ show u)
instance IsPath Report ReportStandard
    where type PathType Report
                        ReportStandard = Path_Report ReportStandard
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportStandard for Report: " ++ show u)
instance IsPath Report ReportStatus
    where type PathType Report ReportStatus = Path_Report ReportStatus
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportStatus for Report: " ++ show u)
instance IsPath Report ReportValueApproachInfo
    where type PathType Report
                        ReportValueApproachInfo = Path_Report ReportValueApproachInfo
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for Report: " ++ show u)
instance IsPath Report ReportValueTypeInfo
    where type PathType Report
                        ReportValueTypeInfo = Path_Report ReportValueTypeInfo
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for Report: " ++ show u)
instance IsPath Report MaybeImageFile
    where type PathType Report
                        MaybeImageFile = Path_Report MaybeImageFile
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Report: " ++ show u)
instance IsPath Report ReportImage
    where type PathType Report ReportImage = Path_Report ReportImage
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportImage for Report: " ++ show u)
instance IsPath Report ReportImages
    where type PathType Report ReportImages = Path_Report ReportImages
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Report: " ++ show u)
instance IsPath Report ReadOnlyFilePath
    where type PathType Report
                        ReadOnlyFilePath = Path_Report ReadOnlyFilePath
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for Report: " ++ show u)
instance IsPath Report ReportImageView
    where type PathType Report
                        ReportImageView = Path_Report ReportImageView
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Report: " ++ show u)
instance IsPath Report ReportView
    where type PathType Report ReportView = Path_Report ReportView
          toLens (Path_Report_View _) = viewLens :: Lens' Report ReportView
          toLens u = error $ ("Unexpected goal ReportView for Report: " ++ show u)
instance IsPath Report SaneSizeImageSize
    where type PathType Report
                        SaneSizeImageSize = Path_Report SaneSizeImageSize
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Report: " ++ show u)
instance IsPath Report Item
    where type PathType Report Item = Path_Report Item
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Item for Report: " ++ show u)
instance IsPath Report CIString
    where type PathType Report CIString = Path_Report CIString
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal CIString for Report: " ++ show u)
instance IsPath Report URI
    where type PathType Report URI = Path_Report URI
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal URI for Report: " ++ show u)
instance IsPath Report Text
    where type PathType Report Text = Path_Report Text
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal Text for Report: " ++ show u)
instance IsPath Report UserId
    where type PathType Report UserId = Path_Report UserId
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal UserId for Report: " ++ show u)
instance IsPath Report UUID
    where type PathType Report UUID = Path_Report UUID
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
          toLens u = error $ ("Unexpected goal UUID for Report: " ++ show u)
instance IsPath ReportElem (Either URI ImageFile)
    where type PathType ReportElem
                        (Either URI ImageFile) = Path_ReportElem (Either URI ImageFile)
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportElem: " ++ show u)
instance IsPath ReportElem (Map ItemFieldName Markup)
    where type PathType ReportElem
                        (Map ItemFieldName Markup) = Path_ReportElem (Map ItemFieldName
                                                                          Markup)
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for ReportElem: " ++ show u)
instance IsPath ReportElem (Maybe (Either URI ImageFile))
    where type PathType ReportElem
                        (Maybe (Either URI ImageFile)) = Path_ReportElem (Maybe (Either URI
                                                                                        ImageFile))
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportElem: " ++ show u)
instance IsPath ReportElem String
    where type PathType ReportElem String = Path_ReportElem String
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportElem: " ++ show u)
instance IsPath ReportElem Bool
    where type PathType ReportElem Bool = Path_ReportElem Bool
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Bool for ReportElem: " ++ show u)
instance IsPath ReportElem Double
    where type PathType ReportElem Double = Path_ReportElem Double
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportElem: " ++ show u)
instance IsPath ReportElem Dimension
    where type PathType ReportElem
                        Dimension = Path_ReportElem Dimension
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportElem: " ++ show u)
instance IsPath ReportElem ImageCrop
    where type PathType ReportElem
                        ImageCrop = Path_ReportElem ImageCrop
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for ReportElem: " ++ show u)
instance IsPath ReportElem ImageSize
    where type PathType ReportElem
                        ImageSize = Path_ReportElem ImageSize
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportElem: " ++ show u)
instance IsPath ReportElem Units
    where type PathType ReportElem Units = Path_ReportElem Units
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportElem: " ++ show u)
instance IsPath ReportElem ImageFile
    where type PathType ReportElem
                        ImageFile = Path_ReportElem ImageFile
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportElem: " ++ show u)
instance IsPath ReportElem JSONText
    where type PathType ReportElem JSONText = Path_ReportElem JSONText
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens (Path_ReportElem_elemText _x) = lens_ReportElem_elemText . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportElem: " ++ show u)
instance IsPath ReportElem Markup
    where type PathType ReportElem Markup = Path_ReportElem Markup
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens (Path_ReportElem_elemText _x) = lens_ReportElem_elemText
          toLens u = error $ ("Unexpected goal Markup for ReportElem: " ++ show u)
instance IsPath ReportElem ReportElem
    where type PathType ReportElem
                        ReportElem = Path_ReportElem ReportElem
          toLens _ = iso id id
instance IsPath ReportElem MaybeImageFile
    where type PathType ReportElem
                        MaybeImageFile = Path_ReportElem MaybeImageFile
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportElem: " ++ show u)
instance IsPath ReportElem ReportImage
    where type PathType ReportElem
                        ReportImage = Path_ReportElem ReportImage
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for ReportElem: " ++ show u)
instance IsPath ReportElem ReportImages
    where type PathType ReportElem
                        ReportImages = Path_ReportElem ReportImages
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for ReportElem: " ++ show u)
instance IsPath ReportElem ReportImageView
    where type PathType ReportElem
                        ReportImageView = Path_ReportElem ReportImageView
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for ReportElem: " ++ show u)
instance IsPath ReportElem SaneSizeImageSize
    where type PathType ReportElem
                        SaneSizeImageSize = Path_ReportElem SaneSizeImageSize
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportElem: " ++ show u)
instance IsPath ReportElem Item
    where type PathType ReportElem Item = Path_ReportElem Item
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem
          toLens u = error $ ("Unexpected goal Item for ReportElem: " ++ show u)
instance IsPath ReportElem URI
    where type PathType ReportElem URI = Path_ReportElem URI
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportElem: " ++ show u)
instance IsPath ReportElem Text
    where type PathType ReportElem Text = Path_ReportElem Text
          toLens (Path_ReportElem_elemItem _x) = lens_ReportElem_elemItem . toLens _x
          toLens (Path_ReportElem_elemText _x) = lens_ReportElem_elemText . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportElem: " ++ show u)
instance IsPath ReportElems (Either URI ImageFile)
    where type PathType ReportElems
                        (Either URI ImageFile) = Path_OMap ReportElemID
                                                           (Path_ReportElem (Either URI ImageFile))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems (Map ItemFieldName Markup)
    where type PathType ReportElems
                        (Map ItemFieldName Markup) = Path_OMap ReportElemID
                                                               (Path_ReportElem (Map ItemFieldName
                                                                                     Markup))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems (Maybe (Either URI ImageFile))
    where type PathType ReportElems
                        (Maybe (Either URI ImageFile)) = Path_OMap ReportElemID
                                                                   (Path_ReportElem (Maybe (Either URI
                                                                                                   ImageFile)))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems String
    where type PathType ReportElems String = Path_OMap ReportElemID
                                                       (Path_ReportElem String)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Bool
    where type PathType ReportElems Bool = Path_OMap ReportElemID
                                                     (Path_ReportElem Bool)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Bool for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Double
    where type PathType ReportElems Double = Path_OMap ReportElemID
                                                       (Path_ReportElem Double)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Double for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Dimension
    where type PathType ReportElems Dimension = Path_OMap ReportElemID
                                                          (Path_ReportElem Dimension)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ImageCrop
    where type PathType ReportElems ImageCrop = Path_OMap ReportElemID
                                                          (Path_ReportElem ImageCrop)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ImageSize
    where type PathType ReportElems ImageSize = Path_OMap ReportElemID
                                                          (Path_ReportElem ImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Units
    where type PathType ReportElems Units = Path_OMap ReportElemID
                                                      (Path_ReportElem Units)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Units for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ImageFile
    where type PathType ReportElems ImageFile = Path_OMap ReportElemID
                                                          (Path_ReportElem ImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems JSONText
    where type PathType ReportElems JSONText = Path_OMap ReportElemID
                                                         (Path_ReportElem JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Markup
    where type PathType ReportElems Markup = Path_OMap ReportElemID
                                                       (Path_ReportElem Markup)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ReportElem
    where type PathType ReportElems ReportElem = Path_OMap ReportElemID
                                                           (Path_ReportElem ReportElem)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal ReportElem for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ReportElems
    where type PathType ReportElems
                        ReportElems = Path_OMap ReportElemID (Path_ReportElem ReportElems)
          toLens _ = iso id id
instance IsPath ReportElems MaybeImageFile
    where type PathType ReportElems
                        MaybeImageFile = Path_OMap ReportElemID
                                                   (Path_ReportElem MaybeImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ReportImage
    where type PathType ReportElems
                        ReportImage = Path_OMap ReportElemID (Path_ReportElem ReportImage)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImage for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ReportImages
    where type PathType ReportElems
                        ReportImages = Path_OMap ReportElemID
                                                 (Path_ReportElem ReportImages)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems ReportImageView
    where type PathType ReportElems
                        ReportImageView = Path_OMap ReportElemID
                                                    (Path_ReportElem ReportImageView)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems SaneSizeImageSize
    where type PathType ReportElems
                        SaneSizeImageSize = Path_OMap ReportElemID
                                                      (Path_ReportElem SaneSizeImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Item
    where type PathType ReportElems Item = Path_OMap ReportElemID
                                                     (Path_ReportElem Item)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Item for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems URI
    where type PathType ReportElems URI = Path_OMap ReportElemID
                                                    (Path_ReportElem URI)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal URI for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportElems Text
    where type PathType ReportElems Text = Path_OMap ReportElemID
                                                     (Path_ReportElem Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order ReportElemID ReportElem (aka ReportElems): " ++ show u)
instance IsPath ReportFlags String
    where type PathType ReportFlags String = Path_ReportFlags String
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = lens_ReportFlags_hideEmptyItemFields . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportFlags: " ++ show u)
instance IsPath ReportFlags Bool
    where type PathType ReportFlags Bool = Path_ReportFlags Bool
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = lens_ReportFlags_hideEmptyItemFields
          toLens u = error $ ("Unexpected goal Bool for ReportFlags: " ++ show u)
instance IsPath ReportFlags JSONText
    where type PathType ReportFlags
                        JSONText = Path_ReportFlags JSONText
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = lens_ReportFlags_hideEmptyItemFields . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportFlags: " ++ show u)
instance IsPath ReportFlags ReportFlags
    where type PathType ReportFlags
                        ReportFlags = Path_ReportFlags ReportFlags
          toLens _ = iso id id
instance IsPath ReportIntendedUse String
    where type PathType ReportIntendedUse
                        String = Path_ReportIntendedUse String
          toLens (Path_ReportIntendedUse_View _) = viewLens :: Lens' ReportIntendedUse
                                                                     String
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportIntendedUse: " ++ show u)
instance IsPath ReportIntendedUse JSONText
    where type PathType ReportIntendedUse
                        JSONText = Path_ReportIntendedUse JSONText
          toLens (Path_ReportIntendedUse_View v) = (viewLens :: Lens' ReportIntendedUse
                                                                      String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for ReportIntendedUse: " ++ show u)
instance IsPath ReportIntendedUse ReportIntendedUse
    where type PathType ReportIntendedUse
                        ReportIntendedUse = Path_ReportIntendedUse ReportIntendedUse
          toLens _ = iso id id
instance IsPath ReportStandard Int
    where type PathType ReportStandard Int = Path_ReportStandard Int
          toLens (Path_ReportStandard_unReportStandard _x) = lens_ReportStandard_unReportStandard
          toLens u = error $ ("Unexpected goal Int (aka Size) for ReportStandard: " ++ show u)
instance IsPath ReportStandard ReportStandard
    where type PathType ReportStandard
                        ReportStandard = Path_ReportStandard ReportStandard
          toLens _ = iso id id
instance IsPath ReportStatus String
    where type PathType ReportStatus String = Path_ReportStatus String
          toLens (Path_ReportStatus_View _) = viewLens :: Lens' ReportStatus
                                                                String
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportStatus: " ++ show u)
instance IsPath ReportStatus JSONText
    where type PathType ReportStatus
                        JSONText = Path_ReportStatus JSONText
          toLens (Path_ReportStatus_View v) = (viewLens :: Lens' ReportStatus
                                                                 String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for ReportStatus: " ++ show u)
instance IsPath ReportStatus ReportStatus
    where type PathType ReportStatus
                        ReportStatus = Path_ReportStatus ReportStatus
          toLens _ = iso id id
instance IsPath ReportValueApproachInfo JSONText
    where type PathType ReportValueApproachInfo
                        JSONText = Path_ReportValueApproachInfo JSONText
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = lens_ReportValueApproachInfo_reportValueApproachName . toLens _x
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportValueApproachInfo: " ++ show u)
instance IsPath ReportValueApproachInfo Markup
    where type PathType ReportValueApproachInfo
                        Markup = Path_ReportValueApproachInfo Markup
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = lens_ReportValueApproachInfo_reportValueApproachName
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = lens_ReportValueApproachInfo_reportValueApproachDescription
          toLens u = error $ ("Unexpected goal Markup for ReportValueApproachInfo: " ++ show u)
instance IsPath ReportValueApproachInfo ReportValueApproachInfo
    where type PathType ReportValueApproachInfo
                        ReportValueApproachInfo = Path_ReportValueApproachInfo ReportValueApproachInfo
          toLens _ = iso id id
instance IsPath ReportValueApproachInfo Text
    where type PathType ReportValueApproachInfo
                        Text = Path_ReportValueApproachInfo Text
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = lens_ReportValueApproachInfo_reportValueApproachName . toLens _x
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = lens_ReportValueApproachInfo_reportValueApproachDescription . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportValueApproachInfo: " ++ show u)
instance IsPath ReportValueTypeInfo JSONText
    where type PathType ReportValueTypeInfo
                        JSONText = Path_ReportValueTypeInfo JSONText
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = lens_ReportValueTypeInfo_reportValueTypeName . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportValueTypeInfo: " ++ show u)
instance IsPath ReportValueTypeInfo Markup
    where type PathType ReportValueTypeInfo
                        Markup = Path_ReportValueTypeInfo Markup
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = lens_ReportValueTypeInfo_reportValueTypeName
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = lens_ReportValueTypeInfo_reportValueTypeDescription
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = lens_ReportValueTypeInfo_reportValueTypeDefinition
          toLens u = error $ ("Unexpected goal Markup for ReportValueTypeInfo: " ++ show u)
instance IsPath ReportValueTypeInfo ReportValueTypeInfo
    where type PathType ReportValueTypeInfo
                        ReportValueTypeInfo = Path_ReportValueTypeInfo ReportValueTypeInfo
          toLens _ = iso id id
instance IsPath ReportValueTypeInfo Text
    where type PathType ReportValueTypeInfo
                        Text = Path_ReportValueTypeInfo Text
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = lens_ReportValueTypeInfo_reportValueTypeName . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = lens_ReportValueTypeInfo_reportValueTypeDescription . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = lens_ReportValueTypeInfo_reportValueTypeDefinition . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportValueTypeInfo: " ++ show u)
instance IsPath MaybeImageFile String
    where type PathType MaybeImageFile
                        String = Path_MaybeImageFile String
          toLens (Path_MaybeImageFile_View _) = viewLens :: Lens' (Maybe ImageFile)
                                                                  String
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Maybe ImageFile (aka MaybeImageFile): " ++ show u)
instance IsPath MaybeImageFile JSONText
    where type PathType MaybeImageFile
                        JSONText = Path_MaybeImageFile JSONText
          toLens (Path_MaybeImageFile_View v) = (viewLens :: Lens' (Maybe ImageFile)
                                                                   String) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Maybe ImageFile (aka MaybeImageFile): " ++ show u)
instance IsPath MaybeImageFile MaybeImageFile
    where type PathType MaybeImageFile
                        MaybeImageFile = Path_MaybeImageFile MaybeImageFile
          toLens _ = iso id id
instance IsPath ReportImage (Either URI ImageFile)
    where type PathType ReportImage
                        (Either URI ImageFile) = Path_ReportImage (Either URI ImageFile)
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportImage: " ++ show u)
instance IsPath ReportImage (Maybe (Either URI ImageFile))
    where type PathType ReportImage
                        (Maybe (Either URI
                                       ImageFile)) = Path_ReportImage (Maybe (Either URI ImageFile))
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportImage: " ++ show u)
instance IsPath ReportImage String
    where type PathType ReportImage String = Path_ReportImage String
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportImage: " ++ show u)
instance IsPath ReportImage Bool
    where type PathType ReportImage Bool = Path_ReportImage Bool
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Bool for ReportImage: " ++ show u)
instance IsPath ReportImage Double
    where type PathType ReportImage Double = Path_ReportImage Double
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Double for ReportImage: " ++ show u)
instance IsPath ReportImage Dimension
    where type PathType ReportImage
                        Dimension = Path_ReportImage Dimension
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Dimension for ReportImage: " ++ show u)
instance IsPath ReportImage ImageCrop
    where type PathType ReportImage
                        ImageCrop = Path_ReportImage ImageCrop
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for ReportImage: " ++ show u)
instance IsPath ReportImage ImageSize
    where type PathType ReportImage
                        ImageSize = Path_ReportImage ImageSize
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for ReportImage: " ++ show u)
instance IsPath ReportImage Units
    where type PathType ReportImage Units = Path_ReportImage Units
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Units for ReportImage: " ++ show u)
instance IsPath ReportImage ImageFile
    where type PathType ReportImage
                        ImageFile = Path_ReportImage ImageFile
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for ReportImage: " ++ show u)
instance IsPath ReportImage JSONText
    where type PathType ReportImage
                        JSONText = Path_ReportImage JSONText
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for ReportImage: " ++ show u)
instance IsPath ReportImage Markup
    where type PathType ReportImage Markup = Path_ReportImage Markup
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Markup for ReportImage: " ++ show u)
instance IsPath ReportImage MaybeImageFile
    where type PathType ReportImage
                        MaybeImageFile = Path_ReportImage MaybeImageFile
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportImage: " ++ show u)
instance IsPath ReportImage ReportImage
    where type PathType ReportImage
                        ReportImage = Path_ReportImage ReportImage
          toLens _ = iso id id
instance IsPath ReportImage ReportImageView
    where type PathType ReportImage
                        ReportImageView = Path_ReportImage ReportImageView
          toLens (Path_ReportImage_View _) = viewLens :: Lens' ReportImage
                                                               ReportImageView
          toLens u = error $ ("Unexpected goal ReportImageView for ReportImage: " ++ show u)
instance IsPath ReportImage SaneSizeImageSize
    where type PathType ReportImage
                        SaneSizeImageSize = Path_ReportImage SaneSizeImageSize
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportImage: " ++ show u)
instance IsPath ReportImage URI
    where type PathType ReportImage URI = Path_ReportImage URI
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal URI for ReportImage: " ++ show u)
instance IsPath ReportImage Text
    where type PathType ReportImage Text = Path_ReportImage Text
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
          toLens u = error $ ("Unexpected goal Text for ReportImage: " ++ show u)
instance IsPath ReportImages (Either URI ImageFile)
    where type PathType ReportImages
                        (Either URI ImageFile) = Path_OMap ReportImageID
                                                           (Path_ReportImage (Either URI ImageFile))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages (Maybe (Either URI ImageFile))
    where type PathType ReportImages
                        (Maybe (Either URI ImageFile)) = Path_OMap ReportImageID
                                                                   (Path_ReportImage (Maybe (Either URI
                                                                                                    ImageFile)))
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages String
    where type PathType ReportImages String = Path_OMap ReportImageID
                                                        (Path_ReportImage String)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages Bool
    where type PathType ReportImages Bool = Path_OMap ReportImageID
                                                      (Path_ReportImage Bool)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Bool for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages Double
    where type PathType ReportImages Double = Path_OMap ReportImageID
                                                        (Path_ReportImage Double)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Double for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages Dimension
    where type PathType ReportImages
                        Dimension = Path_OMap ReportImageID (Path_ReportImage Dimension)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Dimension for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages ImageCrop
    where type PathType ReportImages
                        ImageCrop = Path_OMap ReportImageID (Path_ReportImage ImageCrop)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageCrop for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages ImageSize
    where type PathType ReportImages
                        ImageSize = Path_OMap ReportImageID (Path_ReportImage ImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageSize for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages Units
    where type PathType ReportImages Units = Path_OMap ReportImageID
                                                       (Path_ReportImage Units)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Units for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages ImageFile
    where type PathType ReportImages
                        ImageFile = Path_OMap ReportImageID (Path_ReportImage ImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ImageFile for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages JSONText
    where type PathType ReportImages JSONText = Path_OMap ReportImageID
                                                          (Path_ReportImage JSONText)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal JSONText for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages Markup
    where type PathType ReportImages Markup = Path_OMap ReportImageID
                                                        (Path_ReportImage Markup)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Markup for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages MaybeImageFile
    where type PathType ReportImages
                        MaybeImageFile = Path_OMap ReportImageID
                                                   (Path_ReportImage MaybeImageFile)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages ReportImage
    where type PathType ReportImages
                        ReportImage = Path_OMap ReportImageID
                                                (Path_ReportImage ReportImage)
          toLens (Path_At k _) = lens_omat k
          toLens u = error $ ("Unexpected goal ReportImage for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages ReportImages
    where type PathType ReportImages
                        ReportImages = Path_OMap ReportImageID
                                                 (Path_ReportImage ReportImages)
          toLens _ = iso id id
instance IsPath ReportImages ReportImageView
    where type PathType ReportImages
                        ReportImageView = Path_OMap ReportImageID
                                                    (Path_ReportImage ReportImageView)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal ReportImageView for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages SaneSizeImageSize
    where type PathType ReportImages
                        SaneSizeImageSize = Path_OMap ReportImageID
                                                      (Path_ReportImage SaneSizeImageSize)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages URI
    where type PathType ReportImages URI = Path_OMap ReportImageID
                                                     (Path_ReportImage URI)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal URI for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReportImages Text
    where type PathType ReportImages Text = Path_OMap ReportImageID
                                                      (Path_ReportImage Text)
          toLens (Path_At k v) = lens_omat k . toLens v
          toLens u = error $ ("Unexpected goal Text for Order ReportImageID ReportImage (aka ReportImages): " ++ show u)
instance IsPath ReadOnlyFilePath ReadOnlyFilePath
    where type PathType ReadOnlyFilePath
                        ReadOnlyFilePath = Path_ReadOnlyFilePath ReadOnlyFilePath
          toLens _ = iso id id
instance IsPath ReportImageView (Either URI ImageFile)
    where type PathType ReportImageView
                        (Either URI ImageFile) = Path_ReportImageView (Either URI
                                                                              ImageFile)
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportImageView: " ++ show u)
instance IsPath ReportImageView (Maybe (Either URI ImageFile))
    where type PathType ReportImageView
                        (Maybe (Either URI
                                       ImageFile)) = Path_ReportImageView (Maybe (Either URI
                                                                                         ImageFile))
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportImageView: " ++ show u)
instance IsPath ReportImageView String
    where type PathType ReportImageView
                        String = Path_ReportImageView String
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens (Path_ReportImageView__picEditedDeprecated _x) = lens_ReportImageView__picEditedDeprecated . toLens _x
          toLens (Path_ReportImageView__picThumbDeprecated _x) = lens_ReportImageView__picThumbDeprecated . toLens _x
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = lens_ReportImageView__picPrinterDeprecated . toLens _x
          toLens (Path_ReportImageView__picMustEnlarge _x) = lens_ReportImageView__picMustEnlarge . toLens _x
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = lens_ReportImageView__picEnlargedDeprecated . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportImageView: " ++ show u)
instance IsPath ReportImageView Bool
    where type PathType ReportImageView
                        Bool = Path_ReportImageView Bool
          toLens (Path_ReportImageView__picMustEnlarge _x) = lens_ReportImageView__picMustEnlarge
          toLens u = error $ ("Unexpected goal Bool for ReportImageView: " ++ show u)
instance IsPath ReportImageView Double
    where type PathType ReportImageView
                        Double = Path_ReportImageView Double
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportImageView: " ++ show u)
instance IsPath ReportImageView Dimension
    where type PathType ReportImageView
                        Dimension = Path_ReportImageView Dimension
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportImageView: " ++ show u)
instance IsPath ReportImageView ImageCrop
    where type PathType ReportImageView
                        ImageCrop = Path_ReportImageView ImageCrop
          toLens (Path_ReportImageView__picCrop _x) = lens_ReportImageView__picCrop
          toLens u = error $ ("Unexpected goal ImageCrop for ReportImageView: " ++ show u)
instance IsPath ReportImageView ImageSize
    where type PathType ReportImageView
                        ImageSize = Path_ReportImageView ImageSize
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportImageView: " ++ show u)
instance IsPath ReportImageView Units
    where type PathType ReportImageView
                        Units = Path_ReportImageView Units
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportImageView: " ++ show u)
instance IsPath ReportImageView ImageFile
    where type PathType ReportImageView
                        ImageFile = Path_ReportImageView ImageFile
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportImageView: " ++ show u)
instance IsPath ReportImageView JSONText
    where type PathType ReportImageView
                        JSONText = Path_ReportImageView JSONText
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize . toLens _x
          toLens (Path_ReportImageView__picCaption _x) = lens_ReportImageView__picCaption . toLens _x
          toLens (Path_ReportImageView__picEditedDeprecated _x) = lens_ReportImageView__picEditedDeprecated . toLens _x
          toLens (Path_ReportImageView__picThumbDeprecated _x) = lens_ReportImageView__picThumbDeprecated . toLens _x
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = lens_ReportImageView__picPrinterDeprecated . toLens _x
          toLens (Path_ReportImageView__picMustEnlarge _x) = lens_ReportImageView__picMustEnlarge . toLens _x
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = lens_ReportImageView__picEnlargedDeprecated . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportImageView: " ++ show u)
instance IsPath ReportImageView Markup
    where type PathType ReportImageView
                        Markup = Path_ReportImageView Markup
          toLens (Path_ReportImageView__picCaption _x) = lens_ReportImageView__picCaption
          toLens u = error $ ("Unexpected goal Markup for ReportImageView: " ++ show u)
instance IsPath ReportImageView MaybeImageFile
    where type PathType ReportImageView
                        MaybeImageFile = Path_ReportImageView MaybeImageFile
          toLens (Path_ReportImageView__picEditedDeprecated _x) = lens_ReportImageView__picEditedDeprecated
          toLens (Path_ReportImageView__picThumbDeprecated _x) = lens_ReportImageView__picThumbDeprecated
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = lens_ReportImageView__picPrinterDeprecated
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = lens_ReportImageView__picEnlargedDeprecated
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportImageView: " ++ show u)
instance IsPath ReportImageView ReportImageView
    where type PathType ReportImageView
                        ReportImageView = Path_ReportImageView ReportImageView
          toLens _ = iso id id
instance IsPath ReportImageView SaneSizeImageSize
    where type PathType ReportImageView
                        SaneSizeImageSize = Path_ReportImageView SaneSizeImageSize
          toLens (Path_ReportImageView__picSize _x) = lens_ReportImageView__picSize
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportImageView: " ++ show u)
instance IsPath ReportImageView URI
    where type PathType ReportImageView URI = Path_ReportImageView URI
          toLens (Path_ReportImageView__picOriginal _x) = lens_ReportImageView__picOriginal . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportImageView: " ++ show u)
instance IsPath ReportImageView Text
    where type PathType ReportImageView
                        Text = Path_ReportImageView Text
          toLens (Path_ReportImageView__picCaption _x) = lens_ReportImageView__picCaption . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportImageView: " ++ show u)
instance IsPath ReportView (Either URI ImageFile)
    where type PathType ReportView
                        (Either URI ImageFile) = Path_ReportView (Either URI ImageFile)
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportView: " ++ show u)
instance IsPath ReportView (Map ItemFieldName Markup)
    where type PathType ReportView
                        (Map ItemFieldName Markup) = Path_ReportView (Map ItemFieldName
                                                                          Markup)
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for ReportView: " ++ show u)
instance IsPath ReportView (Maybe (Either URI ImageFile))
    where type PathType ReportView
                        (Maybe (Either URI ImageFile)) = Path_ReportView (Maybe (Either URI
                                                                                        ImageFile))
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportView: " ++ show u)
instance IsPath ReportView String
    where type PathType ReportView String = Path_ReportView String
          toLens (Path_ReportView__reportIntendedUse _x) = lens_ReportView__reportIntendedUse . toLens _x
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportStatus _x) = lens_ReportView__reportStatus . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = lens_ReportView__reportRedacted . toLens _x
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = lens_ReportView__reportOrderByItemName . toLens _x
          toLens (Path_ReportView__reportDisplayItemName _x) = lens_ReportView__reportDisplayItemName . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportView: " ++ show u)
instance IsPath ReportView Int64
    where type PathType ReportView Int64 = Path_ReportView Int64
          toLens (Path_ReportView__reportCreated _x) = lens_ReportView__reportCreated
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for ReportView: " ++ show u)
instance IsPath ReportView Bool
    where type PathType ReportView Bool = Path_ReportView Bool
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = lens_ReportView__reportRedacted
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = lens_ReportView__reportOrderByItemName
          toLens (Path_ReportView__reportDisplayItemName _x) = lens_ReportView__reportDisplayItemName
          toLens u = error $ ("Unexpected goal Bool for ReportView: " ++ show u)
instance IsPath ReportView Double
    where type PathType ReportView Double = Path_ReportView Double
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportView: " ++ show u)
instance IsPath ReportView Int
    where type PathType ReportView Int = Path_ReportView Int
          toLens (Path_ReportView__reportStandardsVersion _x) = lens_ReportView__reportStandardsVersion . toLens _x
          toLens u = error $ ("Unexpected goal Int (aka Size) for ReportView: " ++ show u)
instance IsPath ReportView Dimension
    where type PathType ReportView
                        Dimension = Path_ReportView Dimension
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportView: " ++ show u)
instance IsPath ReportView ImageCrop
    where type PathType ReportView
                        ImageCrop = Path_ReportView ImageCrop
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for ReportView: " ++ show u)
instance IsPath ReportView ImageSize
    where type PathType ReportView
                        ImageSize = Path_ReportView ImageSize
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportView: " ++ show u)
instance IsPath ReportView Units
    where type PathType ReportView Units = Path_ReportView Units
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportView: " ++ show u)
instance IsPath ReportView ImageFile
    where type PathType ReportView
                        ImageFile = Path_ReportView ImageFile
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportView: " ++ show u)
instance IsPath ReportView Integer
    where type PathType ReportView Integer = Path_ReportView Integer
          toLens (Path_ReportView__reportRevision _x) = lens_ReportView__reportRevision
          toLens u = error $ ("Unexpected goal Integer for ReportView: " ++ show u)
instance IsPath ReportView JSONText
    where type PathType ReportView JSONText = Path_ReportView JSONText
          toLens (Path_ReportView__reportName _x) = lens_ReportView__reportName . toLens _x
          toLens (Path_ReportView__reportDate _x) = lens_ReportView__reportDate . toLens _x
          toLens (Path_ReportView__reportContractDate _x) = lens_ReportView__reportContractDate . toLens _x
          toLens (Path_ReportView__reportInspectionDate _x) = lens_ReportView__reportInspectionDate . toLens _x
          toLens (Path_ReportView__reportEffectiveDate _x) = lens_ReportView__reportEffectiveDate . toLens _x
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = lens_ReportView__reportPreparer . toLens _x
          toLens (Path_ReportView__reportPreparerEIN _x) = lens_ReportView__reportPreparerEIN . toLens _x
          toLens (Path_ReportView__reportPreparerAddress _x) = lens_ReportView__reportPreparerAddress . toLens _x
          toLens (Path_ReportView__reportPreparerEMail _x) = lens_ReportView__reportPreparerEMail . toLens _x
          toLens (Path_ReportView__reportPreparerWebsite _x) = lens_ReportView__reportPreparerWebsite . toLens _x
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens (Path_ReportView__reportTitle _x) = lens_ReportView__reportTitle . toLens _x
          toLens (Path_ReportView__reportHeader _x) = lens_ReportView__reportHeader . toLens _x
          toLens (Path_ReportView__reportFooter _x) = lens_ReportView__reportFooter . toLens _x
          toLens (Path_ReportView__reportIntendedUse _x) = lens_ReportView__reportIntendedUse . toLens _x
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo . toLens _x
          toLens (Path_ReportView__reportClientName _x) = lens_ReportView__reportClientName . toLens _x
          toLens (Path_ReportView__reportClientAddress _x) = lens_ReportView__reportClientAddress . toLens _x
          toLens (Path_ReportView__reportClientGreeting _x) = lens_ReportView__reportClientGreeting . toLens _x
          toLens (Path_ReportView__reportItemsOwnerFull _x) = lens_ReportView__reportItemsOwnerFull . toLens _x
          toLens (Path_ReportView__reportItemsOwner _x) = lens_ReportView__reportItemsOwner . toLens _x
          toLens (Path_ReportView__reportBriefItems _x) = lens_ReportView__reportBriefItems . toLens _x
          toLens (Path_ReportView__reportInspectionLocation _x) = lens_ReportView__reportInspectionLocation . toLens _x
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = lens_ReportView__reportLetterOfTransmittal . toLens _x
          toLens (Path_ReportView__reportScopeOfWork _x) = lens_ReportView__reportScopeOfWork . toLens _x
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = lens_ReportView__reportPrivacyPolicy . toLens _x
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens (Path_ReportView__reportBranding _x) = lens_ReportView__reportBranding . toLens _x
          toLens (Path_ReportView__reportStatus _x) = lens_ReportView__reportStatus . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = lens_ReportView__reportRedacted . toLens _x
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = lens_ReportView__reportOrderByItemName . toLens _x
          toLens (Path_ReportView__reportDisplayItemName _x) = lens_ReportView__reportDisplayItemName . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportView: " ++ show u)
instance IsPath ReportView Markup
    where type PathType ReportView Markup = Path_ReportView Markup
          toLens (Path_ReportView__reportName _x) = lens_ReportView__reportName
          toLens (Path_ReportView__reportDate _x) = lens_ReportView__reportDate
          toLens (Path_ReportView__reportContractDate _x) = lens_ReportView__reportContractDate
          toLens (Path_ReportView__reportInspectionDate _x) = lens_ReportView__reportInspectionDate
          toLens (Path_ReportView__reportEffectiveDate _x) = lens_ReportView__reportEffectiveDate
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = lens_ReportView__reportPreparer
          toLens (Path_ReportView__reportPreparerEIN _x) = lens_ReportView__reportPreparerEIN
          toLens (Path_ReportView__reportPreparerAddress _x) = lens_ReportView__reportPreparerAddress
          toLens (Path_ReportView__reportPreparerEMail _x) = lens_ReportView__reportPreparerEMail
          toLens (Path_ReportView__reportPreparerWebsite _x) = lens_ReportView__reportPreparerWebsite
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens (Path_ReportView__reportTitle _x) = lens_ReportView__reportTitle
          toLens (Path_ReportView__reportHeader _x) = lens_ReportView__reportHeader
          toLens (Path_ReportView__reportFooter _x) = lens_ReportView__reportFooter
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo . toLens _x
          toLens (Path_ReportView__reportClientName _x) = lens_ReportView__reportClientName
          toLens (Path_ReportView__reportClientAddress _x) = lens_ReportView__reportClientAddress
          toLens (Path_ReportView__reportClientGreeting _x) = lens_ReportView__reportClientGreeting
          toLens (Path_ReportView__reportItemsOwnerFull _x) = lens_ReportView__reportItemsOwnerFull
          toLens (Path_ReportView__reportItemsOwner _x) = lens_ReportView__reportItemsOwner
          toLens (Path_ReportView__reportBriefItems _x) = lens_ReportView__reportBriefItems
          toLens (Path_ReportView__reportInspectionLocation _x) = lens_ReportView__reportInspectionLocation
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = lens_ReportView__reportLetterOfTransmittal
          toLens (Path_ReportView__reportScopeOfWork _x) = lens_ReportView__reportScopeOfWork
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = lens_ReportView__reportPrivacyPolicy
          toLens u = error $ ("Unexpected goal Markup for ReportView: " ++ show u)
instance IsPath ReportView Permissions
    where type PathType ReportView
                        Permissions = Path_ReportView Permissions
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms
          toLens u = error $ ("Unexpected goal Permissions for ReportView: " ++ show u)
instance IsPath ReportView UserIds
    where type PathType ReportView UserIds = Path_ReportView UserIds
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for ReportView: " ++ show u)
instance IsPath ReportView AbbrevPair
    where type PathType ReportView
                        AbbrevPair = Path_ReportView AbbrevPair
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for ReportView: " ++ show u)
instance IsPath ReportView AbbrevPairs
    where type PathType ReportView
                        AbbrevPairs = Path_ReportView AbbrevPairs
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for ReportView: " ++ show u)
instance IsPath ReportView Author
    where type PathType ReportView Author = Path_ReportView Author
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens u = error $ ("Unexpected goal Author for ReportView: " ++ show u)
instance IsPath ReportView Authors
    where type PathType ReportView Authors = Path_ReportView Authors
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for ReportView: " ++ show u)
instance IsPath ReportView Branding
    where type PathType ReportView Branding = Path_ReportView Branding
          toLens (Path_ReportView__reportBranding _x) = lens_ReportView__reportBranding
          toLens u = error $ ("Unexpected goal Branding for ReportView: " ++ show u)
instance IsPath ReportView MarkupPair
    where type PathType ReportView
                        MarkupPair = Path_ReportView MarkupPair
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for ReportView: " ++ show u)
instance IsPath ReportView MarkupPairs
    where type PathType ReportView
                        MarkupPairs = Path_ReportView MarkupPairs
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for ReportView: " ++ show u)
instance IsPath ReportView Markups
    where type PathType ReportView Markups = Path_ReportView Markups
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for ReportView: " ++ show u)
instance IsPath ReportView MaybeReportIntendedUse
    where type PathType ReportView
                        MaybeReportIntendedUse = Path_ReportView MaybeReportIntendedUse
          toLens (Path_ReportView__reportIntendedUse _x) = lens_ReportView__reportIntendedUse
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for ReportView: " ++ show u)
instance IsPath ReportView ReportElem
    where type PathType ReportView
                        ReportElem = Path_ReportView ReportElem
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ReportElem for ReportView: " ++ show u)
instance IsPath ReportView ReportElems
    where type PathType ReportView
                        ReportElems = Path_ReportView ReportElems
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for ReportView: " ++ show u)
instance IsPath ReportView ReportFlags
    where type PathType ReportView
                        ReportFlags = Path_ReportView ReportFlags
          toLens (Path_ReportView__reportFlags _x) = lens_ReportView__reportFlags
          toLens u = error $ ("Unexpected goal ReportFlags for ReportView: " ++ show u)
instance IsPath ReportView ReportStandard
    where type PathType ReportView
                        ReportStandard = Path_ReportView ReportStandard
          toLens (Path_ReportView__reportStandardsVersion _x) = lens_ReportView__reportStandardsVersion
          toLens u = error $ ("Unexpected goal ReportStandard for ReportView: " ++ show u)
instance IsPath ReportView ReportStatus
    where type PathType ReportView
                        ReportStatus = Path_ReportView ReportStatus
          toLens (Path_ReportView__reportStatus _x) = lens_ReportView__reportStatus
          toLens u = error $ ("Unexpected goal ReportStatus for ReportView: " ++ show u)
instance IsPath ReportView ReportValueApproachInfo
    where type PathType ReportView
                        ReportValueApproachInfo = Path_ReportView ReportValueApproachInfo
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for ReportView: " ++ show u)
instance IsPath ReportView ReportValueTypeInfo
    where type PathType ReportView
                        ReportValueTypeInfo = Path_ReportView ReportValueTypeInfo
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for ReportView: " ++ show u)
instance IsPath ReportView MaybeImageFile
    where type PathType ReportView
                        MaybeImageFile = Path_ReportView MaybeImageFile
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportView: " ++ show u)
instance IsPath ReportView ReportImage
    where type PathType ReportView
                        ReportImage = Path_ReportView ReportImage
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for ReportView: " ++ show u)
instance IsPath ReportView ReportImages
    where type PathType ReportView
                        ReportImages = Path_ReportView ReportImages
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for ReportView: " ++ show u)
instance IsPath ReportView ReadOnlyFilePath
    where type PathType ReportView
                        ReadOnlyFilePath = Path_ReportView ReadOnlyFilePath
          toLens (Path_ReportView__reportFolder _x) = lens_ReportView__reportFolder
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for ReportView: " ++ show u)
instance IsPath ReportView ReportImageView
    where type PathType ReportView
                        ReportImageView = Path_ReportView ReportImageView
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for ReportView: " ++ show u)
instance IsPath ReportView ReportView
    where type PathType ReportView
                        ReportView = Path_ReportView ReportView
          toLens _ = iso id id
instance IsPath ReportView SaneSizeImageSize
    where type PathType ReportView
                        SaneSizeImageSize = Path_ReportView SaneSizeImageSize
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportView: " ++ show u)
instance IsPath ReportView Item
    where type PathType ReportView Item = Path_ReportView Item
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal Item for ReportView: " ++ show u)
instance IsPath ReportView CIString
    where type PathType ReportView CIString = Path_ReportView CIString
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens u = error $ ("Unexpected goal CIString for ReportView: " ++ show u)
instance IsPath ReportView URI
    where type PathType ReportView URI = Path_ReportView URI
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportView: " ++ show u)
instance IsPath ReportView Text
    where type PathType ReportView Text = Path_ReportView Text
          toLens (Path_ReportView__reportName _x) = lens_ReportView__reportName . toLens _x
          toLens (Path_ReportView__reportDate _x) = lens_ReportView__reportDate . toLens _x
          toLens (Path_ReportView__reportContractDate _x) = lens_ReportView__reportContractDate . toLens _x
          toLens (Path_ReportView__reportInspectionDate _x) = lens_ReportView__reportInspectionDate . toLens _x
          toLens (Path_ReportView__reportEffectiveDate _x) = lens_ReportView__reportEffectiveDate . toLens _x
          toLens (Path_ReportView__reportAuthors _x) = lens_ReportView__reportAuthors . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = lens_ReportView__reportPreparer . toLens _x
          toLens (Path_ReportView__reportPreparerEIN _x) = lens_ReportView__reportPreparerEIN . toLens _x
          toLens (Path_ReportView__reportPreparerAddress _x) = lens_ReportView__reportPreparerAddress . toLens _x
          toLens (Path_ReportView__reportPreparerEMail _x) = lens_ReportView__reportPreparerEMail . toLens _x
          toLens (Path_ReportView__reportPreparerWebsite _x) = lens_ReportView__reportPreparerWebsite . toLens _x
          toLens (Path_ReportView__reportAbbrevs _x) = lens_ReportView__reportAbbrevs . toLens _x
          toLens (Path_ReportView__reportTitle _x) = lens_ReportView__reportTitle . toLens _x
          toLens (Path_ReportView__reportHeader _x) = lens_ReportView__reportHeader . toLens _x
          toLens (Path_ReportView__reportFooter _x) = lens_ReportView__reportFooter . toLens _x
          toLens (Path_ReportView__reportValueTypeInfo _x) = lens_ReportView__reportValueTypeInfo . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = lens_ReportView__reportValueApproachInfo . toLens _x
          toLens (Path_ReportView__reportClientName _x) = lens_ReportView__reportClientName . toLens _x
          toLens (Path_ReportView__reportClientAddress _x) = lens_ReportView__reportClientAddress . toLens _x
          toLens (Path_ReportView__reportClientGreeting _x) = lens_ReportView__reportClientGreeting . toLens _x
          toLens (Path_ReportView__reportItemsOwnerFull _x) = lens_ReportView__reportItemsOwnerFull . toLens _x
          toLens (Path_ReportView__reportItemsOwner _x) = lens_ReportView__reportItemsOwner . toLens _x
          toLens (Path_ReportView__reportBriefItems _x) = lens_ReportView__reportBriefItems . toLens _x
          toLens (Path_ReportView__reportInspectionLocation _x) = lens_ReportView__reportInspectionLocation . toLens _x
          toLens (Path_ReportView__reportBody _x) = lens_ReportView__reportBody . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = lens_ReportView__reportGlossary . toLens _x
          toLens (Path_ReportView__reportSources _x) = lens_ReportView__reportSources . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = lens_ReportView__reportLetterOfTransmittal . toLens _x
          toLens (Path_ReportView__reportScopeOfWork _x) = lens_ReportView__reportScopeOfWork . toLens _x
          toLens (Path_ReportView__reportCertification _x) = lens_ReportView__reportCertification . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = lens_ReportView__reportLimitingConditions . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = lens_ReportView__reportPrivacyPolicy . toLens _x
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens (Path_ReportView__reportBranding _x) = lens_ReportView__reportBranding . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportView: " ++ show u)
instance IsPath ReportView UserId
    where type PathType ReportView UserId = Path_ReportView UserId
          toLens (Path_ReportView__reportPerms _x) = lens_ReportView__reportPerms . toLens _x
          toLens u = error $ ("Unexpected goal UserId for ReportView: " ++ show u)
instance IsPath ReportView UUID
    where type PathType ReportView UUID = Path_ReportView UUID
          toLens (Path_ReportView__reportUUID _x) = lens_ReportView__reportUUID
          toLens u = error $ ("Unexpected goal UUID for ReportView: " ++ show u)
instance IsPath SaneSizeImageSize String
    where type PathType SaneSizeImageSize
                        String = Path_SaneSizeImageSize String
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance IsPath SaneSizeImageSize Double
    where type PathType SaneSizeImageSize
                        Double = Path_SaneSizeImageSize Double
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal Double for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance IsPath SaneSizeImageSize Dimension
    where type PathType SaneSizeImageSize
                        Dimension = Path_SaneSizeImageSize Dimension
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal Dimension for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance IsPath SaneSizeImageSize ImageSize
    where type PathType SaneSizeImageSize
                        ImageSize = Path_SaneSizeImageSize ImageSize
          toLens (Path_SaneSizeImageSize_View _) = viewLens :: Lens' (SaneSize ImageSize)
                                                                     ImageSize
          toLens u = error $ ("Unexpected goal ImageSize for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance IsPath SaneSizeImageSize Units
    where type PathType SaneSizeImageSize
                        Units = Path_SaneSizeImageSize Units
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal Units for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance IsPath SaneSizeImageSize JSONText
    where type PathType SaneSizeImageSize
                        JSONText = Path_SaneSizeImageSize JSONText
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for SaneSize ImageSize (aka SaneSizeImageSize): " ++ show u)
instance IsPath SaneSizeImageSize SaneSizeImageSize
    where type PathType SaneSizeImageSize
                        SaneSizeImageSize = Path_SaneSizeImageSize SaneSizeImageSize
          toLens _ = iso id id
instance IsPath Item (Either URI ImageFile)
    where type PathType Item
                        (Either URI ImageFile) = Path_Item (Either URI ImageFile)
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for Item: " ++ show u)
instance IsPath Item (Map ItemFieldName Markup)
    where type PathType Item
                        (Map ItemFieldName Markup) = Path_Item (Map ItemFieldName Markup)
          toLens (Path_Item_fields _x) = lens_Item_fields
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for Item: " ++ show u)
instance IsPath Item (Maybe (Either URI ImageFile))
    where type PathType Item
                        (Maybe (Either URI ImageFile)) = Path_Item (Maybe (Either URI
                                                                                  ImageFile))
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for Item: " ++ show u)
instance IsPath Item String
    where type PathType Item String = Path_Item String
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for Item: " ++ show u)
instance IsPath Item Bool
    where type PathType Item Bool = Path_Item Bool
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Bool for Item: " ++ show u)
instance IsPath Item Double
    where type PathType Item Double = Path_Item Double
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Double for Item: " ++ show u)
instance IsPath Item Dimension
    where type PathType Item Dimension = Path_Item Dimension
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for Item: " ++ show u)
instance IsPath Item ImageCrop
    where type PathType Item ImageCrop = Path_Item ImageCrop
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for Item: " ++ show u)
instance IsPath Item ImageSize
    where type PathType Item ImageSize = Path_Item ImageSize
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for Item: " ++ show u)
instance IsPath Item Units
    where type PathType Item Units = Path_Item Units
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Units for Item: " ++ show u)
instance IsPath Item ImageFile
    where type PathType Item ImageFile = Path_Item ImageFile
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for Item: " ++ show u)
instance IsPath Item JSONText
    where type PathType Item JSONText = Path_Item JSONText
          toLens (Path_Item_itemName _x) = lens_Item_itemName . toLens _x
          toLens (Path_Item_fields _x) = lens_Item_fields . toLens _x
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for Item: " ++ show u)
instance IsPath Item Markup
    where type PathType Item Markup = Path_Item Markup
          toLens (Path_Item_fields _x) = lens_Item_fields . toLens _x
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Markup for Item: " ++ show u)
instance IsPath Item MaybeImageFile
    where type PathType Item MaybeImageFile = Path_Item MaybeImageFile
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for Item: " ++ show u)
instance IsPath Item ReportImage
    where type PathType Item ReportImage = Path_Item ReportImage
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for Item: " ++ show u)
instance IsPath Item ReportImages
    where type PathType Item ReportImages = Path_Item ReportImages
          toLens (Path_Item_images _x) = lens_Item_images
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for Item: " ++ show u)
instance IsPath Item ReportImageView
    where type PathType Item
                        ReportImageView = Path_Item ReportImageView
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for Item: " ++ show u)
instance IsPath Item SaneSizeImageSize
    where type PathType Item
                        SaneSizeImageSize = Path_Item SaneSizeImageSize
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for Item: " ++ show u)
instance IsPath Item Item
    where type PathType Item Item = Path_Item Item
          toLens _ = iso id id
instance IsPath Item URI
    where type PathType Item URI = Path_Item URI
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal URI for Item: " ++ show u)
instance IsPath Item Text
    where type PathType Item Text = Path_Item Text
          toLens (Path_Item_itemName _x) = lens_Item_itemName
          toLens (Path_Item_fields _x) = lens_Item_fields . toLens _x
          toLens (Path_Item_images _x) = lens_Item_images . toLens _x
          toLens u = error $ ("Unexpected goal Text for Item: " ++ show u)
instance IsPath ReportMap (Either URI ImageFile)
    where type PathType ReportMap
                        (Either URI ImageFile) = Path_ReportMap (Either URI ImageFile)
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Either URI ImageFile for ReportMap: " ++ show u)
instance IsPath ReportMap (Map ItemFieldName Markup)
    where type PathType ReportMap
                        (Map ItemFieldName Markup) = Path_ReportMap (Map ItemFieldName
                                                                         Markup)
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Map ItemFieldName Markup for ReportMap: " ++ show u)
instance IsPath ReportMap (Map ReportID Report)
    where type PathType ReportMap
                        (Map ReportID Report) = Path_ReportMap (Map ReportID Report)
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap
          toLens u = error $ ("Unexpected goal Map ReportID Report for ReportMap: " ++ show u)
instance IsPath ReportMap (Maybe (Either URI ImageFile))
    where type PathType ReportMap
                        (Maybe (Either URI ImageFile)) = Path_ReportMap (Maybe (Either URI
                                                                                       ImageFile))
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Maybe (Either URI ImageFile) for ReportMap: " ++ show u)
instance IsPath ReportMap String
    where type PathType ReportMap String = Path_ReportMap String
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal [Char] (aka Checksum, aka FilePath, aka String) for ReportMap: " ++ show u)
instance IsPath ReportMap Int64
    where type PathType ReportMap Int64 = Path_ReportMap Int64
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Int64 (aka EpochMilli) for ReportMap: " ++ show u)
instance IsPath ReportMap Bool
    where type PathType ReportMap Bool = Path_ReportMap Bool
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Bool for ReportMap: " ++ show u)
instance IsPath ReportMap Double
    where type PathType ReportMap Double = Path_ReportMap Double
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Double for ReportMap: " ++ show u)
instance IsPath ReportMap Int
    where type PathType ReportMap Int = Path_ReportMap Int
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Int (aka Size) for ReportMap: " ++ show u)
instance IsPath ReportMap Dimension
    where type PathType ReportMap Dimension = Path_ReportMap Dimension
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Dimension for ReportMap: " ++ show u)
instance IsPath ReportMap ImageCrop
    where type PathType ReportMap ImageCrop = Path_ReportMap ImageCrop
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ImageCrop for ReportMap: " ++ show u)
instance IsPath ReportMap ImageSize
    where type PathType ReportMap ImageSize = Path_ReportMap ImageSize
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ImageSize for ReportMap: " ++ show u)
instance IsPath ReportMap Units
    where type PathType ReportMap Units = Path_ReportMap Units
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Units for ReportMap: " ++ show u)
instance IsPath ReportMap ImageFile
    where type PathType ReportMap ImageFile = Path_ReportMap ImageFile
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ImageFile for ReportMap: " ++ show u)
instance IsPath ReportMap Integer
    where type PathType ReportMap Integer = Path_ReportMap Integer
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Integer for ReportMap: " ++ show u)
instance IsPath ReportMap JSONText
    where type PathType ReportMap JSONText = Path_ReportMap JSONText
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal JSONText for ReportMap: " ++ show u)
instance IsPath ReportMap Markup
    where type PathType ReportMap Markup = Path_ReportMap Markup
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Markup for ReportMap: " ++ show u)
instance IsPath ReportMap Permissions
    where type PathType ReportMap
                        Permissions = Path_ReportMap Permissions
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Permissions for ReportMap: " ++ show u)
instance IsPath ReportMap UserIds
    where type PathType ReportMap UserIds = Path_ReportMap UserIds
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal [UserId] (aka UserIds) for ReportMap: " ++ show u)
instance IsPath ReportMap AbbrevPair
    where type PathType ReportMap
                        AbbrevPair = Path_ReportMap AbbrevPair
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal (CIString, Markup) (aka AbbrevPair) for ReportMap: " ++ show u)
instance IsPath ReportMap AbbrevPairs
    where type PathType ReportMap
                        AbbrevPairs = Path_ReportMap AbbrevPairs
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order AbbrevPairID ((CIString, Markup)) (aka AbbrevPairs) for ReportMap: " ++ show u)
instance IsPath ReportMap Author
    where type PathType ReportMap Author = Path_ReportMap Author
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Author for ReportMap: " ++ show u)
instance IsPath ReportMap Authors
    where type PathType ReportMap Authors = Path_ReportMap Authors
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order AuthorID Author (aka Authors) for ReportMap: " ++ show u)
instance IsPath ReportMap Branding
    where type PathType ReportMap Branding = Path_ReportMap Branding
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Branding for ReportMap: " ++ show u)
instance IsPath ReportMap MarkupPair
    where type PathType ReportMap
                        MarkupPair = Path_ReportMap MarkupPair
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal (Markup, Markup) (aka MarkupPair) for ReportMap: " ++ show u)
instance IsPath ReportMap MarkupPairs
    where type PathType ReportMap
                        MarkupPairs = Path_ReportMap MarkupPairs
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order MarkupPairID ((Markup, Markup)) (aka MarkupPairs) for ReportMap: " ++ show u)
instance IsPath ReportMap Markups
    where type PathType ReportMap Markups = Path_ReportMap Markups
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order MarkupID Markup (aka Markups) for ReportMap: " ++ show u)
instance IsPath ReportMap MaybeReportIntendedUse
    where type PathType ReportMap
                        MaybeReportIntendedUse = Path_ReportMap MaybeReportIntendedUse
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ReportIntendedUse (aka MaybeReportIntendedUse) for ReportMap: " ++ show u)
instance IsPath ReportMap Report
    where type PathType ReportMap Report = Path_ReportMap Report
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Report for ReportMap: " ++ show u)
instance IsPath ReportMap ReportElem
    where type PathType ReportMap
                        ReportElem = Path_ReportMap ReportElem
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportElem for ReportMap: " ++ show u)
instance IsPath ReportMap ReportElems
    where type PathType ReportMap
                        ReportElems = Path_ReportMap ReportElems
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportElemID ReportElem (aka ReportElems) for ReportMap: " ++ show u)
instance IsPath ReportMap ReportFlags
    where type PathType ReportMap
                        ReportFlags = Path_ReportMap ReportFlags
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportFlags for ReportMap: " ++ show u)
instance IsPath ReportMap ReportStandard
    where type PathType ReportMap
                        ReportStandard = Path_ReportMap ReportStandard
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportStandard for ReportMap: " ++ show u)
instance IsPath ReportMap ReportStatus
    where type PathType ReportMap
                        ReportStatus = Path_ReportMap ReportStatus
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportStatus for ReportMap: " ++ show u)
instance IsPath ReportMap ReportValueApproachInfo
    where type PathType ReportMap
                        ReportValueApproachInfo = Path_ReportMap ReportValueApproachInfo
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportValueApproachInfo for ReportMap: " ++ show u)
instance IsPath ReportMap ReportValueTypeInfo
    where type PathType ReportMap
                        ReportValueTypeInfo = Path_ReportMap ReportValueTypeInfo
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportValueTypeInfo for ReportMap: " ++ show u)
instance IsPath ReportMap MaybeImageFile
    where type PathType ReportMap
                        MaybeImageFile = Path_ReportMap MaybeImageFile
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Maybe ImageFile (aka MaybeImageFile) for ReportMap: " ++ show u)
instance IsPath ReportMap ReportImage
    where type PathType ReportMap
                        ReportImage = Path_ReportMap ReportImage
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportImage for ReportMap: " ++ show u)
instance IsPath ReportMap ReportImages
    where type PathType ReportMap
                        ReportImages = Path_ReportMap ReportImages
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Order ReportImageID ReportImage (aka ReportImages) for ReportMap: " ++ show u)
instance IsPath ReportMap ReadOnlyFilePath
    where type PathType ReportMap
                        ReadOnlyFilePath = Path_ReportMap ReadOnlyFilePath
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReadOnly ([Char]) (aka ReadOnlyFilePath) for ReportMap: " ++ show u)
instance IsPath ReportMap ReportImageView
    where type PathType ReportMap
                        ReportImageView = Path_ReportMap ReportImageView
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportImageView for ReportMap: " ++ show u)
instance IsPath ReportMap ReportView
    where type PathType ReportMap
                        ReportView = Path_ReportMap ReportView
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal ReportView for ReportMap: " ++ show u)
instance IsPath ReportMap SaneSizeImageSize
    where type PathType ReportMap
                        SaneSizeImageSize = Path_ReportMap SaneSizeImageSize
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal SaneSize ImageSize (aka SaneSizeImageSize) for ReportMap: " ++ show u)
instance IsPath ReportMap Item
    where type PathType ReportMap Item = Path_ReportMap Item
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Item for ReportMap: " ++ show u)
instance IsPath ReportMap ReportMap
    where type PathType ReportMap ReportMap = Path_ReportMap ReportMap
          toLens _ = iso id id
instance IsPath ReportMap CIString
    where type PathType ReportMap CIString = Path_ReportMap CIString
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal CIString for ReportMap: " ++ show u)
instance IsPath ReportMap URI
    where type PathType ReportMap URI = Path_ReportMap URI
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal URI for ReportMap: " ++ show u)
instance IsPath ReportMap Text
    where type PathType ReportMap Text = Path_ReportMap Text
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal Text for ReportMap: " ++ show u)
instance IsPath ReportMap UserId
    where type PathType ReportMap UserId = Path_ReportMap UserId
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal UserId for ReportMap: " ++ show u)
instance IsPath ReportMap UUID
    where type PathType ReportMap UUID = Path_ReportMap UUID
          toLens (Path_ReportMap_unReportMap _x) = lens_ReportMap_unReportMap . toLens _x
          toLens u = error $ ("Unexpected goal UUID for ReportMap: " ++ show u)
instance IsPath CIString JSONText
    where type PathType CIString JSONText = Path_CIString JSONText
          toLens (Path_CIString_View v) = (viewLens :: Lens' CIString
                                                             Text) . toLens v
          toLens u = error $ ("Unexpected goal JSONText for CIString: " ++ show u)
instance IsPath CIString CIString
    where type PathType CIString CIString = Path_CIString CIString
          toLens _ = iso id id
instance IsPath CIString Text
    where type PathType CIString Text = Path_CIString Text
          toLens (Path_CIString_View _) = viewLens :: Lens' CIString Text
          toLens u = error $ ("Unexpected goal Text for CIString: " ++ show u)
instance IsPath URI URI
    where type PathType URI URI = Path_URI URI
          toLens _ = iso id id
instance IsPath Text JSONText
    where type PathType Text JSONText = Path_Text JSONText
          toLens (Path_Text_View _) = viewLens :: Lens' Text JSONText
          toLens u = error $ ("Unexpected goal JSONText for Text: " ++ show u)
instance IsPath Text Text
    where type PathType Text Text = Path_Text Text
          toLens _ = iso id id
instance IsPath UserId UserId
    where type PathType UserId UserId = Path_UserId UserId
          toLens _ = iso id id
instance IsPath UUID UUID
    where type PathType UUID UUID = Path_UUID UUID
          toLens _ = iso id id
instance HasAuthor Author
    where lens_author = id
          lens_Author_authorCredentials f (Author x1
                                                  x2) = fmap (\y1 -> Author x1 y1) (f x2)
          {-# INLINE lens_Author_authorCredentials #-}
          lens_Author_authorName f (Author x1
                                           x2) = fmap (\y1 -> Author y1 x2) (f x1)
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
instance HasImageSize ImageSize
    where lens_imageSize = id
          lens_ImageSize_dim f (ImageSize x1
                                          x2
                                          x3) = fmap (\y1 -> ImageSize y1 x2 x3) (f x1)
          {-# INLINE lens_ImageSize_dim #-}
          lens_ImageSize_size f (ImageSize x1
                                           x2
                                           x3) = fmap (\y1 -> ImageSize x1 y1 x3) (f x2)
          {-# INLINE lens_ImageSize_size #-}
          lens_ImageSize_units f (ImageSize x1
                                            x2
                                            x3) = fmap (\y1 -> ImageSize x1 x2 y1) (f x3)
          {-# INLINE lens_ImageSize_units #-}
instance HasItem Item
    where lens_item = id
          lens_Item_fields f (Item x1
                                   x2
                                   x3) = fmap (\y1 -> Item x1 y1 x3) (f x2)
          {-# INLINE lens_Item_fields #-}
          lens_Item_images f (Item x1
                                   x2
                                   x3) = fmap (\y1 -> Item x1 x2 y1) (f x3)
          {-# INLINE lens_Item_images #-}
          lens_Item_itemName f (Item x1
                                     x2
                                     x3) = fmap (\y1 -> Item y1 x2 x3) (f x1)
          {-# INLINE lens_Item_itemName #-}
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
          lens_Permissions_owner f (Permissions x1
                                                x2
                                                x3) = fmap (\y1 -> Permissions y1 x2 x3) (f x1)
          {-# INLINE lens_Permissions_owner #-}
          lens_Permissions_readers f (Permissions x1
                                                  x2
                                                  x3) = fmap (\y1 -> Permissions x1 x2 y1) (f x3)
          {-# INLINE lens_Permissions_readers #-}
          lens_Permissions_writers f (Permissions x1
                                                  x2
                                                  x3) = fmap (\y1 -> Permissions x1 y1 x3) (f x2)
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
          lens_ReportImage_picCaption f (Pic x1
                                             x2
                                             x3
                                             x4
                                             x5
                                             x6
                                             x7
                                             x8
                                             x9) = fmap (\y1 -> Pic x1 x2 y1 x4 x5 x6 x7 x8 x9) (f x3)
          {-# INLINE lens_ReportImage_picCaption #-}
          lens_ReportImage_picCrop f (Pic x1
                                          x2
                                          x3
                                          x4
                                          x5
                                          x6
                                          x7
                                          x8
                                          x9) = fmap (\y1 -> Pic x1 y1 x3 x4 x5 x6 x7 x8 x9) (f x2)
          {-# INLINE lens_ReportImage_picCrop #-}
          lens_ReportImage_picEditedDeprecated f (Pic x1
                                                      x2
                                                      x3
                                                      x4
                                                      x5
                                                      x6
                                                      x7
                                                      x8
                                                      x9) = fmap (\y1 -> Pic x1 x2 x3 x4 y1 x6 x7 x8 x9) (f x5)
          {-# INLINE lens_ReportImage_picEditedDeprecated #-}
          lens_ReportImage_picEnlargedDeprecated f (Pic x1
                                                        x2
                                                        x3
                                                        x4
                                                        x5
                                                        x6
                                                        x7
                                                        x8
                                                        x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 x7 x8 y1) (f x9)
          {-# INLINE lens_ReportImage_picEnlargedDeprecated #-}
          lens_ReportImage_picMustEnlarge f (Pic x1
                                                 x2
                                                 x3
                                                 x4
                                                 x5
                                                 x6
                                                 x7
                                                 x8
                                                 x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 x7 y1 x9) (f x8)
          {-# INLINE lens_ReportImage_picMustEnlarge #-}
          lens_ReportImage_picOriginal f (Pic x1
                                              x2
                                              x3
                                              x4
                                              x5
                                              x6
                                              x7
                                              x8
                                              x9) = fmap (\y1 -> Pic x1 x2 x3 y1 x5 x6 x7 x8 x9) (f x4)
          {-# INLINE lens_ReportImage_picOriginal #-}
          lens_ReportImage_picPrinterDeprecated f (Pic x1
                                                       x2
                                                       x3
                                                       x4
                                                       x5
                                                       x6
                                                       x7
                                                       x8
                                                       x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 x6 y1 x8 x9) (f x7)
          {-# INLINE lens_ReportImage_picPrinterDeprecated #-}
          lens_ReportImage_picSize f (Pic x1
                                          x2
                                          x3
                                          x4
                                          x5
                                          x6
                                          x7
                                          x8
                                          x9) = fmap (\y1 -> Pic y1 x2 x3 x4 x5 x6 x7 x8 x9) (f x1)
          {-# INLINE lens_ReportImage_picSize #-}
          lens_ReportImage_picThumbDeprecated f (Pic x1
                                                     x2
                                                     x3
                                                     x4
                                                     x5
                                                     x6
                                                     x7
                                                     x8
                                                     x9) = fmap (\y1 -> Pic x1 x2 x3 x4 x5 y1 x7 x8 x9) (f x6)
          {-# INLINE lens_ReportImage_picThumbDeprecated #-}
instance HasReportImageView ReportImageView
    where lens_reportImageView = id
          lens_ReportImageView__picCaption f (ReportImageView x1
                                                              x2
                                                              x3
                                                              x4
                                                              x5
                                                              x6
                                                              x7
                                                              x8
                                                              x9) = fmap (\y1 -> ReportImageView x1 x2 y1 x4 x5 x6 x7 x8 x9) (f x3)
          {-# INLINE lens_ReportImageView__picCaption #-}
          lens_ReportImageView__picCrop f (ReportImageView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9) = fmap (\y1 -> ReportImageView x1 y1 x3 x4 x5 x6 x7 x8 x9) (f x2)
          {-# INLINE lens_ReportImageView__picCrop #-}
          lens_ReportImageView__picEditedDeprecated f (ReportImageView x1
                                                                       x2
                                                                       x3
                                                                       x4
                                                                       x5
                                                                       x6
                                                                       x7
                                                                       x8
                                                                       x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 y1 x6 x7 x8 x9) (f x5)
          {-# INLINE lens_ReportImageView__picEditedDeprecated #-}
          lens_ReportImageView__picEnlargedDeprecated f (ReportImageView x1
                                                                         x2
                                                                         x3
                                                                         x4
                                                                         x5
                                                                         x6
                                                                         x7
                                                                         x8
                                                                         x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 x7 x8 y1) (f x9)
          {-# INLINE lens_ReportImageView__picEnlargedDeprecated #-}
          lens_ReportImageView__picMustEnlarge f (ReportImageView x1
                                                                  x2
                                                                  x3
                                                                  x4
                                                                  x5
                                                                  x6
                                                                  x7
                                                                  x8
                                                                  x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 x7 y1 x9) (f x8)
          {-# INLINE lens_ReportImageView__picMustEnlarge #-}
          lens_ReportImageView__picOriginal f (ReportImageView x1
                                                               x2
                                                               x3
                                                               x4
                                                               x5
                                                               x6
                                                               x7
                                                               x8
                                                               x9) = fmap (\y1 -> ReportImageView x1 x2 x3 y1 x5 x6 x7 x8 x9) (f x4)
          {-# INLINE lens_ReportImageView__picOriginal #-}
          lens_ReportImageView__picPrinterDeprecated f (ReportImageView x1
                                                                        x2
                                                                        x3
                                                                        x4
                                                                        x5
                                                                        x6
                                                                        x7
                                                                        x8
                                                                        x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 x6 y1 x8 x9) (f x7)
          {-# INLINE lens_ReportImageView__picPrinterDeprecated #-}
          lens_ReportImageView__picSize f (ReportImageView x1
                                                           x2
                                                           x3
                                                           x4
                                                           x5
                                                           x6
                                                           x7
                                                           x8
                                                           x9) = fmap (\y1 -> ReportImageView y1 x2 x3 x4 x5 x6 x7 x8 x9) (f x1)
          {-# INLINE lens_ReportImageView__picSize #-}
          lens_ReportImageView__picThumbDeprecated f (ReportImageView x1
                                                                      x2
                                                                      x3
                                                                      x4
                                                                      x5
                                                                      x6
                                                                      x7
                                                                      x8
                                                                      x9) = fmap (\y1 -> ReportImageView x1 x2 x3 x4 x5 y1 x7 x8 x9) (f x6)
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
          lens_ReportValueApproachInfo_reportValueApproachDescription f (ReportValueApproachInfo x1
                                                                                                 x2) = fmap (\y1 -> ReportValueApproachInfo x1 y1) (f x2)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachDescription #-}
          lens_ReportValueApproachInfo_reportValueApproachName f (ReportValueApproachInfo x1
                                                                                          x2) = fmap (\y1 -> ReportValueApproachInfo y1 x2) (f x1)
          {-# INLINE lens_ReportValueApproachInfo_reportValueApproachName #-}
instance HasReportValueTypeInfo ReportValueTypeInfo
    where lens_reportValueTypeInfo = id
          lens_ReportValueTypeInfo_reportValueTypeDefinition f (ReportValueTypeInfo x1
                                                                                    x2
                                                                                    x3) = fmap (\y1 -> ReportValueTypeInfo x1 x2 y1) (f x3)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDefinition #-}
          lens_ReportValueTypeInfo_reportValueTypeDescription f (ReportValueTypeInfo x1
                                                                                     x2
                                                                                     x3) = fmap (\y1 -> ReportValueTypeInfo x1 y1 x3) (f x2)
          {-# INLINE lens_ReportValueTypeInfo_reportValueTypeDescription #-}
          lens_ReportValueTypeInfo_reportValueTypeName f (ReportValueTypeInfo x1
                                                                              x2
                                                                              x3) = fmap (\y1 -> ReportValueTypeInfo y1 x2 x3) (f x1)
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
instance HasUnits Units
    where lens_units = id
instance IsPathType (Path_Author a)
    where idPath = Path_Author
instance IsPathType (Path_Bool a)
    where idPath = Path_Bool
instance IsPathType (Path_Branding a)
    where idPath = Path_Branding
instance IsPathType (Path_CIString a)
    where idPath = Path_CIString
instance IsPathType (Path_Dimension a)
    where idPath = Path_Dimension
instance IsPathType (Path_Double a)
    where idPath = Path_Double
instance IsPathType (Path_ImageCrop a)
    where idPath = Path_ImageCrop
instance IsPathType (Path_ImageFile a)
    where idPath = Path_ImageFile
instance IsPathType (Path_ImageSize a)
    where idPath = Path_ImageSize
instance IsPathType (Path_Int a)
    where idPath = Path_Int
instance IsPathType (Path_Int64 a)
    where idPath = Path_Int64
instance IsPathType (Path_Integer a)
    where idPath = Path_Integer
instance IsPathType (Path_Item a)
    where idPath = Path_Item
instance IsPathType (Path_JSONText a)
    where idPath = Path_JSONText
instance IsPathType (Path_Markup a)
    where idPath = Path_Markup
instance IsPathType (Path_MaybeImageFile a)
    where idPath = Path_MaybeImageFile
instance IsPathType (Path_MaybeReportIntendedUse a)
    where idPath = Path_MaybeReportIntendedUse
instance IsPathType (Path_Permissions a)
    where idPath = Path_Permissions
instance IsPathType (Path_ReadOnlyFilePath a)
    where idPath = Path_ReadOnlyFilePath
instance IsPathType (Path_Report a)
    where idPath = Path_Report
instance IsPathType (Path_ReportElem a)
    where idPath = Path_ReportElem
instance IsPathType (Path_ReportFlags a)
    where idPath = Path_ReportFlags
instance IsPathType (Path_ReportImage a)
    where idPath = Path_ReportImage
instance IsPathType (Path_ReportImageView a)
    where idPath = Path_ReportImageView
instance IsPathType (Path_ReportIntendedUse a)
    where idPath = Path_ReportIntendedUse
instance IsPathType (Path_ReportMap a)
    where idPath = Path_ReportMap
instance IsPathType (Path_ReportStandard a)
    where idPath = Path_ReportStandard
instance IsPathType (Path_ReportStatus a)
    where idPath = Path_ReportStatus
instance IsPathType (Path_ReportValueApproachInfo a)
    where idPath = Path_ReportValueApproachInfo
instance IsPathType (Path_ReportValueTypeInfo a)
    where idPath = Path_ReportValueTypeInfo
instance IsPathType (Path_ReportView a)
    where idPath = Path_ReportView
instance IsPathType (Path_SaneSizeImageSize a)
    where idPath = Path_SaneSizeImageSize
instance IsPathType (Path_String a)
    where idPath = Path_String
instance IsPathType (Path_Text a)
    where idPath = Path_Text
instance IsPathType (Path_URI a)
    where idPath = Path_URI
instance IsPathType (Path_UUID a)
    where idPath = Path_UUID
instance IsPathType (Path_Units a)
    where idPath = Path_Units
instance IsPathType (Path_UserId a)
    where idPath = Path_UserId
instance IsPathType (Path_UserIds a)
    where idPath = Path_UserIds
