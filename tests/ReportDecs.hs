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
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
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
import Appraisal.Utils.Debug (trace'')
import Control.Lens (iso, _Just, _1, _2, _Left, _Right, Lens', lens, prism', toListOf, Traversal', view)
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map, toList)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node), Forest)
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Language.Haskell.TH.Path.Core (IsPath(Path, pathsOf, toLens), IsPathNode(Peek, peek), IsPathType(idPath),
                                      Path_Either(Path_Left, Path_Right), Path_Map(Path_Look),
                                      Path_Maybe(Path_Just), Path_Pair(Path_First, Path_Second), mat)
import Language.Haskell.TH.Path.Decs.Common (forestMap)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(Path_OMap, Path_At), toPairs)
import Language.Haskell.TH.Path.View (View(viewLens))
import Network.URI (nullURI, URI(URI), URIAuth(URIAuth))

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
    | Path_Item_fields (Path_MIM a)
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
    = Path_ReadOnlyFilePath_View (Path_String a)
    | Path_ReadOnlyFilePath
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
    = Path_ReportMap_unReportMap (Path_MRR a) | Path_ReportMap
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
type Path_EUI a = Path_Either (Path_URI a) (Path_ImageFile a)
type Path_EpochMilli a = Path_Int64 a
type Path_FilePath a = Path_String a
type Path_MEUI a = Path_Maybe (Path_Either (Path_URI a)
                                           (Path_ImageFile a))
type Path_MIM a = Path_Map ItemFieldName (Path_Markup a)
type Path_MRR a = Path_Map ReportID (Path_Report a)
type Path_MarkupPair a = Path_Pair (Path_Markup a) (Path_Markup a)
type Path_MarkupPairs a = Path_OMap MarkupPairID
                                    (Path_Pair (Path_Markup a) (Path_Markup a))
type Path_Markups a = Path_OMap MarkupID (Path_Markup a)
type Path_ReportElems a = Path_OMap ReportElemID
                                    (Path_ReportElem a)
type Path_ReportImages a = Path_OMap ReportImageID
                                     (Path_ReportImage a)
type Path_Size a = Path_Int a
instance IsPath String String
    where type Path String String = Path_String String
          pathsOf _ _ = [idPath]
instance IsPath String JSONText
    where type Path String JSONText = Path_String JSONText
          pathsOf x a = let {p = Path_String_View idPath :: Path ([Char])
                                                                 JSONText;
                             [x'] = toListOf (toLens p) x :: [JSONText]}
                         in map Path_String_View (pathsOf x' a :: [Path JSONText JSONText])
instance IsPath Int64 Int64
    where type Path Int64 Int64 = Path_Int64 Int64
          pathsOf _ _ = [idPath]
instance IsPath Bool String
    where type Path Bool String = Path_Bool String
          pathsOf x a = let {p = Path_Bool_View idPath :: Path Bool ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_Bool_View (pathsOf x' a :: [Path ([Char]) ([Char])])
instance IsPath Bool Bool
    where type Path Bool Bool = Path_Bool Bool
          pathsOf _ _ = [idPath]
instance IsPath Bool JSONText
    where type Path Bool JSONText = Path_Bool JSONText
          pathsOf x a = let {p = Path_Bool_View idPath :: Path Bool ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_Bool_View (pathsOf x' a :: [Path ([Char]) JSONText])
instance IsPath Double String
    where type Path Double String = Path_Double String
          pathsOf x a = let {p = Path_Double_View idPath :: Path Double
                                                                 ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_Double_View (pathsOf x' a :: [Path ([Char]) ([Char])])
instance IsPath Double Double
    where type Path Double Double = Path_Double Double
          pathsOf _ _ = [idPath]
instance IsPath Double JSONText
    where type Path Double JSONText = Path_Double JSONText
          pathsOf x a = let {p = Path_Double_View idPath :: Path Double
                                                                 ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_Double_View (pathsOf x' a :: [Path ([Char]) JSONText])
instance IsPath Int Int
    where type Path Int Int = Path_Int Int
          pathsOf _ _ = [idPath]
instance IsPath Dimension Dimension
    where type Path Dimension Dimension = Path_Dimension Dimension
          pathsOf _ _ = [idPath]
instance IsPath Dimension JSONText
    where type Path Dimension JSONText = Path_Dimension JSONText
          pathsOf x a = let {p = Path_Dimension_View idPath :: Path Dimension
                                                                    JSONText;
                             [x'] = toListOf (toLens p) x :: [JSONText]}
                         in map Path_Dimension_View (pathsOf x' a :: [Path JSONText
                                                                           JSONText])
instance IsPath ImageCrop ImageCrop
    where type Path ImageCrop ImageCrop = Path_ImageCrop ImageCrop
          pathsOf _ _ = [idPath]
instance IsPath ImageSize String
    where type Path ImageSize String = Path_ImageSize String
          pathsOf (x@(ImageSize {})) a = concat [map Path_ImageSize_size (pathsOf (size x) a)]
instance IsPath ImageSize Double
    where type Path ImageSize Double = Path_ImageSize Double
          pathsOf (x@(ImageSize {})) a = concat [map Path_ImageSize_size (pathsOf (size x) a)]
instance IsPath ImageSize Dimension
    where type Path ImageSize Dimension = Path_ImageSize Dimension
          pathsOf (x@(ImageSize {})) a = concat [map Path_ImageSize_dim (pathsOf (dim x) a)]
instance IsPath ImageSize ImageSize
    where type Path ImageSize ImageSize = Path_ImageSize ImageSize
          pathsOf _ _ = [idPath]
instance IsPath ImageSize Units
    where type Path ImageSize Units = Path_ImageSize Units
          pathsOf (x@(ImageSize {})) a = concat [map Path_ImageSize_units (pathsOf (units x) a)]
instance IsPath ImageSize JSONText
    where type Path ImageSize JSONText = Path_ImageSize JSONText
          pathsOf (x@(ImageSize {})) a = concat [map Path_ImageSize_dim (pathsOf (dim x) a),
                                                 map Path_ImageSize_size (pathsOf (size x) a),
                                                 map Path_ImageSize_units (pathsOf (units x) a)]
instance IsPath Units Units
    where type Path Units Units = Path_Units Units
          pathsOf _ _ = [idPath]
instance IsPath Units JSONText
    where type Path Units JSONText = Path_Units JSONText
          pathsOf x a = let {p = Path_Units_View idPath :: Path Units
                                                                JSONText;
                             [x'] = toListOf (toLens p) x :: [JSONText]}
                         in map Path_Units_View (pathsOf x' a :: [Path JSONText JSONText])
instance IsPath ImageFile ImageFile
    where type Path ImageFile ImageFile = Path_ImageFile ImageFile
          pathsOf _ _ = [idPath]
instance IsPath Integer Integer
    where type Path Integer Integer = Path_Integer Integer
          pathsOf _ _ = [idPath]
instance IsPath JSONText JSONText
    where type Path JSONText JSONText = Path_JSONText JSONText
          pathsOf _ _ = [idPath]
instance IsPath Markup JSONText
    where type Path Markup JSONText = Path_Markup JSONText
          pathsOf (x@(Markdown {})) a = concat [map Path_Markup_markdownText (pathsOf (markdownText x) a)]
          pathsOf (x@(Html {})) a = concat [map Path_Markup_htmlText (pathsOf (htmlText x) a)]
          pathsOf (LaTeX p1) a = concat []
          pathsOf (Pandoc p1) a = concat []
          pathsOf (Markup p1) a = concat []
instance IsPath Markup Markup
    where type Path Markup Markup = Path_Markup Markup
          pathsOf _ _ = [idPath]
instance IsPath Markup Text
    where type Path Markup Text = Path_Markup Text
          pathsOf (x@(Markdown {})) a = concat [map Path_Markup_markdownText (pathsOf (markdownText x) a)]
          pathsOf (x@(Html {})) a = concat [map Path_Markup_htmlText (pathsOf (htmlText x) a)]
          pathsOf (LaTeX p1) a = concat []
          pathsOf (Pandoc p1) a = concat []
          pathsOf (Markup p1) a = concat []
instance IsPath Permissions JSONText
    where type Path Permissions JSONText = Path_Permissions JSONText
          pathsOf (x@(Permissions {})) a = concat [map Path_Permissions_writers (pathsOf (writers x) a),
                                                   map Path_Permissions_readers (pathsOf (readers x) a)]
instance IsPath Permissions Permissions
    where type Path Permissions
                    Permissions = Path_Permissions Permissions
          pathsOf _ _ = [idPath]
instance IsPath Permissions UserIds
    where type Path Permissions UserIds = Path_Permissions UserIds
          pathsOf (x@(Permissions {})) a = concat [map Path_Permissions_writers (pathsOf (writers x) a),
                                                   map Path_Permissions_readers (pathsOf (readers x) a)]
instance IsPath Permissions Text
    where type Path Permissions Text = Path_Permissions Text
          pathsOf (x@(Permissions {})) a = concat [map Path_Permissions_writers (pathsOf (writers x) a),
                                                   map Path_Permissions_readers (pathsOf (readers x) a)]
instance IsPath Permissions UserId
    where type Path Permissions UserId = Path_Permissions UserId
          pathsOf (x@(Permissions {})) a = concat [map Path_Permissions_owner (pathsOf (owner x) a)]
instance IsPath UserIds JSONText
    where type Path UserIds JSONText = Path_UserIds JSONText
          pathsOf x a = let {p = Path_UserIds_View idPath :: Path ([UserId])
                                                                  Text;
                             [x'] = toListOf (toLens p) x :: [Text]}
                         in map Path_UserIds_View (pathsOf x' a :: [Path Text JSONText])
instance IsPath UserIds UserIds
    where type Path UserIds UserIds = Path_UserIds UserIds
          pathsOf _ _ = [idPath]
instance IsPath UserIds Text
    where type Path UserIds Text = Path_UserIds Text
          pathsOf x a = let {p = Path_UserIds_View idPath :: Path ([UserId])
                                                                  Text;
                             [x'] = toListOf (toLens p) x :: [Text]}
                         in map Path_UserIds_View (pathsOf x' a :: [Path Text Text])
instance IsPath AbbrevPair JSONText
    where type Path AbbrevPair
                    JSONText = Path_Pair (Path_CIString JSONText)
                                         (Path_Markup JSONText)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_First) pv) (pathsOf (fst pv) a :: [Path CIString
                                                                                                   JSONText])) ((: []) x)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Second) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                    JSONText])) ((: []) x)
instance IsPath AbbrevPair Markup
    where type Path AbbrevPair
                    Markup = Path_Pair (Path_CIString Markup) (Path_Markup Markup)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Second) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                    Markup])) ((: []) x)
instance IsPath AbbrevPair AbbrevPair
    where type Path AbbrevPair
                    AbbrevPair = Path_Pair (Path_CIString AbbrevPair)
                                           (Path_Markup AbbrevPair)
          pathsOf _ _ = [idPath]
instance IsPath AbbrevPair CIString
    where type Path AbbrevPair
                    CIString = Path_Pair (Path_CIString CIString)
                                         (Path_Markup CIString)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_First) pv) (pathsOf (fst pv) a :: [Path CIString
                                                                                                   CIString])) ((: []) x)
instance IsPath AbbrevPair Text
    where type Path AbbrevPair Text = Path_Pair (Path_CIString Text)
                                                (Path_Markup Text)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_First) pv) (pathsOf (fst pv) a :: [Path CIString
                                                                                                   Text])) ((: []) x)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Second) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                    Text])) ((: []) x)
instance IsPath AbbrevPairs JSONText
    where type Path AbbrevPairs JSONText = Path_OMap AbbrevPairID
                                                     (Path_Pair (Path_CIString JSONText)
                                                                (Path_Markup JSONText))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((CIString,
                                                                                                  Markup))
                                                                                                JSONText])) ((toPairs :: Order AbbrevPairID
                                                                                                                               ((CIString,
                                                                                                                                 Markup)) ->
                                                                                                                         [(AbbrevPairID,
                                                                                                                           (CIString,
                                                                                                                            Markup))]) x)
instance IsPath AbbrevPairs Markup
    where type Path AbbrevPairs Markup = Path_OMap AbbrevPairID
                                                   (Path_Pair (Path_CIString Markup)
                                                              (Path_Markup Markup))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((CIString,
                                                                                                  Markup))
                                                                                                Markup])) ((toPairs :: Order AbbrevPairID
                                                                                                                             ((CIString,
                                                                                                                               Markup)) ->
                                                                                                                       [(AbbrevPairID,
                                                                                                                         (CIString,
                                                                                                                          Markup))]) x)
instance IsPath AbbrevPairs AbbrevPair
    where type Path AbbrevPairs AbbrevPair = Path_OMap AbbrevPairID
                                                       (Path_Pair (Path_CIString AbbrevPair)
                                                                  (Path_Markup AbbrevPair))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((CIString,
                                                                                                  Markup))
                                                                                                ((CIString,
                                                                                                  Markup))])) ((toPairs :: Order AbbrevPairID
                                                                                                                                 ((CIString,
                                                                                                                                   Markup)) ->
                                                                                                                           [(AbbrevPairID,
                                                                                                                             (CIString,
                                                                                                                              Markup))]) x)
instance IsPath AbbrevPairs AbbrevPairs
    where type Path AbbrevPairs AbbrevPairs = Path_OMap AbbrevPairID
                                                        (Path_Pair (Path_CIString AbbrevPairs)
                                                                   (Path_Markup AbbrevPairs))
          pathsOf _ _ = [idPath]
instance IsPath AbbrevPairs CIString
    where type Path AbbrevPairs CIString = Path_OMap AbbrevPairID
                                                     (Path_Pair (Path_CIString CIString)
                                                                (Path_Markup CIString))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((CIString,
                                                                                                  Markup))
                                                                                                CIString])) ((toPairs :: Order AbbrevPairID
                                                                                                                               ((CIString,
                                                                                                                                 Markup)) ->
                                                                                                                         [(AbbrevPairID,
                                                                                                                           (CIString,
                                                                                                                            Markup))]) x)
instance IsPath AbbrevPairs Text
    where type Path AbbrevPairs Text = Path_OMap AbbrevPairID
                                                 (Path_Pair (Path_CIString Text) (Path_Markup Text))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((CIString,
                                                                                                  Markup))
                                                                                                Text])) ((toPairs :: Order AbbrevPairID
                                                                                                                           ((CIString,
                                                                                                                             Markup)) ->
                                                                                                                     [(AbbrevPairID,
                                                                                                                       (CIString,
                                                                                                                        Markup))]) x)
instance IsPath Author JSONText
    where type Path Author JSONText = Path_Author JSONText
          pathsOf (x@(Author {})) a = concat [map Path_Author_authorName (pathsOf (authorName x) a),
                                              map Path_Author_authorCredentials (pathsOf (authorCredentials x) a)]
instance IsPath Author Markup
    where type Path Author Markup = Path_Author Markup
          pathsOf (x@(Author {})) a = concat [map Path_Author_authorName (pathsOf (authorName x) a),
                                              map Path_Author_authorCredentials (pathsOf (authorCredentials x) a)]
instance IsPath Author Author
    where type Path Author Author = Path_Author Author
          pathsOf _ _ = [idPath]
instance IsPath Author Text
    where type Path Author Text = Path_Author Text
          pathsOf (x@(Author {})) a = concat [map Path_Author_authorName (pathsOf (authorName x) a),
                                              map Path_Author_authorCredentials (pathsOf (authorCredentials x) a)]
instance IsPath Authors JSONText
    where type Path Authors JSONText = Path_OMap AuthorID
                                                 (Path_Author JSONText)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Author
                                                                                                JSONText])) ((toPairs :: Order AuthorID
                                                                                                                               Author ->
                                                                                                                         [(AuthorID,
                                                                                                                           Author)]) x)
instance IsPath Authors Markup
    where type Path Authors Markup = Path_OMap AuthorID
                                               (Path_Author Markup)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Author
                                                                                                Markup])) ((toPairs :: Order AuthorID
                                                                                                                             Author ->
                                                                                                                       [(AuthorID,
                                                                                                                         Author)]) x)
instance IsPath Authors Author
    where type Path Authors Author = Path_OMap AuthorID
                                               (Path_Author Author)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Author
                                                                                                Author])) ((toPairs :: Order AuthorID
                                                                                                                             Author ->
                                                                                                                       [(AuthorID,
                                                                                                                         Author)]) x)
instance IsPath Authors Authors
    where type Path Authors Authors = Path_OMap AuthorID
                                                (Path_Author Authors)
          pathsOf _ _ = [idPath]
instance IsPath Authors Text
    where type Path Authors Text = Path_OMap AuthorID
                                             (Path_Author Text)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Author
                                                                                                Text])) ((toPairs :: Order AuthorID
                                                                                                                           Author ->
                                                                                                                     [(AuthorID,
                                                                                                                       Author)]) x)
instance IsPath Branding JSONText
    where type Path Branding JSONText = Path_Branding JSONText
          pathsOf x a = let {p = Path_Branding_View idPath :: Path Branding
                                                                   Text;
                             [x'] = toListOf (toLens p) x :: [Text]}
                         in map Path_Branding_View (pathsOf x' a :: [Path Text JSONText])
instance IsPath Branding Branding
    where type Path Branding Branding = Path_Branding Branding
          pathsOf _ _ = [idPath]
instance IsPath Branding Text
    where type Path Branding Text = Path_Branding Text
          pathsOf x a = let {p = Path_Branding_View idPath :: Path Branding
                                                                   Text;
                             [x'] = toListOf (toLens p) x :: [Text]}
                         in map Path_Branding_View (pathsOf x' a :: [Path Text Text])
instance IsPath MarkupPair JSONText
    where type Path MarkupPair
                    JSONText = Path_Pair (Path_Markup JSONText) (Path_Markup JSONText)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_First) pv) (pathsOf (fst pv) a :: [Path Markup
                                                                                                   JSONText])) ((: []) x)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Second) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                    JSONText])) ((: []) x)
instance IsPath MarkupPair Markup
    where type Path MarkupPair Markup = Path_Pair (Path_Markup Markup)
                                                  (Path_Markup Markup)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_First) pv) (pathsOf (fst pv) a :: [Path Markup
                                                                                                   Markup])) ((: []) x)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Second) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                    Markup])) ((: []) x)
instance IsPath MarkupPair MarkupPair
    where type Path MarkupPair
                    MarkupPair = Path_Pair (Path_Markup MarkupPair)
                                           (Path_Markup MarkupPair)
          pathsOf _ _ = [idPath]
instance IsPath MarkupPair Text
    where type Path MarkupPair Text = Path_Pair (Path_Markup Text)
                                                (Path_Markup Text)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_First) pv) (pathsOf (fst pv) a :: [Path Markup
                                                                                                   Text])) ((: []) x)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Second) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                    Text])) ((: []) x)
instance IsPath MarkupPairs JSONText
    where type Path MarkupPairs JSONText = Path_OMap MarkupPairID
                                                     (Path_Pair (Path_Markup JSONText)
                                                                (Path_Markup JSONText))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((Markup,
                                                                                                  Markup))
                                                                                                JSONText])) ((toPairs :: Order MarkupPairID
                                                                                                                               ((Markup,
                                                                                                                                 Markup)) ->
                                                                                                                         [(MarkupPairID,
                                                                                                                           (Markup,
                                                                                                                            Markup))]) x)
instance IsPath MarkupPairs Markup
    where type Path MarkupPairs Markup = Path_OMap MarkupPairID
                                                   (Path_Pair (Path_Markup Markup)
                                                              (Path_Markup Markup))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((Markup,
                                                                                                  Markup))
                                                                                                Markup])) ((toPairs :: Order MarkupPairID
                                                                                                                             ((Markup,
                                                                                                                               Markup)) ->
                                                                                                                       [(MarkupPairID,
                                                                                                                         (Markup,
                                                                                                                          Markup))]) x)
instance IsPath MarkupPairs MarkupPair
    where type Path MarkupPairs MarkupPair = Path_OMap MarkupPairID
                                                       (Path_Pair (Path_Markup MarkupPair)
                                                                  (Path_Markup MarkupPair))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((Markup,
                                                                                                  Markup))
                                                                                                ((Markup,
                                                                                                  Markup))])) ((toPairs :: Order MarkupPairID
                                                                                                                                 ((Markup,
                                                                                                                                   Markup)) ->
                                                                                                                           [(MarkupPairID,
                                                                                                                             (Markup,
                                                                                                                              Markup))]) x)
instance IsPath MarkupPairs MarkupPairs
    where type Path MarkupPairs MarkupPairs = Path_OMap MarkupPairID
                                                        (Path_Pair (Path_Markup MarkupPairs)
                                                                   (Path_Markup MarkupPairs))
          pathsOf _ _ = [idPath]
instance IsPath MarkupPairs Text
    where type Path MarkupPairs Text = Path_OMap MarkupPairID
                                                 (Path_Pair (Path_Markup Text) (Path_Markup Text))
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ((Markup,
                                                                                                  Markup))
                                                                                                Text])) ((toPairs :: Order MarkupPairID
                                                                                                                           ((Markup,
                                                                                                                             Markup)) ->
                                                                                                                     [(MarkupPairID,
                                                                                                                       (Markup,
                                                                                                                        Markup))]) x)
instance IsPath Markups JSONText
    where type Path Markups JSONText = Path_OMap MarkupID
                                                 (Path_Markup JSONText)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                JSONText])) ((toPairs :: Order MarkupID
                                                                                                                               Markup ->
                                                                                                                         [(MarkupID,
                                                                                                                           Markup)]) x)
instance IsPath Markups Markup
    where type Path Markups Markup = Path_OMap MarkupID
                                               (Path_Markup Markup)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                Markup])) ((toPairs :: Order MarkupID
                                                                                                                             Markup ->
                                                                                                                       [(MarkupID,
                                                                                                                         Markup)]) x)
instance IsPath Markups Markups
    where type Path Markups Markups = Path_OMap MarkupID
                                                (Path_Markup Markups)
          pathsOf _ _ = [idPath]
instance IsPath Markups Text
    where type Path Markups Text = Path_OMap MarkupID
                                             (Path_Markup Text)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                Text])) ((toPairs :: Order MarkupID
                                                                                                                           Markup ->
                                                                                                                     [(MarkupID,
                                                                                                                       Markup)]) x)
instance IsPath MaybeReportIntendedUse String
    where type Path MaybeReportIntendedUse
                    String = Path_MaybeReportIntendedUse String
          pathsOf x a = let {p = Path_MaybeReportIntendedUse_View idPath :: Path (Maybe ReportIntendedUse)
                                                                                 ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_MaybeReportIntendedUse_View (pathsOf x' a :: [Path ([Char])
                                                                                        ([Char])])
instance IsPath MaybeReportIntendedUse JSONText
    where type Path MaybeReportIntendedUse
                    JSONText = Path_MaybeReportIntendedUse JSONText
          pathsOf x a = let {p = Path_MaybeReportIntendedUse_View idPath :: Path (Maybe ReportIntendedUse)
                                                                                 ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_MaybeReportIntendedUse_View (pathsOf x' a :: [Path ([Char])
                                                                                        JSONText])
instance IsPath MaybeReportIntendedUse MaybeReportIntendedUse
    where type Path MaybeReportIntendedUse
                    MaybeReportIntendedUse = Path_MaybeReportIntendedUse MaybeReportIntendedUse
          pathsOf _ _ = [idPath]
instance IsPath Report String
    where type Path Report String = Path_Report String
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ([Char])])
instance IsPath Report Int64
    where type Path Report Int64 = Path_Report Int64
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Int64])
instance IsPath Report Bool
    where type Path Report Bool = Path_Report Bool
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Bool])
instance IsPath Report Double
    where type Path Report Double = Path_Report Double
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Double])
instance IsPath Report Int
    where type Path Report Int = Path_Report Int
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Int])
instance IsPath Report Dimension
    where type Path Report Dimension = Path_Report Dimension
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        Dimension])
instance IsPath Report ImageCrop
    where type Path Report ImageCrop = Path_Report ImageCrop
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ImageCrop])
instance IsPath Report ImageSize
    where type Path Report ImageSize = Path_Report ImageSize
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ImageSize])
instance IsPath Report Units
    where type Path Report Units = Path_Report Units
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Units])
instance IsPath Report ImageFile
    where type Path Report ImageFile = Path_Report ImageFile
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ImageFile])
instance IsPath Report Integer
    where type Path Report Integer = Path_Report Integer
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        Integer])
instance IsPath Report JSONText
    where type Path Report JSONText = Path_Report JSONText
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        JSONText])
instance IsPath Report Markup
    where type Path Report Markup = Path_Report Markup
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Markup])
instance IsPath Report Permissions
    where type Path Report Permissions = Path_Report Permissions
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        Permissions])
instance IsPath Report UserIds
    where type Path Report UserIds = Path_Report UserIds
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ([UserId])])
instance IsPath Report AbbrevPair
    where type Path Report AbbrevPair = Path_Report AbbrevPair
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ((CIString, Markup))])
instance IsPath Report AbbrevPairs
    where type Path Report AbbrevPairs = Path_Report AbbrevPairs
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Order AbbrevPairID
                                                                               ((CIString,
                                                                                 Markup)))])
instance IsPath Report Author
    where type Path Report Author = Path_Report Author
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Author])
instance IsPath Report Authors
    where type Path Report Authors = Path_Report Authors
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Order AuthorID Author)])
instance IsPath Report Branding
    where type Path Report Branding = Path_Report Branding
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        Branding])
instance IsPath Report MarkupPair
    where type Path Report MarkupPair = Path_Report MarkupPair
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ((Markup, Markup))])
instance IsPath Report MarkupPairs
    where type Path Report MarkupPairs = Path_Report MarkupPairs
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Order MarkupPairID
                                                                               ((Markup, Markup)))])
instance IsPath Report Markups
    where type Path Report Markups = Path_Report Markups
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Order MarkupID Markup)])
instance IsPath Report MaybeReportIntendedUse
    where type Path Report
                    MaybeReportIntendedUse = Path_Report MaybeReportIntendedUse
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Maybe ReportIntendedUse)])
instance IsPath Report Report
    where type Path Report Report = Path_Report Report
          pathsOf _ _ = [idPath]
instance IsPath Report ReportElem
    where type Path Report ReportElem = Path_Report ReportElem
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportElem])
instance IsPath Report ReportElems
    where type Path Report ReportElems = Path_Report ReportElems
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Order ReportElemID
                                                                               ReportElem)])
instance IsPath Report ReportFlags
    where type Path Report ReportFlags = Path_Report ReportFlags
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportFlags])
instance IsPath Report ReportStandard
    where type Path Report ReportStandard = Path_Report ReportStandard
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportStandard])
instance IsPath Report ReportStatus
    where type Path Report ReportStatus = Path_Report ReportStatus
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportStatus])
instance IsPath Report ReportValueApproachInfo
    where type Path Report
                    ReportValueApproachInfo = Path_Report ReportValueApproachInfo
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportValueApproachInfo])
instance IsPath Report ReportValueTypeInfo
    where type Path Report
                    ReportValueTypeInfo = Path_Report ReportValueTypeInfo
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportValueTypeInfo])
instance IsPath Report EUI
    where type Path Report EUI = Path_Report EUI
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Either URI ImageFile)])
instance IsPath Report MEUI
    where type Path Report MEUI = Path_Report MEUI
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Maybe (Either URI
                                                                                       ImageFile))])
instance IsPath Report MaybeImageFile
    where type Path Report MaybeImageFile = Path_Report MaybeImageFile
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Maybe ImageFile)])
instance IsPath Report ReportImage
    where type Path Report ReportImage = Path_Report ReportImage
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportImage])
instance IsPath Report ReportImages
    where type Path Report ReportImages = Path_Report ReportImages
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Order ReportImageID
                                                                               ReportImage)])
instance IsPath Report ReadOnlyFilePath
    where type Path Report
                    ReadOnlyFilePath = Path_Report ReadOnlyFilePath
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (ReadOnly ([Char]))])
instance IsPath Report ReportImageView
    where type Path Report
                    ReportImageView = Path_Report ReportImageView
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportImageView])
instance IsPath Report ReportView
    where type Path Report ReportView = Path_Report ReportView
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        ReportView])
instance IsPath Report SaneSizeImageSize
    where type Path Report
                    SaneSizeImageSize = Path_Report SaneSizeImageSize
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (SaneSize ImageSize)])
instance IsPath Report Item
    where type Path Report Item = Path_Report Item
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Item])
instance IsPath Report MIM
    where type Path Report MIM = Path_Report MIM
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        (Map ItemFieldName Markup)])
instance IsPath Report CIString
    where type Path Report CIString = Path_Report CIString
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView
                                                                        CIString])
instance IsPath Report URI
    where type Path Report URI = Path_Report URI
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView URI])
instance IsPath Report Text
    where type Path Report Text = Path_Report Text
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView Text])
instance IsPath Report UserId
    where type Path Report UserId = Path_Report UserId
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView UserId])
instance IsPath Report UUID
    where type Path Report UUID = Path_Report UUID
          pathsOf x a = let {p = Path_Report_View idPath :: Path Report
                                                                 ReportView;
                             [x'] = toListOf (toLens p) x :: [ReportView]}
                         in map Path_Report_View (pathsOf x' a :: [Path ReportView UUID])
instance IsPath ReportElem String
    where type Path ReportElem String = Path_ReportElem String
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Bool
    where type Path ReportElem Bool = Path_ReportElem Bool
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Double
    where type Path ReportElem Double = Path_ReportElem Double
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Dimension
    where type Path ReportElem Dimension = Path_ReportElem Dimension
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ImageCrop
    where type Path ReportElem ImageCrop = Path_ReportElem ImageCrop
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ImageSize
    where type Path ReportElem ImageSize = Path_ReportElem ImageSize
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Units
    where type Path ReportElem Units = Path_ReportElem Units
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ImageFile
    where type Path ReportElem ImageFile = Path_ReportElem ImageFile
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem JSONText
    where type Path ReportElem JSONText = Path_ReportElem JSONText
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat [map Path_ReportElem_elemText (pathsOf (elemText x) a)]
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Markup
    where type Path ReportElem Markup = Path_ReportElem Markup
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat [map Path_ReportElem_elemText (pathsOf (elemText x) a)]
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ReportElem
    where type Path ReportElem ReportElem = Path_ReportElem ReportElem
          pathsOf _ _ = [idPath]
instance IsPath ReportElem EUI
    where type Path ReportElem EUI = Path_ReportElem EUI
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem MEUI
    where type Path ReportElem MEUI = Path_ReportElem MEUI
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem MaybeImageFile
    where type Path ReportElem
                    MaybeImageFile = Path_ReportElem MaybeImageFile
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ReportImage
    where type Path ReportElem
                    ReportImage = Path_ReportElem ReportImage
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ReportImages
    where type Path ReportElem
                    ReportImages = Path_ReportElem ReportImages
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem ReportImageView
    where type Path ReportElem
                    ReportImageView = Path_ReportElem ReportImageView
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem SaneSizeImageSize
    where type Path ReportElem
                    SaneSizeImageSize = Path_ReportElem SaneSizeImageSize
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Item
    where type Path ReportElem Item = Path_ReportElem Item
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem MIM
    where type Path ReportElem MIM = Path_ReportElem MIM
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem URI
    where type Path ReportElem URI = Path_ReportElem URI
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat []
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElem Text
    where type Path ReportElem Text = Path_ReportElem Text
          pathsOf (x@(ReportItem {})) a = concat [map Path_ReportElem_elemItem (pathsOf (elemItem x) a)]
          pathsOf (x@(ReportParagraph {})) a = concat [map Path_ReportElem_elemText (pathsOf (elemText x) a)]
          pathsOf (ReportUndecided) a = concat []
instance IsPath ReportElems String
    where type Path ReportElems String = Path_OMap ReportElemID
                                                   (Path_ReportElem String)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ([Char])])) ((toPairs :: Order ReportElemID
                                                                                                                               ReportElem ->
                                                                                                                         [(ReportElemID,
                                                                                                                           ReportElem)]) x)
instance IsPath ReportElems Bool
    where type Path ReportElems Bool = Path_OMap ReportElemID
                                                 (Path_ReportElem Bool)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Bool])) ((toPairs :: Order ReportElemID
                                                                                                                           ReportElem ->
                                                                                                                     [(ReportElemID,
                                                                                                                       ReportElem)]) x)
instance IsPath ReportElems Double
    where type Path ReportElems Double = Path_OMap ReportElemID
                                                   (Path_ReportElem Double)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Double])) ((toPairs :: Order ReportElemID
                                                                                                                             ReportElem ->
                                                                                                                       [(ReportElemID,
                                                                                                                         ReportElem)]) x)
instance IsPath ReportElems Dimension
    where type Path ReportElems Dimension = Path_OMap ReportElemID
                                                      (Path_ReportElem Dimension)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Dimension])) ((toPairs :: Order ReportElemID
                                                                                                                                ReportElem ->
                                                                                                                          [(ReportElemID,
                                                                                                                            ReportElem)]) x)
instance IsPath ReportElems ImageCrop
    where type Path ReportElems ImageCrop = Path_OMap ReportElemID
                                                      (Path_ReportElem ImageCrop)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ImageCrop])) ((toPairs :: Order ReportElemID
                                                                                                                                ReportElem ->
                                                                                                                          [(ReportElemID,
                                                                                                                            ReportElem)]) x)
instance IsPath ReportElems ImageSize
    where type Path ReportElems ImageSize = Path_OMap ReportElemID
                                                      (Path_ReportElem ImageSize)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ImageSize])) ((toPairs :: Order ReportElemID
                                                                                                                                ReportElem ->
                                                                                                                          [(ReportElemID,
                                                                                                                            ReportElem)]) x)
instance IsPath ReportElems Units
    where type Path ReportElems Units = Path_OMap ReportElemID
                                                  (Path_ReportElem Units)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Units])) ((toPairs :: Order ReportElemID
                                                                                                                            ReportElem ->
                                                                                                                      [(ReportElemID,
                                                                                                                        ReportElem)]) x)
instance IsPath ReportElems ImageFile
    where type Path ReportElems ImageFile = Path_OMap ReportElemID
                                                      (Path_ReportElem ImageFile)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ImageFile])) ((toPairs :: Order ReportElemID
                                                                                                                                ReportElem ->
                                                                                                                          [(ReportElemID,
                                                                                                                            ReportElem)]) x)
instance IsPath ReportElems JSONText
    where type Path ReportElems JSONText = Path_OMap ReportElemID
                                                     (Path_ReportElem JSONText)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                JSONText])) ((toPairs :: Order ReportElemID
                                                                                                                               ReportElem ->
                                                                                                                         [(ReportElemID,
                                                                                                                           ReportElem)]) x)
instance IsPath ReportElems Markup
    where type Path ReportElems Markup = Path_OMap ReportElemID
                                                   (Path_ReportElem Markup)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Markup])) ((toPairs :: Order ReportElemID
                                                                                                                             ReportElem ->
                                                                                                                       [(ReportElemID,
                                                                                                                         ReportElem)]) x)
instance IsPath ReportElems ReportElem
    where type Path ReportElems ReportElem = Path_OMap ReportElemID
                                                       (Path_ReportElem ReportElem)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ReportElem])) ((toPairs :: Order ReportElemID
                                                                                                                                 ReportElem ->
                                                                                                                           [(ReportElemID,
                                                                                                                             ReportElem)]) x)
instance IsPath ReportElems ReportElems
    where type Path ReportElems ReportElems = Path_OMap ReportElemID
                                                        (Path_ReportElem ReportElems)
          pathsOf _ _ = [idPath]
instance IsPath ReportElems EUI
    where type Path ReportElems EUI = Path_OMap ReportElemID
                                                (Path_ReportElem EUI)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                (Either URI
                                                                                                        ImageFile)])) ((toPairs :: Order ReportElemID
                                                                                                                                         ReportElem ->
                                                                                                                                   [(ReportElemID,
                                                                                                                                     ReportElem)]) x)
instance IsPath ReportElems MEUI
    where type Path ReportElems MEUI = Path_OMap ReportElemID
                                                 (Path_ReportElem MEUI)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                (Maybe (Either URI
                                                                                                               ImageFile))])) ((toPairs :: Order ReportElemID
                                                                                                                                                 ReportElem ->
                                                                                                                                           [(ReportElemID,
                                                                                                                                             ReportElem)]) x)
instance IsPath ReportElems MaybeImageFile
    where type Path ReportElems MaybeImageFile = Path_OMap ReportElemID
                                                           (Path_ReportElem MaybeImageFile)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                (Maybe ImageFile)])) ((toPairs :: Order ReportElemID
                                                                                                                                        ReportElem ->
                                                                                                                                  [(ReportElemID,
                                                                                                                                    ReportElem)]) x)
instance IsPath ReportElems ReportImage
    where type Path ReportElems ReportImage = Path_OMap ReportElemID
                                                        (Path_ReportElem ReportImage)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ReportImage])) ((toPairs :: Order ReportElemID
                                                                                                                                  ReportElem ->
                                                                                                                            [(ReportElemID,
                                                                                                                              ReportElem)]) x)
instance IsPath ReportElems ReportImages
    where type Path ReportElems ReportImages = Path_OMap ReportElemID
                                                         (Path_ReportElem ReportImages)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                (Order ReportImageID
                                                                                                       ReportImage)])) ((toPairs :: Order ReportElemID
                                                                                                                                          ReportElem ->
                                                                                                                                    [(ReportElemID,
                                                                                                                                      ReportElem)]) x)
instance IsPath ReportElems ReportImageView
    where type Path ReportElems
                    ReportImageView = Path_OMap ReportElemID
                                                (Path_ReportElem ReportImageView)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                ReportImageView])) ((toPairs :: Order ReportElemID
                                                                                                                                      ReportElem ->
                                                                                                                                [(ReportElemID,
                                                                                                                                  ReportElem)]) x)
instance IsPath ReportElems SaneSizeImageSize
    where type Path ReportElems
                    SaneSizeImageSize = Path_OMap ReportElemID
                                                  (Path_ReportElem SaneSizeImageSize)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                (SaneSize ImageSize)])) ((toPairs :: Order ReportElemID
                                                                                                                                           ReportElem ->
                                                                                                                                     [(ReportElemID,
                                                                                                                                       ReportElem)]) x)
instance IsPath ReportElems Item
    where type Path ReportElems Item = Path_OMap ReportElemID
                                                 (Path_ReportElem Item)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Item])) ((toPairs :: Order ReportElemID
                                                                                                                           ReportElem ->
                                                                                                                     [(ReportElemID,
                                                                                                                       ReportElem)]) x)
instance IsPath ReportElems MIM
    where type Path ReportElems MIM = Path_OMap ReportElemID
                                                (Path_ReportElem MIM)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                (Map ItemFieldName
                                                                                                     Markup)])) ((toPairs :: Order ReportElemID
                                                                                                                                   ReportElem ->
                                                                                                                             [(ReportElemID,
                                                                                                                               ReportElem)]) x)
instance IsPath ReportElems URI
    where type Path ReportElems URI = Path_OMap ReportElemID
                                                (Path_ReportElem URI)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                URI])) ((toPairs :: Order ReportElemID
                                                                                                                          ReportElem ->
                                                                                                                    [(ReportElemID,
                                                                                                                      ReportElem)]) x)
instance IsPath ReportElems Text
    where type Path ReportElems Text = Path_OMap ReportElemID
                                                 (Path_ReportElem Text)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportElem
                                                                                                Text])) ((toPairs :: Order ReportElemID
                                                                                                                           ReportElem ->
                                                                                                                     [(ReportElemID,
                                                                                                                       ReportElem)]) x)
instance IsPath ReportFlags String
    where type Path ReportFlags String = Path_ReportFlags String
          pathsOf (x@(ReportFlags {})) a = concat [map Path_ReportFlags_hideEmptyItemFields (pathsOf (hideEmptyItemFields x) a)]
instance IsPath ReportFlags Bool
    where type Path ReportFlags Bool = Path_ReportFlags Bool
          pathsOf (x@(ReportFlags {})) a = concat [map Path_ReportFlags_hideEmptyItemFields (pathsOf (hideEmptyItemFields x) a)]
instance IsPath ReportFlags JSONText
    where type Path ReportFlags JSONText = Path_ReportFlags JSONText
          pathsOf (x@(ReportFlags {})) a = concat [map Path_ReportFlags_hideEmptyItemFields (pathsOf (hideEmptyItemFields x) a)]
instance IsPath ReportFlags ReportFlags
    where type Path ReportFlags
                    ReportFlags = Path_ReportFlags ReportFlags
          pathsOf _ _ = [idPath]
instance IsPath ReportIntendedUse String
    where type Path ReportIntendedUse
                    String = Path_ReportIntendedUse String
          pathsOf x a = let {p = Path_ReportIntendedUse_View idPath :: Path ReportIntendedUse
                                                                            ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_ReportIntendedUse_View (pathsOf x' a :: [Path ([Char])
                                                                                   ([Char])])
instance IsPath ReportIntendedUse JSONText
    where type Path ReportIntendedUse
                    JSONText = Path_ReportIntendedUse JSONText
          pathsOf x a = let {p = Path_ReportIntendedUse_View idPath :: Path ReportIntendedUse
                                                                            ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_ReportIntendedUse_View (pathsOf x' a :: [Path ([Char])
                                                                                   JSONText])
instance IsPath ReportIntendedUse ReportIntendedUse
    where type Path ReportIntendedUse
                    ReportIntendedUse = Path_ReportIntendedUse ReportIntendedUse
          pathsOf _ _ = [idPath]
instance IsPath ReportStandard Int
    where type Path ReportStandard Int = Path_ReportStandard Int
          pathsOf (x@(ReportStandard {})) a = concat [map Path_ReportStandard_unReportStandard (pathsOf (unReportStandard x) a)]
instance IsPath ReportStandard ReportStandard
    where type Path ReportStandard
                    ReportStandard = Path_ReportStandard ReportStandard
          pathsOf _ _ = [idPath]
instance IsPath ReportStatus String
    where type Path ReportStatus String = Path_ReportStatus String
          pathsOf x a = let {p = Path_ReportStatus_View idPath :: Path ReportStatus
                                                                       ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_ReportStatus_View (pathsOf x' a :: [Path ([Char])
                                                                              ([Char])])
instance IsPath ReportStatus JSONText
    where type Path ReportStatus JSONText = Path_ReportStatus JSONText
          pathsOf x a = let {p = Path_ReportStatus_View idPath :: Path ReportStatus
                                                                       ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_ReportStatus_View (pathsOf x' a :: [Path ([Char])
                                                                              JSONText])
instance IsPath ReportStatus ReportStatus
    where type Path ReportStatus
                    ReportStatus = Path_ReportStatus ReportStatus
          pathsOf _ _ = [idPath]
instance IsPath ReportValueApproachInfo JSONText
    where type Path ReportValueApproachInfo
                    JSONText = Path_ReportValueApproachInfo JSONText
          pathsOf (x@(ReportValueApproachInfo {})) a = concat [map Path_ReportValueApproachInfo_reportValueApproachName (pathsOf (reportValueApproachName x) a),
                                                               map Path_ReportValueApproachInfo_reportValueApproachDescription (pathsOf (reportValueApproachDescription x) a)]
instance IsPath ReportValueApproachInfo Markup
    where type Path ReportValueApproachInfo
                    Markup = Path_ReportValueApproachInfo Markup
          pathsOf (x@(ReportValueApproachInfo {})) a = concat [map Path_ReportValueApproachInfo_reportValueApproachName (pathsOf (reportValueApproachName x) a),
                                                               map Path_ReportValueApproachInfo_reportValueApproachDescription (pathsOf (reportValueApproachDescription x) a)]
instance IsPath ReportValueApproachInfo ReportValueApproachInfo
    where type Path ReportValueApproachInfo
                    ReportValueApproachInfo = Path_ReportValueApproachInfo ReportValueApproachInfo
          pathsOf _ _ = [idPath]
instance IsPath ReportValueApproachInfo Text
    where type Path ReportValueApproachInfo
                    Text = Path_ReportValueApproachInfo Text
          pathsOf (x@(ReportValueApproachInfo {})) a = concat [map Path_ReportValueApproachInfo_reportValueApproachName (pathsOf (reportValueApproachName x) a),
                                                               map Path_ReportValueApproachInfo_reportValueApproachDescription (pathsOf (reportValueApproachDescription x) a)]
instance IsPath ReportValueTypeInfo JSONText
    where type Path ReportValueTypeInfo
                    JSONText = Path_ReportValueTypeInfo JSONText
          pathsOf (x@(ReportValueTypeInfo {})) a = concat [map Path_ReportValueTypeInfo_reportValueTypeName (pathsOf (reportValueTypeName x) a),
                                                           map Path_ReportValueTypeInfo_reportValueTypeDescription (pathsOf (reportValueTypeDescription x) a),
                                                           map Path_ReportValueTypeInfo_reportValueTypeDefinition (pathsOf (reportValueTypeDefinition x) a)]
instance IsPath ReportValueTypeInfo Markup
    where type Path ReportValueTypeInfo
                    Markup = Path_ReportValueTypeInfo Markup
          pathsOf (x@(ReportValueTypeInfo {})) a = concat [map Path_ReportValueTypeInfo_reportValueTypeName (pathsOf (reportValueTypeName x) a),
                                                           map Path_ReportValueTypeInfo_reportValueTypeDescription (pathsOf (reportValueTypeDescription x) a),
                                                           map Path_ReportValueTypeInfo_reportValueTypeDefinition (pathsOf (reportValueTypeDefinition x) a)]
instance IsPath ReportValueTypeInfo ReportValueTypeInfo
    where type Path ReportValueTypeInfo
                    ReportValueTypeInfo = Path_ReportValueTypeInfo ReportValueTypeInfo
          pathsOf _ _ = [idPath]
instance IsPath ReportValueTypeInfo Text
    where type Path ReportValueTypeInfo
                    Text = Path_ReportValueTypeInfo Text
          pathsOf (x@(ReportValueTypeInfo {})) a = concat [map Path_ReportValueTypeInfo_reportValueTypeName (pathsOf (reportValueTypeName x) a),
                                                           map Path_ReportValueTypeInfo_reportValueTypeDescription (pathsOf (reportValueTypeDescription x) a),
                                                           map Path_ReportValueTypeInfo_reportValueTypeDefinition (pathsOf (reportValueTypeDefinition x) a)]
instance IsPath EUI ImageFile
    where type Path EUI ImageFile = Path_Either (Path_URI ImageFile)
                                                (Path_ImageFile ImageFile)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Right) pv) (pathsOf (id pv) a :: [Path ImageFile
                                                                                                  ImageFile])) (either (const []) (: []) x)
instance IsPath EUI EUI
    where type Path EUI EUI = Path_Either (Path_URI EUI)
                                          (Path_ImageFile EUI)
          pathsOf _ _ = [idPath]
instance IsPath EUI URI
    where type Path EUI URI = Path_Either (Path_URI URI)
                                          (Path_ImageFile URI)
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Left) pv) (pathsOf (id pv) a :: [Path URI
                                                                                                 URI])) (either (: []) (const []) x)
instance IsPath MEUI ImageFile
    where type Path MEUI
                    ImageFile = Path_Maybe (Path_Either (Path_URI ImageFile)
                                                        (Path_ImageFile ImageFile))
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Just) pv) (pathsOf (id pv) a :: [Path (Either URI
                                                                                                         ImageFile)
                                                                                                 ImageFile])) (maybe [] (: []) x)
instance IsPath MEUI EUI
    where type Path MEUI EUI = Path_Maybe (Path_Either (Path_URI EUI)
                                                       (Path_ImageFile EUI))
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Just) pv) (pathsOf (id pv) a :: [Path (Either URI
                                                                                                         ImageFile)
                                                                                                 (Either URI
                                                                                                         ImageFile)])) (maybe [] (: []) x)
instance IsPath MEUI MEUI
    where type Path MEUI MEUI = Path_Maybe (Path_Either (Path_URI MEUI)
                                                        (Path_ImageFile MEUI))
          pathsOf _ _ = [idPath]
instance IsPath MEUI URI
    where type Path MEUI URI = Path_Maybe (Path_Either (Path_URI URI)
                                                       (Path_ImageFile URI))
          pathsOf x a = concatMap (\pv -> map ((\_ -> Path_Just) pv) (pathsOf (id pv) a :: [Path (Either URI
                                                                                                         ImageFile)
                                                                                                 URI])) (maybe [] (: []) x)
instance IsPath MaybeImageFile String
    where type Path MaybeImageFile String = Path_MaybeImageFile String
          pathsOf x a = let {p = Path_MaybeImageFile_View idPath :: Path (Maybe ImageFile)
                                                                         ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_MaybeImageFile_View (pathsOf x' a :: [Path ([Char])
                                                                                ([Char])])
instance IsPath MaybeImageFile JSONText
    where type Path MaybeImageFile
                    JSONText = Path_MaybeImageFile JSONText
          pathsOf x a = let {p = Path_MaybeImageFile_View idPath :: Path (Maybe ImageFile)
                                                                         ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_MaybeImageFile_View (pathsOf x' a :: [Path ([Char])
                                                                                JSONText])
instance IsPath MaybeImageFile MaybeImageFile
    where type Path MaybeImageFile
                    MaybeImageFile = Path_MaybeImageFile MaybeImageFile
          pathsOf _ _ = [idPath]
instance IsPath ReportImage String
    where type Path ReportImage String = Path_ReportImage String
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             ([Char])])
instance IsPath ReportImage Bool
    where type Path ReportImage Bool = Path_ReportImage Bool
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             Bool])
instance IsPath ReportImage Double
    where type Path ReportImage Double = Path_ReportImage Double
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             Double])
instance IsPath ReportImage Dimension
    where type Path ReportImage Dimension = Path_ReportImage Dimension
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             Dimension])
instance IsPath ReportImage ImageCrop
    where type Path ReportImage ImageCrop = Path_ReportImage ImageCrop
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             ImageCrop])
instance IsPath ReportImage ImageSize
    where type Path ReportImage ImageSize = Path_ReportImage ImageSize
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             ImageSize])
instance IsPath ReportImage Units
    where type Path ReportImage Units = Path_ReportImage Units
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             Units])
instance IsPath ReportImage ImageFile
    where type Path ReportImage ImageFile = Path_ReportImage ImageFile
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             ImageFile])
instance IsPath ReportImage JSONText
    where type Path ReportImage JSONText = Path_ReportImage JSONText
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             JSONText])
instance IsPath ReportImage Markup
    where type Path ReportImage Markup = Path_ReportImage Markup
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             Markup])
instance IsPath ReportImage EUI
    where type Path ReportImage EUI = Path_ReportImage EUI
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             (Either URI
                                                                                     ImageFile)])
instance IsPath ReportImage MEUI
    where type Path ReportImage MEUI = Path_ReportImage MEUI
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             (Maybe (Either URI
                                                                                            ImageFile))])
instance IsPath ReportImage MaybeImageFile
    where type Path ReportImage
                    MaybeImageFile = Path_ReportImage MaybeImageFile
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             (Maybe ImageFile)])
instance IsPath ReportImage ReportImage
    where type Path ReportImage
                    ReportImage = Path_ReportImage ReportImage
          pathsOf _ _ = [idPath]
instance IsPath ReportImage ReportImageView
    where type Path ReportImage
                    ReportImageView = Path_ReportImage ReportImageView
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             ReportImageView])
instance IsPath ReportImage SaneSizeImageSize
    where type Path ReportImage
                    SaneSizeImageSize = Path_ReportImage SaneSizeImageSize
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             (SaneSize ImageSize)])
instance IsPath ReportImage URI
    where type Path ReportImage URI = Path_ReportImage URI
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             URI])
instance IsPath ReportImage Text
    where type Path ReportImage Text = Path_ReportImage Text
          pathsOf x a = let {p = Path_ReportImage_View idPath :: Path ReportImage
                                                                      ReportImageView;
                             [x'] = toListOf (toLens p) x :: [ReportImageView]}
                         in map Path_ReportImage_View (pathsOf x' a :: [Path ReportImageView
                                                                             Text])
instance IsPath ReportImages String
    where type Path ReportImages String = Path_OMap ReportImageID
                                                    (Path_ReportImage String)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                ([Char])])) ((toPairs :: Order ReportImageID
                                                                                                                               ReportImage ->
                                                                                                                         [(ReportImageID,
                                                                                                                           ReportImage)]) x)
instance IsPath ReportImages Bool
    where type Path ReportImages Bool = Path_OMap ReportImageID
                                                  (Path_ReportImage Bool)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                Bool])) ((toPairs :: Order ReportImageID
                                                                                                                           ReportImage ->
                                                                                                                     [(ReportImageID,
                                                                                                                       ReportImage)]) x)
instance IsPath ReportImages Double
    where type Path ReportImages Double = Path_OMap ReportImageID
                                                    (Path_ReportImage Double)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                Double])) ((toPairs :: Order ReportImageID
                                                                                                                             ReportImage ->
                                                                                                                       [(ReportImageID,
                                                                                                                         ReportImage)]) x)
instance IsPath ReportImages Dimension
    where type Path ReportImages Dimension = Path_OMap ReportImageID
                                                       (Path_ReportImage Dimension)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                Dimension])) ((toPairs :: Order ReportImageID
                                                                                                                                ReportImage ->
                                                                                                                          [(ReportImageID,
                                                                                                                            ReportImage)]) x)
instance IsPath ReportImages ImageCrop
    where type Path ReportImages ImageCrop = Path_OMap ReportImageID
                                                       (Path_ReportImage ImageCrop)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                ImageCrop])) ((toPairs :: Order ReportImageID
                                                                                                                                ReportImage ->
                                                                                                                          [(ReportImageID,
                                                                                                                            ReportImage)]) x)
instance IsPath ReportImages ImageSize
    where type Path ReportImages ImageSize = Path_OMap ReportImageID
                                                       (Path_ReportImage ImageSize)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                ImageSize])) ((toPairs :: Order ReportImageID
                                                                                                                                ReportImage ->
                                                                                                                          [(ReportImageID,
                                                                                                                            ReportImage)]) x)
instance IsPath ReportImages Units
    where type Path ReportImages Units = Path_OMap ReportImageID
                                                   (Path_ReportImage Units)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                Units])) ((toPairs :: Order ReportImageID
                                                                                                                            ReportImage ->
                                                                                                                      [(ReportImageID,
                                                                                                                        ReportImage)]) x)
instance IsPath ReportImages ImageFile
    where type Path ReportImages ImageFile = Path_OMap ReportImageID
                                                       (Path_ReportImage ImageFile)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                ImageFile])) ((toPairs :: Order ReportImageID
                                                                                                                                ReportImage ->
                                                                                                                          [(ReportImageID,
                                                                                                                            ReportImage)]) x)
instance IsPath ReportImages JSONText
    where type Path ReportImages JSONText = Path_OMap ReportImageID
                                                      (Path_ReportImage JSONText)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                JSONText])) ((toPairs :: Order ReportImageID
                                                                                                                               ReportImage ->
                                                                                                                         [(ReportImageID,
                                                                                                                           ReportImage)]) x)
instance IsPath ReportImages Markup
    where type Path ReportImages Markup = Path_OMap ReportImageID
                                                    (Path_ReportImage Markup)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                Markup])) ((toPairs :: Order ReportImageID
                                                                                                                             ReportImage ->
                                                                                                                       [(ReportImageID,
                                                                                                                         ReportImage)]) x)
instance IsPath ReportImages EUI
    where type Path ReportImages EUI = Path_OMap ReportImageID
                                                 (Path_ReportImage EUI)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                (Either URI
                                                                                                        ImageFile)])) ((toPairs :: Order ReportImageID
                                                                                                                                         ReportImage ->
                                                                                                                                   [(ReportImageID,
                                                                                                                                     ReportImage)]) x)
instance IsPath ReportImages MEUI
    where type Path ReportImages MEUI = Path_OMap ReportImageID
                                                  (Path_ReportImage MEUI)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                (Maybe (Either URI
                                                                                                               ImageFile))])) ((toPairs :: Order ReportImageID
                                                                                                                                                 ReportImage ->
                                                                                                                                           [(ReportImageID,
                                                                                                                                             ReportImage)]) x)
instance IsPath ReportImages MaybeImageFile
    where type Path ReportImages
                    MaybeImageFile = Path_OMap ReportImageID
                                               (Path_ReportImage MaybeImageFile)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                (Maybe ImageFile)])) ((toPairs :: Order ReportImageID
                                                                                                                                        ReportImage ->
                                                                                                                                  [(ReportImageID,
                                                                                                                                    ReportImage)]) x)
instance IsPath ReportImages ReportImage
    where type Path ReportImages ReportImage = Path_OMap ReportImageID
                                                         (Path_ReportImage ReportImage)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                ReportImage])) ((toPairs :: Order ReportImageID
                                                                                                                                  ReportImage ->
                                                                                                                            [(ReportImageID,
                                                                                                                              ReportImage)]) x)
instance IsPath ReportImages ReportImages
    where type Path ReportImages ReportImages = Path_OMap ReportImageID
                                                          (Path_ReportImage ReportImages)
          pathsOf _ _ = [idPath]
instance IsPath ReportImages ReportImageView
    where type Path ReportImages
                    ReportImageView = Path_OMap ReportImageID
                                                (Path_ReportImage ReportImageView)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                ReportImageView])) ((toPairs :: Order ReportImageID
                                                                                                                                      ReportImage ->
                                                                                                                                [(ReportImageID,
                                                                                                                                  ReportImage)]) x)
instance IsPath ReportImages SaneSizeImageSize
    where type Path ReportImages
                    SaneSizeImageSize = Path_OMap ReportImageID
                                                  (Path_ReportImage SaneSizeImageSize)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                (SaneSize ImageSize)])) ((toPairs :: Order ReportImageID
                                                                                                                                           ReportImage ->
                                                                                                                                     [(ReportImageID,
                                                                                                                                       ReportImage)]) x)
instance IsPath ReportImages URI
    where type Path ReportImages URI = Path_OMap ReportImageID
                                                 (Path_ReportImage URI)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                URI])) ((toPairs :: Order ReportImageID
                                                                                                                          ReportImage ->
                                                                                                                    [(ReportImageID,
                                                                                                                      ReportImage)]) x)
instance IsPath ReportImages Text
    where type Path ReportImages Text = Path_OMap ReportImageID
                                                  (Path_ReportImage Text)
          pathsOf x a = concatMap (\pv -> map ((Path_At . fst) pv) (pathsOf (snd pv) a :: [Path ReportImage
                                                                                                Text])) ((toPairs :: Order ReportImageID
                                                                                                                           ReportImage ->
                                                                                                                     [(ReportImageID,
                                                                                                                       ReportImage)]) x)
instance IsPath ReadOnlyFilePath String
    where type Path ReadOnlyFilePath
                    String = Path_ReadOnlyFilePath String
          pathsOf x a = let {p = Path_ReadOnlyFilePath_View idPath :: Path (ReadOnly ([Char]))
                                                                           ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_ReadOnlyFilePath_View (pathsOf x' a :: [Path ([Char])
                                                                                  ([Char])])
instance IsPath ReadOnlyFilePath JSONText
    where type Path ReadOnlyFilePath
                    JSONText = Path_ReadOnlyFilePath JSONText
          pathsOf x a = let {p = Path_ReadOnlyFilePath_View idPath :: Path (ReadOnly ([Char]))
                                                                           ([Char]);
                             [x'] = toListOf (toLens p) x :: [[Char]]}
                         in map Path_ReadOnlyFilePath_View (pathsOf x' a :: [Path ([Char])
                                                                                  JSONText])
instance IsPath ReadOnlyFilePath ReadOnlyFilePath
    where type Path ReadOnlyFilePath
                    ReadOnlyFilePath = Path_ReadOnlyFilePath ReadOnlyFilePath
          pathsOf _ _ = [idPath]
instance IsPath ReportImageView String
    where type Path ReportImageView
                    String = Path_ReportImageView String
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a),
                                                       map Path_ReportImageView__picEditedDeprecated (pathsOf (_picEditedDeprecated x) a),
                                                       map Path_ReportImageView__picThumbDeprecated (pathsOf (_picThumbDeprecated x) a),
                                                       map Path_ReportImageView__picPrinterDeprecated (pathsOf (_picPrinterDeprecated x) a),
                                                       map Path_ReportImageView__picMustEnlarge (pathsOf (_picMustEnlarge x) a),
                                                       map Path_ReportImageView__picEnlargedDeprecated (pathsOf (_picEnlargedDeprecated x) a)]
instance IsPath ReportImageView Bool
    where type Path ReportImageView Bool = Path_ReportImageView Bool
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picMustEnlarge (pathsOf (_picMustEnlarge x) a)]
instance IsPath ReportImageView Double
    where type Path ReportImageView
                    Double = Path_ReportImageView Double
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a)]
instance IsPath ReportImageView Dimension
    where type Path ReportImageView
                    Dimension = Path_ReportImageView Dimension
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a)]
instance IsPath ReportImageView ImageCrop
    where type Path ReportImageView
                    ImageCrop = Path_ReportImageView ImageCrop
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picCrop (pathsOf (_picCrop x) a)]
instance IsPath ReportImageView ImageSize
    where type Path ReportImageView
                    ImageSize = Path_ReportImageView ImageSize
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a)]
instance IsPath ReportImageView Units
    where type Path ReportImageView Units = Path_ReportImageView Units
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a)]
instance IsPath ReportImageView ImageFile
    where type Path ReportImageView
                    ImageFile = Path_ReportImageView ImageFile
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picOriginal (pathsOf (_picOriginal x) a)]
instance IsPath ReportImageView JSONText
    where type Path ReportImageView
                    JSONText = Path_ReportImageView JSONText
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a),
                                                       map Path_ReportImageView__picCaption (pathsOf (_picCaption x) a),
                                                       map Path_ReportImageView__picEditedDeprecated (pathsOf (_picEditedDeprecated x) a),
                                                       map Path_ReportImageView__picThumbDeprecated (pathsOf (_picThumbDeprecated x) a),
                                                       map Path_ReportImageView__picPrinterDeprecated (pathsOf (_picPrinterDeprecated x) a),
                                                       map Path_ReportImageView__picMustEnlarge (pathsOf (_picMustEnlarge x) a),
                                                       map Path_ReportImageView__picEnlargedDeprecated (pathsOf (_picEnlargedDeprecated x) a)]
instance IsPath ReportImageView Markup
    where type Path ReportImageView
                    Markup = Path_ReportImageView Markup
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picCaption (pathsOf (_picCaption x) a)]
instance IsPath ReportImageView EUI
    where type Path ReportImageView EUI = Path_ReportImageView EUI
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picOriginal (pathsOf (_picOriginal x) a)]
instance IsPath ReportImageView MEUI
    where type Path ReportImageView MEUI = Path_ReportImageView MEUI
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picOriginal (pathsOf (_picOriginal x) a)]
instance IsPath ReportImageView MaybeImageFile
    where type Path ReportImageView
                    MaybeImageFile = Path_ReportImageView MaybeImageFile
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picEditedDeprecated (pathsOf (_picEditedDeprecated x) a),
                                                       map Path_ReportImageView__picThumbDeprecated (pathsOf (_picThumbDeprecated x) a),
                                                       map Path_ReportImageView__picPrinterDeprecated (pathsOf (_picPrinterDeprecated x) a),
                                                       map Path_ReportImageView__picEnlargedDeprecated (pathsOf (_picEnlargedDeprecated x) a)]
instance IsPath ReportImageView ReportImageView
    where type Path ReportImageView
                    ReportImageView = Path_ReportImageView ReportImageView
          pathsOf _ _ = [idPath]
instance IsPath ReportImageView SaneSizeImageSize
    where type Path ReportImageView
                    SaneSizeImageSize = Path_ReportImageView SaneSizeImageSize
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picSize (pathsOf (_picSize x) a)]
instance IsPath ReportImageView URI
    where type Path ReportImageView URI = Path_ReportImageView URI
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picOriginal (pathsOf (_picOriginal x) a)]
instance IsPath ReportImageView Text
    where type Path ReportImageView Text = Path_ReportImageView Text
          pathsOf (x@(ReportImageView {})) a = concat [map Path_ReportImageView__picCaption (pathsOf (_picCaption x) a)]
instance IsPath ReportView String
    where type Path ReportView String = Path_ReportView String
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportFolder (pathsOf (_reportFolder x) a),
                                                  map Path_ReportView__reportIntendedUse (pathsOf (_reportIntendedUse x) a),
                                                  map Path_ReportView__reportBody (pathsOf (_reportBody x) a),
                                                  map Path_ReportView__reportStatus (pathsOf (_reportStatus x) a),
                                                  map Path_ReportView__reportRedacted (pathsOf (_reportRedacted x) a),
                                                  map Path_ReportView__reportFlags (pathsOf (_reportFlags x) a),
                                                  map Path_ReportView__reportOrderByItemName (pathsOf (_reportOrderByItemName x) a),
                                                  map Path_ReportView__reportDisplayItemName (pathsOf (_reportDisplayItemName x) a)]
instance IsPath ReportView Int64
    where type Path ReportView Int64 = Path_ReportView Int64
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportCreated (pathsOf (_reportCreated x) a)]
instance IsPath ReportView Bool
    where type Path ReportView Bool = Path_ReportView Bool
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a),
                                                  map Path_ReportView__reportRedacted (pathsOf (_reportRedacted x) a),
                                                  map Path_ReportView__reportFlags (pathsOf (_reportFlags x) a),
                                                  map Path_ReportView__reportOrderByItemName (pathsOf (_reportOrderByItemName x) a),
                                                  map Path_ReportView__reportDisplayItemName (pathsOf (_reportDisplayItemName x) a)]
instance IsPath ReportView Double
    where type Path ReportView Double = Path_ReportView Double
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView Int
    where type Path ReportView Int = Path_ReportView Int
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportStandardsVersion (pathsOf (_reportStandardsVersion x) a)]
instance IsPath ReportView Dimension
    where type Path ReportView Dimension = Path_ReportView Dimension
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ImageCrop
    where type Path ReportView ImageCrop = Path_ReportView ImageCrop
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ImageSize
    where type Path ReportView ImageSize = Path_ReportView ImageSize
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView Units
    where type Path ReportView Units = Path_ReportView Units
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ImageFile
    where type Path ReportView ImageFile = Path_ReportView ImageFile
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView Integer
    where type Path ReportView Integer = Path_ReportView Integer
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportRevision (pathsOf (_reportRevision x) a)]
instance IsPath ReportView JSONText
    where type Path ReportView JSONText = Path_ReportView JSONText
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportFolder (pathsOf (_reportFolder x) a),
                                                  map Path_ReportView__reportName (pathsOf (_reportName x) a),
                                                  map Path_ReportView__reportDate (pathsOf (_reportDate x) a),
                                                  map Path_ReportView__reportContractDate (pathsOf (_reportContractDate x) a),
                                                  map Path_ReportView__reportInspectionDate (pathsOf (_reportInspectionDate x) a),
                                                  map Path_ReportView__reportEffectiveDate (pathsOf (_reportEffectiveDate x) a),
                                                  map Path_ReportView__reportAuthors (pathsOf (_reportAuthors x) a),
                                                  map Path_ReportView__reportPreparer (pathsOf (_reportPreparer x) a),
                                                  map Path_ReportView__reportPreparerEIN (pathsOf (_reportPreparerEIN x) a),
                                                  map Path_ReportView__reportPreparerAddress (pathsOf (_reportPreparerAddress x) a),
                                                  map Path_ReportView__reportPreparerEMail (pathsOf (_reportPreparerEMail x) a),
                                                  map Path_ReportView__reportPreparerWebsite (pathsOf (_reportPreparerWebsite x) a),
                                                  map Path_ReportView__reportAbbrevs (pathsOf (_reportAbbrevs x) a),
                                                  map Path_ReportView__reportTitle (pathsOf (_reportTitle x) a),
                                                  map Path_ReportView__reportHeader (pathsOf (_reportHeader x) a),
                                                  map Path_ReportView__reportFooter (pathsOf (_reportFooter x) a),
                                                  map Path_ReportView__reportIntendedUse (pathsOf (_reportIntendedUse x) a),
                                                  map Path_ReportView__reportValueTypeInfo (pathsOf (_reportValueTypeInfo x) a),
                                                  map Path_ReportView__reportValueApproachInfo (pathsOf (_reportValueApproachInfo x) a),
                                                  map Path_ReportView__reportClientName (pathsOf (_reportClientName x) a),
                                                  map Path_ReportView__reportClientAddress (pathsOf (_reportClientAddress x) a),
                                                  map Path_ReportView__reportClientGreeting (pathsOf (_reportClientGreeting x) a),
                                                  map Path_ReportView__reportItemsOwnerFull (pathsOf (_reportItemsOwnerFull x) a),
                                                  map Path_ReportView__reportItemsOwner (pathsOf (_reportItemsOwner x) a),
                                                  map Path_ReportView__reportBriefItems (pathsOf (_reportBriefItems x) a),
                                                  map Path_ReportView__reportInspectionLocation (pathsOf (_reportInspectionLocation x) a),
                                                  map Path_ReportView__reportBody (pathsOf (_reportBody x) a),
                                                  map Path_ReportView__reportGlossary (pathsOf (_reportGlossary x) a),
                                                  map Path_ReportView__reportSources (pathsOf (_reportSources x) a),
                                                  map Path_ReportView__reportLetterOfTransmittal (pathsOf (_reportLetterOfTransmittal x) a),
                                                  map Path_ReportView__reportScopeOfWork (pathsOf (_reportScopeOfWork x) a),
                                                  map Path_ReportView__reportCertification (pathsOf (_reportCertification x) a),
                                                  map Path_ReportView__reportLimitingConditions (pathsOf (_reportLimitingConditions x) a),
                                                  map Path_ReportView__reportPrivacyPolicy (pathsOf (_reportPrivacyPolicy x) a),
                                                  map Path_ReportView__reportPerms (pathsOf (_reportPerms x) a),
                                                  map Path_ReportView__reportBranding (pathsOf (_reportBranding x) a),
                                                  map Path_ReportView__reportStatus (pathsOf (_reportStatus x) a),
                                                  map Path_ReportView__reportRedacted (pathsOf (_reportRedacted x) a),
                                                  map Path_ReportView__reportFlags (pathsOf (_reportFlags x) a),
                                                  map Path_ReportView__reportOrderByItemName (pathsOf (_reportOrderByItemName x) a),
                                                  map Path_ReportView__reportDisplayItemName (pathsOf (_reportDisplayItemName x) a)]
instance IsPath ReportView Markup
    where type Path ReportView Markup = Path_ReportView Markup
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportName (pathsOf (_reportName x) a),
                                                  map Path_ReportView__reportDate (pathsOf (_reportDate x) a),
                                                  map Path_ReportView__reportContractDate (pathsOf (_reportContractDate x) a),
                                                  map Path_ReportView__reportInspectionDate (pathsOf (_reportInspectionDate x) a),
                                                  map Path_ReportView__reportEffectiveDate (pathsOf (_reportEffectiveDate x) a),
                                                  map Path_ReportView__reportAuthors (pathsOf (_reportAuthors x) a),
                                                  map Path_ReportView__reportPreparer (pathsOf (_reportPreparer x) a),
                                                  map Path_ReportView__reportPreparerEIN (pathsOf (_reportPreparerEIN x) a),
                                                  map Path_ReportView__reportPreparerAddress (pathsOf (_reportPreparerAddress x) a),
                                                  map Path_ReportView__reportPreparerEMail (pathsOf (_reportPreparerEMail x) a),
                                                  map Path_ReportView__reportPreparerWebsite (pathsOf (_reportPreparerWebsite x) a),
                                                  map Path_ReportView__reportAbbrevs (pathsOf (_reportAbbrevs x) a),
                                                  map Path_ReportView__reportTitle (pathsOf (_reportTitle x) a),
                                                  map Path_ReportView__reportHeader (pathsOf (_reportHeader x) a),
                                                  map Path_ReportView__reportFooter (pathsOf (_reportFooter x) a),
                                                  map Path_ReportView__reportValueTypeInfo (pathsOf (_reportValueTypeInfo x) a),
                                                  map Path_ReportView__reportValueApproachInfo (pathsOf (_reportValueApproachInfo x) a),
                                                  map Path_ReportView__reportClientName (pathsOf (_reportClientName x) a),
                                                  map Path_ReportView__reportClientAddress (pathsOf (_reportClientAddress x) a),
                                                  map Path_ReportView__reportClientGreeting (pathsOf (_reportClientGreeting x) a),
                                                  map Path_ReportView__reportItemsOwnerFull (pathsOf (_reportItemsOwnerFull x) a),
                                                  map Path_ReportView__reportItemsOwner (pathsOf (_reportItemsOwner x) a),
                                                  map Path_ReportView__reportBriefItems (pathsOf (_reportBriefItems x) a),
                                                  map Path_ReportView__reportInspectionLocation (pathsOf (_reportInspectionLocation x) a),
                                                  map Path_ReportView__reportBody (pathsOf (_reportBody x) a),
                                                  map Path_ReportView__reportGlossary (pathsOf (_reportGlossary x) a),
                                                  map Path_ReportView__reportSources (pathsOf (_reportSources x) a),
                                                  map Path_ReportView__reportLetterOfTransmittal (pathsOf (_reportLetterOfTransmittal x) a),
                                                  map Path_ReportView__reportScopeOfWork (pathsOf (_reportScopeOfWork x) a),
                                                  map Path_ReportView__reportCertification (pathsOf (_reportCertification x) a),
                                                  map Path_ReportView__reportLimitingConditions (pathsOf (_reportLimitingConditions x) a),
                                                  map Path_ReportView__reportPrivacyPolicy (pathsOf (_reportPrivacyPolicy x) a)]
instance IsPath ReportView Permissions
    where type Path ReportView
                    Permissions = Path_ReportView Permissions
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportPerms (pathsOf (_reportPerms x) a)]
instance IsPath ReportView UserIds
    where type Path ReportView UserIds = Path_ReportView UserIds
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportPerms (pathsOf (_reportPerms x) a)]
instance IsPath ReportView AbbrevPair
    where type Path ReportView AbbrevPair = Path_ReportView AbbrevPair
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportAbbrevs (pathsOf (_reportAbbrevs x) a)]
instance IsPath ReportView AbbrevPairs
    where type Path ReportView
                    AbbrevPairs = Path_ReportView AbbrevPairs
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportAbbrevs (pathsOf (_reportAbbrevs x) a)]
instance IsPath ReportView Author
    where type Path ReportView Author = Path_ReportView Author
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportAuthors (pathsOf (_reportAuthors x) a)]
instance IsPath ReportView Authors
    where type Path ReportView Authors = Path_ReportView Authors
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportAuthors (pathsOf (_reportAuthors x) a)]
instance IsPath ReportView Branding
    where type Path ReportView Branding = Path_ReportView Branding
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBranding (pathsOf (_reportBranding x) a)]
instance IsPath ReportView MarkupPair
    where type Path ReportView MarkupPair = Path_ReportView MarkupPair
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportGlossary (pathsOf (_reportGlossary x) a),
                                                  map Path_ReportView__reportSources (pathsOf (_reportSources x) a)]
instance IsPath ReportView MarkupPairs
    where type Path ReportView
                    MarkupPairs = Path_ReportView MarkupPairs
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportGlossary (pathsOf (_reportGlossary x) a),
                                                  map Path_ReportView__reportSources (pathsOf (_reportSources x) a)]
instance IsPath ReportView Markups
    where type Path ReportView Markups = Path_ReportView Markups
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportCertification (pathsOf (_reportCertification x) a),
                                                  map Path_ReportView__reportLimitingConditions (pathsOf (_reportLimitingConditions x) a)]
instance IsPath ReportView MaybeReportIntendedUse
    where type Path ReportView
                    MaybeReportIntendedUse = Path_ReportView MaybeReportIntendedUse
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportIntendedUse (pathsOf (_reportIntendedUse x) a)]
instance IsPath ReportView ReportElem
    where type Path ReportView ReportElem = Path_ReportView ReportElem
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ReportElems
    where type Path ReportView
                    ReportElems = Path_ReportView ReportElems
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ReportFlags
    where type Path ReportView
                    ReportFlags = Path_ReportView ReportFlags
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportFlags (pathsOf (_reportFlags x) a)]
instance IsPath ReportView ReportStandard
    where type Path ReportView
                    ReportStandard = Path_ReportView ReportStandard
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportStandardsVersion (pathsOf (_reportStandardsVersion x) a)]
instance IsPath ReportView ReportStatus
    where type Path ReportView
                    ReportStatus = Path_ReportView ReportStatus
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportStatus (pathsOf (_reportStatus x) a)]
instance IsPath ReportView ReportValueApproachInfo
    where type Path ReportView
                    ReportValueApproachInfo = Path_ReportView ReportValueApproachInfo
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportValueApproachInfo (pathsOf (_reportValueApproachInfo x) a)]
instance IsPath ReportView ReportValueTypeInfo
    where type Path ReportView
                    ReportValueTypeInfo = Path_ReportView ReportValueTypeInfo
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportValueTypeInfo (pathsOf (_reportValueTypeInfo x) a)]
instance IsPath ReportView EUI
    where type Path ReportView EUI = Path_ReportView EUI
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView MEUI
    where type Path ReportView MEUI = Path_ReportView MEUI
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView MaybeImageFile
    where type Path ReportView
                    MaybeImageFile = Path_ReportView MaybeImageFile
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ReportImage
    where type Path ReportView
                    ReportImage = Path_ReportView ReportImage
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ReportImages
    where type Path ReportView
                    ReportImages = Path_ReportView ReportImages
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ReadOnlyFilePath
    where type Path ReportView
                    ReadOnlyFilePath = Path_ReportView ReadOnlyFilePath
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportFolder (pathsOf (_reportFolder x) a)]
instance IsPath ReportView ReportImageView
    where type Path ReportView
                    ReportImageView = Path_ReportView ReportImageView
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView ReportView
    where type Path ReportView ReportView = Path_ReportView ReportView
          pathsOf _ _ = [idPath]
instance IsPath ReportView SaneSizeImageSize
    where type Path ReportView
                    SaneSizeImageSize = Path_ReportView SaneSizeImageSize
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView Item
    where type Path ReportView Item = Path_ReportView Item
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView MIM
    where type Path ReportView MIM = Path_ReportView MIM
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView CIString
    where type Path ReportView CIString = Path_ReportView CIString
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportAbbrevs (pathsOf (_reportAbbrevs x) a)]
instance IsPath ReportView URI
    where type Path ReportView URI = Path_ReportView URI
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportBody (pathsOf (_reportBody x) a)]
instance IsPath ReportView Text
    where type Path ReportView Text = Path_ReportView Text
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportName (pathsOf (_reportName x) a),
                                                  map Path_ReportView__reportDate (pathsOf (_reportDate x) a),
                                                  map Path_ReportView__reportContractDate (pathsOf (_reportContractDate x) a),
                                                  map Path_ReportView__reportInspectionDate (pathsOf (_reportInspectionDate x) a),
                                                  map Path_ReportView__reportEffectiveDate (pathsOf (_reportEffectiveDate x) a),
                                                  map Path_ReportView__reportAuthors (pathsOf (_reportAuthors x) a),
                                                  map Path_ReportView__reportPreparer (pathsOf (_reportPreparer x) a),
                                                  map Path_ReportView__reportPreparerEIN (pathsOf (_reportPreparerEIN x) a),
                                                  map Path_ReportView__reportPreparerAddress (pathsOf (_reportPreparerAddress x) a),
                                                  map Path_ReportView__reportPreparerEMail (pathsOf (_reportPreparerEMail x) a),
                                                  map Path_ReportView__reportPreparerWebsite (pathsOf (_reportPreparerWebsite x) a),
                                                  map Path_ReportView__reportAbbrevs (pathsOf (_reportAbbrevs x) a),
                                                  map Path_ReportView__reportTitle (pathsOf (_reportTitle x) a),
                                                  map Path_ReportView__reportHeader (pathsOf (_reportHeader x) a),
                                                  map Path_ReportView__reportFooter (pathsOf (_reportFooter x) a),
                                                  map Path_ReportView__reportValueTypeInfo (pathsOf (_reportValueTypeInfo x) a),
                                                  map Path_ReportView__reportValueApproachInfo (pathsOf (_reportValueApproachInfo x) a),
                                                  map Path_ReportView__reportClientName (pathsOf (_reportClientName x) a),
                                                  map Path_ReportView__reportClientAddress (pathsOf (_reportClientAddress x) a),
                                                  map Path_ReportView__reportClientGreeting (pathsOf (_reportClientGreeting x) a),
                                                  map Path_ReportView__reportItemsOwnerFull (pathsOf (_reportItemsOwnerFull x) a),
                                                  map Path_ReportView__reportItemsOwner (pathsOf (_reportItemsOwner x) a),
                                                  map Path_ReportView__reportBriefItems (pathsOf (_reportBriefItems x) a),
                                                  map Path_ReportView__reportInspectionLocation (pathsOf (_reportInspectionLocation x) a),
                                                  map Path_ReportView__reportBody (pathsOf (_reportBody x) a),
                                                  map Path_ReportView__reportGlossary (pathsOf (_reportGlossary x) a),
                                                  map Path_ReportView__reportSources (pathsOf (_reportSources x) a),
                                                  map Path_ReportView__reportLetterOfTransmittal (pathsOf (_reportLetterOfTransmittal x) a),
                                                  map Path_ReportView__reportScopeOfWork (pathsOf (_reportScopeOfWork x) a),
                                                  map Path_ReportView__reportCertification (pathsOf (_reportCertification x) a),
                                                  map Path_ReportView__reportLimitingConditions (pathsOf (_reportLimitingConditions x) a),
                                                  map Path_ReportView__reportPrivacyPolicy (pathsOf (_reportPrivacyPolicy x) a),
                                                  map Path_ReportView__reportPerms (pathsOf (_reportPerms x) a),
                                                  map Path_ReportView__reportBranding (pathsOf (_reportBranding x) a)]
instance IsPath ReportView UserId
    where type Path ReportView UserId = Path_ReportView UserId
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportPerms (pathsOf (_reportPerms x) a)]
instance IsPath ReportView UUID
    where type Path ReportView UUID = Path_ReportView UUID
          pathsOf (x@(ReportView {})) a = concat [map Path_ReportView__reportUUID (pathsOf (_reportUUID x) a)]
instance IsPath SaneSizeImageSize String
    where type Path SaneSizeImageSize
                    String = Path_SaneSizeImageSize String
          pathsOf x a = let {p = Path_SaneSizeImageSize_View idPath :: Path (SaneSize ImageSize)
                                                                            ImageSize;
                             [x'] = toListOf (toLens p) x :: [ImageSize]}
                         in map Path_SaneSizeImageSize_View (pathsOf x' a :: [Path ImageSize
                                                                                   ([Char])])
instance IsPath SaneSizeImageSize Double
    where type Path SaneSizeImageSize
                    Double = Path_SaneSizeImageSize Double
          pathsOf x a = let {p = Path_SaneSizeImageSize_View idPath :: Path (SaneSize ImageSize)
                                                                            ImageSize;
                             [x'] = toListOf (toLens p) x :: [ImageSize]}
                         in map Path_SaneSizeImageSize_View (pathsOf x' a :: [Path ImageSize
                                                                                   Double])
instance IsPath SaneSizeImageSize Dimension
    where type Path SaneSizeImageSize
                    Dimension = Path_SaneSizeImageSize Dimension
          pathsOf x a = let {p = Path_SaneSizeImageSize_View idPath :: Path (SaneSize ImageSize)
                                                                            ImageSize;
                             [x'] = toListOf (toLens p) x :: [ImageSize]}
                         in map Path_SaneSizeImageSize_View (pathsOf x' a :: [Path ImageSize
                                                                                   Dimension])
instance IsPath SaneSizeImageSize ImageSize
    where type Path SaneSizeImageSize
                    ImageSize = Path_SaneSizeImageSize ImageSize
          pathsOf x a = let {p = Path_SaneSizeImageSize_View idPath :: Path (SaneSize ImageSize)
                                                                            ImageSize;
                             [x'] = toListOf (toLens p) x :: [ImageSize]}
                         in map Path_SaneSizeImageSize_View (pathsOf x' a :: [Path ImageSize
                                                                                   ImageSize])
instance IsPath SaneSizeImageSize Units
    where type Path SaneSizeImageSize
                    Units = Path_SaneSizeImageSize Units
          pathsOf x a = let {p = Path_SaneSizeImageSize_View idPath :: Path (SaneSize ImageSize)
                                                                            ImageSize;
                             [x'] = toListOf (toLens p) x :: [ImageSize]}
                         in map Path_SaneSizeImageSize_View (pathsOf x' a :: [Path ImageSize
                                                                                   Units])
instance IsPath SaneSizeImageSize JSONText
    where type Path SaneSizeImageSize
                    JSONText = Path_SaneSizeImageSize JSONText
          pathsOf x a = let {p = Path_SaneSizeImageSize_View idPath :: Path (SaneSize ImageSize)
                                                                            ImageSize;
                             [x'] = toListOf (toLens p) x :: [ImageSize]}
                         in map Path_SaneSizeImageSize_View (pathsOf x' a :: [Path ImageSize
                                                                                   JSONText])
instance IsPath SaneSizeImageSize SaneSizeImageSize
    where type Path SaneSizeImageSize
                    SaneSizeImageSize = Path_SaneSizeImageSize SaneSizeImageSize
          pathsOf _ _ = [idPath]
instance IsPath Item String
    where type Path Item String = Path_Item String
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Bool
    where type Path Item Bool = Path_Item Bool
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Double
    where type Path Item Double = Path_Item Double
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Dimension
    where type Path Item Dimension = Path_Item Dimension
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item ImageCrop
    where type Path Item ImageCrop = Path_Item ImageCrop
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item ImageSize
    where type Path Item ImageSize = Path_Item ImageSize
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Units
    where type Path Item Units = Path_Item Units
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item ImageFile
    where type Path Item ImageFile = Path_Item ImageFile
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item JSONText
    where type Path Item JSONText = Path_Item JSONText
          pathsOf (x@(Item {})) a = concat [map Path_Item_itemName (pathsOf (itemName x) a),
                                            map Path_Item_fields (pathsOf (fields x) a),
                                            map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Markup
    where type Path Item Markup = Path_Item Markup
          pathsOf (x@(Item {})) a = concat [map Path_Item_fields (pathsOf (fields x) a),
                                            map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item EUI
    where type Path Item EUI = Path_Item EUI
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item MEUI
    where type Path Item MEUI = Path_Item MEUI
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item MaybeImageFile
    where type Path Item MaybeImageFile = Path_Item MaybeImageFile
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item ReportImage
    where type Path Item ReportImage = Path_Item ReportImage
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item ReportImages
    where type Path Item ReportImages = Path_Item ReportImages
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item ReportImageView
    where type Path Item ReportImageView = Path_Item ReportImageView
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item SaneSizeImageSize
    where type Path Item
                    SaneSizeImageSize = Path_Item SaneSizeImageSize
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Item
    where type Path Item Item = Path_Item Item
          pathsOf _ _ = [idPath]
instance IsPath Item MIM
    where type Path Item MIM = Path_Item MIM
          pathsOf (x@(Item {})) a = concat [map Path_Item_fields (pathsOf (fields x) a)]
instance IsPath Item URI
    where type Path Item URI = Path_Item URI
          pathsOf (x@(Item {})) a = concat [map Path_Item_images (pathsOf (images x) a)]
instance IsPath Item Text
    where type Path Item Text = Path_Item Text
          pathsOf (x@(Item {})) a = concat [map Path_Item_itemName (pathsOf (itemName x) a),
                                            map Path_Item_fields (pathsOf (fields x) a),
                                            map Path_Item_images (pathsOf (images x) a)]
instance IsPath MIM JSONText
    where type Path MIM JSONText = Path_Map ItemFieldName
                                            (Path_Markup JSONText)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                  JSONText])) (toList x)
instance IsPath MIM Markup
    where type Path MIM Markup = Path_Map ItemFieldName
                                          (Path_Markup Markup)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                  Markup])) (toList x)
instance IsPath MIM MIM
    where type Path MIM MIM = Path_Map ItemFieldName (Path_Markup MIM)
          pathsOf _ _ = [idPath]
instance IsPath MIM Text
    where type Path MIM Text = Path_Map ItemFieldName
                                        (Path_Markup Text)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Markup
                                                                                                  Text])) (toList x)
instance IsPath MRR String
    where type Path MRR String = Path_Map ReportID (Path_Report String)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ([Char])])) (toList x)
instance IsPath MRR Int64
    where type Path MRR Int64 = Path_Map ReportID (Path_Report Int64)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Int64])) (toList x)
instance IsPath MRR Bool
    where type Path MRR Bool = Path_Map ReportID (Path_Report Bool)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Bool])) (toList x)
instance IsPath MRR Double
    where type Path MRR Double = Path_Map ReportID (Path_Report Double)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Double])) (toList x)
instance IsPath MRR Int
    where type Path MRR Int = Path_Map ReportID (Path_Report Int)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Int])) (toList x)
instance IsPath MRR Dimension
    where type Path MRR Dimension = Path_Map ReportID
                                             (Path_Report Dimension)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Dimension])) (toList x)
instance IsPath MRR ImageCrop
    where type Path MRR ImageCrop = Path_Map ReportID
                                             (Path_Report ImageCrop)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ImageCrop])) (toList x)
instance IsPath MRR ImageSize
    where type Path MRR ImageSize = Path_Map ReportID
                                             (Path_Report ImageSize)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ImageSize])) (toList x)
instance IsPath MRR Units
    where type Path MRR Units = Path_Map ReportID (Path_Report Units)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Units])) (toList x)
instance IsPath MRR ImageFile
    where type Path MRR ImageFile = Path_Map ReportID
                                             (Path_Report ImageFile)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ImageFile])) (toList x)
instance IsPath MRR Integer
    where type Path MRR Integer = Path_Map ReportID
                                           (Path_Report Integer)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Integer])) (toList x)
instance IsPath MRR JSONText
    where type Path MRR JSONText = Path_Map ReportID
                                            (Path_Report JSONText)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  JSONText])) (toList x)
instance IsPath MRR Markup
    where type Path MRR Markup = Path_Map ReportID (Path_Report Markup)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Markup])) (toList x)
instance IsPath MRR Permissions
    where type Path MRR Permissions = Path_Map ReportID
                                               (Path_Report Permissions)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Permissions])) (toList x)
instance IsPath MRR UserIds
    where type Path MRR UserIds = Path_Map ReportID
                                           (Path_Report UserIds)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ([UserId])])) (toList x)
instance IsPath MRR AbbrevPair
    where type Path MRR AbbrevPair = Path_Map ReportID
                                              (Path_Report AbbrevPair)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ((CIString,
                                                                                                    Markup))])) (toList x)
instance IsPath MRR AbbrevPairs
    where type Path MRR AbbrevPairs = Path_Map ReportID
                                               (Path_Report AbbrevPairs)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Order AbbrevPairID
                                                                                                         ((CIString,
                                                                                                           Markup)))])) (toList x)
instance IsPath MRR Author
    where type Path MRR Author = Path_Map ReportID (Path_Report Author)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Author])) (toList x)
instance IsPath MRR Authors
    where type Path MRR Authors = Path_Map ReportID
                                           (Path_Report Authors)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Order AuthorID
                                                                                                         Author)])) (toList x)
instance IsPath MRR Branding
    where type Path MRR Branding = Path_Map ReportID
                                            (Path_Report Branding)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Branding])) (toList x)
instance IsPath MRR MarkupPair
    where type Path MRR MarkupPair = Path_Map ReportID
                                              (Path_Report MarkupPair)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ((Markup,
                                                                                                    Markup))])) (toList x)
instance IsPath MRR MarkupPairs
    where type Path MRR MarkupPairs = Path_Map ReportID
                                               (Path_Report MarkupPairs)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Order MarkupPairID
                                                                                                         ((Markup,
                                                                                                           Markup)))])) (toList x)
instance IsPath MRR Markups
    where type Path MRR Markups = Path_Map ReportID
                                           (Path_Report Markups)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Order MarkupID
                                                                                                         Markup)])) (toList x)
instance IsPath MRR MaybeReportIntendedUse
    where type Path MRR MaybeReportIntendedUse = Path_Map ReportID
                                                          (Path_Report MaybeReportIntendedUse)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Maybe ReportIntendedUse)])) (toList x)
instance IsPath MRR Report
    where type Path MRR Report = Path_Map ReportID (Path_Report Report)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Report])) (toList x)
instance IsPath MRR ReportElem
    where type Path MRR ReportElem = Path_Map ReportID
                                              (Path_Report ReportElem)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportElem])) (toList x)
instance IsPath MRR ReportElems
    where type Path MRR ReportElems = Path_Map ReportID
                                               (Path_Report ReportElems)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Order ReportElemID
                                                                                                         ReportElem)])) (toList x)
instance IsPath MRR ReportFlags
    where type Path MRR ReportFlags = Path_Map ReportID
                                               (Path_Report ReportFlags)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportFlags])) (toList x)
instance IsPath MRR ReportStandard
    where type Path MRR ReportStandard = Path_Map ReportID
                                                  (Path_Report ReportStandard)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportStandard])) (toList x)
instance IsPath MRR ReportStatus
    where type Path MRR ReportStatus = Path_Map ReportID
                                                (Path_Report ReportStatus)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportStatus])) (toList x)
instance IsPath MRR ReportValueApproachInfo
    where type Path MRR ReportValueApproachInfo = Path_Map ReportID
                                                           (Path_Report ReportValueApproachInfo)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportValueApproachInfo])) (toList x)
instance IsPath MRR ReportValueTypeInfo
    where type Path MRR ReportValueTypeInfo = Path_Map ReportID
                                                       (Path_Report ReportValueTypeInfo)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportValueTypeInfo])) (toList x)
instance IsPath MRR EUI
    where type Path MRR EUI = Path_Map ReportID (Path_Report EUI)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Either URI
                                                                                                          ImageFile)])) (toList x)
instance IsPath MRR MEUI
    where type Path MRR MEUI = Path_Map ReportID (Path_Report MEUI)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Maybe (Either URI
                                                                                                                 ImageFile))])) (toList x)
instance IsPath MRR MaybeImageFile
    where type Path MRR MaybeImageFile = Path_Map ReportID
                                                  (Path_Report MaybeImageFile)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Maybe ImageFile)])) (toList x)
instance IsPath MRR ReportImage
    where type Path MRR ReportImage = Path_Map ReportID
                                               (Path_Report ReportImage)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportImage])) (toList x)
instance IsPath MRR ReportImages
    where type Path MRR ReportImages = Path_Map ReportID
                                                (Path_Report ReportImages)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Order ReportImageID
                                                                                                         ReportImage)])) (toList x)
instance IsPath MRR ReadOnlyFilePath
    where type Path MRR ReadOnlyFilePath = Path_Map ReportID
                                                    (Path_Report ReadOnlyFilePath)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (ReadOnly ([Char]))])) (toList x)
instance IsPath MRR ReportImageView
    where type Path MRR ReportImageView = Path_Map ReportID
                                                   (Path_Report ReportImageView)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportImageView])) (toList x)
instance IsPath MRR ReportView
    where type Path MRR ReportView = Path_Map ReportID
                                              (Path_Report ReportView)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  ReportView])) (toList x)
instance IsPath MRR SaneSizeImageSize
    where type Path MRR SaneSizeImageSize = Path_Map ReportID
                                                     (Path_Report SaneSizeImageSize)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (SaneSize ImageSize)])) (toList x)
instance IsPath MRR Item
    where type Path MRR Item = Path_Map ReportID (Path_Report Item)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Item])) (toList x)
instance IsPath MRR MIM
    where type Path MRR MIM = Path_Map ReportID (Path_Report MIM)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  (Map ItemFieldName
                                                                                                       Markup)])) (toList x)
instance IsPath MRR MRR
    where type Path MRR MRR = Path_Map ReportID (Path_Report MRR)
          pathsOf _ _ = [idPath]
instance IsPath MRR CIString
    where type Path MRR CIString = Path_Map ReportID
                                            (Path_Report CIString)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  CIString])) (toList x)
instance IsPath MRR URI
    where type Path MRR URI = Path_Map ReportID (Path_Report URI)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  URI])) (toList x)
instance IsPath MRR Text
    where type Path MRR Text = Path_Map ReportID (Path_Report Text)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  Text])) (toList x)
instance IsPath MRR UserId
    where type Path MRR UserId = Path_Map ReportID (Path_Report UserId)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  UserId])) (toList x)
instance IsPath MRR UUID
    where type Path MRR UUID = Path_Map ReportID (Path_Report UUID)
          pathsOf x a = concatMap (\pv -> map ((Path_Look . fst) pv) (pathsOf (snd pv) a :: [Path Report
                                                                                                  UUID])) (toList x)
instance IsPath ReportMap String
    where type Path ReportMap String = Path_ReportMap String
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Int64
    where type Path ReportMap Int64 = Path_ReportMap Int64
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Bool
    where type Path ReportMap Bool = Path_ReportMap Bool
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Double
    where type Path ReportMap Double = Path_ReportMap Double
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Int
    where type Path ReportMap Int = Path_ReportMap Int
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Dimension
    where type Path ReportMap Dimension = Path_ReportMap Dimension
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ImageCrop
    where type Path ReportMap ImageCrop = Path_ReportMap ImageCrop
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ImageSize
    where type Path ReportMap ImageSize = Path_ReportMap ImageSize
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Units
    where type Path ReportMap Units = Path_ReportMap Units
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ImageFile
    where type Path ReportMap ImageFile = Path_ReportMap ImageFile
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Integer
    where type Path ReportMap Integer = Path_ReportMap Integer
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap JSONText
    where type Path ReportMap JSONText = Path_ReportMap JSONText
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Markup
    where type Path ReportMap Markup = Path_ReportMap Markup
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Permissions
    where type Path ReportMap Permissions = Path_ReportMap Permissions
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap UserIds
    where type Path ReportMap UserIds = Path_ReportMap UserIds
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap AbbrevPair
    where type Path ReportMap AbbrevPair = Path_ReportMap AbbrevPair
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap AbbrevPairs
    where type Path ReportMap AbbrevPairs = Path_ReportMap AbbrevPairs
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Author
    where type Path ReportMap Author = Path_ReportMap Author
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Authors
    where type Path ReportMap Authors = Path_ReportMap Authors
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Branding
    where type Path ReportMap Branding = Path_ReportMap Branding
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MarkupPair
    where type Path ReportMap MarkupPair = Path_ReportMap MarkupPair
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MarkupPairs
    where type Path ReportMap MarkupPairs = Path_ReportMap MarkupPairs
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Markups
    where type Path ReportMap Markups = Path_ReportMap Markups
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MaybeReportIntendedUse
    where type Path ReportMap
                    MaybeReportIntendedUse = Path_ReportMap MaybeReportIntendedUse
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Report
    where type Path ReportMap Report = Path_ReportMap Report
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportElem
    where type Path ReportMap ReportElem = Path_ReportMap ReportElem
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportElems
    where type Path ReportMap ReportElems = Path_ReportMap ReportElems
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportFlags
    where type Path ReportMap ReportFlags = Path_ReportMap ReportFlags
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportStandard
    where type Path ReportMap
                    ReportStandard = Path_ReportMap ReportStandard
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportStatus
    where type Path ReportMap
                    ReportStatus = Path_ReportMap ReportStatus
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportValueApproachInfo
    where type Path ReportMap
                    ReportValueApproachInfo = Path_ReportMap ReportValueApproachInfo
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportValueTypeInfo
    where type Path ReportMap
                    ReportValueTypeInfo = Path_ReportMap ReportValueTypeInfo
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap EUI
    where type Path ReportMap EUI = Path_ReportMap EUI
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MEUI
    where type Path ReportMap MEUI = Path_ReportMap MEUI
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MaybeImageFile
    where type Path ReportMap
                    MaybeImageFile = Path_ReportMap MaybeImageFile
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportImage
    where type Path ReportMap ReportImage = Path_ReportMap ReportImage
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportImages
    where type Path ReportMap
                    ReportImages = Path_ReportMap ReportImages
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReadOnlyFilePath
    where type Path ReportMap
                    ReadOnlyFilePath = Path_ReportMap ReadOnlyFilePath
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportImageView
    where type Path ReportMap
                    ReportImageView = Path_ReportMap ReportImageView
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportView
    where type Path ReportMap ReportView = Path_ReportMap ReportView
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap SaneSizeImageSize
    where type Path ReportMap
                    SaneSizeImageSize = Path_ReportMap SaneSizeImageSize
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Item
    where type Path ReportMap Item = Path_ReportMap Item
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MIM
    where type Path ReportMap MIM = Path_ReportMap MIM
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap MRR
    where type Path ReportMap MRR = Path_ReportMap MRR
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap ReportMap
    where type Path ReportMap ReportMap = Path_ReportMap ReportMap
          pathsOf _ _ = [idPath]
instance IsPath ReportMap CIString
    where type Path ReportMap CIString = Path_ReportMap CIString
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap URI
    where type Path ReportMap URI = Path_ReportMap URI
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap Text
    where type Path ReportMap Text = Path_ReportMap Text
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap UserId
    where type Path ReportMap UserId = Path_ReportMap UserId
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath ReportMap UUID
    where type Path ReportMap UUID = Path_ReportMap UUID
          pathsOf (x@(ReportMap {})) a = concat [map Path_ReportMap_unReportMap (pathsOf (unReportMap x) a)]
instance IsPath CIString JSONText
    where type Path CIString JSONText = Path_CIString JSONText
          pathsOf x a = let {p = Path_CIString_View idPath :: Path CIString
                                                                   Text;
                             [x'] = toListOf (toLens p) x :: [Text]}
                         in map Path_CIString_View (pathsOf x' a :: [Path Text JSONText])
instance IsPath CIString CIString
    where type Path CIString CIString = Path_CIString CIString
          pathsOf _ _ = [idPath]
instance IsPath CIString Text
    where type Path CIString Text = Path_CIString Text
          pathsOf x a = let {p = Path_CIString_View idPath :: Path CIString
                                                                   Text;
                             [x'] = toListOf (toLens p) x :: [Text]}
                         in map Path_CIString_View (pathsOf x' a :: [Path Text Text])
instance IsPath URI URI
    where type Path URI URI = Path_URI URI
          pathsOf _ _ = [idPath]
instance IsPath Text JSONText
    where type Path Text JSONText = Path_Text JSONText
          pathsOf x a = let {p = Path_Text_View idPath :: Path Text JSONText;
                             [x'] = toListOf (toLens p) x :: [JSONText]}
                         in map Path_Text_View (pathsOf x' a :: [Path JSONText JSONText])
instance IsPath Text Text
    where type Path Text Text = Path_Text Text
          pathsOf _ _ = [idPath]
instance IsPath UserId UserId
    where type Path UserId UserId = Path_UserId UserId
          pathsOf _ _ = [idPath]
instance IsPath UUID UUID
    where type Path UUID UUID = Path_UUID UUID
          pathsOf _ _ = [idPath]
instance IsPathNode (Either URI ImageFile)
    where data Peek (Either URI ImageFile)
              = Peek_EUI_ImageFile (Path_EUI ImageFile) ImageFile
              | Peek_EUI_EUI (Path_EUI (Either URI ImageFile))
                             (Either URI ImageFile)
              | Peek_EUI_URI (Path_EUI URI) URI
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Left _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy URI)) :: [Path_EUI URI]
                    in map (\path -> case path of
                                         p@(Path_Left _) -> let [y] = toListOf (toLens p) x :: [URI]
                                                             in Node (Peek_EUI_URI p y) (forestMap (\v -> case v of
                                                                                                              Peek_URI_URI q
                                                                                                                           x -> Peek_EUI_URI ((Path_Left :: Path_URI URI ->
                                                                                                                                                            Path_EUI URI) q) x) (peek y :: Forest (Peek URI)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
          peek x = let paths = filter (\p -> case p of
                                                 Path_Right _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ImageFile)) :: [Path_EUI ImageFile]
                    in map (\path -> case path of
                                         p@(Path_Right _) -> let [y] = toListOf (toLens p) x :: [ImageFile]
                                                              in Node (Peek_EUI_ImageFile p y) (forestMap (\v -> case v of
                                                                                                                     Peek_ImageFile_ImageFile q
                                                                                                                                              x -> Peek_EUI_ImageFile ((Path_Right :: Path_ImageFile ImageFile ->
                                                                                                                                                                                      Path_EUI ImageFile) q) x) (peek y :: Forest (Peek ImageFile)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode (Map ItemFieldName Markup)
    where data Peek (Map ItemFieldName Markup)
              = Peek_MIM_JSONText (Path_MIM JSONText) JSONText
              | Peek_MIM_Markup (Path_MIM Markup) Markup
              | Peek_MIM_MIM (Path_MIM (Map ItemFieldName Markup))
                             (Map ItemFieldName Markup)
              | Peek_MIM_Text (Path_MIM Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy Markup) :: [Path_MIM Markup]
                    in map (\path -> case path of
                                         p@(Path_Look k
                                                      _) -> let [y] = toListOf (toLens p) x :: [Markup]
                                                             in Node (Peek_MIM_Markup p y) (forestMap (\v -> case v of
                                                                                                                 Peek_Markup_JSONText q
                                                                                                                                      x -> Peek_MIM_JSONText ((Path_Look k :: Path_Markup JSONText ->
                                                                                                                                                                              Path_MIM JSONText) q) x
                                                                                                                 Peek_Markup_Markup q
                                                                                                                                    x -> Peek_MIM_Markup ((Path_Look k :: Path_Markup Markup ->
                                                                                                                                                                          Path_MIM Markup) q) x
                                                                                                                 Peek_Markup_Text q
                                                                                                                                  x -> Peek_MIM_Text ((Path_Look k :: Path_Markup Text ->
                                                                                                                                                                      Path_MIM Text) q) x) (peek y :: Forest (Peek Markup)))
                                         _ -> error ("doPeekNodesOfMap: " ++ show path)) paths :: Forest (Peek (Map ItemFieldName
                                                                                                                    Markup))
instance IsPathNode (Map ReportID Report)
    where data Peek (Map ReportID Report)
              = Peek_MRR_String (Path_MRR ([Char])) ([Char])
              | Peek_MRR_Int64 (Path_MRR Int64) Int64
              | Peek_MRR_Int (Path_MRR Int) Int
              | Peek_MRR_Bool (Path_MRR Bool) Bool
              | Peek_MRR_Double (Path_MRR Double) Double
              | Peek_MRR_Dimension (Path_MRR Dimension) Dimension
              | Peek_MRR_ImageCrop (Path_MRR ImageCrop) ImageCrop
              | Peek_MRR_ImageSize (Path_MRR ImageSize) ImageSize
              | Peek_MRR_Units (Path_MRR Units) Units
              | Peek_MRR_ImageFile (Path_MRR ImageFile) ImageFile
              | Peek_MRR_Integer (Path_MRR Integer) Integer
              | Peek_MRR_JSONText (Path_MRR JSONText) JSONText
              | Peek_MRR_Markup (Path_MRR Markup) Markup
              | Peek_MRR_Permissions (Path_MRR Permissions) Permissions
              | Peek_MRR_UserIds (Path_MRR ([UserId])) ([UserId])
              | Peek_MRR_AbbrevPair (Path_MRR ((CIString, Markup)))
                                    ((CIString, Markup))
              | Peek_MRR_AbbrevPairs (Path_MRR (Order AbbrevPairID
                                                      ((CIString, Markup))))
                                     (Order AbbrevPairID ((CIString, Markup)))
              | Peek_MRR_Author (Path_MRR Author) Author
              | Peek_MRR_Authors (Path_MRR (Order AuthorID Author))
                                 (Order AuthorID Author)
              | Peek_MRR_Branding (Path_MRR Branding) Branding
              | Peek_MRR_MarkupPair (Path_MRR ((Markup, Markup)))
                                    ((Markup, Markup))
              | Peek_MRR_MarkupPairs (Path_MRR (Order MarkupPairID
                                                      ((Markup, Markup))))
                                     (Order MarkupPairID ((Markup, Markup)))
              | Peek_MRR_Markups (Path_MRR (Order MarkupID Markup))
                                 (Order MarkupID Markup)
              | Peek_MRR_MaybeReportIntendedUse (Path_MRR (Maybe ReportIntendedUse))
                                                (Maybe ReportIntendedUse)
              | Peek_MRR_Report (Path_MRR Report) Report
              | Peek_MRR_ReportElem (Path_MRR ReportElem) ReportElem
              | Peek_MRR_ReportElems (Path_MRR (Order ReportElemID ReportElem))
                                     (Order ReportElemID ReportElem)
              | Peek_MRR_ReportFlags (Path_MRR ReportFlags) ReportFlags
              | Peek_MRR_ReportStandard (Path_MRR ReportStandard) ReportStandard
              | Peek_MRR_ReportStatus (Path_MRR ReportStatus) ReportStatus
              | Peek_MRR_ReportValueApproachInfo (Path_MRR ReportValueApproachInfo)
                                                 ReportValueApproachInfo
              | Peek_MRR_ReportValueTypeInfo (Path_MRR ReportValueTypeInfo)
                                             ReportValueTypeInfo
              | Peek_MRR_EUI (Path_MRR (Either URI ImageFile))
                             (Either URI ImageFile)
              | Peek_MRR_MEUI (Path_MRR (Maybe (Either URI ImageFile)))
                              (Maybe (Either URI ImageFile))
              | Peek_MRR_MaybeImageFile (Path_MRR (Maybe ImageFile))
                                        (Maybe ImageFile)
              | Peek_MRR_ReportImage (Path_MRR ReportImage) ReportImage
              | Peek_MRR_ReportImages (Path_MRR (Order ReportImageID
                                                       ReportImage))
                                      (Order ReportImageID ReportImage)
              | Peek_MRR_ReadOnlyFilePath (Path_MRR (ReadOnly ([Char])))
                                          (ReadOnly ([Char]))
              | Peek_MRR_ReportImageView (Path_MRR ReportImageView)
                                         ReportImageView
              | Peek_MRR_ReportView (Path_MRR ReportView) ReportView
              | Peek_MRR_SaneSizeImageSize (Path_MRR (SaneSize ImageSize))
                                           (SaneSize ImageSize)
              | Peek_MRR_Item (Path_MRR Item) Item
              | Peek_MRR_MIM (Path_MRR (Map ItemFieldName Markup))
                             (Map ItemFieldName Markup)
              | Peek_MRR_MRR (Path_MRR (Map ReportID Report))
                             (Map ReportID Report)
              | Peek_MRR_CIString (Path_MRR CIString) CIString
              | Peek_MRR_URI (Path_MRR URI) URI
              | Peek_MRR_Text (Path_MRR Text) Text
              | Peek_MRR_UserId (Path_MRR UserId) UserId
              | Peek_MRR_UUID (Path_MRR UUID) UUID
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy Report) :: [Path_MRR Report]
                    in map (\path -> case path of
                                         p@(Path_Look k
                                                      _) -> let [y] = toListOf (toLens p) x :: [Report]
                                                             in Node (Peek_MRR_Report p y) (forestMap (\v -> case v of
                                                                                                                 Peek_Report_String q
                                                                                                                                    x -> Peek_MRR_String ((Path_Look k :: Path_Report ([Char]) ->
                                                                                                                                                                          Path_MRR ([Char])) q) x
                                                                                                                 Peek_Report_Int64 q
                                                                                                                                   x -> Peek_MRR_Int64 ((Path_Look k :: Path_Report Int64 ->
                                                                                                                                                                        Path_MRR Int64) q) x
                                                                                                                 Peek_Report_Int q
                                                                                                                                 x -> Peek_MRR_Int ((Path_Look k :: Path_Report Int ->
                                                                                                                                                                    Path_MRR Int) q) x
                                                                                                                 Peek_Report_Bool q
                                                                                                                                  x -> Peek_MRR_Bool ((Path_Look k :: Path_Report Bool ->
                                                                                                                                                                      Path_MRR Bool) q) x
                                                                                                                 Peek_Report_Double q
                                                                                                                                    x -> Peek_MRR_Double ((Path_Look k :: Path_Report Double ->
                                                                                                                                                                          Path_MRR Double) q) x
                                                                                                                 Peek_Report_Dimension q
                                                                                                                                       x -> Peek_MRR_Dimension ((Path_Look k :: Path_Report Dimension ->
                                                                                                                                                                                Path_MRR Dimension) q) x
                                                                                                                 Peek_Report_ImageCrop q
                                                                                                                                       x -> Peek_MRR_ImageCrop ((Path_Look k :: Path_Report ImageCrop ->
                                                                                                                                                                                Path_MRR ImageCrop) q) x
                                                                                                                 Peek_Report_ImageSize q
                                                                                                                                       x -> Peek_MRR_ImageSize ((Path_Look k :: Path_Report ImageSize ->
                                                                                                                                                                                Path_MRR ImageSize) q) x
                                                                                                                 Peek_Report_Units q
                                                                                                                                   x -> Peek_MRR_Units ((Path_Look k :: Path_Report Units ->
                                                                                                                                                                        Path_MRR Units) q) x
                                                                                                                 Peek_Report_ImageFile q
                                                                                                                                       x -> Peek_MRR_ImageFile ((Path_Look k :: Path_Report ImageFile ->
                                                                                                                                                                                Path_MRR ImageFile) q) x
                                                                                                                 Peek_Report_Integer q
                                                                                                                                     x -> Peek_MRR_Integer ((Path_Look k :: Path_Report Integer ->
                                                                                                                                                                            Path_MRR Integer) q) x
                                                                                                                 Peek_Report_JSONText q
                                                                                                                                      x -> Peek_MRR_JSONText ((Path_Look k :: Path_Report JSONText ->
                                                                                                                                                                              Path_MRR JSONText) q) x
                                                                                                                 Peek_Report_Markup q
                                                                                                                                    x -> Peek_MRR_Markup ((Path_Look k :: Path_Report Markup ->
                                                                                                                                                                          Path_MRR Markup) q) x
                                                                                                                 Peek_Report_Permissions q
                                                                                                                                         x -> Peek_MRR_Permissions ((Path_Look k :: Path_Report Permissions ->
                                                                                                                                                                                    Path_MRR Permissions) q) x
                                                                                                                 Peek_Report_UserIds q
                                                                                                                                     x -> Peek_MRR_UserIds ((Path_Look k :: Path_Report ([UserId]) ->
                                                                                                                                                                            Path_MRR ([UserId])) q) x
                                                                                                                 Peek_Report_AbbrevPair q
                                                                                                                                        x -> Peek_MRR_AbbrevPair ((Path_Look k :: Path_Report ((CIString,
                                                                                                                                                                                                Markup)) ->
                                                                                                                                                                                  Path_MRR ((CIString,
                                                                                                                                                                                             Markup))) q) x
                                                                                                                 Peek_Report_AbbrevPairs q
                                                                                                                                         x -> Peek_MRR_AbbrevPairs ((Path_Look k :: Path_Report (Order AbbrevPairID
                                                                                                                                                                                                       ((CIString,
                                                                                                                                                                                                         Markup))) ->
                                                                                                                                                                                    Path_MRR (Order AbbrevPairID
                                                                                                                                                                                                    ((CIString,
                                                                                                                                                                                                      Markup)))) q) x
                                                                                                                 Peek_Report_Author q
                                                                                                                                    x -> Peek_MRR_Author ((Path_Look k :: Path_Report Author ->
                                                                                                                                                                          Path_MRR Author) q) x
                                                                                                                 Peek_Report_Authors q
                                                                                                                                     x -> Peek_MRR_Authors ((Path_Look k :: Path_Report (Order AuthorID
                                                                                                                                                                                               Author) ->
                                                                                                                                                                            Path_MRR (Order AuthorID
                                                                                                                                                                                            Author)) q) x
                                                                                                                 Peek_Report_Branding q
                                                                                                                                      x -> Peek_MRR_Branding ((Path_Look k :: Path_Report Branding ->
                                                                                                                                                                              Path_MRR Branding) q) x
                                                                                                                 Peek_Report_MarkupPair q
                                                                                                                                        x -> Peek_MRR_MarkupPair ((Path_Look k :: Path_Report ((Markup,
                                                                                                                                                                                                Markup)) ->
                                                                                                                                                                                  Path_MRR ((Markup,
                                                                                                                                                                                             Markup))) q) x
                                                                                                                 Peek_Report_MarkupPairs q
                                                                                                                                         x -> Peek_MRR_MarkupPairs ((Path_Look k :: Path_Report (Order MarkupPairID
                                                                                                                                                                                                       ((Markup,
                                                                                                                                                                                                         Markup))) ->
                                                                                                                                                                                    Path_MRR (Order MarkupPairID
                                                                                                                                                                                                    ((Markup,
                                                                                                                                                                                                      Markup)))) q) x
                                                                                                                 Peek_Report_Markups q
                                                                                                                                     x -> Peek_MRR_Markups ((Path_Look k :: Path_Report (Order MarkupID
                                                                                                                                                                                               Markup) ->
                                                                                                                                                                            Path_MRR (Order MarkupID
                                                                                                                                                                                            Markup)) q) x
                                                                                                                 Peek_Report_MaybeReportIntendedUse q
                                                                                                                                                    x -> Peek_MRR_MaybeReportIntendedUse ((Path_Look k :: Path_Report (Maybe ReportIntendedUse) ->
                                                                                                                                                                                                          Path_MRR (Maybe ReportIntendedUse)) q) x
                                                                                                                 Peek_Report_Report q
                                                                                                                                    x -> Peek_MRR_Report ((Path_Look k :: Path_Report Report ->
                                                                                                                                                                          Path_MRR Report) q) x
                                                                                                                 Peek_Report_ReportElem q
                                                                                                                                        x -> Peek_MRR_ReportElem ((Path_Look k :: Path_Report ReportElem ->
                                                                                                                                                                                  Path_MRR ReportElem) q) x
                                                                                                                 Peek_Report_ReportElems q
                                                                                                                                         x -> Peek_MRR_ReportElems ((Path_Look k :: Path_Report (Order ReportElemID
                                                                                                                                                                                                       ReportElem) ->
                                                                                                                                                                                    Path_MRR (Order ReportElemID
                                                                                                                                                                                                    ReportElem)) q) x
                                                                                                                 Peek_Report_ReportFlags q
                                                                                                                                         x -> Peek_MRR_ReportFlags ((Path_Look k :: Path_Report ReportFlags ->
                                                                                                                                                                                    Path_MRR ReportFlags) q) x
                                                                                                                 Peek_Report_ReportStandard q
                                                                                                                                            x -> Peek_MRR_ReportStandard ((Path_Look k :: Path_Report ReportStandard ->
                                                                                                                                                                                          Path_MRR ReportStandard) q) x
                                                                                                                 Peek_Report_ReportStatus q
                                                                                                                                          x -> Peek_MRR_ReportStatus ((Path_Look k :: Path_Report ReportStatus ->
                                                                                                                                                                                      Path_MRR ReportStatus) q) x
                                                                                                                 Peek_Report_ReportValueApproachInfo q
                                                                                                                                                     x -> Peek_MRR_ReportValueApproachInfo ((Path_Look k :: Path_Report ReportValueApproachInfo ->
                                                                                                                                                                                                            Path_MRR ReportValueApproachInfo) q) x
                                                                                                                 Peek_Report_ReportValueTypeInfo q
                                                                                                                                                 x -> Peek_MRR_ReportValueTypeInfo ((Path_Look k :: Path_Report ReportValueTypeInfo ->
                                                                                                                                                                                                    Path_MRR ReportValueTypeInfo) q) x
                                                                                                                 Peek_Report_EUI q
                                                                                                                                 x -> Peek_MRR_EUI ((Path_Look k :: Path_Report (Either URI
                                                                                                                                                                                        ImageFile) ->
                                                                                                                                                                    Path_MRR (Either URI
                                                                                                                                                                                     ImageFile)) q) x
                                                                                                                 Peek_Report_MEUI q
                                                                                                                                  x -> Peek_MRR_MEUI ((Path_Look k :: Path_Report (Maybe (Either URI
                                                                                                                                                                                                 ImageFile)) ->
                                                                                                                                                                      Path_MRR (Maybe (Either URI
                                                                                                                                                                                              ImageFile))) q) x
                                                                                                                 Peek_Report_MaybeImageFile q
                                                                                                                                            x -> Peek_MRR_MaybeImageFile ((Path_Look k :: Path_Report (Maybe ImageFile) ->
                                                                                                                                                                                          Path_MRR (Maybe ImageFile)) q) x
                                                                                                                 Peek_Report_ReportImage q
                                                                                                                                         x -> Peek_MRR_ReportImage ((Path_Look k :: Path_Report ReportImage ->
                                                                                                                                                                                    Path_MRR ReportImage) q) x
                                                                                                                 Peek_Report_ReportImages q
                                                                                                                                          x -> Peek_MRR_ReportImages ((Path_Look k :: Path_Report (Order ReportImageID
                                                                                                                                                                                                         ReportImage) ->
                                                                                                                                                                                      Path_MRR (Order ReportImageID
                                                                                                                                                                                                      ReportImage)) q) x
                                                                                                                 Peek_Report_ReadOnlyFilePath q
                                                                                                                                              x -> Peek_MRR_ReadOnlyFilePath ((Path_Look k :: Path_Report (ReadOnly ([Char])) ->
                                                                                                                                                                                              Path_MRR (ReadOnly ([Char]))) q) x
                                                                                                                 Peek_Report_ReportImageView q
                                                                                                                                             x -> Peek_MRR_ReportImageView ((Path_Look k :: Path_Report ReportImageView ->
                                                                                                                                                                                            Path_MRR ReportImageView) q) x
                                                                                                                 Peek_Report_ReportView q
                                                                                                                                        x -> Peek_MRR_ReportView ((Path_Look k :: Path_Report ReportView ->
                                                                                                                                                                                  Path_MRR ReportView) q) x
                                                                                                                 Peek_Report_SaneSizeImageSize q
                                                                                                                                               x -> Peek_MRR_SaneSizeImageSize ((Path_Look k :: Path_Report (SaneSize ImageSize) ->
                                                                                                                                                                                                Path_MRR (SaneSize ImageSize)) q) x
                                                                                                                 Peek_Report_Item q
                                                                                                                                  x -> Peek_MRR_Item ((Path_Look k :: Path_Report Item ->
                                                                                                                                                                      Path_MRR Item) q) x
                                                                                                                 Peek_Report_MIM q
                                                                                                                                 x -> Peek_MRR_MIM ((Path_Look k :: Path_Report (Map ItemFieldName
                                                                                                                                                                                     Markup) ->
                                                                                                                                                                    Path_MRR (Map ItemFieldName
                                                                                                                                                                                  Markup)) q) x
                                                                                                                 Peek_Report_CIString q
                                                                                                                                      x -> Peek_MRR_CIString ((Path_Look k :: Path_Report CIString ->
                                                                                                                                                                              Path_MRR CIString) q) x
                                                                                                                 Peek_Report_URI q
                                                                                                                                 x -> Peek_MRR_URI ((Path_Look k :: Path_Report URI ->
                                                                                                                                                                    Path_MRR URI) q) x
                                                                                                                 Peek_Report_Text q
                                                                                                                                  x -> Peek_MRR_Text ((Path_Look k :: Path_Report Text ->
                                                                                                                                                                      Path_MRR Text) q) x
                                                                                                                 Peek_Report_UserId q
                                                                                                                                    x -> Peek_MRR_UserId ((Path_Look k :: Path_Report UserId ->
                                                                                                                                                                          Path_MRR UserId) q) x
                                                                                                                 Peek_Report_UUID q
                                                                                                                                  x -> Peek_MRR_UUID ((Path_Look k :: Path_Report UUID ->
                                                                                                                                                                      Path_MRR UUID) q) x) (peek y :: Forest (Peek Report)))
                                         _ -> error ("doPeekNodesOfMap: " ++ show path)) paths :: Forest (Peek (Map ReportID
                                                                                                                    Report))
instance IsPathNode (Order AbbrevPairID ((CIString, Markup)))
    where data Peek (Order AbbrevPairID ((CIString, Markup)))
              = Peek_AbbrevPairs_JSONText (Path_AbbrevPairs JSONText) JSONText
              | Peek_AbbrevPairs_Markup (Path_AbbrevPairs Markup) Markup
              | Peek_AbbrevPairs_AbbrevPair (Path_AbbrevPairs ((CIString,
                                                                Markup)))
                                            ((CIString, Markup))
              | Peek_AbbrevPairs_AbbrevPairs (Path_AbbrevPairs (Order AbbrevPairID
                                                                      ((CIString, Markup))))
                                             (Order AbbrevPairID ((CIString, Markup)))
              | Peek_AbbrevPairs_CIString (Path_AbbrevPairs CIString) CIString
              | Peek_AbbrevPairs_Text (Path_AbbrevPairs Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy ((CIString,
                                                               Markup))) :: [Path_AbbrevPairs ((CIString,
                                                                                                Markup))]
                    in map (\path -> case path of
                                         p@(Path_At k
                                                    _) -> let [y] = toListOf (toLens p) x :: [(CIString,
                                                                                               Markup)]
                                                           in Node (Peek_AbbrevPairs_AbbrevPair p y) (forestMap (\v -> case v of
                                                                                                                           Peek_AbbrevPair_JSONText q
                                                                                                                                                    x -> Peek_AbbrevPairs_JSONText ((Path_At k :: Path_AbbrevPair JSONText ->
                                                                                                                                                                                                  Path_AbbrevPairs JSONText) q) x
                                                                                                                           Peek_AbbrevPair_Markup q
                                                                                                                                                  x -> Peek_AbbrevPairs_Markup ((Path_At k :: Path_AbbrevPair Markup ->
                                                                                                                                                                                              Path_AbbrevPairs Markup) q) x
                                                                                                                           Peek_AbbrevPair_AbbrevPair q
                                                                                                                                                      x -> Peek_AbbrevPairs_AbbrevPair ((Path_At k :: Path_AbbrevPair ((CIString,
                                                                                                                                                                                                                        Markup)) ->
                                                                                                                                                                                                      Path_AbbrevPairs ((CIString,
                                                                                                                                                                                                                         Markup))) q) x
                                                                                                                           Peek_AbbrevPair_CIString q
                                                                                                                                                    x -> Peek_AbbrevPairs_CIString ((Path_At k :: Path_AbbrevPair CIString ->
                                                                                                                                                                                                  Path_AbbrevPairs CIString) q) x
                                                                                                                           Peek_AbbrevPair_Text q
                                                                                                                                                x -> Peek_AbbrevPairs_Text ((Path_At k :: Path_AbbrevPair Text ->
                                                                                                                                                                                          Path_AbbrevPairs Text) q) x) (peek y :: Forest (Peek ((CIString,
                                                                                                                                                                                                                                                 Markup)))))
                                         _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths :: Forest (Peek (Order AbbrevPairID
                                                                                                                        ((CIString,
                                                                                                                          Markup))))
instance IsPathNode (Order AuthorID Author)
    where data Peek (Order AuthorID Author)
              = Peek_Authors_JSONText (Path_Authors JSONText) JSONText
              | Peek_Authors_Markup (Path_Authors Markup) Markup
              | Peek_Authors_Author (Path_Authors Author) Author
              | Peek_Authors_Authors (Path_Authors (Order AuthorID Author))
                                     (Order AuthorID Author)
              | Peek_Authors_Text (Path_Authors Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy Author) :: [Path_Authors Author]
                    in map (\path -> case path of
                                         p@(Path_At k
                                                    _) -> let [y] = toListOf (toLens p) x :: [Author]
                                                           in Node (Peek_Authors_Author p y) (forestMap (\v -> case v of
                                                                                                                   Peek_Author_JSONText q
                                                                                                                                        x -> Peek_Authors_JSONText ((Path_At k :: Path_Author JSONText ->
                                                                                                                                                                                  Path_Authors JSONText) q) x
                                                                                                                   Peek_Author_Markup q
                                                                                                                                      x -> Peek_Authors_Markup ((Path_At k :: Path_Author Markup ->
                                                                                                                                                                              Path_Authors Markup) q) x
                                                                                                                   Peek_Author_Author q
                                                                                                                                      x -> Peek_Authors_Author ((Path_At k :: Path_Author Author ->
                                                                                                                                                                              Path_Authors Author) q) x
                                                                                                                   Peek_Author_Text q
                                                                                                                                    x -> Peek_Authors_Text ((Path_At k :: Path_Author Text ->
                                                                                                                                                                          Path_Authors Text) q) x) (peek y :: Forest (Peek Author)))
                                         _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths :: Forest (Peek (Order AuthorID
                                                                                                                        Author))
instance IsPathNode (Order MarkupID Markup)
    where data Peek (Order MarkupID Markup)
              = Peek_Markups_JSONText (Path_Markups JSONText) JSONText
              | Peek_Markups_Markup (Path_Markups Markup) Markup
              | Peek_Markups_Markups (Path_Markups (Order MarkupID Markup))
                                     (Order MarkupID Markup)
              | Peek_Markups_Text (Path_Markups Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy Markup) :: [Path_Markups Markup]
                    in map (\path -> case path of
                                         p@(Path_At k
                                                    _) -> let [y] = toListOf (toLens p) x :: [Markup]
                                                           in Node (Peek_Markups_Markup p y) (forestMap (\v -> case v of
                                                                                                                   Peek_Markup_JSONText q
                                                                                                                                        x -> Peek_Markups_JSONText ((Path_At k :: Path_Markup JSONText ->
                                                                                                                                                                                  Path_Markups JSONText) q) x
                                                                                                                   Peek_Markup_Markup q
                                                                                                                                      x -> Peek_Markups_Markup ((Path_At k :: Path_Markup Markup ->
                                                                                                                                                                              Path_Markups Markup) q) x
                                                                                                                   Peek_Markup_Text q
                                                                                                                                    x -> Peek_Markups_Text ((Path_At k :: Path_Markup Text ->
                                                                                                                                                                          Path_Markups Text) q) x) (peek y :: Forest (Peek Markup)))
                                         _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths :: Forest (Peek (Order MarkupID
                                                                                                                        Markup))
instance IsPathNode (Order MarkupPairID ((Markup, Markup)))
    where data Peek (Order MarkupPairID ((Markup, Markup)))
              = Peek_MarkupPairs_JSONText (Path_MarkupPairs JSONText) JSONText
              | Peek_MarkupPairs_Markup (Path_MarkupPairs Markup) Markup
              | Peek_MarkupPairs_MarkupPair (Path_MarkupPairs ((Markup, Markup)))
                                            ((Markup, Markup))
              | Peek_MarkupPairs_MarkupPairs (Path_MarkupPairs (Order MarkupPairID
                                                                      ((Markup, Markup))))
                                             (Order MarkupPairID ((Markup, Markup)))
              | Peek_MarkupPairs_Text (Path_MarkupPairs Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy ((Markup,
                                                               Markup))) :: [Path_MarkupPairs ((Markup,
                                                                                                Markup))]
                    in map (\path -> case path of
                                         p@(Path_At k
                                                    _) -> let [y] = toListOf (toLens p) x :: [(Markup,
                                                                                               Markup)]
                                                           in Node (Peek_MarkupPairs_MarkupPair p y) (forestMap (\v -> case v of
                                                                                                                           Peek_MarkupPair_JSONText q
                                                                                                                                                    x -> Peek_MarkupPairs_JSONText ((Path_At k :: Path_MarkupPair JSONText ->
                                                                                                                                                                                                  Path_MarkupPairs JSONText) q) x
                                                                                                                           Peek_MarkupPair_Markup q
                                                                                                                                                  x -> Peek_MarkupPairs_Markup ((Path_At k :: Path_MarkupPair Markup ->
                                                                                                                                                                                              Path_MarkupPairs Markup) q) x
                                                                                                                           Peek_MarkupPair_MarkupPair q
                                                                                                                                                      x -> Peek_MarkupPairs_MarkupPair ((Path_At k :: Path_MarkupPair ((Markup,
                                                                                                                                                                                                                        Markup)) ->
                                                                                                                                                                                                      Path_MarkupPairs ((Markup,
                                                                                                                                                                                                                         Markup))) q) x
                                                                                                                           Peek_MarkupPair_Text q
                                                                                                                                                x -> Peek_MarkupPairs_Text ((Path_At k :: Path_MarkupPair Text ->
                                                                                                                                                                                          Path_MarkupPairs Text) q) x) (peek y :: Forest (Peek ((Markup,
                                                                                                                                                                                                                                                 Markup)))))
                                         _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths :: Forest (Peek (Order MarkupPairID
                                                                                                                        ((Markup,
                                                                                                                          Markup))))
instance IsPathNode (Order ReportElemID ReportElem)
    where data Peek (Order ReportElemID ReportElem)
              = Peek_ReportElems_String (Path_ReportElems ([Char])) ([Char])
              | Peek_ReportElems_Bool (Path_ReportElems Bool) Bool
              | Peek_ReportElems_Double (Path_ReportElems Double) Double
              | Peek_ReportElems_Dimension (Path_ReportElems Dimension) Dimension
              | Peek_ReportElems_ImageCrop (Path_ReportElems ImageCrop) ImageCrop
              | Peek_ReportElems_ImageSize (Path_ReportElems ImageSize) ImageSize
              | Peek_ReportElems_Units (Path_ReportElems Units) Units
              | Peek_ReportElems_ImageFile (Path_ReportElems ImageFile) ImageFile
              | Peek_ReportElems_JSONText (Path_ReportElems JSONText) JSONText
              | Peek_ReportElems_Markup (Path_ReportElems Markup) Markup
              | Peek_ReportElems_ReportElem (Path_ReportElems ReportElem)
                                            ReportElem
              | Peek_ReportElems_ReportElems (Path_ReportElems (Order ReportElemID
                                                                      ReportElem))
                                             (Order ReportElemID ReportElem)
              | Peek_ReportElems_EUI (Path_ReportElems (Either URI ImageFile))
                                     (Either URI ImageFile)
              | Peek_ReportElems_MEUI (Path_ReportElems (Maybe (Either URI
                                                                       ImageFile)))
                                      (Maybe (Either URI ImageFile))
              | Peek_ReportElems_MaybeImageFile (Path_ReportElems (Maybe ImageFile))
                                                (Maybe ImageFile)
              | Peek_ReportElems_ReportImage (Path_ReportElems ReportImage)
                                             ReportImage
              | Peek_ReportElems_ReportImages (Path_ReportElems (Order ReportImageID
                                                                       ReportImage))
                                              (Order ReportImageID ReportImage)
              | Peek_ReportElems_ReportImageView (Path_ReportElems ReportImageView)
                                                 ReportImageView
              | Peek_ReportElems_SaneSizeImageSize (Path_ReportElems (SaneSize ImageSize))
                                                   (SaneSize ImageSize)
              | Peek_ReportElems_Item (Path_ReportElems Item) Item
              | Peek_ReportElems_MIM (Path_ReportElems (Map ItemFieldName
                                                            Markup))
                                     (Map ItemFieldName Markup)
              | Peek_ReportElems_URI (Path_ReportElems URI) URI
              | Peek_ReportElems_Text (Path_ReportElems Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy ReportElem) :: [Path_ReportElems ReportElem]
                    in map (\path -> case path of
                                         p@(Path_At k
                                                    _) -> let [y] = toListOf (toLens p) x :: [ReportElem]
                                                           in Node (Peek_ReportElems_ReportElem p y) (forestMap (\v -> case v of
                                                                                                                           Peek_ReportElem_String q
                                                                                                                                                  x -> Peek_ReportElems_String ((Path_At k :: Path_ReportElem ([Char]) ->
                                                                                                                                                                                              Path_ReportElems ([Char])) q) x
                                                                                                                           Peek_ReportElem_Bool q
                                                                                                                                                x -> Peek_ReportElems_Bool ((Path_At k :: Path_ReportElem Bool ->
                                                                                                                                                                                          Path_ReportElems Bool) q) x
                                                                                                                           Peek_ReportElem_Double q
                                                                                                                                                  x -> Peek_ReportElems_Double ((Path_At k :: Path_ReportElem Double ->
                                                                                                                                                                                              Path_ReportElems Double) q) x
                                                                                                                           Peek_ReportElem_Dimension q
                                                                                                                                                     x -> Peek_ReportElems_Dimension ((Path_At k :: Path_ReportElem Dimension ->
                                                                                                                                                                                                    Path_ReportElems Dimension) q) x
                                                                                                                           Peek_ReportElem_ImageCrop q
                                                                                                                                                     x -> Peek_ReportElems_ImageCrop ((Path_At k :: Path_ReportElem ImageCrop ->
                                                                                                                                                                                                    Path_ReportElems ImageCrop) q) x
                                                                                                                           Peek_ReportElem_ImageSize q
                                                                                                                                                     x -> Peek_ReportElems_ImageSize ((Path_At k :: Path_ReportElem ImageSize ->
                                                                                                                                                                                                    Path_ReportElems ImageSize) q) x
                                                                                                                           Peek_ReportElem_Units q
                                                                                                                                                 x -> Peek_ReportElems_Units ((Path_At k :: Path_ReportElem Units ->
                                                                                                                                                                                            Path_ReportElems Units) q) x
                                                                                                                           Peek_ReportElem_ImageFile q
                                                                                                                                                     x -> Peek_ReportElems_ImageFile ((Path_At k :: Path_ReportElem ImageFile ->
                                                                                                                                                                                                    Path_ReportElems ImageFile) q) x
                                                                                                                           Peek_ReportElem_JSONText q
                                                                                                                                                    x -> Peek_ReportElems_JSONText ((Path_At k :: Path_ReportElem JSONText ->
                                                                                                                                                                                                  Path_ReportElems JSONText) q) x
                                                                                                                           Peek_ReportElem_Markup q
                                                                                                                                                  x -> Peek_ReportElems_Markup ((Path_At k :: Path_ReportElem Markup ->
                                                                                                                                                                                              Path_ReportElems Markup) q) x
                                                                                                                           Peek_ReportElem_ReportElem q
                                                                                                                                                      x -> Peek_ReportElems_ReportElem ((Path_At k :: Path_ReportElem ReportElem ->
                                                                                                                                                                                                      Path_ReportElems ReportElem) q) x
                                                                                                                           Peek_ReportElem_EUI q
                                                                                                                                               x -> Peek_ReportElems_EUI ((Path_At k :: Path_ReportElem (Either URI
                                                                                                                                                                                                                ImageFile) ->
                                                                                                                                                                                        Path_ReportElems (Either URI
                                                                                                                                                                                                                 ImageFile)) q) x
                                                                                                                           Peek_ReportElem_MEUI q
                                                                                                                                                x -> Peek_ReportElems_MEUI ((Path_At k :: Path_ReportElem (Maybe (Either URI
                                                                                                                                                                                                                         ImageFile)) ->
                                                                                                                                                                                          Path_ReportElems (Maybe (Either URI
                                                                                                                                                                                                                          ImageFile))) q) x
                                                                                                                           Peek_ReportElem_MaybeImageFile q
                                                                                                                                                          x -> Peek_ReportElems_MaybeImageFile ((Path_At k :: Path_ReportElem (Maybe ImageFile) ->
                                                                                                                                                                                                              Path_ReportElems (Maybe ImageFile)) q) x
                                                                                                                           Peek_ReportElem_ReportImage q
                                                                                                                                                       x -> Peek_ReportElems_ReportImage ((Path_At k :: Path_ReportElem ReportImage ->
                                                                                                                                                                                                        Path_ReportElems ReportImage) q) x
                                                                                                                           Peek_ReportElem_ReportImages q
                                                                                                                                                        x -> Peek_ReportElems_ReportImages ((Path_At k :: Path_ReportElem (Order ReportImageID
                                                                                                                                                                                                                                 ReportImage) ->
                                                                                                                                                                                                          Path_ReportElems (Order ReportImageID
                                                                                                                                                                                                                                  ReportImage)) q) x
                                                                                                                           Peek_ReportElem_ReportImageView q
                                                                                                                                                           x -> Peek_ReportElems_ReportImageView ((Path_At k :: Path_ReportElem ReportImageView ->
                                                                                                                                                                                                                Path_ReportElems ReportImageView) q) x
                                                                                                                           Peek_ReportElem_SaneSizeImageSize q
                                                                                                                                                             x -> Peek_ReportElems_SaneSizeImageSize ((Path_At k :: Path_ReportElem (SaneSize ImageSize) ->
                                                                                                                                                                                                                    Path_ReportElems (SaneSize ImageSize)) q) x
                                                                                                                           Peek_ReportElem_Item q
                                                                                                                                                x -> Peek_ReportElems_Item ((Path_At k :: Path_ReportElem Item ->
                                                                                                                                                                                          Path_ReportElems Item) q) x
                                                                                                                           Peek_ReportElem_MIM q
                                                                                                                                               x -> Peek_ReportElems_MIM ((Path_At k :: Path_ReportElem (Map ItemFieldName
                                                                                                                                                                                                             Markup) ->
                                                                                                                                                                                        Path_ReportElems (Map ItemFieldName
                                                                                                                                                                                                              Markup)) q) x
                                                                                                                           Peek_ReportElem_URI q
                                                                                                                                               x -> Peek_ReportElems_URI ((Path_At k :: Path_ReportElem URI ->
                                                                                                                                                                                        Path_ReportElems URI) q) x
                                                                                                                           Peek_ReportElem_Text q
                                                                                                                                                x -> Peek_ReportElems_Text ((Path_At k :: Path_ReportElem Text ->
                                                                                                                                                                                          Path_ReportElems Text) q) x) (peek y :: Forest (Peek ReportElem)))
                                         _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths :: Forest (Peek (Order ReportElemID
                                                                                                                        ReportElem))
instance IsPathNode (Order ReportImageID ReportImage)
    where data Peek (Order ReportImageID ReportImage)
              = Peek_ReportImages_String (Path_ReportImages ([Char])) ([Char])
              | Peek_ReportImages_Bool (Path_ReportImages Bool) Bool
              | Peek_ReportImages_Double (Path_ReportImages Double) Double
              | Peek_ReportImages_Dimension (Path_ReportImages Dimension)
                                            Dimension
              | Peek_ReportImages_ImageCrop (Path_ReportImages ImageCrop)
                                            ImageCrop
              | Peek_ReportImages_ImageSize (Path_ReportImages ImageSize)
                                            ImageSize
              | Peek_ReportImages_Units (Path_ReportImages Units) Units
              | Peek_ReportImages_ImageFile (Path_ReportImages ImageFile)
                                            ImageFile
              | Peek_ReportImages_JSONText (Path_ReportImages JSONText) JSONText
              | Peek_ReportImages_Markup (Path_ReportImages Markup) Markup
              | Peek_ReportImages_EUI (Path_ReportImages (Either URI ImageFile))
                                      (Either URI ImageFile)
              | Peek_ReportImages_MEUI (Path_ReportImages (Maybe (Either URI
                                                                         ImageFile)))
                                       (Maybe (Either URI ImageFile))
              | Peek_ReportImages_MaybeImageFile (Path_ReportImages (Maybe ImageFile))
                                                 (Maybe ImageFile)
              | Peek_ReportImages_ReportImage (Path_ReportImages ReportImage)
                                              ReportImage
              | Peek_ReportImages_ReportImages (Path_ReportImages (Order ReportImageID
                                                                         ReportImage))
                                               (Order ReportImageID ReportImage)
              | Peek_ReportImages_ReportImageView (Path_ReportImages ReportImageView)
                                                  ReportImageView
              | Peek_ReportImages_SaneSizeImageSize (Path_ReportImages (SaneSize ImageSize))
                                                    (SaneSize ImageSize)
              | Peek_ReportImages_URI (Path_ReportImages URI) URI
              | Peek_ReportImages_Text (Path_ReportImages Text) Text
              deriving (Eq, Show)
          peek x = let paths = pathsOf x (undefined :: Proxy ReportImage) :: [Path_ReportImages ReportImage]
                    in map (\path -> case path of
                                         p@(Path_At k
                                                    _) -> let [y] = toListOf (toLens p) x :: [ReportImage]
                                                           in Node (Peek_ReportImages_ReportImage p y) (forestMap (\v -> case v of
                                                                                                                             Peek_ReportImage_String q
                                                                                                                                                     x -> Peek_ReportImages_String ((Path_At k :: Path_ReportImage ([Char]) ->
                                                                                                                                                                                                  Path_ReportImages ([Char])) q) x
                                                                                                                             Peek_ReportImage_Bool q
                                                                                                                                                   x -> Peek_ReportImages_Bool ((Path_At k :: Path_ReportImage Bool ->
                                                                                                                                                                                              Path_ReportImages Bool) q) x
                                                                                                                             Peek_ReportImage_Double q
                                                                                                                                                     x -> Peek_ReportImages_Double ((Path_At k :: Path_ReportImage Double ->
                                                                                                                                                                                                  Path_ReportImages Double) q) x
                                                                                                                             Peek_ReportImage_Dimension q
                                                                                                                                                        x -> Peek_ReportImages_Dimension ((Path_At k :: Path_ReportImage Dimension ->
                                                                                                                                                                                                        Path_ReportImages Dimension) q) x
                                                                                                                             Peek_ReportImage_ImageCrop q
                                                                                                                                                        x -> Peek_ReportImages_ImageCrop ((Path_At k :: Path_ReportImage ImageCrop ->
                                                                                                                                                                                                        Path_ReportImages ImageCrop) q) x
                                                                                                                             Peek_ReportImage_ImageSize q
                                                                                                                                                        x -> Peek_ReportImages_ImageSize ((Path_At k :: Path_ReportImage ImageSize ->
                                                                                                                                                                                                        Path_ReportImages ImageSize) q) x
                                                                                                                             Peek_ReportImage_Units q
                                                                                                                                                    x -> Peek_ReportImages_Units ((Path_At k :: Path_ReportImage Units ->
                                                                                                                                                                                                Path_ReportImages Units) q) x
                                                                                                                             Peek_ReportImage_ImageFile q
                                                                                                                                                        x -> Peek_ReportImages_ImageFile ((Path_At k :: Path_ReportImage ImageFile ->
                                                                                                                                                                                                        Path_ReportImages ImageFile) q) x
                                                                                                                             Peek_ReportImage_JSONText q
                                                                                                                                                       x -> Peek_ReportImages_JSONText ((Path_At k :: Path_ReportImage JSONText ->
                                                                                                                                                                                                      Path_ReportImages JSONText) q) x
                                                                                                                             Peek_ReportImage_Markup q
                                                                                                                                                     x -> Peek_ReportImages_Markup ((Path_At k :: Path_ReportImage Markup ->
                                                                                                                                                                                                  Path_ReportImages Markup) q) x
                                                                                                                             Peek_ReportImage_EUI q
                                                                                                                                                  x -> Peek_ReportImages_EUI ((Path_At k :: Path_ReportImage (Either URI
                                                                                                                                                                                                                     ImageFile) ->
                                                                                                                                                                                            Path_ReportImages (Either URI
                                                                                                                                                                                                                      ImageFile)) q) x
                                                                                                                             Peek_ReportImage_MEUI q
                                                                                                                                                   x -> Peek_ReportImages_MEUI ((Path_At k :: Path_ReportImage (Maybe (Either URI
                                                                                                                                                                                                                              ImageFile)) ->
                                                                                                                                                                                              Path_ReportImages (Maybe (Either URI
                                                                                                                                                                                                                               ImageFile))) q) x
                                                                                                                             Peek_ReportImage_MaybeImageFile q
                                                                                                                                                             x -> Peek_ReportImages_MaybeImageFile ((Path_At k :: Path_ReportImage (Maybe ImageFile) ->
                                                                                                                                                                                                                  Path_ReportImages (Maybe ImageFile)) q) x
                                                                                                                             Peek_ReportImage_ReportImage q
                                                                                                                                                          x -> Peek_ReportImages_ReportImage ((Path_At k :: Path_ReportImage ReportImage ->
                                                                                                                                                                                                            Path_ReportImages ReportImage) q) x
                                                                                                                             Peek_ReportImage_ReportImageView q
                                                                                                                                                              x -> Peek_ReportImages_ReportImageView ((Path_At k :: Path_ReportImage ReportImageView ->
                                                                                                                                                                                                                    Path_ReportImages ReportImageView) q) x
                                                                                                                             Peek_ReportImage_SaneSizeImageSize q
                                                                                                                                                                x -> Peek_ReportImages_SaneSizeImageSize ((Path_At k :: Path_ReportImage (SaneSize ImageSize) ->
                                                                                                                                                                                                                        Path_ReportImages (SaneSize ImageSize)) q) x
                                                                                                                             Peek_ReportImage_URI q
                                                                                                                                                  x -> Peek_ReportImages_URI ((Path_At k :: Path_ReportImage URI ->
                                                                                                                                                                                            Path_ReportImages URI) q) x
                                                                                                                             Peek_ReportImage_Text q
                                                                                                                                                   x -> Peek_ReportImages_Text ((Path_At k :: Path_ReportImage Text ->
                                                                                                                                                                                              Path_ReportImages Text) q) x) (peek y :: Forest (Peek ReportImage)))
                                         _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths :: Forest (Peek (Order ReportImageID
                                                                                                                        ReportImage))
instance IsPathNode ((Markup, Markup))
    where data Peek ((Markup, Markup))
              = Peek_MarkupPair_JSONText (Path_MarkupPair JSONText) JSONText
              | Peek_MarkupPair_Markup (Path_MarkupPair Markup) Markup
              | Peek_MarkupPair_MarkupPair (Path_MarkupPair ((Markup, Markup)))
                                           ((Markup, Markup))
              | Peek_MarkupPair_Text (Path_MarkupPair Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_First _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_MarkupPair Markup]
                    in map (\path -> case path of
                                         p@(Path_First _) -> let [y] = toListOf (toLens p) x :: [Markup]
                                                              in Node (Peek_MarkupPair_Markup p y) (forestMap (\v -> case v of
                                                                                                                         Peek_Markup_JSONText q
                                                                                                                                              x -> Peek_MarkupPair_JSONText ((Path_First :: Path_Markup JSONText ->
                                                                                                                                                                                            Path_MarkupPair JSONText) q) x
                                                                                                                         Peek_Markup_Markup q
                                                                                                                                            x -> Peek_MarkupPair_Markup ((Path_First :: Path_Markup Markup ->
                                                                                                                                                                                        Path_MarkupPair Markup) q) x
                                                                                                                         Peek_Markup_Text q
                                                                                                                                          x -> Peek_MarkupPair_Text ((Path_First :: Path_Markup Text ->
                                                                                                                                                                                    Path_MarkupPair Text) q) x) (peek y :: Forest (Peek Markup)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
          peek x = let paths = filter (\p -> case p of
                                                 Path_Second _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_MarkupPair Markup]
                    in map (\path -> case path of
                                         p@(Path_Second _) -> let [y] = toListOf (toLens p) x :: [Markup]
                                                               in Node (Peek_MarkupPair_Markup p y) (forestMap (\v -> case v of
                                                                                                                          Peek_Markup_JSONText q
                                                                                                                                               x -> Peek_MarkupPair_JSONText ((Path_Second :: Path_Markup JSONText ->
                                                                                                                                                                                              Path_MarkupPair JSONText) q) x
                                                                                                                          Peek_Markup_Markup q
                                                                                                                                             x -> Peek_MarkupPair_Markup ((Path_Second :: Path_Markup Markup ->
                                                                                                                                                                                          Path_MarkupPair Markup) q) x
                                                                                                                          Peek_Markup_Text q
                                                                                                                                           x -> Peek_MarkupPair_Text ((Path_Second :: Path_Markup Text ->
                                                                                                                                                                                      Path_MarkupPair Text) q) x) (peek y :: Forest (Peek Markup)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ((CIString, Markup))
    where data Peek ((CIString, Markup))
              = Peek_AbbrevPair_JSONText (Path_AbbrevPair JSONText) JSONText
              | Peek_AbbrevPair_Markup (Path_AbbrevPair Markup) Markup
              | Peek_AbbrevPair_AbbrevPair (Path_AbbrevPair ((CIString, Markup)))
                                           ((CIString, Markup))
              | Peek_AbbrevPair_CIString (Path_AbbrevPair CIString) CIString
              | Peek_AbbrevPair_Text (Path_AbbrevPair Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_First _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy CIString)) :: [Path_AbbrevPair CIString]
                    in map (\path -> case path of
                                         p@(Path_First _) -> let [y] = toListOf (toLens p) x :: [CIString]
                                                              in Node (Peek_AbbrevPair_CIString p y) (forestMap (\v -> case v of
                                                                                                                           Peek_CIString_JSONText q
                                                                                                                                                  x -> Peek_AbbrevPair_JSONText ((Path_First :: Path_CIString JSONText ->
                                                                                                                                                                                                Path_AbbrevPair JSONText) q) x
                                                                                                                           Peek_CIString_CIString q
                                                                                                                                                  x -> Peek_AbbrevPair_CIString ((Path_First :: Path_CIString CIString ->
                                                                                                                                                                                                Path_AbbrevPair CIString) q) x
                                                                                                                           Peek_CIString_Text q
                                                                                                                                              x -> Peek_AbbrevPair_Text ((Path_First :: Path_CIString Text ->
                                                                                                                                                                                        Path_AbbrevPair Text) q) x) (peek y :: Forest (Peek CIString)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
          peek x = let paths = filter (\p -> case p of
                                                 Path_Second _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_AbbrevPair Markup]
                    in map (\path -> case path of
                                         p@(Path_Second _) -> let [y] = toListOf (toLens p) x :: [Markup]
                                                               in Node (Peek_AbbrevPair_Markup p y) (forestMap (\v -> case v of
                                                                                                                          Peek_Markup_JSONText q
                                                                                                                                               x -> Peek_AbbrevPair_JSONText ((Path_Second :: Path_Markup JSONText ->
                                                                                                                                                                                              Path_AbbrevPair JSONText) q) x
                                                                                                                          Peek_Markup_Markup q
                                                                                                                                             x -> Peek_AbbrevPair_Markup ((Path_Second :: Path_Markup Markup ->
                                                                                                                                                                                          Path_AbbrevPair Markup) q) x
                                                                                                                          Peek_Markup_Text q
                                                                                                                                           x -> Peek_AbbrevPair_Text ((Path_Second :: Path_Markup Text ->
                                                                                                                                                                                      Path_AbbrevPair Text) q) x) (peek y :: Forest (Peek Markup)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode (Maybe (Either URI ImageFile))
    where data Peek (Maybe (Either URI ImageFile))
              = Peek_MEUI_ImageFile (Path_MEUI ImageFile) ImageFile
              | Peek_MEUI_EUI (Path_MEUI (Either URI ImageFile))
                              (Either URI ImageFile)
              | Peek_MEUI_MEUI (Path_MEUI (Maybe (Either URI ImageFile)))
                               (Maybe (Either URI ImageFile))
              | Peek_MEUI_URI (Path_MEUI URI) URI
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Just _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy (Either URI
                                                                                                    ImageFile))) :: [Path_MEUI (Either URI
                                                                                                                                       ImageFile)]
                    in map (\path -> case path of
                                         p@(Path_Just _) -> let [y] = toListOf (toLens p) x :: [Either URI
                                                                                                       ImageFile]
                                                             in Node (Peek_MEUI_EUI p y) (forestMap (\v -> case v of
                                                                                                               Peek_EUI_ImageFile q
                                                                                                                                  x -> Peek_MEUI_ImageFile ((Path_Just :: Path_EUI ImageFile ->
                                                                                                                                                                          Path_MEUI ImageFile) q) x
                                                                                                               Peek_EUI_EUI q
                                                                                                                            x -> Peek_MEUI_EUI ((Path_Just :: Path_EUI (Either URI
                                                                                                                                                                               ImageFile) ->
                                                                                                                                                              Path_MEUI (Either URI
                                                                                                                                                                                ImageFile)) q) x
                                                                                                               Peek_EUI_URI q
                                                                                                                            x -> Peek_MEUI_URI ((Path_Just :: Path_EUI URI ->
                                                                                                                                                              Path_MEUI URI) q) x) (peek y :: Forest (Peek (Either URI
                                                                                                                                                                                                                   ImageFile))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode (Maybe ImageFile)
    where data Peek (Maybe ImageFile)
              = Peek_MaybeImageFile_String (Path_MaybeImageFile ([Char]))
                                           ([Char])
              | Peek_MaybeImageFile_JSONText (Path_MaybeImageFile JSONText)
                                             JSONText
              | Peek_MaybeImageFile_MaybeImageFile (Path_MaybeImageFile (Maybe ImageFile))
                                                   (Maybe ImageFile)
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_MaybeImageFile_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_MaybeImageFile ([Char])]
                    in map (\path -> case path of
                                         p@(Path_MaybeImageFile_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                            in Node (Peek_MaybeImageFile_String p y) (forestMap (\v -> case v of
                                                                                                                                           Peek_String_String q
                                                                                                                                                              x -> Peek_MaybeImageFile_String ((Path_MaybeImageFile_View :: Path_String ([Char]) ->
                                                                                                                                                                                                                            Path_MaybeImageFile ([Char])) q) x
                                                                                                                                           Peek_String_JSONText q
                                                                                                                                                                x -> Peek_MaybeImageFile_JSONText ((Path_MaybeImageFile_View :: Path_String JSONText ->
                                                                                                                                                                                                                                Path_MaybeImageFile JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode (Maybe ReportIntendedUse)
    where data Peek (Maybe ReportIntendedUse)
              = Peek_MaybeReportIntendedUse_String (Path_MaybeReportIntendedUse ([Char]))
                                                   ([Char])
              | Peek_MaybeReportIntendedUse_JSONText (Path_MaybeReportIntendedUse JSONText)
                                                     JSONText
              | Peek_MaybeReportIntendedUse_MaybeReportIntendedUse (Path_MaybeReportIntendedUse (Maybe ReportIntendedUse))
                                                                   (Maybe ReportIntendedUse)
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_MaybeReportIntendedUse_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_MaybeReportIntendedUse ([Char])]
                    in map (\path -> case path of
                                         p@(Path_MaybeReportIntendedUse_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                                    in Node (Peek_MaybeReportIntendedUse_String p y) (forestMap (\v -> case v of
                                                                                                                                                           Peek_String_String q
                                                                                                                                                                              x -> Peek_MaybeReportIntendedUse_String ((Path_MaybeReportIntendedUse_View :: Path_String ([Char]) ->
                                                                                                                                                                                                                                                            Path_MaybeReportIntendedUse ([Char])) q) x
                                                                                                                                                           Peek_String_JSONText q
                                                                                                                                                                                x -> Peek_MaybeReportIntendedUse_JSONText ((Path_MaybeReportIntendedUse_View :: Path_String JSONText ->
                                                                                                                                                                                                                                                                Path_MaybeReportIntendedUse JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode (ReadOnly ([Char]))
    where data Peek (ReadOnly ([Char]))
              = Peek_ReadOnlyFilePath_String (Path_ReadOnlyFilePath ([Char]))
                                             ([Char])
              | Peek_ReadOnlyFilePath_JSONText (Path_ReadOnlyFilePath JSONText)
                                               JSONText
              | Peek_ReadOnlyFilePath_ReadOnlyFilePath (Path_ReadOnlyFilePath (ReadOnly ([Char])))
                                                       (ReadOnly ([Char]))
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_ReadOnlyFilePath_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_ReadOnlyFilePath ([Char])]
                    in map (\path -> case path of
                                         p@(Path_ReadOnlyFilePath_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                              in Node (Peek_ReadOnlyFilePath_String p y) (forestMap (\v -> case v of
                                                                                                                                               Peek_String_String q
                                                                                                                                                                  x -> Peek_ReadOnlyFilePath_String ((Path_ReadOnlyFilePath_View :: Path_String ([Char]) ->
                                                                                                                                                                                                                                    Path_ReadOnlyFilePath ([Char])) q) x
                                                                                                                                               Peek_String_JSONText q
                                                                                                                                                                    x -> Peek_ReadOnlyFilePath_JSONText ((Path_ReadOnlyFilePath_View :: Path_String JSONText ->
                                                                                                                                                                                                                                        Path_ReadOnlyFilePath JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode (SaneSize ImageSize)
    where data Peek (SaneSize ImageSize)
              = Peek_SaneSizeImageSize_String (Path_SaneSizeImageSize ([Char]))
                                              ([Char])
              | Peek_SaneSizeImageSize_Double (Path_SaneSizeImageSize Double)
                                              Double
              | Peek_SaneSizeImageSize_Dimension (Path_SaneSizeImageSize Dimension)
                                                 Dimension
              | Peek_SaneSizeImageSize_ImageSize (Path_SaneSizeImageSize ImageSize)
                                                 ImageSize
              | Peek_SaneSizeImageSize_Units (Path_SaneSizeImageSize Units) Units
              | Peek_SaneSizeImageSize_JSONText (Path_SaneSizeImageSize JSONText)
                                                JSONText
              | Peek_SaneSizeImageSize_SaneSizeImageSize (Path_SaneSizeImageSize (SaneSize ImageSize))
                                                         (SaneSize ImageSize)
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_SaneSizeImageSize_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ImageSize)) :: [Path_SaneSizeImageSize ImageSize]
                    in map (\path -> case path of
                                         p@(Path_SaneSizeImageSize_View _) -> let [y] = toListOf (toLens p) x :: [ImageSize]
                                                                               in Node (Peek_SaneSizeImageSize_ImageSize p y) (forestMap (\v -> case v of
                                                                                                                                                    Peek_ImageSize_String q
                                                                                                                                                                          x -> Peek_SaneSizeImageSize_String ((Path_SaneSizeImageSize_View :: Path_ImageSize ([Char]) ->
                                                                                                                                                                                                                                              Path_SaneSizeImageSize ([Char])) q) x
                                                                                                                                                    Peek_ImageSize_Double q
                                                                                                                                                                          x -> Peek_SaneSizeImageSize_Double ((Path_SaneSizeImageSize_View :: Path_ImageSize Double ->
                                                                                                                                                                                                                                              Path_SaneSizeImageSize Double) q) x
                                                                                                                                                    Peek_ImageSize_Dimension q
                                                                                                                                                                             x -> Peek_SaneSizeImageSize_Dimension ((Path_SaneSizeImageSize_View :: Path_ImageSize Dimension ->
                                                                                                                                                                                                                                                    Path_SaneSizeImageSize Dimension) q) x
                                                                                                                                                    Peek_ImageSize_ImageSize q
                                                                                                                                                                             x -> Peek_SaneSizeImageSize_ImageSize ((Path_SaneSizeImageSize_View :: Path_ImageSize ImageSize ->
                                                                                                                                                                                                                                                    Path_SaneSizeImageSize ImageSize) q) x
                                                                                                                                                    Peek_ImageSize_Units q
                                                                                                                                                                         x -> Peek_SaneSizeImageSize_Units ((Path_SaneSizeImageSize_View :: Path_ImageSize Units ->
                                                                                                                                                                                                                                            Path_SaneSizeImageSize Units) q) x
                                                                                                                                                    Peek_ImageSize_JSONText q
                                                                                                                                                                            x -> Peek_SaneSizeImageSize_JSONText ((Path_SaneSizeImageSize_View :: Path_ImageSize JSONText ->
                                                                                                                                                                                                                                                  Path_SaneSizeImageSize JSONText) q) x) (peek y :: Forest (Peek ImageSize)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ([Char])
    where data Peek ([Char])
              = Peek_String_String (Path_String ([Char])) ([Char])
              | Peek_String_JSONText (Path_String JSONText) JSONText
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_String_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy JSONText)) :: [Path_String JSONText]
                    in map (\path -> case path of
                                         p@(Path_String_View _) -> let [y] = toListOf (toLens p) x :: [JSONText]
                                                                    in Node (Peek_String_JSONText p y) (forestMap (\v -> case v of
                                                                                                                             Peek_JSONText_JSONText q
                                                                                                                                                    x -> Peek_String_JSONText ((Path_String_View :: Path_JSONText JSONText ->
                                                                                                                                                                                                    Path_String JSONText) q) x) (peek y :: Forest (Peek JSONText)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ([UserId])
    where data Peek ([UserId])
              = Peek_UserIds_JSONText (Path_UserIds JSONText) JSONText
              | Peek_UserIds_UserIds (Path_UserIds ([UserId])) ([UserId])
              | Peek_UserIds_Text (Path_UserIds Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_UserIds_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy Text)) :: [Path_UserIds Text]
                    in map (\path -> case path of
                                         p@(Path_UserIds_View _) -> let [y] = toListOf (toLens p) x :: [Text]
                                                                     in Node (Peek_UserIds_Text p y) (forestMap (\v -> case v of
                                                                                                                           Peek_Text_JSONText q
                                                                                                                                              x -> Peek_UserIds_JSONText ((Path_UserIds_View :: Path_Text JSONText ->
                                                                                                                                                                                                Path_UserIds JSONText) q) x
                                                                                                                           Peek_Text_Text q
                                                                                                                                          x -> Peek_UserIds_Text ((Path_UserIds_View :: Path_Text Text ->
                                                                                                                                                                                        Path_UserIds Text) q) x) (peek y :: Forest (Peek Text)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode Int64
    where data Peek Int64
              = Peek_Int64_Int64 (Path_Int64 Int64) Int64
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode Bool
    where data Peek Bool
              = Peek_Bool_String (Path_Bool ([Char])) ([Char])
              | Peek_Bool_Bool (Path_Bool Bool) Bool
              | Peek_Bool_JSONText (Path_Bool JSONText) JSONText
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Bool_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_Bool ([Char])]
                    in map (\path -> case path of
                                         p@(Path_Bool_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                  in Node (Peek_Bool_String p y) (forestMap (\v -> case v of
                                                                                                                       Peek_String_String q
                                                                                                                                          x -> Peek_Bool_String ((Path_Bool_View :: Path_String ([Char]) ->
                                                                                                                                                                                    Path_Bool ([Char])) q) x
                                                                                                                       Peek_String_JSONText q
                                                                                                                                            x -> Peek_Bool_JSONText ((Path_Bool_View :: Path_String JSONText ->
                                                                                                                                                                                        Path_Bool JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode Double
    where data Peek Double
              = Peek_Double_String (Path_Double ([Char])) ([Char])
              | Peek_Double_Double (Path_Double Double) Double
              | Peek_Double_JSONText (Path_Double JSONText) JSONText
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Double_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_Double ([Char])]
                    in map (\path -> case path of
                                         p@(Path_Double_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                    in Node (Peek_Double_String p y) (forestMap (\v -> case v of
                                                                                                                           Peek_String_String q
                                                                                                                                              x -> Peek_Double_String ((Path_Double_View :: Path_String ([Char]) ->
                                                                                                                                                                                            Path_Double ([Char])) q) x
                                                                                                                           Peek_String_JSONText q
                                                                                                                                                x -> Peek_Double_JSONText ((Path_Double_View :: Path_String JSONText ->
                                                                                                                                                                                                Path_Double JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode Int
    where data Peek Int
              = Peek_Int_Int (Path_Int Int) Int
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode Dimension
    where data Peek Dimension
              = Peek_Dimension_Dimension (Path_Dimension Dimension) Dimension
              | Peek_Dimension_JSONText (Path_Dimension JSONText) JSONText
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Dimension_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy JSONText)) :: [Path_Dimension JSONText]
                    in map (\path -> case path of
                                         p@(Path_Dimension_View _) -> let [y] = toListOf (toLens p) x :: [JSONText]
                                                                       in Node (Peek_Dimension_JSONText p y) (forestMap (\v -> case v of
                                                                                                                                   Peek_JSONText_JSONText q
                                                                                                                                                          x -> Peek_Dimension_JSONText ((Path_Dimension_View :: Path_JSONText JSONText ->
                                                                                                                                                                                                                Path_Dimension JSONText) q) x) (peek y :: Forest (Peek JSONText)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ImageCrop
    where data Peek ImageCrop
              = Peek_ImageCrop_ImageCrop (Path_ImageCrop ImageCrop) ImageCrop
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode ImageSize
    where data Peek ImageSize
              = Peek_ImageSize_String (Path_ImageSize ([Char])) ([Char])
              | Peek_ImageSize_Double (Path_ImageSize Double) Double
              | Peek_ImageSize_Dimension (Path_ImageSize Dimension) Dimension
              | Peek_ImageSize_ImageSize (Path_ImageSize ImageSize) ImageSize
              | Peek_ImageSize_Units (Path_ImageSize Units) Units
              | Peek_ImageSize_JSONText (Path_ImageSize JSONText) JSONText
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ImageSize_dim _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Dimension)) :: [Path_ImageSize Dimension] of
                        [p@(Path_ImageSize_dim _)] -> let [y] = toListOf (toLens p) x :: [Dimension]
                                                       in Node (Peek_ImageSize_Dimension p y) (forestMap (\v -> case v of
                                                                                                                    Peek_Dimension_Dimension q
                                                                                                                                             x -> Peek_ImageSize_Dimension ((Path_ImageSize_dim :: Path_Dimension Dimension ->
                                                                                                                                                                                                   Path_ImageSize Dimension) q) x
                                                                                                                    Peek_Dimension_JSONText q
                                                                                                                                            x -> Peek_ImageSize_JSONText ((Path_ImageSize_dim :: Path_Dimension JSONText ->
                                                                                                                                                                                                 Path_ImageSize JSONText) q) x) (peek y :: Forest (Peek Dimension)))
                        [] -> error "No Path_ImageSize_dim field found"
                        ps -> error $ ("Multiple Path_ImageSize_dim fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ImageSize_size _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Double)) :: [Path_ImageSize Double] of
                        [p@(Path_ImageSize_size _)] -> let [y] = toListOf (toLens p) x :: [Double]
                                                        in Node (Peek_ImageSize_Double p y) (forestMap (\v -> case v of
                                                                                                                  Peek_Double_String q
                                                                                                                                     x -> Peek_ImageSize_String ((Path_ImageSize_size :: Path_Double ([Char]) ->
                                                                                                                                                                                         Path_ImageSize ([Char])) q) x
                                                                                                                  Peek_Double_Double q
                                                                                                                                     x -> Peek_ImageSize_Double ((Path_ImageSize_size :: Path_Double Double ->
                                                                                                                                                                                         Path_ImageSize Double) q) x
                                                                                                                  Peek_Double_JSONText q
                                                                                                                                       x -> Peek_ImageSize_JSONText ((Path_ImageSize_size :: Path_Double JSONText ->
                                                                                                                                                                                             Path_ImageSize JSONText) q) x) (peek y :: Forest (Peek Double)))
                        [] -> error "No Path_ImageSize_size field found"
                        ps -> error $ ("Multiple Path_ImageSize_size fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ImageSize_units _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Units)) :: [Path_ImageSize Units] of
                        [p@(Path_ImageSize_units _)] -> let [y] = toListOf (toLens p) x :: [Units]
                                                         in Node (Peek_ImageSize_Units p y) (forestMap (\v -> case v of
                                                                                                                  Peek_Units_Units q
                                                                                                                                   x -> Peek_ImageSize_Units ((Path_ImageSize_units :: Path_Units Units ->
                                                                                                                                                                                       Path_ImageSize Units) q) x
                                                                                                                  Peek_Units_JSONText q
                                                                                                                                      x -> Peek_ImageSize_JSONText ((Path_ImageSize_units :: Path_Units JSONText ->
                                                                                                                                                                                             Path_ImageSize JSONText) q) x) (peek y :: Forest (Peek Units)))
                        [] -> error "No Path_ImageSize_units field found"
                        ps -> error $ ("Multiple Path_ImageSize_units fields found: " ++ show ps)]
instance IsPathNode Units
    where data Peek Units
              = Peek_Units_Units (Path_Units Units) Units
              | Peek_Units_JSONText (Path_Units JSONText) JSONText
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Units_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy JSONText)) :: [Path_Units JSONText]
                    in map (\path -> case path of
                                         p@(Path_Units_View _) -> let [y] = toListOf (toLens p) x :: [JSONText]
                                                                   in Node (Peek_Units_JSONText p y) (forestMap (\v -> case v of
                                                                                                                           Peek_JSONText_JSONText q
                                                                                                                                                  x -> Peek_Units_JSONText ((Path_Units_View :: Path_JSONText JSONText ->
                                                                                                                                                                                                Path_Units JSONText) q) x) (peek y :: Forest (Peek JSONText)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ImageFile
    where data Peek ImageFile
              = Peek_ImageFile_ImageFile (Path_ImageFile ImageFile) ImageFile
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode Integer
    where data Peek Integer
              = Peek_Integer_Integer (Path_Integer Integer) Integer
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode JSONText
    where data Peek JSONText
              = Peek_JSONText_JSONText (Path_JSONText JSONText) JSONText
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode Markup
    where data Peek Markup
              = Peek_Markup_JSONText (Path_Markup JSONText) JSONText
              | Peek_Markup_Markup (Path_Markup Markup) Markup
              | Peek_Markup_Text (Path_Markup Text) Text
              deriving (Eq, Show)
          peek (x@(Markdown {})) = [case filter (\p -> case p of
                                                           Path_Markup_markdownText _ -> True
                                                           _ -> False) (pathsOf x (undefined :: Proxy Text)) :: [Path_Markup Text] of
                                        [p@(Path_Markup_markdownText _)] -> let [y] = toListOf (toLens p) x :: [Text]
                                                                             in Node (Peek_Markup_Text p y) (forestMap (\v -> case v of
                                                                                                                                  Peek_Text_JSONText q
                                                                                                                                                     x -> Peek_Markup_JSONText ((Path_Markup_markdownText :: Path_Text JSONText ->
                                                                                                                                                                                                             Path_Markup JSONText) q) x
                                                                                                                                  Peek_Text_Text q
                                                                                                                                                 x -> Peek_Markup_Text ((Path_Markup_markdownText :: Path_Text Text ->
                                                                                                                                                                                                     Path_Markup Text) q) x) (peek y :: Forest (Peek Text)))
                                        [] -> error "No Path_Markup_markdownText field found"
                                        ps -> error $ ("Multiple Path_Markup_markdownText fields found: " ++ show ps)]
          peek (x@(Html {})) = [case filter (\p -> case p of
                                                       Path_Markup_htmlText _ -> True
                                                       _ -> False) (pathsOf x (undefined :: Proxy Text)) :: [Path_Markup Text] of
                                    [p@(Path_Markup_htmlText _)] -> let [y] = toListOf (toLens p) x :: [Text]
                                                                     in Node (Peek_Markup_Text p y) (forestMap (\v -> case v of
                                                                                                                          Peek_Text_JSONText q
                                                                                                                                             x -> Peek_Markup_JSONText ((Path_Markup_htmlText :: Path_Text JSONText ->
                                                                                                                                                                                                 Path_Markup JSONText) q) x
                                                                                                                          Peek_Text_Text q
                                                                                                                                         x -> Peek_Markup_Text ((Path_Markup_htmlText :: Path_Text Text ->
                                                                                                                                                                                         Path_Markup Text) q) x) (peek y :: Forest (Peek Text)))
                                    [] -> error "No Path_Markup_htmlText field found"
                                    ps -> error $ ("Multiple Path_Markup_htmlText fields found: " ++ show ps)]
          peek (x@(LaTeX {})) = [error "doField' Text.LaTeX.Base.Syntax.LaTeX"]
          peek (x@(Pandoc {})) = [error "doField' Text.Pandoc.Definition.Pandoc"]
          peek (x@(Markup {})) = [error "doField' [Appraisal.Markup.Markup]"]
instance IsPathNode Permissions
    where data Peek Permissions
              = Peek_Permissions_JSONText (Path_Permissions JSONText) JSONText
              | Peek_Permissions_Permissions (Path_Permissions Permissions)
                                             Permissions
              | Peek_Permissions_UserIds (Path_Permissions ([UserId])) ([UserId])
              | Peek_Permissions_Text (Path_Permissions Text) Text
              | Peek_Permissions_UserId (Path_Permissions UserId) UserId
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_Permissions_owner _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy UserId)) :: [Path_Permissions UserId] of
                        [p@(Path_Permissions_owner _)] -> let [y] = toListOf (toLens p) x :: [UserId]
                                                           in Node (Peek_Permissions_UserId p y) (forestMap (\v -> case v of
                                                                                                                       Peek_UserId_UserId q
                                                                                                                                          x -> Peek_Permissions_UserId ((Path_Permissions_owner :: Path_UserId UserId ->
                                                                                                                                                                                                   Path_Permissions UserId) q) x) (peek y :: Forest (Peek UserId)))
                        [] -> error "No Path_Permissions_owner field found"
                        ps -> error $ ("Multiple Path_Permissions_owner fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_Permissions_writers _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ([UserId]))) :: [Path_Permissions ([UserId])] of
                        [p@(Path_Permissions_writers _)] -> let [y] = toListOf (toLens p) x :: [[UserId]]
                                                             in Node (Peek_Permissions_UserIds p y) (forestMap (\v -> case v of
                                                                                                                          Peek_UserIds_JSONText q
                                                                                                                                                x -> Peek_Permissions_JSONText ((Path_Permissions_writers :: Path_UserIds JSONText ->
                                                                                                                                                                                                             Path_Permissions JSONText) q) x
                                                                                                                          Peek_UserIds_UserIds q
                                                                                                                                               x -> Peek_Permissions_UserIds ((Path_Permissions_writers :: Path_UserIds ([UserId]) ->
                                                                                                                                                                                                           Path_Permissions ([UserId])) q) x
                                                                                                                          Peek_UserIds_Text q
                                                                                                                                            x -> Peek_Permissions_Text ((Path_Permissions_writers :: Path_UserIds Text ->
                                                                                                                                                                                                     Path_Permissions Text) q) x) (peek y :: Forest (Peek ([UserId]))))
                        [] -> error "No Path_Permissions_writers field found"
                        ps -> error $ ("Multiple Path_Permissions_writers fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_Permissions_readers _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ([UserId]))) :: [Path_Permissions ([UserId])] of
                        [p@(Path_Permissions_readers _)] -> let [y] = toListOf (toLens p) x :: [[UserId]]
                                                             in Node (Peek_Permissions_UserIds p y) (forestMap (\v -> case v of
                                                                                                                          Peek_UserIds_JSONText q
                                                                                                                                                x -> Peek_Permissions_JSONText ((Path_Permissions_readers :: Path_UserIds JSONText ->
                                                                                                                                                                                                             Path_Permissions JSONText) q) x
                                                                                                                          Peek_UserIds_UserIds q
                                                                                                                                               x -> Peek_Permissions_UserIds ((Path_Permissions_readers :: Path_UserIds ([UserId]) ->
                                                                                                                                                                                                           Path_Permissions ([UserId])) q) x
                                                                                                                          Peek_UserIds_Text q
                                                                                                                                            x -> Peek_Permissions_Text ((Path_Permissions_readers :: Path_UserIds Text ->
                                                                                                                                                                                                     Path_Permissions Text) q) x) (peek y :: Forest (Peek ([UserId]))))
                        [] -> error "No Path_Permissions_readers field found"
                        ps -> error $ ("Multiple Path_Permissions_readers fields found: " ++ show ps)]
instance IsPathNode Author
    where data Peek Author
              = Peek_Author_JSONText (Path_Author JSONText) JSONText
              | Peek_Author_Markup (Path_Author Markup) Markup
              | Peek_Author_Author (Path_Author Author) Author
              | Peek_Author_Text (Path_Author Text) Text
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_Author_authorName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_Author Markup] of
                        [p@(Path_Author_authorName _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                           in Node (Peek_Author_Markup p y) (forestMap (\v -> case v of
                                                                                                                  Peek_Markup_JSONText q
                                                                                                                                       x -> Peek_Author_JSONText ((Path_Author_authorName :: Path_Markup JSONText ->
                                                                                                                                                                                             Path_Author JSONText) q) x
                                                                                                                  Peek_Markup_Markup q
                                                                                                                                     x -> Peek_Author_Markup ((Path_Author_authorName :: Path_Markup Markup ->
                                                                                                                                                                                         Path_Author Markup) q) x
                                                                                                                  Peek_Markup_Text q
                                                                                                                                   x -> Peek_Author_Text ((Path_Author_authorName :: Path_Markup Text ->
                                                                                                                                                                                     Path_Author Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_Author_authorName field found"
                        ps -> error $ ("Multiple Path_Author_authorName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_Author_authorCredentials _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_Author Markup] of
                        [p@(Path_Author_authorCredentials _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                  in Node (Peek_Author_Markup p y) (forestMap (\v -> case v of
                                                                                                                         Peek_Markup_JSONText q
                                                                                                                                              x -> Peek_Author_JSONText ((Path_Author_authorCredentials :: Path_Markup JSONText ->
                                                                                                                                                                                                           Path_Author JSONText) q) x
                                                                                                                         Peek_Markup_Markup q
                                                                                                                                            x -> Peek_Author_Markup ((Path_Author_authorCredentials :: Path_Markup Markup ->
                                                                                                                                                                                                       Path_Author Markup) q) x
                                                                                                                         Peek_Markup_Text q
                                                                                                                                          x -> Peek_Author_Text ((Path_Author_authorCredentials :: Path_Markup Text ->
                                                                                                                                                                                                   Path_Author Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_Author_authorCredentials field found"
                        ps -> error $ ("Multiple Path_Author_authorCredentials fields found: " ++ show ps)]
instance IsPathNode Branding
    where data Peek Branding
              = Peek_Branding_JSONText (Path_Branding JSONText) JSONText
              | Peek_Branding_Branding (Path_Branding Branding) Branding
              | Peek_Branding_Text (Path_Branding Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Branding_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy Text)) :: [Path_Branding Text]
                    in map (\path -> case path of
                                         p@(Path_Branding_View _) -> let [y] = toListOf (toLens p) x :: [Text]
                                                                      in Node (Peek_Branding_Text p y) (forestMap (\v -> case v of
                                                                                                                             Peek_Text_JSONText q
                                                                                                                                                x -> Peek_Branding_JSONText ((Path_Branding_View :: Path_Text JSONText ->
                                                                                                                                                                                                    Path_Branding JSONText) q) x
                                                                                                                             Peek_Text_Text q
                                                                                                                                            x -> Peek_Branding_Text ((Path_Branding_View :: Path_Text Text ->
                                                                                                                                                                                            Path_Branding Text) q) x) (peek y :: Forest (Peek Text)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode Report
    where data Peek Report
              = Peek_Report_String (Path_Report ([Char])) ([Char])
              | Peek_Report_Int64 (Path_Report Int64) Int64
              | Peek_Report_Int (Path_Report Int) Int
              | Peek_Report_Bool (Path_Report Bool) Bool
              | Peek_Report_Double (Path_Report Double) Double
              | Peek_Report_Dimension (Path_Report Dimension) Dimension
              | Peek_Report_ImageCrop (Path_Report ImageCrop) ImageCrop
              | Peek_Report_ImageSize (Path_Report ImageSize) ImageSize
              | Peek_Report_Units (Path_Report Units) Units
              | Peek_Report_ImageFile (Path_Report ImageFile) ImageFile
              | Peek_Report_Integer (Path_Report Integer) Integer
              | Peek_Report_JSONText (Path_Report JSONText) JSONText
              | Peek_Report_Markup (Path_Report Markup) Markup
              | Peek_Report_Permissions (Path_Report Permissions) Permissions
              | Peek_Report_UserIds (Path_Report ([UserId])) ([UserId])
              | Peek_Report_AbbrevPair (Path_Report ((CIString, Markup)))
                                       ((CIString, Markup))
              | Peek_Report_AbbrevPairs (Path_Report (Order AbbrevPairID
                                                            ((CIString, Markup))))
                                        (Order AbbrevPairID ((CIString, Markup)))
              | Peek_Report_Author (Path_Report Author) Author
              | Peek_Report_Authors (Path_Report (Order AuthorID Author))
                                    (Order AuthorID Author)
              | Peek_Report_Branding (Path_Report Branding) Branding
              | Peek_Report_MarkupPair (Path_Report ((Markup, Markup)))
                                       ((Markup, Markup))
              | Peek_Report_MarkupPairs (Path_Report (Order MarkupPairID
                                                            ((Markup, Markup))))
                                        (Order MarkupPairID ((Markup, Markup)))
              | Peek_Report_Markups (Path_Report (Order MarkupID Markup))
                                    (Order MarkupID Markup)
              | Peek_Report_MaybeReportIntendedUse (Path_Report (Maybe ReportIntendedUse))
                                                   (Maybe ReportIntendedUse)
              | Peek_Report_Report (Path_Report Report) Report
              | Peek_Report_ReportElem (Path_Report ReportElem) ReportElem
              | Peek_Report_ReportElems (Path_Report (Order ReportElemID
                                                            ReportElem))
                                        (Order ReportElemID ReportElem)
              | Peek_Report_ReportFlags (Path_Report ReportFlags) ReportFlags
              | Peek_Report_ReportStandard (Path_Report ReportStandard)
                                           ReportStandard
              | Peek_Report_ReportStatus (Path_Report ReportStatus) ReportStatus
              | Peek_Report_ReportValueApproachInfo (Path_Report ReportValueApproachInfo)
                                                    ReportValueApproachInfo
              | Peek_Report_ReportValueTypeInfo (Path_Report ReportValueTypeInfo)
                                                ReportValueTypeInfo
              | Peek_Report_EUI (Path_Report (Either URI ImageFile))
                                (Either URI ImageFile)
              | Peek_Report_MEUI (Path_Report (Maybe (Either URI ImageFile)))
                                 (Maybe (Either URI ImageFile))
              | Peek_Report_MaybeImageFile (Path_Report (Maybe ImageFile))
                                           (Maybe ImageFile)
              | Peek_Report_ReportImage (Path_Report ReportImage) ReportImage
              | Peek_Report_ReportImages (Path_Report (Order ReportImageID
                                                             ReportImage))
                                         (Order ReportImageID ReportImage)
              | Peek_Report_ReadOnlyFilePath (Path_Report (ReadOnly ([Char])))
                                             (ReadOnly ([Char]))
              | Peek_Report_ReportImageView (Path_Report ReportImageView)
                                            ReportImageView
              | Peek_Report_ReportView (Path_Report ReportView) ReportView
              | Peek_Report_SaneSizeImageSize (Path_Report (SaneSize ImageSize))
                                              (SaneSize ImageSize)
              | Peek_Report_Item (Path_Report Item) Item
              | Peek_Report_MIM (Path_Report (Map ItemFieldName Markup))
                                (Map ItemFieldName Markup)
              | Peek_Report_CIString (Path_Report CIString) CIString
              | Peek_Report_URI (Path_Report URI) URI
              | Peek_Report_Text (Path_Report Text) Text
              | Peek_Report_UserId (Path_Report UserId) UserId
              | Peek_Report_UUID (Path_Report UUID) UUID
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Report_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ReportView)) :: [Path_Report ReportView]
                    in map (\path -> case path of
                                         p@(Path_Report_View _) -> let [y] = toListOf (toLens p) x :: [ReportView]
                                                                    in Node (Peek_Report_ReportView p y) (forestMap (\v -> case v of
                                                                                                                               Peek_ReportView_String q
                                                                                                                                                      x -> Peek_Report_String ((Path_Report_View :: Path_ReportView ([Char]) ->
                                                                                                                                                                                                    Path_Report ([Char])) q) x
                                                                                                                               Peek_ReportView_Int64 q
                                                                                                                                                     x -> Peek_Report_Int64 ((Path_Report_View :: Path_ReportView Int64 ->
                                                                                                                                                                                                  Path_Report Int64) q) x
                                                                                                                               Peek_ReportView_Int q
                                                                                                                                                   x -> Peek_Report_Int ((Path_Report_View :: Path_ReportView Int ->
                                                                                                                                                                                              Path_Report Int) q) x
                                                                                                                               Peek_ReportView_Bool q
                                                                                                                                                    x -> Peek_Report_Bool ((Path_Report_View :: Path_ReportView Bool ->
                                                                                                                                                                                                Path_Report Bool) q) x
                                                                                                                               Peek_ReportView_Double q
                                                                                                                                                      x -> Peek_Report_Double ((Path_Report_View :: Path_ReportView Double ->
                                                                                                                                                                                                    Path_Report Double) q) x
                                                                                                                               Peek_ReportView_Dimension q
                                                                                                                                                         x -> Peek_Report_Dimension ((Path_Report_View :: Path_ReportView Dimension ->
                                                                                                                                                                                                          Path_Report Dimension) q) x
                                                                                                                               Peek_ReportView_ImageCrop q
                                                                                                                                                         x -> Peek_Report_ImageCrop ((Path_Report_View :: Path_ReportView ImageCrop ->
                                                                                                                                                                                                          Path_Report ImageCrop) q) x
                                                                                                                               Peek_ReportView_ImageSize q
                                                                                                                                                         x -> Peek_Report_ImageSize ((Path_Report_View :: Path_ReportView ImageSize ->
                                                                                                                                                                                                          Path_Report ImageSize) q) x
                                                                                                                               Peek_ReportView_Units q
                                                                                                                                                     x -> Peek_Report_Units ((Path_Report_View :: Path_ReportView Units ->
                                                                                                                                                                                                  Path_Report Units) q) x
                                                                                                                               Peek_ReportView_ImageFile q
                                                                                                                                                         x -> Peek_Report_ImageFile ((Path_Report_View :: Path_ReportView ImageFile ->
                                                                                                                                                                                                          Path_Report ImageFile) q) x
                                                                                                                               Peek_ReportView_Integer q
                                                                                                                                                       x -> Peek_Report_Integer ((Path_Report_View :: Path_ReportView Integer ->
                                                                                                                                                                                                      Path_Report Integer) q) x
                                                                                                                               Peek_ReportView_JSONText q
                                                                                                                                                        x -> Peek_Report_JSONText ((Path_Report_View :: Path_ReportView JSONText ->
                                                                                                                                                                                                        Path_Report JSONText) q) x
                                                                                                                               Peek_ReportView_Markup q
                                                                                                                                                      x -> Peek_Report_Markup ((Path_Report_View :: Path_ReportView Markup ->
                                                                                                                                                                                                    Path_Report Markup) q) x
                                                                                                                               Peek_ReportView_Permissions q
                                                                                                                                                           x -> Peek_Report_Permissions ((Path_Report_View :: Path_ReportView Permissions ->
                                                                                                                                                                                                              Path_Report Permissions) q) x
                                                                                                                               Peek_ReportView_UserIds q
                                                                                                                                                       x -> Peek_Report_UserIds ((Path_Report_View :: Path_ReportView ([UserId]) ->
                                                                                                                                                                                                      Path_Report ([UserId])) q) x
                                                                                                                               Peek_ReportView_AbbrevPair q
                                                                                                                                                          x -> Peek_Report_AbbrevPair ((Path_Report_View :: Path_ReportView ((CIString,
                                                                                                                                                                                                                              Markup)) ->
                                                                                                                                                                                                            Path_Report ((CIString,
                                                                                                                                                                                                                          Markup))) q) x
                                                                                                                               Peek_ReportView_AbbrevPairs q
                                                                                                                                                           x -> Peek_Report_AbbrevPairs ((Path_Report_View :: Path_ReportView (Order AbbrevPairID
                                                                                                                                                                                                                                     ((CIString,
                                                                                                                                                                                                                                       Markup))) ->
                                                                                                                                                                                                              Path_Report (Order AbbrevPairID
                                                                                                                                                                                                                                 ((CIString,
                                                                                                                                                                                                                                   Markup)))) q) x
                                                                                                                               Peek_ReportView_Author q
                                                                                                                                                      x -> Peek_Report_Author ((Path_Report_View :: Path_ReportView Author ->
                                                                                                                                                                                                    Path_Report Author) q) x
                                                                                                                               Peek_ReportView_Authors q
                                                                                                                                                       x -> Peek_Report_Authors ((Path_Report_View :: Path_ReportView (Order AuthorID
                                                                                                                                                                                                                             Author) ->
                                                                                                                                                                                                      Path_Report (Order AuthorID
                                                                                                                                                                                                                         Author)) q) x
                                                                                                                               Peek_ReportView_Branding q
                                                                                                                                                        x -> Peek_Report_Branding ((Path_Report_View :: Path_ReportView Branding ->
                                                                                                                                                                                                        Path_Report Branding) q) x
                                                                                                                               Peek_ReportView_MarkupPair q
                                                                                                                                                          x -> Peek_Report_MarkupPair ((Path_Report_View :: Path_ReportView ((Markup,
                                                                                                                                                                                                                              Markup)) ->
                                                                                                                                                                                                            Path_Report ((Markup,
                                                                                                                                                                                                                          Markup))) q) x
                                                                                                                               Peek_ReportView_MarkupPairs q
                                                                                                                                                           x -> Peek_Report_MarkupPairs ((Path_Report_View :: Path_ReportView (Order MarkupPairID
                                                                                                                                                                                                                                     ((Markup,
                                                                                                                                                                                                                                       Markup))) ->
                                                                                                                                                                                                              Path_Report (Order MarkupPairID
                                                                                                                                                                                                                                 ((Markup,
                                                                                                                                                                                                                                   Markup)))) q) x
                                                                                                                               Peek_ReportView_Markups q
                                                                                                                                                       x -> Peek_Report_Markups ((Path_Report_View :: Path_ReportView (Order MarkupID
                                                                                                                                                                                                                             Markup) ->
                                                                                                                                                                                                      Path_Report (Order MarkupID
                                                                                                                                                                                                                         Markup)) q) x
                                                                                                                               Peek_ReportView_MaybeReportIntendedUse q
                                                                                                                                                                      x -> Peek_Report_MaybeReportIntendedUse ((Path_Report_View :: Path_ReportView (Maybe ReportIntendedUse) ->
                                                                                                                                                                                                                                    Path_Report (Maybe ReportIntendedUse)) q) x
                                                                                                                               Peek_ReportView_ReportElem q
                                                                                                                                                          x -> Peek_Report_ReportElem ((Path_Report_View :: Path_ReportView ReportElem ->
                                                                                                                                                                                                            Path_Report ReportElem) q) x
                                                                                                                               Peek_ReportView_ReportElems q
                                                                                                                                                           x -> Peek_Report_ReportElems ((Path_Report_View :: Path_ReportView (Order ReportElemID
                                                                                                                                                                                                                                     ReportElem) ->
                                                                                                                                                                                                              Path_Report (Order ReportElemID
                                                                                                                                                                                                                                 ReportElem)) q) x
                                                                                                                               Peek_ReportView_ReportFlags q
                                                                                                                                                           x -> Peek_Report_ReportFlags ((Path_Report_View :: Path_ReportView ReportFlags ->
                                                                                                                                                                                                              Path_Report ReportFlags) q) x
                                                                                                                               Peek_ReportView_ReportStandard q
                                                                                                                                                              x -> Peek_Report_ReportStandard ((Path_Report_View :: Path_ReportView ReportStandard ->
                                                                                                                                                                                                                    Path_Report ReportStandard) q) x
                                                                                                                               Peek_ReportView_ReportStatus q
                                                                                                                                                            x -> Peek_Report_ReportStatus ((Path_Report_View :: Path_ReportView ReportStatus ->
                                                                                                                                                                                                                Path_Report ReportStatus) q) x
                                                                                                                               Peek_ReportView_ReportValueApproachInfo q
                                                                                                                                                                       x -> Peek_Report_ReportValueApproachInfo ((Path_Report_View :: Path_ReportView ReportValueApproachInfo ->
                                                                                                                                                                                                                                      Path_Report ReportValueApproachInfo) q) x
                                                                                                                               Peek_ReportView_ReportValueTypeInfo q
                                                                                                                                                                   x -> Peek_Report_ReportValueTypeInfo ((Path_Report_View :: Path_ReportView ReportValueTypeInfo ->
                                                                                                                                                                                                                              Path_Report ReportValueTypeInfo) q) x
                                                                                                                               Peek_ReportView_EUI q
                                                                                                                                                   x -> Peek_Report_EUI ((Path_Report_View :: Path_ReportView (Either URI
                                                                                                                                                                                                                      ImageFile) ->
                                                                                                                                                                                              Path_Report (Either URI
                                                                                                                                                                                                                  ImageFile)) q) x
                                                                                                                               Peek_ReportView_MEUI q
                                                                                                                                                    x -> Peek_Report_MEUI ((Path_Report_View :: Path_ReportView (Maybe (Either URI
                                                                                                                                                                                                                               ImageFile)) ->
                                                                                                                                                                                                Path_Report (Maybe (Either URI
                                                                                                                                                                                                                           ImageFile))) q) x
                                                                                                                               Peek_ReportView_MaybeImageFile q
                                                                                                                                                              x -> Peek_Report_MaybeImageFile ((Path_Report_View :: Path_ReportView (Maybe ImageFile) ->
                                                                                                                                                                                                                    Path_Report (Maybe ImageFile)) q) x
                                                                                                                               Peek_ReportView_ReportImage q
                                                                                                                                                           x -> Peek_Report_ReportImage ((Path_Report_View :: Path_ReportView ReportImage ->
                                                                                                                                                                                                              Path_Report ReportImage) q) x
                                                                                                                               Peek_ReportView_ReportImages q
                                                                                                                                                            x -> Peek_Report_ReportImages ((Path_Report_View :: Path_ReportView (Order ReportImageID
                                                                                                                                                                                                                                       ReportImage) ->
                                                                                                                                                                                                                Path_Report (Order ReportImageID
                                                                                                                                                                                                                                   ReportImage)) q) x
                                                                                                                               Peek_ReportView_ReadOnlyFilePath q
                                                                                                                                                                x -> Peek_Report_ReadOnlyFilePath ((Path_Report_View :: Path_ReportView (ReadOnly ([Char])) ->
                                                                                                                                                                                                                        Path_Report (ReadOnly ([Char]))) q) x
                                                                                                                               Peek_ReportView_ReportImageView q
                                                                                                                                                               x -> Peek_Report_ReportImageView ((Path_Report_View :: Path_ReportView ReportImageView ->
                                                                                                                                                                                                                      Path_Report ReportImageView) q) x
                                                                                                                               Peek_ReportView_ReportView q
                                                                                                                                                          x -> Peek_Report_ReportView ((Path_Report_View :: Path_ReportView ReportView ->
                                                                                                                                                                                                            Path_Report ReportView) q) x
                                                                                                                               Peek_ReportView_SaneSizeImageSize q
                                                                                                                                                                 x -> Peek_Report_SaneSizeImageSize ((Path_Report_View :: Path_ReportView (SaneSize ImageSize) ->
                                                                                                                                                                                                                          Path_Report (SaneSize ImageSize)) q) x
                                                                                                                               Peek_ReportView_Item q
                                                                                                                                                    x -> Peek_Report_Item ((Path_Report_View :: Path_ReportView Item ->
                                                                                                                                                                                                Path_Report Item) q) x
                                                                                                                               Peek_ReportView_MIM q
                                                                                                                                                   x -> Peek_Report_MIM ((Path_Report_View :: Path_ReportView (Map ItemFieldName
                                                                                                                                                                                                                   Markup) ->
                                                                                                                                                                                              Path_Report (Map ItemFieldName
                                                                                                                                                                                                               Markup)) q) x
                                                                                                                               Peek_ReportView_CIString q
                                                                                                                                                        x -> Peek_Report_CIString ((Path_Report_View :: Path_ReportView CIString ->
                                                                                                                                                                                                        Path_Report CIString) q) x
                                                                                                                               Peek_ReportView_URI q
                                                                                                                                                   x -> Peek_Report_URI ((Path_Report_View :: Path_ReportView URI ->
                                                                                                                                                                                              Path_Report URI) q) x
                                                                                                                               Peek_ReportView_Text q
                                                                                                                                                    x -> Peek_Report_Text ((Path_Report_View :: Path_ReportView Text ->
                                                                                                                                                                                                Path_Report Text) q) x
                                                                                                                               Peek_ReportView_UserId q
                                                                                                                                                      x -> Peek_Report_UserId ((Path_Report_View :: Path_ReportView UserId ->
                                                                                                                                                                                                    Path_Report UserId) q) x
                                                                                                                               Peek_ReportView_UUID q
                                                                                                                                                    x -> Peek_Report_UUID ((Path_Report_View :: Path_ReportView UUID ->
                                                                                                                                                                                                Path_Report UUID) q) x) (peek y :: Forest (Peek ReportView)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ReportElem
    where data Peek ReportElem
              = Peek_ReportElem_String (Path_ReportElem ([Char])) ([Char])
              | Peek_ReportElem_Bool (Path_ReportElem Bool) Bool
              | Peek_ReportElem_Double (Path_ReportElem Double) Double
              | Peek_ReportElem_Dimension (Path_ReportElem Dimension) Dimension
              | Peek_ReportElem_ImageCrop (Path_ReportElem ImageCrop) ImageCrop
              | Peek_ReportElem_ImageSize (Path_ReportElem ImageSize) ImageSize
              | Peek_ReportElem_Units (Path_ReportElem Units) Units
              | Peek_ReportElem_ImageFile (Path_ReportElem ImageFile) ImageFile
              | Peek_ReportElem_JSONText (Path_ReportElem JSONText) JSONText
              | Peek_ReportElem_Markup (Path_ReportElem Markup) Markup
              | Peek_ReportElem_ReportElem (Path_ReportElem ReportElem)
                                           ReportElem
              | Peek_ReportElem_EUI (Path_ReportElem (Either URI ImageFile))
                                    (Either URI ImageFile)
              | Peek_ReportElem_MEUI (Path_ReportElem (Maybe (Either URI
                                                                     ImageFile)))
                                     (Maybe (Either URI ImageFile))
              | Peek_ReportElem_MaybeImageFile (Path_ReportElem (Maybe ImageFile))
                                               (Maybe ImageFile)
              | Peek_ReportElem_ReportImage (Path_ReportElem ReportImage)
                                            ReportImage
              | Peek_ReportElem_ReportImages (Path_ReportElem (Order ReportImageID
                                                                     ReportImage))
                                             (Order ReportImageID ReportImage)
              | Peek_ReportElem_ReportImageView (Path_ReportElem ReportImageView)
                                                ReportImageView
              | Peek_ReportElem_SaneSizeImageSize (Path_ReportElem (SaneSize ImageSize))
                                                  (SaneSize ImageSize)
              | Peek_ReportElem_Item (Path_ReportElem Item) Item
              | Peek_ReportElem_MIM (Path_ReportElem (Map ItemFieldName Markup))
                                    (Map ItemFieldName Markup)
              | Peek_ReportElem_URI (Path_ReportElem URI) URI
              | Peek_ReportElem_Text (Path_ReportElem Text) Text
              deriving (Eq, Show)
          peek (x@(ReportItem {})) = [case filter (\p -> case p of
                                                             Path_ReportElem_elemItem _ -> True
                                                             _ -> False) (pathsOf x (undefined :: Proxy Item)) :: [Path_ReportElem Item] of
                                          [p@(Path_ReportElem_elemItem _)] -> let [y] = toListOf (toLens p) x :: [Item]
                                                                               in Node (Peek_ReportElem_Item p y) (forestMap (\v -> case v of
                                                                                                                                        Peek_Item_String q
                                                                                                                                                         x -> Peek_ReportElem_String ((Path_ReportElem_elemItem :: Path_Item ([Char]) ->
                                                                                                                                                                                                                   Path_ReportElem ([Char])) q) x
                                                                                                                                        Peek_Item_Bool q
                                                                                                                                                       x -> Peek_ReportElem_Bool ((Path_ReportElem_elemItem :: Path_Item Bool ->
                                                                                                                                                                                                               Path_ReportElem Bool) q) x
                                                                                                                                        Peek_Item_Double q
                                                                                                                                                         x -> Peek_ReportElem_Double ((Path_ReportElem_elemItem :: Path_Item Double ->
                                                                                                                                                                                                                   Path_ReportElem Double) q) x
                                                                                                                                        Peek_Item_Dimension q
                                                                                                                                                            x -> Peek_ReportElem_Dimension ((Path_ReportElem_elemItem :: Path_Item Dimension ->
                                                                                                                                                                                                                         Path_ReportElem Dimension) q) x
                                                                                                                                        Peek_Item_ImageCrop q
                                                                                                                                                            x -> Peek_ReportElem_ImageCrop ((Path_ReportElem_elemItem :: Path_Item ImageCrop ->
                                                                                                                                                                                                                         Path_ReportElem ImageCrop) q) x
                                                                                                                                        Peek_Item_ImageSize q
                                                                                                                                                            x -> Peek_ReportElem_ImageSize ((Path_ReportElem_elemItem :: Path_Item ImageSize ->
                                                                                                                                                                                                                         Path_ReportElem ImageSize) q) x
                                                                                                                                        Peek_Item_Units q
                                                                                                                                                        x -> Peek_ReportElem_Units ((Path_ReportElem_elemItem :: Path_Item Units ->
                                                                                                                                                                                                                 Path_ReportElem Units) q) x
                                                                                                                                        Peek_Item_ImageFile q
                                                                                                                                                            x -> Peek_ReportElem_ImageFile ((Path_ReportElem_elemItem :: Path_Item ImageFile ->
                                                                                                                                                                                                                         Path_ReportElem ImageFile) q) x
                                                                                                                                        Peek_Item_JSONText q
                                                                                                                                                           x -> Peek_ReportElem_JSONText ((Path_ReportElem_elemItem :: Path_Item JSONText ->
                                                                                                                                                                                                                       Path_ReportElem JSONText) q) x
                                                                                                                                        Peek_Item_Markup q
                                                                                                                                                         x -> Peek_ReportElem_Markup ((Path_ReportElem_elemItem :: Path_Item Markup ->
                                                                                                                                                                                                                   Path_ReportElem Markup) q) x
                                                                                                                                        Peek_Item_EUI q
                                                                                                                                                      x -> Peek_ReportElem_EUI ((Path_ReportElem_elemItem :: Path_Item (Either URI
                                                                                                                                                                                                                               ImageFile) ->
                                                                                                                                                                                                             Path_ReportElem (Either URI
                                                                                                                                                                                                                                     ImageFile)) q) x
                                                                                                                                        Peek_Item_MEUI q
                                                                                                                                                       x -> Peek_ReportElem_MEUI ((Path_ReportElem_elemItem :: Path_Item (Maybe (Either URI
                                                                                                                                                                                                                                        ImageFile)) ->
                                                                                                                                                                                                               Path_ReportElem (Maybe (Either URI
                                                                                                                                                                                                                                              ImageFile))) q) x
                                                                                                                                        Peek_Item_MaybeImageFile q
                                                                                                                                                                 x -> Peek_ReportElem_MaybeImageFile ((Path_ReportElem_elemItem :: Path_Item (Maybe ImageFile) ->
                                                                                                                                                                                                                                   Path_ReportElem (Maybe ImageFile)) q) x
                                                                                                                                        Peek_Item_ReportImage q
                                                                                                                                                              x -> Peek_ReportElem_ReportImage ((Path_ReportElem_elemItem :: Path_Item ReportImage ->
                                                                                                                                                                                                                             Path_ReportElem ReportImage) q) x
                                                                                                                                        Peek_Item_ReportImages q
                                                                                                                                                               x -> Peek_ReportElem_ReportImages ((Path_ReportElem_elemItem :: Path_Item (Order ReportImageID
                                                                                                                                                                                                                                                ReportImage) ->
                                                                                                                                                                                                                               Path_ReportElem (Order ReportImageID
                                                                                                                                                                                                                                                      ReportImage)) q) x
                                                                                                                                        Peek_Item_ReportImageView q
                                                                                                                                                                  x -> Peek_ReportElem_ReportImageView ((Path_ReportElem_elemItem :: Path_Item ReportImageView ->
                                                                                                                                                                                                                                     Path_ReportElem ReportImageView) q) x
                                                                                                                                        Peek_Item_SaneSizeImageSize q
                                                                                                                                                                    x -> Peek_ReportElem_SaneSizeImageSize ((Path_ReportElem_elemItem :: Path_Item (SaneSize ImageSize) ->
                                                                                                                                                                                                                                         Path_ReportElem (SaneSize ImageSize)) q) x
                                                                                                                                        Peek_Item_Item q
                                                                                                                                                       x -> Peek_ReportElem_Item ((Path_ReportElem_elemItem :: Path_Item Item ->
                                                                                                                                                                                                               Path_ReportElem Item) q) x
                                                                                                                                        Peek_Item_MIM q
                                                                                                                                                      x -> Peek_ReportElem_MIM ((Path_ReportElem_elemItem :: Path_Item (Map ItemFieldName
                                                                                                                                                                                                                            Markup) ->
                                                                                                                                                                                                             Path_ReportElem (Map ItemFieldName
                                                                                                                                                                                                                                  Markup)) q) x
                                                                                                                                        Peek_Item_URI q
                                                                                                                                                      x -> Peek_ReportElem_URI ((Path_ReportElem_elemItem :: Path_Item URI ->
                                                                                                                                                                                                             Path_ReportElem URI) q) x
                                                                                                                                        Peek_Item_Text q
                                                                                                                                                       x -> Peek_ReportElem_Text ((Path_ReportElem_elemItem :: Path_Item Text ->
                                                                                                                                                                                                               Path_ReportElem Text) q) x) (peek y :: Forest (Peek Item)))
                                          [] -> error "No Path_ReportElem_elemItem field found"
                                          ps -> error $ ("Multiple Path_ReportElem_elemItem fields found: " ++ show ps)]
          peek (x@(ReportParagraph {})) = [case filter (\p -> case p of
                                                                  Path_ReportElem_elemText _ -> True
                                                                  _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportElem Markup] of
                                               [p@(Path_ReportElem_elemText _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                                    in Node (Peek_ReportElem_Markup p y) (forestMap (\v -> case v of
                                                                                                                                               Peek_Markup_JSONText q
                                                                                                                                                                    x -> Peek_ReportElem_JSONText ((Path_ReportElem_elemText :: Path_Markup JSONText ->
                                                                                                                                                                                                                                Path_ReportElem JSONText) q) x
                                                                                                                                               Peek_Markup_Markup q
                                                                                                                                                                  x -> Peek_ReportElem_Markup ((Path_ReportElem_elemText :: Path_Markup Markup ->
                                                                                                                                                                                                                            Path_ReportElem Markup) q) x
                                                                                                                                               Peek_Markup_Text q
                                                                                                                                                                x -> Peek_ReportElem_Text ((Path_ReportElem_elemText :: Path_Markup Text ->
                                                                                                                                                                                                                        Path_ReportElem Text) q) x) (peek y :: Forest (Peek Markup)))
                                               [] -> error "No Path_ReportElem_elemText field found"
                                               ps -> error $ ("Multiple Path_ReportElem_elemText fields found: " ++ show ps)]
          peek (x@(ReportUndecided {})) = []
instance IsPathNode ReportFlags
    where data Peek ReportFlags
              = Peek_ReportFlags_String (Path_ReportFlags ([Char])) ([Char])
              | Peek_ReportFlags_Bool (Path_ReportFlags Bool) Bool
              | Peek_ReportFlags_JSONText (Path_ReportFlags JSONText) JSONText
              | Peek_ReportFlags_ReportFlags (Path_ReportFlags ReportFlags)
                                             ReportFlags
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportFlags_hideEmptyItemFields _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Bool)) :: [Path_ReportFlags Bool] of
                        [p@(Path_ReportFlags_hideEmptyItemFields _)] -> let [y] = toListOf (toLens p) x :: [Bool]
                                                                         in Node (Peek_ReportFlags_Bool p y) (forestMap (\v -> case v of
                                                                                                                                   Peek_Bool_String q
                                                                                                                                                    x -> Peek_ReportFlags_String ((Path_ReportFlags_hideEmptyItemFields :: Path_Bool ([Char]) ->
                                                                                                                                                                                                                           Path_ReportFlags ([Char])) q) x
                                                                                                                                   Peek_Bool_Bool q
                                                                                                                                                  x -> Peek_ReportFlags_Bool ((Path_ReportFlags_hideEmptyItemFields :: Path_Bool Bool ->
                                                                                                                                                                                                                       Path_ReportFlags Bool) q) x
                                                                                                                                   Peek_Bool_JSONText q
                                                                                                                                                      x -> Peek_ReportFlags_JSONText ((Path_ReportFlags_hideEmptyItemFields :: Path_Bool JSONText ->
                                                                                                                                                                                                                               Path_ReportFlags JSONText) q) x) (peek y :: Forest (Peek Bool)))
                        [] -> error "No Path_ReportFlags_hideEmptyItemFields field found"
                        ps -> error $ ("Multiple Path_ReportFlags_hideEmptyItemFields fields found: " ++ show ps)]
instance IsPathNode ReportIntendedUse
    where data Peek ReportIntendedUse
              = Peek_ReportIntendedUse_String (Path_ReportIntendedUse ([Char]))
                                              ([Char])
              | Peek_ReportIntendedUse_JSONText (Path_ReportIntendedUse JSONText)
                                                JSONText
              | Peek_ReportIntendedUse_ReportIntendedUse (Path_ReportIntendedUse ReportIntendedUse)
                                                         ReportIntendedUse
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_ReportIntendedUse_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_ReportIntendedUse ([Char])]
                    in map (\path -> case path of
                                         p@(Path_ReportIntendedUse_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                               in Node (Peek_ReportIntendedUse_String p y) (forestMap (\v -> case v of
                                                                                                                                                 Peek_String_String q
                                                                                                                                                                    x -> Peek_ReportIntendedUse_String ((Path_ReportIntendedUse_View :: Path_String ([Char]) ->
                                                                                                                                                                                                                                        Path_ReportIntendedUse ([Char])) q) x
                                                                                                                                                 Peek_String_JSONText q
                                                                                                                                                                      x -> Peek_ReportIntendedUse_JSONText ((Path_ReportIntendedUse_View :: Path_String JSONText ->
                                                                                                                                                                                                                                            Path_ReportIntendedUse JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ReportStandard
    where data Peek ReportStandard
              = Peek_ReportStandard_Int (Path_ReportStandard Int) Int
              | Peek_ReportStandard_ReportStandard (Path_ReportStandard ReportStandard)
                                                   ReportStandard
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportStandard_unReportStandard _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Int)) :: [Path_ReportStandard Int] of
                        [p@(Path_ReportStandard_unReportStandard _)] -> let [y] = toListOf (toLens p) x :: [Int]
                                                                         in Node (Peek_ReportStandard_Int p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_Int_Int q
                                                                                                                                                  x -> Peek_ReportStandard_Int ((Path_ReportStandard_unReportStandard :: Path_Int Int ->
                                                                                                                                                                                                                         Path_ReportStandard Int) q) x) (peek y :: Forest (Peek Int)))
                        [] -> error "No Path_ReportStandard_unReportStandard field found"
                        ps -> error $ ("Multiple Path_ReportStandard_unReportStandard fields found: " ++ show ps)]
instance IsPathNode ReportStatus
    where data Peek ReportStatus
              = Peek_ReportStatus_String (Path_ReportStatus ([Char])) ([Char])
              | Peek_ReportStatus_JSONText (Path_ReportStatus JSONText) JSONText
              | Peek_ReportStatus_ReportStatus (Path_ReportStatus ReportStatus)
                                               ReportStatus
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_ReportStatus_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ([Char]))) :: [Path_ReportStatus ([Char])]
                    in map (\path -> case path of
                                         p@(Path_ReportStatus_View _) -> let [y] = toListOf (toLens p) x :: [[Char]]
                                                                          in Node (Peek_ReportStatus_String p y) (forestMap (\v -> case v of
                                                                                                                                       Peek_String_String q
                                                                                                                                                          x -> Peek_ReportStatus_String ((Path_ReportStatus_View :: Path_String ([Char]) ->
                                                                                                                                                                                                                    Path_ReportStatus ([Char])) q) x
                                                                                                                                       Peek_String_JSONText q
                                                                                                                                                            x -> Peek_ReportStatus_JSONText ((Path_ReportStatus_View :: Path_String JSONText ->
                                                                                                                                                                                                                        Path_ReportStatus JSONText) q) x) (peek y :: Forest (Peek ([Char]))))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ReportValueApproachInfo
    where data Peek ReportValueApproachInfo
              = Peek_ReportValueApproachInfo_JSONText (Path_ReportValueApproachInfo JSONText)
                                                      JSONText
              | Peek_ReportValueApproachInfo_Markup (Path_ReportValueApproachInfo Markup)
                                                    Markup
              | Peek_ReportValueApproachInfo_ReportValueApproachInfo (Path_ReportValueApproachInfo ReportValueApproachInfo)
                                                                     ReportValueApproachInfo
              | Peek_ReportValueApproachInfo_Text (Path_ReportValueApproachInfo Text)
                                                  Text
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportValueApproachInfo_reportValueApproachName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportValueApproachInfo Markup] of
                        [p@(Path_ReportValueApproachInfo_reportValueApproachName _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                                         in Node (Peek_ReportValueApproachInfo_Markup p y) (forestMap (\v -> case v of
                                                                                                                                                                 Peek_Markup_JSONText q
                                                                                                                                                                                      x -> Peek_ReportValueApproachInfo_JSONText ((Path_ReportValueApproachInfo_reportValueApproachName :: Path_Markup JSONText ->
                                                                                                                                                                                                                                                                                           Path_ReportValueApproachInfo JSONText) q) x
                                                                                                                                                                 Peek_Markup_Markup q
                                                                                                                                                                                    x -> Peek_ReportValueApproachInfo_Markup ((Path_ReportValueApproachInfo_reportValueApproachName :: Path_Markup Markup ->
                                                                                                                                                                                                                                                                                       Path_ReportValueApproachInfo Markup) q) x
                                                                                                                                                                 Peek_Markup_Text q
                                                                                                                                                                                  x -> Peek_ReportValueApproachInfo_Text ((Path_ReportValueApproachInfo_reportValueApproachName :: Path_Markup Text ->
                                                                                                                                                                                                                                                                                   Path_ReportValueApproachInfo Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportValueApproachInfo_reportValueApproachName field found"
                        ps -> error $ ("Multiple Path_ReportValueApproachInfo_reportValueApproachName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportValueApproachInfo_reportValueApproachDescription _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportValueApproachInfo Markup] of
                        [p@(Path_ReportValueApproachInfo_reportValueApproachDescription _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                                                in Node (Peek_ReportValueApproachInfo_Markup p y) (forestMap (\v -> case v of
                                                                                                                                                                        Peek_Markup_JSONText q
                                                                                                                                                                                             x -> Peek_ReportValueApproachInfo_JSONText ((Path_ReportValueApproachInfo_reportValueApproachDescription :: Path_Markup JSONText ->
                                                                                                                                                                                                                                                                                                         Path_ReportValueApproachInfo JSONText) q) x
                                                                                                                                                                        Peek_Markup_Markup q
                                                                                                                                                                                           x -> Peek_ReportValueApproachInfo_Markup ((Path_ReportValueApproachInfo_reportValueApproachDescription :: Path_Markup Markup ->
                                                                                                                                                                                                                                                                                                     Path_ReportValueApproachInfo Markup) q) x
                                                                                                                                                                        Peek_Markup_Text q
                                                                                                                                                                                         x -> Peek_ReportValueApproachInfo_Text ((Path_ReportValueApproachInfo_reportValueApproachDescription :: Path_Markup Text ->
                                                                                                                                                                                                                                                                                                 Path_ReportValueApproachInfo Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportValueApproachInfo_reportValueApproachDescription field found"
                        ps -> error $ ("Multiple Path_ReportValueApproachInfo_reportValueApproachDescription fields found: " ++ show ps)]
instance IsPathNode ReportValueTypeInfo
    where data Peek ReportValueTypeInfo
              = Peek_ReportValueTypeInfo_JSONText (Path_ReportValueTypeInfo JSONText)
                                                  JSONText
              | Peek_ReportValueTypeInfo_Markup (Path_ReportValueTypeInfo Markup)
                                                Markup
              | Peek_ReportValueTypeInfo_ReportValueTypeInfo (Path_ReportValueTypeInfo ReportValueTypeInfo)
                                                             ReportValueTypeInfo
              | Peek_ReportValueTypeInfo_Text (Path_ReportValueTypeInfo Text)
                                              Text
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportValueTypeInfo_reportValueTypeName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportValueTypeInfo Markup] of
                        [p@(Path_ReportValueTypeInfo_reportValueTypeName _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                                 in Node (Peek_ReportValueTypeInfo_Markup p y) (forestMap (\v -> case v of
                                                                                                                                                     Peek_Markup_JSONText q
                                                                                                                                                                          x -> Peek_ReportValueTypeInfo_JSONText ((Path_ReportValueTypeInfo_reportValueTypeName :: Path_Markup JSONText ->
                                                                                                                                                                                                                                                                   Path_ReportValueTypeInfo JSONText) q) x
                                                                                                                                                     Peek_Markup_Markup q
                                                                                                                                                                        x -> Peek_ReportValueTypeInfo_Markup ((Path_ReportValueTypeInfo_reportValueTypeName :: Path_Markup Markup ->
                                                                                                                                                                                                                                                               Path_ReportValueTypeInfo Markup) q) x
                                                                                                                                                     Peek_Markup_Text q
                                                                                                                                                                      x -> Peek_ReportValueTypeInfo_Text ((Path_ReportValueTypeInfo_reportValueTypeName :: Path_Markup Text ->
                                                                                                                                                                                                                                                           Path_ReportValueTypeInfo Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportValueTypeInfo_reportValueTypeName field found"
                        ps -> error $ ("Multiple Path_ReportValueTypeInfo_reportValueTypeName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportValueTypeInfo_reportValueTypeDescription _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportValueTypeInfo Markup] of
                        [p@(Path_ReportValueTypeInfo_reportValueTypeDescription _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                                        in Node (Peek_ReportValueTypeInfo_Markup p y) (forestMap (\v -> case v of
                                                                                                                                                            Peek_Markup_JSONText q
                                                                                                                                                                                 x -> Peek_ReportValueTypeInfo_JSONText ((Path_ReportValueTypeInfo_reportValueTypeDescription :: Path_Markup JSONText ->
                                                                                                                                                                                                                                                                                 Path_ReportValueTypeInfo JSONText) q) x
                                                                                                                                                            Peek_Markup_Markup q
                                                                                                                                                                               x -> Peek_ReportValueTypeInfo_Markup ((Path_ReportValueTypeInfo_reportValueTypeDescription :: Path_Markup Markup ->
                                                                                                                                                                                                                                                                             Path_ReportValueTypeInfo Markup) q) x
                                                                                                                                                            Peek_Markup_Text q
                                                                                                                                                                             x -> Peek_ReportValueTypeInfo_Text ((Path_ReportValueTypeInfo_reportValueTypeDescription :: Path_Markup Text ->
                                                                                                                                                                                                                                                                         Path_ReportValueTypeInfo Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportValueTypeInfo_reportValueTypeDescription field found"
                        ps -> error $ ("Multiple Path_ReportValueTypeInfo_reportValueTypeDescription fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportValueTypeInfo_reportValueTypeDefinition _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportValueTypeInfo Markup] of
                        [p@(Path_ReportValueTypeInfo_reportValueTypeDefinition _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                                       in Node (Peek_ReportValueTypeInfo_Markup p y) (forestMap (\v -> case v of
                                                                                                                                                           Peek_Markup_JSONText q
                                                                                                                                                                                x -> Peek_ReportValueTypeInfo_JSONText ((Path_ReportValueTypeInfo_reportValueTypeDefinition :: Path_Markup JSONText ->
                                                                                                                                                                                                                                                                               Path_ReportValueTypeInfo JSONText) q) x
                                                                                                                                                           Peek_Markup_Markup q
                                                                                                                                                                              x -> Peek_ReportValueTypeInfo_Markup ((Path_ReportValueTypeInfo_reportValueTypeDefinition :: Path_Markup Markup ->
                                                                                                                                                                                                                                                                           Path_ReportValueTypeInfo Markup) q) x
                                                                                                                                                           Peek_Markup_Text q
                                                                                                                                                                            x -> Peek_ReportValueTypeInfo_Text ((Path_ReportValueTypeInfo_reportValueTypeDefinition :: Path_Markup Text ->
                                                                                                                                                                                                                                                                       Path_ReportValueTypeInfo Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportValueTypeInfo_reportValueTypeDefinition field found"
                        ps -> error $ ("Multiple Path_ReportValueTypeInfo_reportValueTypeDefinition fields found: " ++ show ps)]
instance IsPathNode ReportImage
    where data Peek ReportImage
              = Peek_ReportImage_String (Path_ReportImage ([Char])) ([Char])
              | Peek_ReportImage_Bool (Path_ReportImage Bool) Bool
              | Peek_ReportImage_Double (Path_ReportImage Double) Double
              | Peek_ReportImage_Dimension (Path_ReportImage Dimension) Dimension
              | Peek_ReportImage_ImageCrop (Path_ReportImage ImageCrop) ImageCrop
              | Peek_ReportImage_ImageSize (Path_ReportImage ImageSize) ImageSize
              | Peek_ReportImage_Units (Path_ReportImage Units) Units
              | Peek_ReportImage_ImageFile (Path_ReportImage ImageFile) ImageFile
              | Peek_ReportImage_JSONText (Path_ReportImage JSONText) JSONText
              | Peek_ReportImage_Markup (Path_ReportImage Markup) Markup
              | Peek_ReportImage_EUI (Path_ReportImage (Either URI ImageFile))
                                     (Either URI ImageFile)
              | Peek_ReportImage_MEUI (Path_ReportImage (Maybe (Either URI
                                                                       ImageFile)))
                                      (Maybe (Either URI ImageFile))
              | Peek_ReportImage_MaybeImageFile (Path_ReportImage (Maybe ImageFile))
                                                (Maybe ImageFile)
              | Peek_ReportImage_ReportImage (Path_ReportImage ReportImage)
                                             ReportImage
              | Peek_ReportImage_ReportImageView (Path_ReportImage ReportImageView)
                                                 ReportImageView
              | Peek_ReportImage_SaneSizeImageSize (Path_ReportImage (SaneSize ImageSize))
                                                   (SaneSize ImageSize)
              | Peek_ReportImage_URI (Path_ReportImage URI) URI
              | Peek_ReportImage_Text (Path_ReportImage Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_ReportImage_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy ReportImageView)) :: [Path_ReportImage ReportImageView]
                    in map (\path -> case path of
                                         p@(Path_ReportImage_View _) -> let [y] = toListOf (toLens p) x :: [ReportImageView]
                                                                         in Node (Peek_ReportImage_ReportImageView p y) (forestMap (\v -> case v of
                                                                                                                                              Peek_ReportImageView_String q
                                                                                                                                                                          x -> Peek_ReportImage_String ((Path_ReportImage_View :: Path_ReportImageView ([Char]) ->
                                                                                                                                                                                                                                  Path_ReportImage ([Char])) q) x
                                                                                                                                              Peek_ReportImageView_Bool q
                                                                                                                                                                        x -> Peek_ReportImage_Bool ((Path_ReportImage_View :: Path_ReportImageView Bool ->
                                                                                                                                                                                                                              Path_ReportImage Bool) q) x
                                                                                                                                              Peek_ReportImageView_Double q
                                                                                                                                                                          x -> Peek_ReportImage_Double ((Path_ReportImage_View :: Path_ReportImageView Double ->
                                                                                                                                                                                                                                  Path_ReportImage Double) q) x
                                                                                                                                              Peek_ReportImageView_Dimension q
                                                                                                                                                                             x -> Peek_ReportImage_Dimension ((Path_ReportImage_View :: Path_ReportImageView Dimension ->
                                                                                                                                                                                                                                        Path_ReportImage Dimension) q) x
                                                                                                                                              Peek_ReportImageView_ImageCrop q
                                                                                                                                                                             x -> Peek_ReportImage_ImageCrop ((Path_ReportImage_View :: Path_ReportImageView ImageCrop ->
                                                                                                                                                                                                                                        Path_ReportImage ImageCrop) q) x
                                                                                                                                              Peek_ReportImageView_ImageSize q
                                                                                                                                                                             x -> Peek_ReportImage_ImageSize ((Path_ReportImage_View :: Path_ReportImageView ImageSize ->
                                                                                                                                                                                                                                        Path_ReportImage ImageSize) q) x
                                                                                                                                              Peek_ReportImageView_Units q
                                                                                                                                                                         x -> Peek_ReportImage_Units ((Path_ReportImage_View :: Path_ReportImageView Units ->
                                                                                                                                                                                                                                Path_ReportImage Units) q) x
                                                                                                                                              Peek_ReportImageView_ImageFile q
                                                                                                                                                                             x -> Peek_ReportImage_ImageFile ((Path_ReportImage_View :: Path_ReportImageView ImageFile ->
                                                                                                                                                                                                                                        Path_ReportImage ImageFile) q) x
                                                                                                                                              Peek_ReportImageView_JSONText q
                                                                                                                                                                            x -> Peek_ReportImage_JSONText ((Path_ReportImage_View :: Path_ReportImageView JSONText ->
                                                                                                                                                                                                                                      Path_ReportImage JSONText) q) x
                                                                                                                                              Peek_ReportImageView_Markup q
                                                                                                                                                                          x -> Peek_ReportImage_Markup ((Path_ReportImage_View :: Path_ReportImageView Markup ->
                                                                                                                                                                                                                                  Path_ReportImage Markup) q) x
                                                                                                                                              Peek_ReportImageView_EUI q
                                                                                                                                                                       x -> Peek_ReportImage_EUI ((Path_ReportImage_View :: Path_ReportImageView (Either URI
                                                                                                                                                                                                                                                         ImageFile) ->
                                                                                                                                                                                                                            Path_ReportImage (Either URI
                                                                                                                                                                                                                                                     ImageFile)) q) x
                                                                                                                                              Peek_ReportImageView_MEUI q
                                                                                                                                                                        x -> Peek_ReportImage_MEUI ((Path_ReportImage_View :: Path_ReportImageView (Maybe (Either URI
                                                                                                                                                                                                                                                                  ImageFile)) ->
                                                                                                                                                                                                                              Path_ReportImage (Maybe (Either URI
                                                                                                                                                                                                                                                              ImageFile))) q) x
                                                                                                                                              Peek_ReportImageView_MaybeImageFile q
                                                                                                                                                                                  x -> Peek_ReportImage_MaybeImageFile ((Path_ReportImage_View :: Path_ReportImageView (Maybe ImageFile) ->
                                                                                                                                                                                                                                                  Path_ReportImage (Maybe ImageFile)) q) x
                                                                                                                                              Peek_ReportImageView_ReportImageView q
                                                                                                                                                                                   x -> Peek_ReportImage_ReportImageView ((Path_ReportImage_View :: Path_ReportImageView ReportImageView ->
                                                                                                                                                                                                                                                    Path_ReportImage ReportImageView) q) x
                                                                                                                                              Peek_ReportImageView_SaneSizeImageSize q
                                                                                                                                                                                     x -> Peek_ReportImage_SaneSizeImageSize ((Path_ReportImage_View :: Path_ReportImageView (SaneSize ImageSize) ->
                                                                                                                                                                                                                                                        Path_ReportImage (SaneSize ImageSize)) q) x
                                                                                                                                              Peek_ReportImageView_URI q
                                                                                                                                                                       x -> Peek_ReportImage_URI ((Path_ReportImage_View :: Path_ReportImageView URI ->
                                                                                                                                                                                                                            Path_ReportImage URI) q) x
                                                                                                                                              Peek_ReportImageView_Text q
                                                                                                                                                                        x -> Peek_ReportImage_Text ((Path_ReportImage_View :: Path_ReportImageView Text ->
                                                                                                                                                                                                                              Path_ReportImage Text) q) x) (peek y :: Forest (Peek ReportImageView)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode ReportImageView
    where data Peek ReportImageView
              = Peek_ReportImageView_String (Path_ReportImageView ([Char]))
                                            ([Char])
              | Peek_ReportImageView_Bool (Path_ReportImageView Bool) Bool
              | Peek_ReportImageView_Double (Path_ReportImageView Double) Double
              | Peek_ReportImageView_Dimension (Path_ReportImageView Dimension)
                                               Dimension
              | Peek_ReportImageView_ImageCrop (Path_ReportImageView ImageCrop)
                                               ImageCrop
              | Peek_ReportImageView_ImageSize (Path_ReportImageView ImageSize)
                                               ImageSize
              | Peek_ReportImageView_Units (Path_ReportImageView Units) Units
              | Peek_ReportImageView_ImageFile (Path_ReportImageView ImageFile)
                                               ImageFile
              | Peek_ReportImageView_JSONText (Path_ReportImageView JSONText)
                                              JSONText
              | Peek_ReportImageView_Markup (Path_ReportImageView Markup) Markup
              | Peek_ReportImageView_EUI (Path_ReportImageView (Either URI
                                                                       ImageFile))
                                         (Either URI ImageFile)
              | Peek_ReportImageView_MEUI (Path_ReportImageView (Maybe (Either URI
                                                                               ImageFile)))
                                          (Maybe (Either URI ImageFile))
              | Peek_ReportImageView_MaybeImageFile (Path_ReportImageView (Maybe ImageFile))
                                                    (Maybe ImageFile)
              | Peek_ReportImageView_ReportImageView (Path_ReportImageView ReportImageView)
                                                     ReportImageView
              | Peek_ReportImageView_SaneSizeImageSize (Path_ReportImageView (SaneSize ImageSize))
                                                       (SaneSize ImageSize)
              | Peek_ReportImageView_URI (Path_ReportImageView URI) URI
              | Peek_ReportImageView_Text (Path_ReportImageView Text) Text
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportImageView__picSize _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (SaneSize ImageSize))) :: [Path_ReportImageView (SaneSize ImageSize)] of
                        [p@(Path_ReportImageView__picSize _)] -> let [y] = toListOf (toLens p) x :: [SaneSize ImageSize]
                                                                  in Node (Peek_ReportImageView_SaneSizeImageSize p y) (forestMap (\v -> case v of
                                                                                                                                             Peek_SaneSizeImageSize_String q
                                                                                                                                                                           x -> Peek_ReportImageView_String ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize ([Char]) ->
                                                                                                                                                                                                                                               Path_ReportImageView ([Char])) q) x
                                                                                                                                             Peek_SaneSizeImageSize_Double q
                                                                                                                                                                           x -> Peek_ReportImageView_Double ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize Double ->
                                                                                                                                                                                                                                               Path_ReportImageView Double) q) x
                                                                                                                                             Peek_SaneSizeImageSize_Dimension q
                                                                                                                                                                              x -> Peek_ReportImageView_Dimension ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize Dimension ->
                                                                                                                                                                                                                                                     Path_ReportImageView Dimension) q) x
                                                                                                                                             Peek_SaneSizeImageSize_ImageSize q
                                                                                                                                                                              x -> Peek_ReportImageView_ImageSize ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize ImageSize ->
                                                                                                                                                                                                                                                     Path_ReportImageView ImageSize) q) x
                                                                                                                                             Peek_SaneSizeImageSize_Units q
                                                                                                                                                                          x -> Peek_ReportImageView_Units ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize Units ->
                                                                                                                                                                                                                                             Path_ReportImageView Units) q) x
                                                                                                                                             Peek_SaneSizeImageSize_JSONText q
                                                                                                                                                                             x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize JSONText ->
                                                                                                                                                                                                                                                   Path_ReportImageView JSONText) q) x
                                                                                                                                             Peek_SaneSizeImageSize_SaneSizeImageSize q
                                                                                                                                                                                      x -> Peek_ReportImageView_SaneSizeImageSize ((Path_ReportImageView__picSize :: Path_SaneSizeImageSize (SaneSize ImageSize) ->
                                                                                                                                                                                                                                                                     Path_ReportImageView (SaneSize ImageSize)) q) x) (peek y :: Forest (Peek (SaneSize ImageSize))))
                        [] -> error "No Path_ReportImageView__picSize field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picSize fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picCrop _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ImageCrop)) :: [Path_ReportImageView ImageCrop] of
                        [p@(Path_ReportImageView__picCrop _)] -> let [y] = toListOf (toLens p) x :: [ImageCrop]
                                                                  in Node (Peek_ReportImageView_ImageCrop p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_ImageCrop_ImageCrop q
                                                                                                                                                              x -> Peek_ReportImageView_ImageCrop ((Path_ReportImageView__picCrop :: Path_ImageCrop ImageCrop ->
                                                                                                                                                                                                                                     Path_ReportImageView ImageCrop) q) x) (peek y :: Forest (Peek ImageCrop)))
                        [] -> error "No Path_ReportImageView__picCrop field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picCrop fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picCaption _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportImageView Markup] of
                        [p@(Path_ReportImageView__picCaption _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                     in Node (Peek_ReportImageView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_Markup_JSONText q
                                                                                                                                                          x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picCaption :: Path_Markup JSONText ->
                                                                                                                                                                                                                                   Path_ReportImageView JSONText) q) x
                                                                                                                                     Peek_Markup_Markup q
                                                                                                                                                        x -> Peek_ReportImageView_Markup ((Path_ReportImageView__picCaption :: Path_Markup Markup ->
                                                                                                                                                                                                                               Path_ReportImageView Markup) q) x
                                                                                                                                     Peek_Markup_Text q
                                                                                                                                                      x -> Peek_ReportImageView_Text ((Path_ReportImageView__picCaption :: Path_Markup Text ->
                                                                                                                                                                                                                           Path_ReportImageView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportImageView__picCaption field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picCaption fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picOriginal _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Maybe (Either URI
                                                                                                     ImageFile)))) :: [Path_ReportImageView (Maybe (Either URI
                                                                                                                                                           ImageFile))] of
                        [p@(Path_ReportImageView__picOriginal _)] -> let [y] = toListOf (toLens p) x :: [Maybe (Either URI
                                                                                                                       ImageFile)]
                                                                      in Node (Peek_ReportImageView_MEUI p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_MEUI_ImageFile q
                                                                                                                                                        x -> Peek_ReportImageView_ImageFile ((Path_ReportImageView__picOriginal :: Path_MEUI ImageFile ->
                                                                                                                                                                                                                                   Path_ReportImageView ImageFile) q) x
                                                                                                                                    Peek_MEUI_EUI q
                                                                                                                                                  x -> Peek_ReportImageView_EUI ((Path_ReportImageView__picOriginal :: Path_MEUI (Either URI
                                                                                                                                                                                                                                         ImageFile) ->
                                                                                                                                                                                                                       Path_ReportImageView (Either URI
                                                                                                                                                                                                                                                    ImageFile)) q) x
                                                                                                                                    Peek_MEUI_MEUI q
                                                                                                                                                   x -> Peek_ReportImageView_MEUI ((Path_ReportImageView__picOriginal :: Path_MEUI (Maybe (Either URI
                                                                                                                                                                                                                                                  ImageFile)) ->
                                                                                                                                                                                                                         Path_ReportImageView (Maybe (Either URI
                                                                                                                                                                                                                                                             ImageFile))) q) x
                                                                                                                                    Peek_MEUI_URI q
                                                                                                                                                  x -> Peek_ReportImageView_URI ((Path_ReportImageView__picOriginal :: Path_MEUI URI ->
                                                                                                                                                                                                                       Path_ReportImageView URI) q) x) (peek y :: Forest (Peek (Maybe (Either URI
                                                                                                                                                                                                                                                                                              ImageFile)))))
                        [] -> error "No Path_ReportImageView__picOriginal field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picOriginal fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picEditedDeprecated _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Maybe ImageFile))) :: [Path_ReportImageView (Maybe ImageFile)] of
                        [p@(Path_ReportImageView__picEditedDeprecated _)] -> let [y] = toListOf (toLens p) x :: [Maybe ImageFile]
                                                                              in Node (Peek_ReportImageView_MaybeImageFile p y) (forestMap (\v -> case v of
                                                                                                                                                      Peek_MaybeImageFile_String q
                                                                                                                                                                                 x -> Peek_ReportImageView_String ((Path_ReportImageView__picEditedDeprecated :: Path_MaybeImageFile ([Char]) ->
                                                                                                                                                                                                                                                                 Path_ReportImageView ([Char])) q) x
                                                                                                                                                      Peek_MaybeImageFile_JSONText q
                                                                                                                                                                                   x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picEditedDeprecated :: Path_MaybeImageFile JSONText ->
                                                                                                                                                                                                                                                                     Path_ReportImageView JSONText) q) x
                                                                                                                                                      Peek_MaybeImageFile_MaybeImageFile q
                                                                                                                                                                                         x -> Peek_ReportImageView_MaybeImageFile ((Path_ReportImageView__picEditedDeprecated :: Path_MaybeImageFile (Maybe ImageFile) ->
                                                                                                                                                                                                                                                                                 Path_ReportImageView (Maybe ImageFile)) q) x) (peek y :: Forest (Peek (Maybe ImageFile))))
                        [] -> error "No Path_ReportImageView__picEditedDeprecated field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picEditedDeprecated fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picThumbDeprecated _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Maybe ImageFile))) :: [Path_ReportImageView (Maybe ImageFile)] of
                        [p@(Path_ReportImageView__picThumbDeprecated _)] -> let [y] = toListOf (toLens p) x :: [Maybe ImageFile]
                                                                             in Node (Peek_ReportImageView_MaybeImageFile p y) (forestMap (\v -> case v of
                                                                                                                                                     Peek_MaybeImageFile_String q
                                                                                                                                                                                x -> Peek_ReportImageView_String ((Path_ReportImageView__picThumbDeprecated :: Path_MaybeImageFile ([Char]) ->
                                                                                                                                                                                                                                                               Path_ReportImageView ([Char])) q) x
                                                                                                                                                     Peek_MaybeImageFile_JSONText q
                                                                                                                                                                                  x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picThumbDeprecated :: Path_MaybeImageFile JSONText ->
                                                                                                                                                                                                                                                                   Path_ReportImageView JSONText) q) x
                                                                                                                                                     Peek_MaybeImageFile_MaybeImageFile q
                                                                                                                                                                                        x -> Peek_ReportImageView_MaybeImageFile ((Path_ReportImageView__picThumbDeprecated :: Path_MaybeImageFile (Maybe ImageFile) ->
                                                                                                                                                                                                                                                                               Path_ReportImageView (Maybe ImageFile)) q) x) (peek y :: Forest (Peek (Maybe ImageFile))))
                        [] -> error "No Path_ReportImageView__picThumbDeprecated field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picThumbDeprecated fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picPrinterDeprecated _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Maybe ImageFile))) :: [Path_ReportImageView (Maybe ImageFile)] of
                        [p@(Path_ReportImageView__picPrinterDeprecated _)] -> let [y] = toListOf (toLens p) x :: [Maybe ImageFile]
                                                                               in Node (Peek_ReportImageView_MaybeImageFile p y) (forestMap (\v -> case v of
                                                                                                                                                       Peek_MaybeImageFile_String q
                                                                                                                                                                                  x -> Peek_ReportImageView_String ((Path_ReportImageView__picPrinterDeprecated :: Path_MaybeImageFile ([Char]) ->
                                                                                                                                                                                                                                                                   Path_ReportImageView ([Char])) q) x
                                                                                                                                                       Peek_MaybeImageFile_JSONText q
                                                                                                                                                                                    x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picPrinterDeprecated :: Path_MaybeImageFile JSONText ->
                                                                                                                                                                                                                                                                       Path_ReportImageView JSONText) q) x
                                                                                                                                                       Peek_MaybeImageFile_MaybeImageFile q
                                                                                                                                                                                          x -> Peek_ReportImageView_MaybeImageFile ((Path_ReportImageView__picPrinterDeprecated :: Path_MaybeImageFile (Maybe ImageFile) ->
                                                                                                                                                                                                                                                                                   Path_ReportImageView (Maybe ImageFile)) q) x) (peek y :: Forest (Peek (Maybe ImageFile))))
                        [] -> error "No Path_ReportImageView__picPrinterDeprecated field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picPrinterDeprecated fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picMustEnlarge _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Bool)) :: [Path_ReportImageView Bool] of
                        [p@(Path_ReportImageView__picMustEnlarge _)] -> let [y] = toListOf (toLens p) x :: [Bool]
                                                                         in Node (Peek_ReportImageView_Bool p y) (forestMap (\v -> case v of
                                                                                                                                       Peek_Bool_String q
                                                                                                                                                        x -> Peek_ReportImageView_String ((Path_ReportImageView__picMustEnlarge :: Path_Bool ([Char]) ->
                                                                                                                                                                                                                                   Path_ReportImageView ([Char])) q) x
                                                                                                                                       Peek_Bool_Bool q
                                                                                                                                                      x -> Peek_ReportImageView_Bool ((Path_ReportImageView__picMustEnlarge :: Path_Bool Bool ->
                                                                                                                                                                                                                               Path_ReportImageView Bool) q) x
                                                                                                                                       Peek_Bool_JSONText q
                                                                                                                                                          x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picMustEnlarge :: Path_Bool JSONText ->
                                                                                                                                                                                                                                       Path_ReportImageView JSONText) q) x) (peek y :: Forest (Peek Bool)))
                        [] -> error "No Path_ReportImageView__picMustEnlarge field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picMustEnlarge fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportImageView__picEnlargedDeprecated _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Maybe ImageFile))) :: [Path_ReportImageView (Maybe ImageFile)] of
                        [p@(Path_ReportImageView__picEnlargedDeprecated _)] -> let [y] = toListOf (toLens p) x :: [Maybe ImageFile]
                                                                                in Node (Peek_ReportImageView_MaybeImageFile p y) (forestMap (\v -> case v of
                                                                                                                                                        Peek_MaybeImageFile_String q
                                                                                                                                                                                   x -> Peek_ReportImageView_String ((Path_ReportImageView__picEnlargedDeprecated :: Path_MaybeImageFile ([Char]) ->
                                                                                                                                                                                                                                                                     Path_ReportImageView ([Char])) q) x
                                                                                                                                                        Peek_MaybeImageFile_JSONText q
                                                                                                                                                                                     x -> Peek_ReportImageView_JSONText ((Path_ReportImageView__picEnlargedDeprecated :: Path_MaybeImageFile JSONText ->
                                                                                                                                                                                                                                                                         Path_ReportImageView JSONText) q) x
                                                                                                                                                        Peek_MaybeImageFile_MaybeImageFile q
                                                                                                                                                                                           x -> Peek_ReportImageView_MaybeImageFile ((Path_ReportImageView__picEnlargedDeprecated :: Path_MaybeImageFile (Maybe ImageFile) ->
                                                                                                                                                                                                                                                                                     Path_ReportImageView (Maybe ImageFile)) q) x) (peek y :: Forest (Peek (Maybe ImageFile))))
                        [] -> error "No Path_ReportImageView__picEnlargedDeprecated field found"
                        ps -> error $ ("Multiple Path_ReportImageView__picEnlargedDeprecated fields found: " ++ show ps)]
instance IsPathNode ReportView
    where data Peek ReportView
              = Peek_ReportView_String (Path_ReportView ([Char])) ([Char])
              | Peek_ReportView_Int64 (Path_ReportView Int64) Int64
              | Peek_ReportView_Int (Path_ReportView Int) Int
              | Peek_ReportView_Bool (Path_ReportView Bool) Bool
              | Peek_ReportView_Double (Path_ReportView Double) Double
              | Peek_ReportView_Dimension (Path_ReportView Dimension) Dimension
              | Peek_ReportView_ImageCrop (Path_ReportView ImageCrop) ImageCrop
              | Peek_ReportView_ImageSize (Path_ReportView ImageSize) ImageSize
              | Peek_ReportView_Units (Path_ReportView Units) Units
              | Peek_ReportView_ImageFile (Path_ReportView ImageFile) ImageFile
              | Peek_ReportView_Integer (Path_ReportView Integer) Integer
              | Peek_ReportView_JSONText (Path_ReportView JSONText) JSONText
              | Peek_ReportView_Markup (Path_ReportView Markup) Markup
              | Peek_ReportView_Permissions (Path_ReportView Permissions)
                                            Permissions
              | Peek_ReportView_UserIds (Path_ReportView ([UserId])) ([UserId])
              | Peek_ReportView_AbbrevPair (Path_ReportView ((CIString, Markup)))
                                           ((CIString, Markup))
              | Peek_ReportView_AbbrevPairs (Path_ReportView (Order AbbrevPairID
                                                                    ((CIString, Markup))))
                                            (Order AbbrevPairID ((CIString, Markup)))
              | Peek_ReportView_Author (Path_ReportView Author) Author
              | Peek_ReportView_Authors (Path_ReportView (Order AuthorID Author))
                                        (Order AuthorID Author)
              | Peek_ReportView_Branding (Path_ReportView Branding) Branding
              | Peek_ReportView_MarkupPair (Path_ReportView ((Markup, Markup)))
                                           ((Markup, Markup))
              | Peek_ReportView_MarkupPairs (Path_ReportView (Order MarkupPairID
                                                                    ((Markup, Markup))))
                                            (Order MarkupPairID ((Markup, Markup)))
              | Peek_ReportView_Markups (Path_ReportView (Order MarkupID Markup))
                                        (Order MarkupID Markup)
              | Peek_ReportView_MaybeReportIntendedUse (Path_ReportView (Maybe ReportIntendedUse))
                                                       (Maybe ReportIntendedUse)
              | Peek_ReportView_ReportElem (Path_ReportView ReportElem)
                                           ReportElem
              | Peek_ReportView_ReportElems (Path_ReportView (Order ReportElemID
                                                                    ReportElem))
                                            (Order ReportElemID ReportElem)
              | Peek_ReportView_ReportFlags (Path_ReportView ReportFlags)
                                            ReportFlags
              | Peek_ReportView_ReportStandard (Path_ReportView ReportStandard)
                                               ReportStandard
              | Peek_ReportView_ReportStatus (Path_ReportView ReportStatus)
                                             ReportStatus
              | Peek_ReportView_ReportValueApproachInfo (Path_ReportView ReportValueApproachInfo)
                                                        ReportValueApproachInfo
              | Peek_ReportView_ReportValueTypeInfo (Path_ReportView ReportValueTypeInfo)
                                                    ReportValueTypeInfo
              | Peek_ReportView_EUI (Path_ReportView (Either URI ImageFile))
                                    (Either URI ImageFile)
              | Peek_ReportView_MEUI (Path_ReportView (Maybe (Either URI
                                                                     ImageFile)))
                                     (Maybe (Either URI ImageFile))
              | Peek_ReportView_MaybeImageFile (Path_ReportView (Maybe ImageFile))
                                               (Maybe ImageFile)
              | Peek_ReportView_ReportImage (Path_ReportView ReportImage)
                                            ReportImage
              | Peek_ReportView_ReportImages (Path_ReportView (Order ReportImageID
                                                                     ReportImage))
                                             (Order ReportImageID ReportImage)
              | Peek_ReportView_ReadOnlyFilePath (Path_ReportView (ReadOnly ([Char])))
                                                 (ReadOnly ([Char]))
              | Peek_ReportView_ReportImageView (Path_ReportView ReportImageView)
                                                ReportImageView
              | Peek_ReportView_ReportView (Path_ReportView ReportView)
                                           ReportView
              | Peek_ReportView_SaneSizeImageSize (Path_ReportView (SaneSize ImageSize))
                                                  (SaneSize ImageSize)
              | Peek_ReportView_Item (Path_ReportView Item) Item
              | Peek_ReportView_MIM (Path_ReportView (Map ItemFieldName Markup))
                                    (Map ItemFieldName Markup)
              | Peek_ReportView_CIString (Path_ReportView CIString) CIString
              | Peek_ReportView_URI (Path_ReportView URI) URI
              | Peek_ReportView_Text (Path_ReportView Text) Text
              | Peek_ReportView_UserId (Path_ReportView UserId) UserId
              | Peek_ReportView_UUID (Path_ReportView UUID) UUID
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportView__reportFolder _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (ReadOnly ([Char])))) :: [Path_ReportView (ReadOnly ([Char]))] of
                        [p@(Path_ReportView__reportFolder _)] -> let [y] = toListOf (toLens p) x :: [ReadOnly ([Char])]
                                                                  in Node (Peek_ReportView_ReadOnlyFilePath p y) (forestMap (\v -> case v of
                                                                                                                                       Peek_ReadOnlyFilePath_String q
                                                                                                                                                                    x -> Peek_ReportView_String ((Path_ReportView__reportFolder :: Path_ReadOnlyFilePath ([Char]) ->
                                                                                                                                                                                                                                   Path_ReportView ([Char])) q) x
                                                                                                                                       Peek_ReadOnlyFilePath_JSONText q
                                                                                                                                                                      x -> Peek_ReportView_JSONText ((Path_ReportView__reportFolder :: Path_ReadOnlyFilePath JSONText ->
                                                                                                                                                                                                                                       Path_ReportView JSONText) q) x
                                                                                                                                       Peek_ReadOnlyFilePath_ReadOnlyFilePath q
                                                                                                                                                                              x -> Peek_ReportView_ReadOnlyFilePath ((Path_ReportView__reportFolder :: Path_ReadOnlyFilePath (ReadOnly ([Char])) ->
                                                                                                                                                                                                                                                       Path_ReportView (ReadOnly ([Char]))) q) x) (peek y :: Forest (Peek (ReadOnly ([Char])))))
                        [] -> error "No Path_ReportView__reportFolder field found"
                        ps -> error $ ("Multiple Path_ReportView__reportFolder fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportName _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                           Peek_Markup_JSONText q
                                                                                                                                                x -> Peek_ReportView_JSONText ((Path_ReportView__reportName :: Path_Markup JSONText ->
                                                                                                                                                                                                               Path_ReportView JSONText) q) x
                                                                                                                           Peek_Markup_Markup q
                                                                                                                                              x -> Peek_ReportView_Markup ((Path_ReportView__reportName :: Path_Markup Markup ->
                                                                                                                                                                                                           Path_ReportView Markup) q) x
                                                                                                                           Peek_Markup_Text q
                                                                                                                                            x -> Peek_ReportView_Text ((Path_ReportView__reportName :: Path_Markup Text ->
                                                                                                                                                                                                       Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportName field found"
                        ps -> error $ ("Multiple Path_ReportView__reportName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportDate _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportDate _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                           Peek_Markup_JSONText q
                                                                                                                                                x -> Peek_ReportView_JSONText ((Path_ReportView__reportDate :: Path_Markup JSONText ->
                                                                                                                                                                                                               Path_ReportView JSONText) q) x
                                                                                                                           Peek_Markup_Markup q
                                                                                                                                              x -> Peek_ReportView_Markup ((Path_ReportView__reportDate :: Path_Markup Markup ->
                                                                                                                                                                                                           Path_ReportView Markup) q) x
                                                                                                                           Peek_Markup_Text q
                                                                                                                                            x -> Peek_ReportView_Text ((Path_ReportView__reportDate :: Path_Markup Text ->
                                                                                                                                                                                                       Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportDate field found"
                        ps -> error $ ("Multiple Path_ReportView__reportDate fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportContractDate _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportContractDate _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                        in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                   Peek_Markup_JSONText q
                                                                                                                                                        x -> Peek_ReportView_JSONText ((Path_ReportView__reportContractDate :: Path_Markup JSONText ->
                                                                                                                                                                                                                               Path_ReportView JSONText) q) x
                                                                                                                                   Peek_Markup_Markup q
                                                                                                                                                      x -> Peek_ReportView_Markup ((Path_ReportView__reportContractDate :: Path_Markup Markup ->
                                                                                                                                                                                                                           Path_ReportView Markup) q) x
                                                                                                                                   Peek_Markup_Text q
                                                                                                                                                    x -> Peek_ReportView_Text ((Path_ReportView__reportContractDate :: Path_Markup Text ->
                                                                                                                                                                                                                       Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportContractDate field found"
                        ps -> error $ ("Multiple Path_ReportView__reportContractDate fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportInspectionDate _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportInspectionDate _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                          in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_Markup_JSONText q
                                                                                                                                                          x -> Peek_ReportView_JSONText ((Path_ReportView__reportInspectionDate :: Path_Markup JSONText ->
                                                                                                                                                                                                                                   Path_ReportView JSONText) q) x
                                                                                                                                     Peek_Markup_Markup q
                                                                                                                                                        x -> Peek_ReportView_Markup ((Path_ReportView__reportInspectionDate :: Path_Markup Markup ->
                                                                                                                                                                                                                               Path_ReportView Markup) q) x
                                                                                                                                     Peek_Markup_Text q
                                                                                                                                                      x -> Peek_ReportView_Text ((Path_ReportView__reportInspectionDate :: Path_Markup Text ->
                                                                                                                                                                                                                           Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportInspectionDate field found"
                        ps -> error $ ("Multiple Path_ReportView__reportInspectionDate fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportEffectiveDate _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportEffectiveDate _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                         in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_Markup_JSONText q
                                                                                                                                                         x -> Peek_ReportView_JSONText ((Path_ReportView__reportEffectiveDate :: Path_Markup JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x
                                                                                                                                    Peek_Markup_Markup q
                                                                                                                                                       x -> Peek_ReportView_Markup ((Path_ReportView__reportEffectiveDate :: Path_Markup Markup ->
                                                                                                                                                                                                                             Path_ReportView Markup) q) x
                                                                                                                                    Peek_Markup_Text q
                                                                                                                                                     x -> Peek_ReportView_Text ((Path_ReportView__reportEffectiveDate :: Path_Markup Text ->
                                                                                                                                                                                                                         Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportEffectiveDate field found"
                        ps -> error $ ("Multiple Path_ReportView__reportEffectiveDate fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportAuthors _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order AuthorID
                                                                                             Author))) :: [Path_ReportView (Order AuthorID
                                                                                                                                  Author)] of
                        [p@(Path_ReportView__reportAuthors _)] -> let [y] = toListOf (toLens p) x :: [Order AuthorID
                                                                                                            Author]
                                                                   in Node (Peek_ReportView_Authors p y) (forestMap (\v -> case v of
                                                                                                                               Peek_Authors_JSONText q
                                                                                                                                                     x -> Peek_ReportView_JSONText ((Path_ReportView__reportAuthors :: Path_Authors JSONText ->
                                                                                                                                                                                                                       Path_ReportView JSONText) q) x
                                                                                                                               Peek_Authors_Markup q
                                                                                                                                                   x -> Peek_ReportView_Markup ((Path_ReportView__reportAuthors :: Path_Authors Markup ->
                                                                                                                                                                                                                   Path_ReportView Markup) q) x
                                                                                                                               Peek_Authors_Author q
                                                                                                                                                   x -> Peek_ReportView_Author ((Path_ReportView__reportAuthors :: Path_Authors Author ->
                                                                                                                                                                                                                   Path_ReportView Author) q) x
                                                                                                                               Peek_Authors_Authors q
                                                                                                                                                    x -> Peek_ReportView_Authors ((Path_ReportView__reportAuthors :: Path_Authors (Order AuthorID
                                                                                                                                                                                                                                         Author) ->
                                                                                                                                                                                                                     Path_ReportView (Order AuthorID
                                                                                                                                                                                                                                            Author)) q) x
                                                                                                                               Peek_Authors_Text q
                                                                                                                                                 x -> Peek_ReportView_Text ((Path_ReportView__reportAuthors :: Path_Authors Text ->
                                                                                                                                                                                                               Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order AuthorID
                                                                                                                                                                                                                                                                          Author))))
                        [] -> error "No Path_ReportView__reportAuthors field found"
                        ps -> error $ ("Multiple Path_ReportView__reportAuthors fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPreparer _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportPreparer _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                    in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                               Peek_Markup_JSONText q
                                                                                                                                                    x -> Peek_ReportView_JSONText ((Path_ReportView__reportPreparer :: Path_Markup JSONText ->
                                                                                                                                                                                                                       Path_ReportView JSONText) q) x
                                                                                                                               Peek_Markup_Markup q
                                                                                                                                                  x -> Peek_ReportView_Markup ((Path_ReportView__reportPreparer :: Path_Markup Markup ->
                                                                                                                                                                                                                   Path_ReportView Markup) q) x
                                                                                                                               Peek_Markup_Text q
                                                                                                                                                x -> Peek_ReportView_Text ((Path_ReportView__reportPreparer :: Path_Markup Text ->
                                                                                                                                                                                                               Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportPreparer field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPreparer fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPreparerEIN _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportPreparerEIN _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                       in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                  Peek_Markup_JSONText q
                                                                                                                                                       x -> Peek_ReportView_JSONText ((Path_ReportView__reportPreparerEIN :: Path_Markup JSONText ->
                                                                                                                                                                                                                             Path_ReportView JSONText) q) x
                                                                                                                                  Peek_Markup_Markup q
                                                                                                                                                     x -> Peek_ReportView_Markup ((Path_ReportView__reportPreparerEIN :: Path_Markup Markup ->
                                                                                                                                                                                                                         Path_ReportView Markup) q) x
                                                                                                                                  Peek_Markup_Text q
                                                                                                                                                   x -> Peek_ReportView_Text ((Path_ReportView__reportPreparerEIN :: Path_Markup Text ->
                                                                                                                                                                                                                     Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportPreparerEIN field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPreparerEIN fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPreparerAddress _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportPreparerAddress _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                           in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                      Peek_Markup_JSONText q
                                                                                                                                                           x -> Peek_ReportView_JSONText ((Path_ReportView__reportPreparerAddress :: Path_Markup JSONText ->
                                                                                                                                                                                                                                     Path_ReportView JSONText) q) x
                                                                                                                                      Peek_Markup_Markup q
                                                                                                                                                         x -> Peek_ReportView_Markup ((Path_ReportView__reportPreparerAddress :: Path_Markup Markup ->
                                                                                                                                                                                                                                 Path_ReportView Markup) q) x
                                                                                                                                      Peek_Markup_Text q
                                                                                                                                                       x -> Peek_ReportView_Text ((Path_ReportView__reportPreparerAddress :: Path_Markup Text ->
                                                                                                                                                                                                                             Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportPreparerAddress field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPreparerAddress fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPreparerEMail _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportPreparerEMail _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                         in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_Markup_JSONText q
                                                                                                                                                         x -> Peek_ReportView_JSONText ((Path_ReportView__reportPreparerEMail :: Path_Markup JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x
                                                                                                                                    Peek_Markup_Markup q
                                                                                                                                                       x -> Peek_ReportView_Markup ((Path_ReportView__reportPreparerEMail :: Path_Markup Markup ->
                                                                                                                                                                                                                             Path_ReportView Markup) q) x
                                                                                                                                    Peek_Markup_Text q
                                                                                                                                                     x -> Peek_ReportView_Text ((Path_ReportView__reportPreparerEMail :: Path_Markup Text ->
                                                                                                                                                                                                                         Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportPreparerEMail field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPreparerEMail fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPreparerWebsite _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportPreparerWebsite _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                           in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                      Peek_Markup_JSONText q
                                                                                                                                                           x -> Peek_ReportView_JSONText ((Path_ReportView__reportPreparerWebsite :: Path_Markup JSONText ->
                                                                                                                                                                                                                                     Path_ReportView JSONText) q) x
                                                                                                                                      Peek_Markup_Markup q
                                                                                                                                                         x -> Peek_ReportView_Markup ((Path_ReportView__reportPreparerWebsite :: Path_Markup Markup ->
                                                                                                                                                                                                                                 Path_ReportView Markup) q) x
                                                                                                                                      Peek_Markup_Text q
                                                                                                                                                       x -> Peek_ReportView_Text ((Path_ReportView__reportPreparerWebsite :: Path_Markup Text ->
                                                                                                                                                                                                                             Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportPreparerWebsite field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPreparerWebsite fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportAbbrevs _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order AbbrevPairID
                                                                                             ((CIString,
                                                                                               Markup))))) :: [Path_ReportView (Order AbbrevPairID
                                                                                                                                      ((CIString,
                                                                                                                                        Markup)))] of
                        [p@(Path_ReportView__reportAbbrevs _)] -> let [y] = toListOf (toLens p) x :: [Order AbbrevPairID
                                                                                                            ((CIString,
                                                                                                              Markup))]
                                                                   in Node (Peek_ReportView_AbbrevPairs p y) (forestMap (\v -> case v of
                                                                                                                                   Peek_AbbrevPairs_JSONText q
                                                                                                                                                             x -> Peek_ReportView_JSONText ((Path_ReportView__reportAbbrevs :: Path_AbbrevPairs JSONText ->
                                                                                                                                                                                                                               Path_ReportView JSONText) q) x
                                                                                                                                   Peek_AbbrevPairs_Markup q
                                                                                                                                                           x -> Peek_ReportView_Markup ((Path_ReportView__reportAbbrevs :: Path_AbbrevPairs Markup ->
                                                                                                                                                                                                                           Path_ReportView Markup) q) x
                                                                                                                                   Peek_AbbrevPairs_AbbrevPair q
                                                                                                                                                               x -> Peek_ReportView_AbbrevPair ((Path_ReportView__reportAbbrevs :: Path_AbbrevPairs ((CIString,
                                                                                                                                                                                                                                                      Markup)) ->
                                                                                                                                                                                                                                   Path_ReportView ((CIString,
                                                                                                                                                                                                                                                     Markup))) q) x
                                                                                                                                   Peek_AbbrevPairs_AbbrevPairs q
                                                                                                                                                                x -> Peek_ReportView_AbbrevPairs ((Path_ReportView__reportAbbrevs :: Path_AbbrevPairs (Order AbbrevPairID
                                                                                                                                                                                                                                                             ((CIString,
                                                                                                                                                                                                                                                               Markup))) ->
                                                                                                                                                                                                                                     Path_ReportView (Order AbbrevPairID
                                                                                                                                                                                                                                                            ((CIString,
                                                                                                                                                                                                                                                              Markup)))) q) x
                                                                                                                                   Peek_AbbrevPairs_CIString q
                                                                                                                                                             x -> Peek_ReportView_CIString ((Path_ReportView__reportAbbrevs :: Path_AbbrevPairs CIString ->
                                                                                                                                                                                                                               Path_ReportView CIString) q) x
                                                                                                                                   Peek_AbbrevPairs_Text q
                                                                                                                                                         x -> Peek_ReportView_Text ((Path_ReportView__reportAbbrevs :: Path_AbbrevPairs Text ->
                                                                                                                                                                                                                       Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order AbbrevPairID
                                                                                                                                                                                                                                                                                  ((CIString,
                                                                                                                                                                                                                                                                                    Markup))))))
                        [] -> error "No Path_ReportView__reportAbbrevs field found"
                        ps -> error $ ("Multiple Path_ReportView__reportAbbrevs fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportTitle _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportTitle _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                 in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                            Peek_Markup_JSONText q
                                                                                                                                                 x -> Peek_ReportView_JSONText ((Path_ReportView__reportTitle :: Path_Markup JSONText ->
                                                                                                                                                                                                                 Path_ReportView JSONText) q) x
                                                                                                                            Peek_Markup_Markup q
                                                                                                                                               x -> Peek_ReportView_Markup ((Path_ReportView__reportTitle :: Path_Markup Markup ->
                                                                                                                                                                                                             Path_ReportView Markup) q) x
                                                                                                                            Peek_Markup_Text q
                                                                                                                                             x -> Peek_ReportView_Text ((Path_ReportView__reportTitle :: Path_Markup Text ->
                                                                                                                                                                                                         Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportTitle field found"
                        ps -> error $ ("Multiple Path_ReportView__reportTitle fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportHeader _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportHeader _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                  in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                             Peek_Markup_JSONText q
                                                                                                                                                  x -> Peek_ReportView_JSONText ((Path_ReportView__reportHeader :: Path_Markup JSONText ->
                                                                                                                                                                                                                   Path_ReportView JSONText) q) x
                                                                                                                             Peek_Markup_Markup q
                                                                                                                                                x -> Peek_ReportView_Markup ((Path_ReportView__reportHeader :: Path_Markup Markup ->
                                                                                                                                                                                                               Path_ReportView Markup) q) x
                                                                                                                             Peek_Markup_Text q
                                                                                                                                              x -> Peek_ReportView_Text ((Path_ReportView__reportHeader :: Path_Markup Text ->
                                                                                                                                                                                                           Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportHeader field found"
                        ps -> error $ ("Multiple Path_ReportView__reportHeader fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportFooter _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportFooter _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                  in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                             Peek_Markup_JSONText q
                                                                                                                                                  x -> Peek_ReportView_JSONText ((Path_ReportView__reportFooter :: Path_Markup JSONText ->
                                                                                                                                                                                                                   Path_ReportView JSONText) q) x
                                                                                                                             Peek_Markup_Markup q
                                                                                                                                                x -> Peek_ReportView_Markup ((Path_ReportView__reportFooter :: Path_Markup Markup ->
                                                                                                                                                                                                               Path_ReportView Markup) q) x
                                                                                                                             Peek_Markup_Text q
                                                                                                                                              x -> Peek_ReportView_Text ((Path_ReportView__reportFooter :: Path_Markup Text ->
                                                                                                                                                                                                           Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportFooter field found"
                        ps -> error $ ("Multiple Path_ReportView__reportFooter fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportIntendedUse _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Maybe ReportIntendedUse))) :: [Path_ReportView (Maybe ReportIntendedUse)] of
                        [p@(Path_ReportView__reportIntendedUse _)] -> let [y] = toListOf (toLens p) x :: [Maybe ReportIntendedUse]
                                                                       in Node (Peek_ReportView_MaybeReportIntendedUse p y) (forestMap (\v -> case v of
                                                                                                                                                  Peek_MaybeReportIntendedUse_String q
                                                                                                                                                                                     x -> Peek_ReportView_String ((Path_ReportView__reportIntendedUse :: Path_MaybeReportIntendedUse ([Char]) ->
                                                                                                                                                                                                                                                         Path_ReportView ([Char])) q) x
                                                                                                                                                  Peek_MaybeReportIntendedUse_JSONText q
                                                                                                                                                                                       x -> Peek_ReportView_JSONText ((Path_ReportView__reportIntendedUse :: Path_MaybeReportIntendedUse JSONText ->
                                                                                                                                                                                                                                                             Path_ReportView JSONText) q) x
                                                                                                                                                  Peek_MaybeReportIntendedUse_MaybeReportIntendedUse q
                                                                                                                                                                                                     x -> Peek_ReportView_MaybeReportIntendedUse ((Path_ReportView__reportIntendedUse :: Path_MaybeReportIntendedUse (Maybe ReportIntendedUse) ->
                                                                                                                                                                                                                                                                                         Path_ReportView (Maybe ReportIntendedUse)) q) x) (peek y :: Forest (Peek (Maybe ReportIntendedUse))))
                        [] -> error "No Path_ReportView__reportIntendedUse field found"
                        ps -> error $ ("Multiple Path_ReportView__reportIntendedUse fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportValueTypeInfo _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ReportValueTypeInfo)) :: [Path_ReportView ReportValueTypeInfo] of
                        [p@(Path_ReportView__reportValueTypeInfo _)] -> let [y] = toListOf (toLens p) x :: [ReportValueTypeInfo]
                                                                         in Node (Peek_ReportView_ReportValueTypeInfo p y) (forestMap (\v -> case v of
                                                                                                                                                 Peek_ReportValueTypeInfo_JSONText q
                                                                                                                                                                                   x -> Peek_ReportView_JSONText ((Path_ReportView__reportValueTypeInfo :: Path_ReportValueTypeInfo JSONText ->
                                                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                                 Peek_ReportValueTypeInfo_Markup q
                                                                                                                                                                                 x -> Peek_ReportView_Markup ((Path_ReportView__reportValueTypeInfo :: Path_ReportValueTypeInfo Markup ->
                                                                                                                                                                                                                                                       Path_ReportView Markup) q) x
                                                                                                                                                 Peek_ReportValueTypeInfo_ReportValueTypeInfo q
                                                                                                                                                                                              x -> Peek_ReportView_ReportValueTypeInfo ((Path_ReportView__reportValueTypeInfo :: Path_ReportValueTypeInfo ReportValueTypeInfo ->
                                                                                                                                                                                                                                                                                 Path_ReportView ReportValueTypeInfo) q) x
                                                                                                                                                 Peek_ReportValueTypeInfo_Text q
                                                                                                                                                                               x -> Peek_ReportView_Text ((Path_ReportView__reportValueTypeInfo :: Path_ReportValueTypeInfo Text ->
                                                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek ReportValueTypeInfo)))
                        [] -> error "No Path_ReportView__reportValueTypeInfo field found"
                        ps -> error $ ("Multiple Path_ReportView__reportValueTypeInfo fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportValueApproachInfo _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ReportValueApproachInfo)) :: [Path_ReportView ReportValueApproachInfo] of
                        [p@(Path_ReportView__reportValueApproachInfo _)] -> let [y] = toListOf (toLens p) x :: [ReportValueApproachInfo]
                                                                             in Node (Peek_ReportView_ReportValueApproachInfo p y) (forestMap (\v -> case v of
                                                                                                                                                         Peek_ReportValueApproachInfo_JSONText q
                                                                                                                                                                                               x -> Peek_ReportView_JSONText ((Path_ReportView__reportValueApproachInfo :: Path_ReportValueApproachInfo JSONText ->
                                                                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                                         Peek_ReportValueApproachInfo_Markup q
                                                                                                                                                                                             x -> Peek_ReportView_Markup ((Path_ReportView__reportValueApproachInfo :: Path_ReportValueApproachInfo Markup ->
                                                                                                                                                                                                                                                                       Path_ReportView Markup) q) x
                                                                                                                                                         Peek_ReportValueApproachInfo_ReportValueApproachInfo q
                                                                                                                                                                                                              x -> Peek_ReportView_ReportValueApproachInfo ((Path_ReportView__reportValueApproachInfo :: Path_ReportValueApproachInfo ReportValueApproachInfo ->
                                                                                                                                                                                                                                                                                                         Path_ReportView ReportValueApproachInfo) q) x
                                                                                                                                                         Peek_ReportValueApproachInfo_Text q
                                                                                                                                                                                           x -> Peek_ReportView_Text ((Path_ReportView__reportValueApproachInfo :: Path_ReportValueApproachInfo Text ->
                                                                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek ReportValueApproachInfo)))
                        [] -> error "No Path_ReportView__reportValueApproachInfo field found"
                        ps -> error $ ("Multiple Path_ReportView__reportValueApproachInfo fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportClientName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportClientName _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                      in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                 Peek_Markup_JSONText q
                                                                                                                                                      x -> Peek_ReportView_JSONText ((Path_ReportView__reportClientName :: Path_Markup JSONText ->
                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                 Peek_Markup_Markup q
                                                                                                                                                    x -> Peek_ReportView_Markup ((Path_ReportView__reportClientName :: Path_Markup Markup ->
                                                                                                                                                                                                                       Path_ReportView Markup) q) x
                                                                                                                                 Peek_Markup_Text q
                                                                                                                                                  x -> Peek_ReportView_Text ((Path_ReportView__reportClientName :: Path_Markup Text ->
                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportClientName field found"
                        ps -> error $ ("Multiple Path_ReportView__reportClientName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportClientAddress _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportClientAddress _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                         in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_Markup_JSONText q
                                                                                                                                                         x -> Peek_ReportView_JSONText ((Path_ReportView__reportClientAddress :: Path_Markup JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x
                                                                                                                                    Peek_Markup_Markup q
                                                                                                                                                       x -> Peek_ReportView_Markup ((Path_ReportView__reportClientAddress :: Path_Markup Markup ->
                                                                                                                                                                                                                             Path_ReportView Markup) q) x
                                                                                                                                    Peek_Markup_Text q
                                                                                                                                                     x -> Peek_ReportView_Text ((Path_ReportView__reportClientAddress :: Path_Markup Text ->
                                                                                                                                                                                                                         Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportClientAddress field found"
                        ps -> error $ ("Multiple Path_ReportView__reportClientAddress fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportClientGreeting _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportClientGreeting _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                          in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_Markup_JSONText q
                                                                                                                                                          x -> Peek_ReportView_JSONText ((Path_ReportView__reportClientGreeting :: Path_Markup JSONText ->
                                                                                                                                                                                                                                   Path_ReportView JSONText) q) x
                                                                                                                                     Peek_Markup_Markup q
                                                                                                                                                        x -> Peek_ReportView_Markup ((Path_ReportView__reportClientGreeting :: Path_Markup Markup ->
                                                                                                                                                                                                                               Path_ReportView Markup) q) x
                                                                                                                                     Peek_Markup_Text q
                                                                                                                                                      x -> Peek_ReportView_Text ((Path_ReportView__reportClientGreeting :: Path_Markup Text ->
                                                                                                                                                                                                                           Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportClientGreeting field found"
                        ps -> error $ ("Multiple Path_ReportView__reportClientGreeting fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportItemsOwnerFull _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportItemsOwnerFull _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                          in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_Markup_JSONText q
                                                                                                                                                          x -> Peek_ReportView_JSONText ((Path_ReportView__reportItemsOwnerFull :: Path_Markup JSONText ->
                                                                                                                                                                                                                                   Path_ReportView JSONText) q) x
                                                                                                                                     Peek_Markup_Markup q
                                                                                                                                                        x -> Peek_ReportView_Markup ((Path_ReportView__reportItemsOwnerFull :: Path_Markup Markup ->
                                                                                                                                                                                                                               Path_ReportView Markup) q) x
                                                                                                                                     Peek_Markup_Text q
                                                                                                                                                      x -> Peek_ReportView_Text ((Path_ReportView__reportItemsOwnerFull :: Path_Markup Text ->
                                                                                                                                                                                                                           Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportItemsOwnerFull field found"
                        ps -> error $ ("Multiple Path_ReportView__reportItemsOwnerFull fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportItemsOwner _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportItemsOwner _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                      in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                 Peek_Markup_JSONText q
                                                                                                                                                      x -> Peek_ReportView_JSONText ((Path_ReportView__reportItemsOwner :: Path_Markup JSONText ->
                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                 Peek_Markup_Markup q
                                                                                                                                                    x -> Peek_ReportView_Markup ((Path_ReportView__reportItemsOwner :: Path_Markup Markup ->
                                                                                                                                                                                                                       Path_ReportView Markup) q) x
                                                                                                                                 Peek_Markup_Text q
                                                                                                                                                  x -> Peek_ReportView_Text ((Path_ReportView__reportItemsOwner :: Path_Markup Text ->
                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportItemsOwner field found"
                        ps -> error $ ("Multiple Path_ReportView__reportItemsOwner fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportBriefItems _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportBriefItems _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                      in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                 Peek_Markup_JSONText q
                                                                                                                                                      x -> Peek_ReportView_JSONText ((Path_ReportView__reportBriefItems :: Path_Markup JSONText ->
                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                 Peek_Markup_Markup q
                                                                                                                                                    x -> Peek_ReportView_Markup ((Path_ReportView__reportBriefItems :: Path_Markup Markup ->
                                                                                                                                                                                                                       Path_ReportView Markup) q) x
                                                                                                                                 Peek_Markup_Text q
                                                                                                                                                  x -> Peek_ReportView_Text ((Path_ReportView__reportBriefItems :: Path_Markup Text ->
                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportBriefItems field found"
                        ps -> error $ ("Multiple Path_ReportView__reportBriefItems fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportInspectionLocation _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportInspectionLocation _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                              in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                         Peek_Markup_JSONText q
                                                                                                                                                              x -> Peek_ReportView_JSONText ((Path_ReportView__reportInspectionLocation :: Path_Markup JSONText ->
                                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                         Peek_Markup_Markup q
                                                                                                                                                            x -> Peek_ReportView_Markup ((Path_ReportView__reportInspectionLocation :: Path_Markup Markup ->
                                                                                                                                                                                                                                       Path_ReportView Markup) q) x
                                                                                                                                         Peek_Markup_Text q
                                                                                                                                                          x -> Peek_ReportView_Text ((Path_ReportView__reportInspectionLocation :: Path_Markup Text ->
                                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportInspectionLocation field found"
                        ps -> error $ ("Multiple Path_ReportView__reportInspectionLocation fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportBody _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order ReportElemID
                                                                                             ReportElem))) :: [Path_ReportView (Order ReportElemID
                                                                                                                                      ReportElem)] of
                        [p@(Path_ReportView__reportBody _)] -> let [y] = toListOf (toLens p) x :: [Order ReportElemID
                                                                                                         ReportElem]
                                                                in Node (Peek_ReportView_ReportElems p y) (forestMap (\v -> case v of
                                                                                                                                Peek_ReportElems_String q
                                                                                                                                                        x -> Peek_ReportView_String ((Path_ReportView__reportBody :: Path_ReportElems ([Char]) ->
                                                                                                                                                                                                                     Path_ReportView ([Char])) q) x
                                                                                                                                Peek_ReportElems_Bool q
                                                                                                                                                      x -> Peek_ReportView_Bool ((Path_ReportView__reportBody :: Path_ReportElems Bool ->
                                                                                                                                                                                                                 Path_ReportView Bool) q) x
                                                                                                                                Peek_ReportElems_Double q
                                                                                                                                                        x -> Peek_ReportView_Double ((Path_ReportView__reportBody :: Path_ReportElems Double ->
                                                                                                                                                                                                                     Path_ReportView Double) q) x
                                                                                                                                Peek_ReportElems_Dimension q
                                                                                                                                                           x -> Peek_ReportView_Dimension ((Path_ReportView__reportBody :: Path_ReportElems Dimension ->
                                                                                                                                                                                                                           Path_ReportView Dimension) q) x
                                                                                                                                Peek_ReportElems_ImageCrop q
                                                                                                                                                           x -> Peek_ReportView_ImageCrop ((Path_ReportView__reportBody :: Path_ReportElems ImageCrop ->
                                                                                                                                                                                                                           Path_ReportView ImageCrop) q) x
                                                                                                                                Peek_ReportElems_ImageSize q
                                                                                                                                                           x -> Peek_ReportView_ImageSize ((Path_ReportView__reportBody :: Path_ReportElems ImageSize ->
                                                                                                                                                                                                                           Path_ReportView ImageSize) q) x
                                                                                                                                Peek_ReportElems_Units q
                                                                                                                                                       x -> Peek_ReportView_Units ((Path_ReportView__reportBody :: Path_ReportElems Units ->
                                                                                                                                                                                                                   Path_ReportView Units) q) x
                                                                                                                                Peek_ReportElems_ImageFile q
                                                                                                                                                           x -> Peek_ReportView_ImageFile ((Path_ReportView__reportBody :: Path_ReportElems ImageFile ->
                                                                                                                                                                                                                           Path_ReportView ImageFile) q) x
                                                                                                                                Peek_ReportElems_JSONText q
                                                                                                                                                          x -> Peek_ReportView_JSONText ((Path_ReportView__reportBody :: Path_ReportElems JSONText ->
                                                                                                                                                                                                                         Path_ReportView JSONText) q) x
                                                                                                                                Peek_ReportElems_Markup q
                                                                                                                                                        x -> Peek_ReportView_Markup ((Path_ReportView__reportBody :: Path_ReportElems Markup ->
                                                                                                                                                                                                                     Path_ReportView Markup) q) x
                                                                                                                                Peek_ReportElems_ReportElem q
                                                                                                                                                            x -> Peek_ReportView_ReportElem ((Path_ReportView__reportBody :: Path_ReportElems ReportElem ->
                                                                                                                                                                                                                             Path_ReportView ReportElem) q) x
                                                                                                                                Peek_ReportElems_ReportElems q
                                                                                                                                                             x -> Peek_ReportView_ReportElems ((Path_ReportView__reportBody :: Path_ReportElems (Order ReportElemID
                                                                                                                                                                                                                                                       ReportElem) ->
                                                                                                                                                                                                                               Path_ReportView (Order ReportElemID
                                                                                                                                                                                                                                                      ReportElem)) q) x
                                                                                                                                Peek_ReportElems_EUI q
                                                                                                                                                     x -> Peek_ReportView_EUI ((Path_ReportView__reportBody :: Path_ReportElems (Either URI
                                                                                                                                                                                                                                        ImageFile) ->
                                                                                                                                                                                                               Path_ReportView (Either URI
                                                                                                                                                                                                                                       ImageFile)) q) x
                                                                                                                                Peek_ReportElems_MEUI q
                                                                                                                                                      x -> Peek_ReportView_MEUI ((Path_ReportView__reportBody :: Path_ReportElems (Maybe (Either URI
                                                                                                                                                                                                                                                 ImageFile)) ->
                                                                                                                                                                                                                 Path_ReportView (Maybe (Either URI
                                                                                                                                                                                                                                                ImageFile))) q) x
                                                                                                                                Peek_ReportElems_MaybeImageFile q
                                                                                                                                                                x -> Peek_ReportView_MaybeImageFile ((Path_ReportView__reportBody :: Path_ReportElems (Maybe ImageFile) ->
                                                                                                                                                                                                                                     Path_ReportView (Maybe ImageFile)) q) x
                                                                                                                                Peek_ReportElems_ReportImage q
                                                                                                                                                             x -> Peek_ReportView_ReportImage ((Path_ReportView__reportBody :: Path_ReportElems ReportImage ->
                                                                                                                                                                                                                               Path_ReportView ReportImage) q) x
                                                                                                                                Peek_ReportElems_ReportImages q
                                                                                                                                                              x -> Peek_ReportView_ReportImages ((Path_ReportView__reportBody :: Path_ReportElems (Order ReportImageID
                                                                                                                                                                                                                                                         ReportImage) ->
                                                                                                                                                                                                                                 Path_ReportView (Order ReportImageID
                                                                                                                                                                                                                                                        ReportImage)) q) x
                                                                                                                                Peek_ReportElems_ReportImageView q
                                                                                                                                                                 x -> Peek_ReportView_ReportImageView ((Path_ReportView__reportBody :: Path_ReportElems ReportImageView ->
                                                                                                                                                                                                                                       Path_ReportView ReportImageView) q) x
                                                                                                                                Peek_ReportElems_SaneSizeImageSize q
                                                                                                                                                                   x -> Peek_ReportView_SaneSizeImageSize ((Path_ReportView__reportBody :: Path_ReportElems (SaneSize ImageSize) ->
                                                                                                                                                                                                                                           Path_ReportView (SaneSize ImageSize)) q) x
                                                                                                                                Peek_ReportElems_Item q
                                                                                                                                                      x -> Peek_ReportView_Item ((Path_ReportView__reportBody :: Path_ReportElems Item ->
                                                                                                                                                                                                                 Path_ReportView Item) q) x
                                                                                                                                Peek_ReportElems_MIM q
                                                                                                                                                     x -> Peek_ReportView_MIM ((Path_ReportView__reportBody :: Path_ReportElems (Map ItemFieldName
                                                                                                                                                                                                                                     Markup) ->
                                                                                                                                                                                                               Path_ReportView (Map ItemFieldName
                                                                                                                                                                                                                                    Markup)) q) x
                                                                                                                                Peek_ReportElems_URI q
                                                                                                                                                     x -> Peek_ReportView_URI ((Path_ReportView__reportBody :: Path_ReportElems URI ->
                                                                                                                                                                                                               Path_ReportView URI) q) x
                                                                                                                                Peek_ReportElems_Text q
                                                                                                                                                      x -> Peek_ReportView_Text ((Path_ReportView__reportBody :: Path_ReportElems Text ->
                                                                                                                                                                                                                 Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order ReportElemID
                                                                                                                                                                                                                                                                            ReportElem))))
                        [] -> error "No Path_ReportView__reportBody field found"
                        ps -> error $ ("Multiple Path_ReportView__reportBody fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportGlossary _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order MarkupPairID
                                                                                             ((Markup,
                                                                                               Markup))))) :: [Path_ReportView (Order MarkupPairID
                                                                                                                                      ((Markup,
                                                                                                                                        Markup)))] of
                        [p@(Path_ReportView__reportGlossary _)] -> let [y] = toListOf (toLens p) x :: [Order MarkupPairID
                                                                                                             ((Markup,
                                                                                                               Markup))]
                                                                    in Node (Peek_ReportView_MarkupPairs p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_MarkupPairs_JSONText q
                                                                                                                                                              x -> Peek_ReportView_JSONText ((Path_ReportView__reportGlossary :: Path_MarkupPairs JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x
                                                                                                                                    Peek_MarkupPairs_Markup q
                                                                                                                                                            x -> Peek_ReportView_Markup ((Path_ReportView__reportGlossary :: Path_MarkupPairs Markup ->
                                                                                                                                                                                                                             Path_ReportView Markup) q) x
                                                                                                                                    Peek_MarkupPairs_MarkupPair q
                                                                                                                                                                x -> Peek_ReportView_MarkupPair ((Path_ReportView__reportGlossary :: Path_MarkupPairs ((Markup,
                                                                                                                                                                                                                                                        Markup)) ->
                                                                                                                                                                                                                                     Path_ReportView ((Markup,
                                                                                                                                                                                                                                                       Markup))) q) x
                                                                                                                                    Peek_MarkupPairs_MarkupPairs q
                                                                                                                                                                 x -> Peek_ReportView_MarkupPairs ((Path_ReportView__reportGlossary :: Path_MarkupPairs (Order MarkupPairID
                                                                                                                                                                                                                                                               ((Markup,
                                                                                                                                                                                                                                                                 Markup))) ->
                                                                                                                                                                                                                                       Path_ReportView (Order MarkupPairID
                                                                                                                                                                                                                                                              ((Markup,
                                                                                                                                                                                                                                                                Markup)))) q) x
                                                                                                                                    Peek_MarkupPairs_Text q
                                                                                                                                                          x -> Peek_ReportView_Text ((Path_ReportView__reportGlossary :: Path_MarkupPairs Text ->
                                                                                                                                                                                                                         Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order MarkupPairID
                                                                                                                                                                                                                                                                                    ((Markup,
                                                                                                                                                                                                                                                                                      Markup))))))
                        [] -> error "No Path_ReportView__reportGlossary field found"
                        ps -> error $ ("Multiple Path_ReportView__reportGlossary fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportSources _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order MarkupPairID
                                                                                             ((Markup,
                                                                                               Markup))))) :: [Path_ReportView (Order MarkupPairID
                                                                                                                                      ((Markup,
                                                                                                                                        Markup)))] of
                        [p@(Path_ReportView__reportSources _)] -> let [y] = toListOf (toLens p) x :: [Order MarkupPairID
                                                                                                            ((Markup,
                                                                                                              Markup))]
                                                                   in Node (Peek_ReportView_MarkupPairs p y) (forestMap (\v -> case v of
                                                                                                                                   Peek_MarkupPairs_JSONText q
                                                                                                                                                             x -> Peek_ReportView_JSONText ((Path_ReportView__reportSources :: Path_MarkupPairs JSONText ->
                                                                                                                                                                                                                               Path_ReportView JSONText) q) x
                                                                                                                                   Peek_MarkupPairs_Markup q
                                                                                                                                                           x -> Peek_ReportView_Markup ((Path_ReportView__reportSources :: Path_MarkupPairs Markup ->
                                                                                                                                                                                                                           Path_ReportView Markup) q) x
                                                                                                                                   Peek_MarkupPairs_MarkupPair q
                                                                                                                                                               x -> Peek_ReportView_MarkupPair ((Path_ReportView__reportSources :: Path_MarkupPairs ((Markup,
                                                                                                                                                                                                                                                      Markup)) ->
                                                                                                                                                                                                                                   Path_ReportView ((Markup,
                                                                                                                                                                                                                                                     Markup))) q) x
                                                                                                                                   Peek_MarkupPairs_MarkupPairs q
                                                                                                                                                                x -> Peek_ReportView_MarkupPairs ((Path_ReportView__reportSources :: Path_MarkupPairs (Order MarkupPairID
                                                                                                                                                                                                                                                             ((Markup,
                                                                                                                                                                                                                                                               Markup))) ->
                                                                                                                                                                                                                                     Path_ReportView (Order MarkupPairID
                                                                                                                                                                                                                                                            ((Markup,
                                                                                                                                                                                                                                                              Markup)))) q) x
                                                                                                                                   Peek_MarkupPairs_Text q
                                                                                                                                                         x -> Peek_ReportView_Text ((Path_ReportView__reportSources :: Path_MarkupPairs Text ->
                                                                                                                                                                                                                       Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order MarkupPairID
                                                                                                                                                                                                                                                                                  ((Markup,
                                                                                                                                                                                                                                                                                    Markup))))))
                        [] -> error "No Path_ReportView__reportSources field found"
                        ps -> error $ ("Multiple Path_ReportView__reportSources fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportLetterOfTransmittal _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportLetterOfTransmittal _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                               in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                          Peek_Markup_JSONText q
                                                                                                                                                               x -> Peek_ReportView_JSONText ((Path_ReportView__reportLetterOfTransmittal :: Path_Markup JSONText ->
                                                                                                                                                                                                                                             Path_ReportView JSONText) q) x
                                                                                                                                          Peek_Markup_Markup q
                                                                                                                                                             x -> Peek_ReportView_Markup ((Path_ReportView__reportLetterOfTransmittal :: Path_Markup Markup ->
                                                                                                                                                                                                                                         Path_ReportView Markup) q) x
                                                                                                                                          Peek_Markup_Text q
                                                                                                                                                           x -> Peek_ReportView_Text ((Path_ReportView__reportLetterOfTransmittal :: Path_Markup Text ->
                                                                                                                                                                                                                                     Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportLetterOfTransmittal field found"
                        ps -> error $ ("Multiple Path_ReportView__reportLetterOfTransmittal fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportScopeOfWork _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportScopeOfWork _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                       in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                  Peek_Markup_JSONText q
                                                                                                                                                       x -> Peek_ReportView_JSONText ((Path_ReportView__reportScopeOfWork :: Path_Markup JSONText ->
                                                                                                                                                                                                                             Path_ReportView JSONText) q) x
                                                                                                                                  Peek_Markup_Markup q
                                                                                                                                                     x -> Peek_ReportView_Markup ((Path_ReportView__reportScopeOfWork :: Path_Markup Markup ->
                                                                                                                                                                                                                         Path_ReportView Markup) q) x
                                                                                                                                  Peek_Markup_Text q
                                                                                                                                                   x -> Peek_ReportView_Text ((Path_ReportView__reportScopeOfWork :: Path_Markup Text ->
                                                                                                                                                                                                                     Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportScopeOfWork field found"
                        ps -> error $ ("Multiple Path_ReportView__reportScopeOfWork fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportCertification _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order MarkupID
                                                                                             Markup))) :: [Path_ReportView (Order MarkupID
                                                                                                                                  Markup)] of
                        [p@(Path_ReportView__reportCertification _)] -> let [y] = toListOf (toLens p) x :: [Order MarkupID
                                                                                                                  Markup]
                                                                         in Node (Peek_ReportView_Markups p y) (forestMap (\v -> case v of
                                                                                                                                     Peek_Markups_JSONText q
                                                                                                                                                           x -> Peek_ReportView_JSONText ((Path_ReportView__reportCertification :: Path_Markups JSONText ->
                                                                                                                                                                                                                                   Path_ReportView JSONText) q) x
                                                                                                                                     Peek_Markups_Markup q
                                                                                                                                                         x -> Peek_ReportView_Markup ((Path_ReportView__reportCertification :: Path_Markups Markup ->
                                                                                                                                                                                                                               Path_ReportView Markup) q) x
                                                                                                                                     Peek_Markups_Markups q
                                                                                                                                                          x -> Peek_ReportView_Markups ((Path_ReportView__reportCertification :: Path_Markups (Order MarkupID
                                                                                                                                                                                                                                                     Markup) ->
                                                                                                                                                                                                                                 Path_ReportView (Order MarkupID
                                                                                                                                                                                                                                                        Markup)) q) x
                                                                                                                                     Peek_Markups_Text q
                                                                                                                                                       x -> Peek_ReportView_Text ((Path_ReportView__reportCertification :: Path_Markups Text ->
                                                                                                                                                                                                                           Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order MarkupID
                                                                                                                                                                                                                                                                                      Markup))))
                        [] -> error "No Path_ReportView__reportCertification field found"
                        ps -> error $ ("Multiple Path_ReportView__reportCertification fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportLimitingConditions _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order MarkupID
                                                                                             Markup))) :: [Path_ReportView (Order MarkupID
                                                                                                                                  Markup)] of
                        [p@(Path_ReportView__reportLimitingConditions _)] -> let [y] = toListOf (toLens p) x :: [Order MarkupID
                                                                                                                       Markup]
                                                                              in Node (Peek_ReportView_Markups p y) (forestMap (\v -> case v of
                                                                                                                                          Peek_Markups_JSONText q
                                                                                                                                                                x -> Peek_ReportView_JSONText ((Path_ReportView__reportLimitingConditions :: Path_Markups JSONText ->
                                                                                                                                                                                                                                             Path_ReportView JSONText) q) x
                                                                                                                                          Peek_Markups_Markup q
                                                                                                                                                              x -> Peek_ReportView_Markup ((Path_ReportView__reportLimitingConditions :: Path_Markups Markup ->
                                                                                                                                                                                                                                         Path_ReportView Markup) q) x
                                                                                                                                          Peek_Markups_Markups q
                                                                                                                                                               x -> Peek_ReportView_Markups ((Path_ReportView__reportLimitingConditions :: Path_Markups (Order MarkupID
                                                                                                                                                                                                                                                               Markup) ->
                                                                                                                                                                                                                                           Path_ReportView (Order MarkupID
                                                                                                                                                                                                                                                                  Markup)) q) x
                                                                                                                                          Peek_Markups_Text q
                                                                                                                                                            x -> Peek_ReportView_Text ((Path_ReportView__reportLimitingConditions :: Path_Markups Text ->
                                                                                                                                                                                                                                     Path_ReportView Text) q) x) (peek y :: Forest (Peek (Order MarkupID
                                                                                                                                                                                                                                                                                                Markup))))
                        [] -> error "No Path_ReportView__reportLimitingConditions field found"
                        ps -> error $ ("Multiple Path_ReportView__reportLimitingConditions fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPrivacyPolicy _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Markup)) :: [Path_ReportView Markup] of
                        [p@(Path_ReportView__reportPrivacyPolicy _)] -> let [y] = toListOf (toLens p) x :: [Markup]
                                                                         in Node (Peek_ReportView_Markup p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_Markup_JSONText q
                                                                                                                                                         x -> Peek_ReportView_JSONText ((Path_ReportView__reportPrivacyPolicy :: Path_Markup JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x
                                                                                                                                    Peek_Markup_Markup q
                                                                                                                                                       x -> Peek_ReportView_Markup ((Path_ReportView__reportPrivacyPolicy :: Path_Markup Markup ->
                                                                                                                                                                                                                             Path_ReportView Markup) q) x
                                                                                                                                    Peek_Markup_Text q
                                                                                                                                                     x -> Peek_ReportView_Text ((Path_ReportView__reportPrivacyPolicy :: Path_Markup Text ->
                                                                                                                                                                                                                         Path_ReportView Text) q) x) (peek y :: Forest (Peek Markup)))
                        [] -> error "No Path_ReportView__reportPrivacyPolicy field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPrivacyPolicy fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportPerms _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Permissions)) :: [Path_ReportView Permissions] of
                        [p@(Path_ReportView__reportPerms _)] -> let [y] = toListOf (toLens p) x :: [Permissions]
                                                                 in Node (Peek_ReportView_Permissions p y) (forestMap (\v -> case v of
                                                                                                                                 Peek_Permissions_JSONText q
                                                                                                                                                           x -> Peek_ReportView_JSONText ((Path_ReportView__reportPerms :: Path_Permissions JSONText ->
                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                 Peek_Permissions_Permissions q
                                                                                                                                                              x -> Peek_ReportView_Permissions ((Path_ReportView__reportPerms :: Path_Permissions Permissions ->
                                                                                                                                                                                                                                 Path_ReportView Permissions) q) x
                                                                                                                                 Peek_Permissions_UserIds q
                                                                                                                                                          x -> Peek_ReportView_UserIds ((Path_ReportView__reportPerms :: Path_Permissions ([UserId]) ->
                                                                                                                                                                                                                         Path_ReportView ([UserId])) q) x
                                                                                                                                 Peek_Permissions_Text q
                                                                                                                                                       x -> Peek_ReportView_Text ((Path_ReportView__reportPerms :: Path_Permissions Text ->
                                                                                                                                                                                                                   Path_ReportView Text) q) x
                                                                                                                                 Peek_Permissions_UserId q
                                                                                                                                                         x -> Peek_ReportView_UserId ((Path_ReportView__reportPerms :: Path_Permissions UserId ->
                                                                                                                                                                                                                       Path_ReportView UserId) q) x) (peek y :: Forest (Peek Permissions)))
                        [] -> error "No Path_ReportView__reportPerms field found"
                        ps -> error $ ("Multiple Path_ReportView__reportPerms fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportRevision _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Integer)) :: [Path_ReportView Integer] of
                        [p@(Path_ReportView__reportRevision _)] -> let [y] = toListOf (toLens p) x :: [Integer]
                                                                    in Node (Peek_ReportView_Integer p y) (forestMap (\v -> case v of
                                                                                                                                Peek_Integer_Integer q
                                                                                                                                                     x -> Peek_ReportView_Integer ((Path_ReportView__reportRevision :: Path_Integer Integer ->
                                                                                                                                                                                                                       Path_ReportView Integer) q) x) (peek y :: Forest (Peek Integer)))
                        [] -> error "No Path_ReportView__reportRevision field found"
                        ps -> error $ ("Multiple Path_ReportView__reportRevision fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportCreated _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Int64)) :: [Path_ReportView Int64] of
                        [p@(Path_ReportView__reportCreated _)] -> let [y] = toListOf (toLens p) x :: [Int64]
                                                                   in Node (Peek_ReportView_Int64 p y) (forestMap (\v -> case v of
                                                                                                                             Peek_Int64_Int64 q
                                                                                                                                              x -> Peek_ReportView_Int64 ((Path_ReportView__reportCreated :: Path_Int64 Int64 ->
                                                                                                                                                                                                             Path_ReportView Int64) q) x) (peek y :: Forest (Peek Int64)))
                        [] -> error "No Path_ReportView__reportCreated field found"
                        ps -> error $ ("Multiple Path_ReportView__reportCreated fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportBranding _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Branding)) :: [Path_ReportView Branding] of
                        [p@(Path_ReportView__reportBranding _)] -> let [y] = toListOf (toLens p) x :: [Branding]
                                                                    in Node (Peek_ReportView_Branding p y) (forestMap (\v -> case v of
                                                                                                                                 Peek_Branding_JSONText q
                                                                                                                                                        x -> Peek_ReportView_JSONText ((Path_ReportView__reportBranding :: Path_Branding JSONText ->
                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                 Peek_Branding_Branding q
                                                                                                                                                        x -> Peek_ReportView_Branding ((Path_ReportView__reportBranding :: Path_Branding Branding ->
                                                                                                                                                                                                                           Path_ReportView Branding) q) x
                                                                                                                                 Peek_Branding_Text q
                                                                                                                                                    x -> Peek_ReportView_Text ((Path_ReportView__reportBranding :: Path_Branding Text ->
                                                                                                                                                                                                                   Path_ReportView Text) q) x) (peek y :: Forest (Peek Branding)))
                        [] -> error "No Path_ReportView__reportBranding field found"
                        ps -> error $ ("Multiple Path_ReportView__reportBranding fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportStatus _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ReportStatus)) :: [Path_ReportView ReportStatus] of
                        [p@(Path_ReportView__reportStatus _)] -> let [y] = toListOf (toLens p) x :: [ReportStatus]
                                                                  in Node (Peek_ReportView_ReportStatus p y) (forestMap (\v -> case v of
                                                                                                                                   Peek_ReportStatus_String q
                                                                                                                                                            x -> Peek_ReportView_String ((Path_ReportView__reportStatus :: Path_ReportStatus ([Char]) ->
                                                                                                                                                                                                                           Path_ReportView ([Char])) q) x
                                                                                                                                   Peek_ReportStatus_JSONText q
                                                                                                                                                              x -> Peek_ReportView_JSONText ((Path_ReportView__reportStatus :: Path_ReportStatus JSONText ->
                                                                                                                                                                                                                               Path_ReportView JSONText) q) x
                                                                                                                                   Peek_ReportStatus_ReportStatus q
                                                                                                                                                                  x -> Peek_ReportView_ReportStatus ((Path_ReportView__reportStatus :: Path_ReportStatus ReportStatus ->
                                                                                                                                                                                                                                       Path_ReportView ReportStatus) q) x) (peek y :: Forest (Peek ReportStatus)))
                        [] -> error "No Path_ReportView__reportStatus field found"
                        ps -> error $ ("Multiple Path_ReportView__reportStatus fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportRedacted _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Bool)) :: [Path_ReportView Bool] of
                        [p@(Path_ReportView__reportRedacted _)] -> let [y] = toListOf (toLens p) x :: [Bool]
                                                                    in Node (Peek_ReportView_Bool p y) (forestMap (\v -> case v of
                                                                                                                             Peek_Bool_String q
                                                                                                                                              x -> Peek_ReportView_String ((Path_ReportView__reportRedacted :: Path_Bool ([Char]) ->
                                                                                                                                                                                                               Path_ReportView ([Char])) q) x
                                                                                                                             Peek_Bool_Bool q
                                                                                                                                            x -> Peek_ReportView_Bool ((Path_ReportView__reportRedacted :: Path_Bool Bool ->
                                                                                                                                                                                                           Path_ReportView Bool) q) x
                                                                                                                             Peek_Bool_JSONText q
                                                                                                                                                x -> Peek_ReportView_JSONText ((Path_ReportView__reportRedacted :: Path_Bool JSONText ->
                                                                                                                                                                                                                   Path_ReportView JSONText) q) x) (peek y :: Forest (Peek Bool)))
                        [] -> error "No Path_ReportView__reportRedacted field found"
                        ps -> error $ ("Multiple Path_ReportView__reportRedacted fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportFlags _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ReportFlags)) :: [Path_ReportView ReportFlags] of
                        [p@(Path_ReportView__reportFlags _)] -> let [y] = toListOf (toLens p) x :: [ReportFlags]
                                                                 in Node (Peek_ReportView_ReportFlags p y) (forestMap (\v -> case v of
                                                                                                                                 Peek_ReportFlags_String q
                                                                                                                                                         x -> Peek_ReportView_String ((Path_ReportView__reportFlags :: Path_ReportFlags ([Char]) ->
                                                                                                                                                                                                                       Path_ReportView ([Char])) q) x
                                                                                                                                 Peek_ReportFlags_Bool q
                                                                                                                                                       x -> Peek_ReportView_Bool ((Path_ReportView__reportFlags :: Path_ReportFlags Bool ->
                                                                                                                                                                                                                   Path_ReportView Bool) q) x
                                                                                                                                 Peek_ReportFlags_JSONText q
                                                                                                                                                           x -> Peek_ReportView_JSONText ((Path_ReportView__reportFlags :: Path_ReportFlags JSONText ->
                                                                                                                                                                                                                           Path_ReportView JSONText) q) x
                                                                                                                                 Peek_ReportFlags_ReportFlags q
                                                                                                                                                              x -> Peek_ReportView_ReportFlags ((Path_ReportView__reportFlags :: Path_ReportFlags ReportFlags ->
                                                                                                                                                                                                                                 Path_ReportView ReportFlags) q) x) (peek y :: Forest (Peek ReportFlags)))
                        [] -> error "No Path_ReportView__reportFlags field found"
                        ps -> error $ ("Multiple Path_ReportView__reportFlags fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportUUID _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy UUID)) :: [Path_ReportView UUID] of
                        [p@(Path_ReportView__reportUUID _)] -> let [y] = toListOf (toLens p) x :: [UUID]
                                                                in Node (Peek_ReportView_UUID p y) (forestMap (\v -> case v of
                                                                                                                         Peek_UUID_UUID q
                                                                                                                                        x -> Peek_ReportView_UUID ((Path_ReportView__reportUUID :: Path_UUID UUID ->
                                                                                                                                                                                                   Path_ReportView UUID) q) x) (peek y :: Forest (Peek UUID)))
                        [] -> error "No Path_ReportView__reportUUID field found"
                        ps -> error $ ("Multiple Path_ReportView__reportUUID fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportOrderByItemName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Bool)) :: [Path_ReportView Bool] of
                        [p@(Path_ReportView__reportOrderByItemName _)] -> let [y] = toListOf (toLens p) x :: [Bool]
                                                                           in Node (Peek_ReportView_Bool p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_Bool_String q
                                                                                                                                                     x -> Peek_ReportView_String ((Path_ReportView__reportOrderByItemName :: Path_Bool ([Char]) ->
                                                                                                                                                                                                                             Path_ReportView ([Char])) q) x
                                                                                                                                    Peek_Bool_Bool q
                                                                                                                                                   x -> Peek_ReportView_Bool ((Path_ReportView__reportOrderByItemName :: Path_Bool Bool ->
                                                                                                                                                                                                                         Path_ReportView Bool) q) x
                                                                                                                                    Peek_Bool_JSONText q
                                                                                                                                                       x -> Peek_ReportView_JSONText ((Path_ReportView__reportOrderByItemName :: Path_Bool JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x) (peek y :: Forest (Peek Bool)))
                        [] -> error "No Path_ReportView__reportOrderByItemName field found"
                        ps -> error $ ("Multiple Path_ReportView__reportOrderByItemName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportDisplayItemName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Bool)) :: [Path_ReportView Bool] of
                        [p@(Path_ReportView__reportDisplayItemName _)] -> let [y] = toListOf (toLens p) x :: [Bool]
                                                                           in Node (Peek_ReportView_Bool p y) (forestMap (\v -> case v of
                                                                                                                                    Peek_Bool_String q
                                                                                                                                                     x -> Peek_ReportView_String ((Path_ReportView__reportDisplayItemName :: Path_Bool ([Char]) ->
                                                                                                                                                                                                                             Path_ReportView ([Char])) q) x
                                                                                                                                    Peek_Bool_Bool q
                                                                                                                                                   x -> Peek_ReportView_Bool ((Path_ReportView__reportDisplayItemName :: Path_Bool Bool ->
                                                                                                                                                                                                                         Path_ReportView Bool) q) x
                                                                                                                                    Peek_Bool_JSONText q
                                                                                                                                                       x -> Peek_ReportView_JSONText ((Path_ReportView__reportDisplayItemName :: Path_Bool JSONText ->
                                                                                                                                                                                                                                 Path_ReportView JSONText) q) x) (peek y :: Forest (Peek Bool)))
                        [] -> error "No Path_ReportView__reportDisplayItemName field found"
                        ps -> error $ ("Multiple Path_ReportView__reportDisplayItemName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_ReportView__reportStandardsVersion _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy ReportStandard)) :: [Path_ReportView ReportStandard] of
                        [p@(Path_ReportView__reportStandardsVersion _)] -> let [y] = toListOf (toLens p) x :: [ReportStandard]
                                                                            in Node (Peek_ReportView_ReportStandard p y) (forestMap (\v -> case v of
                                                                                                                                               Peek_ReportStandard_Int q
                                                                                                                                                                       x -> Peek_ReportView_Int ((Path_ReportView__reportStandardsVersion :: Path_ReportStandard Int ->
                                                                                                                                                                                                                                             Path_ReportView Int) q) x
                                                                                                                                               Peek_ReportStandard_ReportStandard q
                                                                                                                                                                                  x -> Peek_ReportView_ReportStandard ((Path_ReportView__reportStandardsVersion :: Path_ReportStandard ReportStandard ->
                                                                                                                                                                                                                                                                   Path_ReportView ReportStandard) q) x) (peek y :: Forest (Peek ReportStandard)))
                        [] -> error "No Path_ReportView__reportStandardsVersion field found"
                        ps -> error $ ("Multiple Path_ReportView__reportStandardsVersion fields found: " ++ show ps)]
instance IsPathNode Item
    where data Peek Item
              = Peek_Item_String (Path_Item ([Char])) ([Char])
              | Peek_Item_Bool (Path_Item Bool) Bool
              | Peek_Item_Double (Path_Item Double) Double
              | Peek_Item_Dimension (Path_Item Dimension) Dimension
              | Peek_Item_ImageCrop (Path_Item ImageCrop) ImageCrop
              | Peek_Item_ImageSize (Path_Item ImageSize) ImageSize
              | Peek_Item_Units (Path_Item Units) Units
              | Peek_Item_ImageFile (Path_Item ImageFile) ImageFile
              | Peek_Item_JSONText (Path_Item JSONText) JSONText
              | Peek_Item_Markup (Path_Item Markup) Markup
              | Peek_Item_EUI (Path_Item (Either URI ImageFile))
                              (Either URI ImageFile)
              | Peek_Item_MEUI (Path_Item (Maybe (Either URI ImageFile)))
                               (Maybe (Either URI ImageFile))
              | Peek_Item_MaybeImageFile (Path_Item (Maybe ImageFile))
                                         (Maybe ImageFile)
              | Peek_Item_ReportImage (Path_Item ReportImage) ReportImage
              | Peek_Item_ReportImages (Path_Item (Order ReportImageID
                                                         ReportImage))
                                       (Order ReportImageID ReportImage)
              | Peek_Item_ReportImageView (Path_Item ReportImageView)
                                          ReportImageView
              | Peek_Item_SaneSizeImageSize (Path_Item (SaneSize ImageSize))
                                            (SaneSize ImageSize)
              | Peek_Item_Item (Path_Item Item) Item
              | Peek_Item_MIM (Path_Item (Map ItemFieldName Markup))
                              (Map ItemFieldName Markup)
              | Peek_Item_URI (Path_Item URI) URI
              | Peek_Item_Text (Path_Item Text) Text
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_Item_itemName _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy Text)) :: [Path_Item Text] of
                        [p@(Path_Item_itemName _)] -> let [y] = toListOf (toLens p) x :: [Text]
                                                       in Node (Peek_Item_Text p y) (forestMap (\v -> case v of
                                                                                                          Peek_Text_JSONText q
                                                                                                                             x -> Peek_Item_JSONText ((Path_Item_itemName :: Path_Text JSONText ->
                                                                                                                                                                             Path_Item JSONText) q) x
                                                                                                          Peek_Text_Text q
                                                                                                                         x -> Peek_Item_Text ((Path_Item_itemName :: Path_Text Text ->
                                                                                                                                                                     Path_Item Text) q) x) (peek y :: Forest (Peek Text)))
                        [] -> error "No Path_Item_itemName field found"
                        ps -> error $ ("Multiple Path_Item_itemName fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_Item_fields _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Map ItemFieldName
                                                                                           Markup))) :: [Path_Item (Map ItemFieldName
                                                                                                                        Markup)] of
                        [p@(Path_Item_fields _)] -> let [y] = toListOf (toLens p) x :: [Map ItemFieldName
                                                                                            Markup]
                                                     in Node (Peek_Item_MIM p y) (forestMap (\v -> case v of
                                                                                                       Peek_MIM_JSONText q
                                                                                                                         x -> Peek_Item_JSONText ((Path_Item_fields :: Path_MIM JSONText ->
                                                                                                                                                                       Path_Item JSONText) q) x
                                                                                                       Peek_MIM_Markup q
                                                                                                                       x -> Peek_Item_Markup ((Path_Item_fields :: Path_MIM Markup ->
                                                                                                                                                                   Path_Item Markup) q) x
                                                                                                       Peek_MIM_MIM q
                                                                                                                    x -> Peek_Item_MIM ((Path_Item_fields :: Path_MIM (Map ItemFieldName
                                                                                                                                                                           Markup) ->
                                                                                                                                                             Path_Item (Map ItemFieldName
                                                                                                                                                                            Markup)) q) x
                                                                                                       Peek_MIM_Text q
                                                                                                                     x -> Peek_Item_Text ((Path_Item_fields :: Path_MIM Text ->
                                                                                                                                                               Path_Item Text) q) x) (peek y :: Forest (Peek (Map ItemFieldName
                                                                                                                                                                                                                  Markup))))
                        [] -> error "No Path_Item_fields field found"
                        ps -> error $ ("Multiple Path_Item_fields fields found: " ++ show ps),
                    case filter (\p -> case p of
                                           Path_Item_images _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Order ReportImageID
                                                                                             ReportImage))) :: [Path_Item (Order ReportImageID
                                                                                                                                 ReportImage)] of
                        [p@(Path_Item_images _)] -> let [y] = toListOf (toLens p) x :: [Order ReportImageID
                                                                                              ReportImage]
                                                     in Node (Peek_Item_ReportImages p y) (forestMap (\v -> case v of
                                                                                                                Peek_ReportImages_String q
                                                                                                                                         x -> Peek_Item_String ((Path_Item_images :: Path_ReportImages ([Char]) ->
                                                                                                                                                                                     Path_Item ([Char])) q) x
                                                                                                                Peek_ReportImages_Bool q
                                                                                                                                       x -> Peek_Item_Bool ((Path_Item_images :: Path_ReportImages Bool ->
                                                                                                                                                                                 Path_Item Bool) q) x
                                                                                                                Peek_ReportImages_Double q
                                                                                                                                         x -> Peek_Item_Double ((Path_Item_images :: Path_ReportImages Double ->
                                                                                                                                                                                     Path_Item Double) q) x
                                                                                                                Peek_ReportImages_Dimension q
                                                                                                                                            x -> Peek_Item_Dimension ((Path_Item_images :: Path_ReportImages Dimension ->
                                                                                                                                                                                           Path_Item Dimension) q) x
                                                                                                                Peek_ReportImages_ImageCrop q
                                                                                                                                            x -> Peek_Item_ImageCrop ((Path_Item_images :: Path_ReportImages ImageCrop ->
                                                                                                                                                                                           Path_Item ImageCrop) q) x
                                                                                                                Peek_ReportImages_ImageSize q
                                                                                                                                            x -> Peek_Item_ImageSize ((Path_Item_images :: Path_ReportImages ImageSize ->
                                                                                                                                                                                           Path_Item ImageSize) q) x
                                                                                                                Peek_ReportImages_Units q
                                                                                                                                        x -> Peek_Item_Units ((Path_Item_images :: Path_ReportImages Units ->
                                                                                                                                                                                   Path_Item Units) q) x
                                                                                                                Peek_ReportImages_ImageFile q
                                                                                                                                            x -> Peek_Item_ImageFile ((Path_Item_images :: Path_ReportImages ImageFile ->
                                                                                                                                                                                           Path_Item ImageFile) q) x
                                                                                                                Peek_ReportImages_JSONText q
                                                                                                                                           x -> Peek_Item_JSONText ((Path_Item_images :: Path_ReportImages JSONText ->
                                                                                                                                                                                         Path_Item JSONText) q) x
                                                                                                                Peek_ReportImages_Markup q
                                                                                                                                         x -> Peek_Item_Markup ((Path_Item_images :: Path_ReportImages Markup ->
                                                                                                                                                                                     Path_Item Markup) q) x
                                                                                                                Peek_ReportImages_EUI q
                                                                                                                                      x -> Peek_Item_EUI ((Path_Item_images :: Path_ReportImages (Either URI
                                                                                                                                                                                                         ImageFile) ->
                                                                                                                                                                               Path_Item (Either URI
                                                                                                                                                                                                 ImageFile)) q) x
                                                                                                                Peek_ReportImages_MEUI q
                                                                                                                                       x -> Peek_Item_MEUI ((Path_Item_images :: Path_ReportImages (Maybe (Either URI
                                                                                                                                                                                                                  ImageFile)) ->
                                                                                                                                                                                 Path_Item (Maybe (Either URI
                                                                                                                                                                                                          ImageFile))) q) x
                                                                                                                Peek_ReportImages_MaybeImageFile q
                                                                                                                                                 x -> Peek_Item_MaybeImageFile ((Path_Item_images :: Path_ReportImages (Maybe ImageFile) ->
                                                                                                                                                                                                     Path_Item (Maybe ImageFile)) q) x
                                                                                                                Peek_ReportImages_ReportImage q
                                                                                                                                              x -> Peek_Item_ReportImage ((Path_Item_images :: Path_ReportImages ReportImage ->
                                                                                                                                                                                               Path_Item ReportImage) q) x
                                                                                                                Peek_ReportImages_ReportImages q
                                                                                                                                               x -> Peek_Item_ReportImages ((Path_Item_images :: Path_ReportImages (Order ReportImageID
                                                                                                                                                                                                                          ReportImage) ->
                                                                                                                                                                                                 Path_Item (Order ReportImageID
                                                                                                                                                                                                                  ReportImage)) q) x
                                                                                                                Peek_ReportImages_ReportImageView q
                                                                                                                                                  x -> Peek_Item_ReportImageView ((Path_Item_images :: Path_ReportImages ReportImageView ->
                                                                                                                                                                                                       Path_Item ReportImageView) q) x
                                                                                                                Peek_ReportImages_SaneSizeImageSize q
                                                                                                                                                    x -> Peek_Item_SaneSizeImageSize ((Path_Item_images :: Path_ReportImages (SaneSize ImageSize) ->
                                                                                                                                                                                                           Path_Item (SaneSize ImageSize)) q) x
                                                                                                                Peek_ReportImages_URI q
                                                                                                                                      x -> Peek_Item_URI ((Path_Item_images :: Path_ReportImages URI ->
                                                                                                                                                                               Path_Item URI) q) x
                                                                                                                Peek_ReportImages_Text q
                                                                                                                                       x -> Peek_Item_Text ((Path_Item_images :: Path_ReportImages Text ->
                                                                                                                                                                                 Path_Item Text) q) x) (peek y :: Forest (Peek (Order ReportImageID
                                                                                                                                                                                                                                      ReportImage))))
                        [] -> error "No Path_Item_images field found"
                        ps -> error $ ("Multiple Path_Item_images fields found: " ++ show ps)]
instance IsPathNode ReportMap
    where data Peek ReportMap
              = Peek_ReportMap_String (Path_ReportMap ([Char])) ([Char])
              | Peek_ReportMap_Int64 (Path_ReportMap Int64) Int64
              | Peek_ReportMap_Int (Path_ReportMap Int) Int
              | Peek_ReportMap_Bool (Path_ReportMap Bool) Bool
              | Peek_ReportMap_Double (Path_ReportMap Double) Double
              | Peek_ReportMap_Dimension (Path_ReportMap Dimension) Dimension
              | Peek_ReportMap_ImageCrop (Path_ReportMap ImageCrop) ImageCrop
              | Peek_ReportMap_ImageSize (Path_ReportMap ImageSize) ImageSize
              | Peek_ReportMap_Units (Path_ReportMap Units) Units
              | Peek_ReportMap_ImageFile (Path_ReportMap ImageFile) ImageFile
              | Peek_ReportMap_Integer (Path_ReportMap Integer) Integer
              | Peek_ReportMap_JSONText (Path_ReportMap JSONText) JSONText
              | Peek_ReportMap_Markup (Path_ReportMap Markup) Markup
              | Peek_ReportMap_Permissions (Path_ReportMap Permissions)
                                           Permissions
              | Peek_ReportMap_UserIds (Path_ReportMap ([UserId])) ([UserId])
              | Peek_ReportMap_AbbrevPair (Path_ReportMap ((CIString, Markup)))
                                          ((CIString, Markup))
              | Peek_ReportMap_AbbrevPairs (Path_ReportMap (Order AbbrevPairID
                                                                  ((CIString, Markup))))
                                           (Order AbbrevPairID ((CIString, Markup)))
              | Peek_ReportMap_Author (Path_ReportMap Author) Author
              | Peek_ReportMap_Authors (Path_ReportMap (Order AuthorID Author))
                                       (Order AuthorID Author)
              | Peek_ReportMap_Branding (Path_ReportMap Branding) Branding
              | Peek_ReportMap_MarkupPair (Path_ReportMap ((Markup, Markup)))
                                          ((Markup, Markup))
              | Peek_ReportMap_MarkupPairs (Path_ReportMap (Order MarkupPairID
                                                                  ((Markup, Markup))))
                                           (Order MarkupPairID ((Markup, Markup)))
              | Peek_ReportMap_Markups (Path_ReportMap (Order MarkupID Markup))
                                       (Order MarkupID Markup)
              | Peek_ReportMap_MaybeReportIntendedUse (Path_ReportMap (Maybe ReportIntendedUse))
                                                      (Maybe ReportIntendedUse)
              | Peek_ReportMap_Report (Path_ReportMap Report) Report
              | Peek_ReportMap_ReportElem (Path_ReportMap ReportElem) ReportElem
              | Peek_ReportMap_ReportElems (Path_ReportMap (Order ReportElemID
                                                                  ReportElem))
                                           (Order ReportElemID ReportElem)
              | Peek_ReportMap_ReportFlags (Path_ReportMap ReportFlags)
                                           ReportFlags
              | Peek_ReportMap_ReportStandard (Path_ReportMap ReportStandard)
                                              ReportStandard
              | Peek_ReportMap_ReportStatus (Path_ReportMap ReportStatus)
                                            ReportStatus
              | Peek_ReportMap_ReportValueApproachInfo (Path_ReportMap ReportValueApproachInfo)
                                                       ReportValueApproachInfo
              | Peek_ReportMap_ReportValueTypeInfo (Path_ReportMap ReportValueTypeInfo)
                                                   ReportValueTypeInfo
              | Peek_ReportMap_EUI (Path_ReportMap (Either URI ImageFile))
                                   (Either URI ImageFile)
              | Peek_ReportMap_MEUI (Path_ReportMap (Maybe (Either URI
                                                                   ImageFile)))
                                    (Maybe (Either URI ImageFile))
              | Peek_ReportMap_MaybeImageFile (Path_ReportMap (Maybe ImageFile))
                                              (Maybe ImageFile)
              | Peek_ReportMap_ReportImage (Path_ReportMap ReportImage)
                                           ReportImage
              | Peek_ReportMap_ReportImages (Path_ReportMap (Order ReportImageID
                                                                   ReportImage))
                                            (Order ReportImageID ReportImage)
              | Peek_ReportMap_ReadOnlyFilePath (Path_ReportMap (ReadOnly ([Char])))
                                                (ReadOnly ([Char]))
              | Peek_ReportMap_ReportImageView (Path_ReportMap ReportImageView)
                                               ReportImageView
              | Peek_ReportMap_ReportView (Path_ReportMap ReportView) ReportView
              | Peek_ReportMap_SaneSizeImageSize (Path_ReportMap (SaneSize ImageSize))
                                                 (SaneSize ImageSize)
              | Peek_ReportMap_Item (Path_ReportMap Item) Item
              | Peek_ReportMap_MIM (Path_ReportMap (Map ItemFieldName Markup))
                                   (Map ItemFieldName Markup)
              | Peek_ReportMap_MRR (Path_ReportMap (Map ReportID Report))
                                   (Map ReportID Report)
              | Peek_ReportMap_ReportMap (Path_ReportMap ReportMap) ReportMap
              | Peek_ReportMap_CIString (Path_ReportMap CIString) CIString
              | Peek_ReportMap_URI (Path_ReportMap URI) URI
              | Peek_ReportMap_Text (Path_ReportMap Text) Text
              | Peek_ReportMap_UserId (Path_ReportMap UserId) UserId
              | Peek_ReportMap_UUID (Path_ReportMap UUID) UUID
              deriving (Eq, Show)
          peek x = [case filter (\p -> case p of
                                           Path_ReportMap_unReportMap _ -> True
                                           _ -> False) (pathsOf x (undefined :: Proxy (Map ReportID
                                                                                           Report))) :: [Path_ReportMap (Map ReportID
                                                                                                                             Report)] of
                        [p@(Path_ReportMap_unReportMap _)] -> let [y] = toListOf (toLens p) x :: [Map ReportID
                                                                                                      Report]
                                                               in Node (Peek_ReportMap_MRR p y) (forestMap (\v -> case v of
                                                                                                                      Peek_MRR_String q
                                                                                                                                      x -> Peek_ReportMap_String ((Path_ReportMap_unReportMap :: Path_MRR ([Char]) ->
                                                                                                                                                                                                 Path_ReportMap ([Char])) q) x
                                                                                                                      Peek_MRR_Int64 q
                                                                                                                                     x -> Peek_ReportMap_Int64 ((Path_ReportMap_unReportMap :: Path_MRR Int64 ->
                                                                                                                                                                                               Path_ReportMap Int64) q) x
                                                                                                                      Peek_MRR_Int q
                                                                                                                                   x -> Peek_ReportMap_Int ((Path_ReportMap_unReportMap :: Path_MRR Int ->
                                                                                                                                                                                           Path_ReportMap Int) q) x
                                                                                                                      Peek_MRR_Bool q
                                                                                                                                    x -> Peek_ReportMap_Bool ((Path_ReportMap_unReportMap :: Path_MRR Bool ->
                                                                                                                                                                                             Path_ReportMap Bool) q) x
                                                                                                                      Peek_MRR_Double q
                                                                                                                                      x -> Peek_ReportMap_Double ((Path_ReportMap_unReportMap :: Path_MRR Double ->
                                                                                                                                                                                                 Path_ReportMap Double) q) x
                                                                                                                      Peek_MRR_Dimension q
                                                                                                                                         x -> Peek_ReportMap_Dimension ((Path_ReportMap_unReportMap :: Path_MRR Dimension ->
                                                                                                                                                                                                       Path_ReportMap Dimension) q) x
                                                                                                                      Peek_MRR_ImageCrop q
                                                                                                                                         x -> Peek_ReportMap_ImageCrop ((Path_ReportMap_unReportMap :: Path_MRR ImageCrop ->
                                                                                                                                                                                                       Path_ReportMap ImageCrop) q) x
                                                                                                                      Peek_MRR_ImageSize q
                                                                                                                                         x -> Peek_ReportMap_ImageSize ((Path_ReportMap_unReportMap :: Path_MRR ImageSize ->
                                                                                                                                                                                                       Path_ReportMap ImageSize) q) x
                                                                                                                      Peek_MRR_Units q
                                                                                                                                     x -> Peek_ReportMap_Units ((Path_ReportMap_unReportMap :: Path_MRR Units ->
                                                                                                                                                                                               Path_ReportMap Units) q) x
                                                                                                                      Peek_MRR_ImageFile q
                                                                                                                                         x -> Peek_ReportMap_ImageFile ((Path_ReportMap_unReportMap :: Path_MRR ImageFile ->
                                                                                                                                                                                                       Path_ReportMap ImageFile) q) x
                                                                                                                      Peek_MRR_Integer q
                                                                                                                                       x -> Peek_ReportMap_Integer ((Path_ReportMap_unReportMap :: Path_MRR Integer ->
                                                                                                                                                                                                   Path_ReportMap Integer) q) x
                                                                                                                      Peek_MRR_JSONText q
                                                                                                                                        x -> Peek_ReportMap_JSONText ((Path_ReportMap_unReportMap :: Path_MRR JSONText ->
                                                                                                                                                                                                     Path_ReportMap JSONText) q) x
                                                                                                                      Peek_MRR_Markup q
                                                                                                                                      x -> Peek_ReportMap_Markup ((Path_ReportMap_unReportMap :: Path_MRR Markup ->
                                                                                                                                                                                                 Path_ReportMap Markup) q) x
                                                                                                                      Peek_MRR_Permissions q
                                                                                                                                           x -> Peek_ReportMap_Permissions ((Path_ReportMap_unReportMap :: Path_MRR Permissions ->
                                                                                                                                                                                                           Path_ReportMap Permissions) q) x
                                                                                                                      Peek_MRR_UserIds q
                                                                                                                                       x -> Peek_ReportMap_UserIds ((Path_ReportMap_unReportMap :: Path_MRR ([UserId]) ->
                                                                                                                                                                                                   Path_ReportMap ([UserId])) q) x
                                                                                                                      Peek_MRR_AbbrevPair q
                                                                                                                                          x -> Peek_ReportMap_AbbrevPair ((Path_ReportMap_unReportMap :: Path_MRR ((CIString,
                                                                                                                                                                                                                    Markup)) ->
                                                                                                                                                                                                         Path_ReportMap ((CIString,
                                                                                                                                                                                                                          Markup))) q) x
                                                                                                                      Peek_MRR_AbbrevPairs q
                                                                                                                                           x -> Peek_ReportMap_AbbrevPairs ((Path_ReportMap_unReportMap :: Path_MRR (Order AbbrevPairID
                                                                                                                                                                                                                           ((CIString,
                                                                                                                                                                                                                             Markup))) ->
                                                                                                                                                                                                           Path_ReportMap (Order AbbrevPairID
                                                                                                                                                                                                                                 ((CIString,
                                                                                                                                                                                                                                   Markup)))) q) x
                                                                                                                      Peek_MRR_Author q
                                                                                                                                      x -> Peek_ReportMap_Author ((Path_ReportMap_unReportMap :: Path_MRR Author ->
                                                                                                                                                                                                 Path_ReportMap Author) q) x
                                                                                                                      Peek_MRR_Authors q
                                                                                                                                       x -> Peek_ReportMap_Authors ((Path_ReportMap_unReportMap :: Path_MRR (Order AuthorID
                                                                                                                                                                                                                   Author) ->
                                                                                                                                                                                                   Path_ReportMap (Order AuthorID
                                                                                                                                                                                                                         Author)) q) x
                                                                                                                      Peek_MRR_Branding q
                                                                                                                                        x -> Peek_ReportMap_Branding ((Path_ReportMap_unReportMap :: Path_MRR Branding ->
                                                                                                                                                                                                     Path_ReportMap Branding) q) x
                                                                                                                      Peek_MRR_MarkupPair q
                                                                                                                                          x -> Peek_ReportMap_MarkupPair ((Path_ReportMap_unReportMap :: Path_MRR ((Markup,
                                                                                                                                                                                                                    Markup)) ->
                                                                                                                                                                                                         Path_ReportMap ((Markup,
                                                                                                                                                                                                                          Markup))) q) x
                                                                                                                      Peek_MRR_MarkupPairs q
                                                                                                                                           x -> Peek_ReportMap_MarkupPairs ((Path_ReportMap_unReportMap :: Path_MRR (Order MarkupPairID
                                                                                                                                                                                                                           ((Markup,
                                                                                                                                                                                                                             Markup))) ->
                                                                                                                                                                                                           Path_ReportMap (Order MarkupPairID
                                                                                                                                                                                                                                 ((Markup,
                                                                                                                                                                                                                                   Markup)))) q) x
                                                                                                                      Peek_MRR_Markups q
                                                                                                                                       x -> Peek_ReportMap_Markups ((Path_ReportMap_unReportMap :: Path_MRR (Order MarkupID
                                                                                                                                                                                                                   Markup) ->
                                                                                                                                                                                                   Path_ReportMap (Order MarkupID
                                                                                                                                                                                                                         Markup)) q) x
                                                                                                                      Peek_MRR_MaybeReportIntendedUse q
                                                                                                                                                      x -> Peek_ReportMap_MaybeReportIntendedUse ((Path_ReportMap_unReportMap :: Path_MRR (Maybe ReportIntendedUse) ->
                                                                                                                                                                                                                                 Path_ReportMap (Maybe ReportIntendedUse)) q) x
                                                                                                                      Peek_MRR_Report q
                                                                                                                                      x -> Peek_ReportMap_Report ((Path_ReportMap_unReportMap :: Path_MRR Report ->
                                                                                                                                                                                                 Path_ReportMap Report) q) x
                                                                                                                      Peek_MRR_ReportElem q
                                                                                                                                          x -> Peek_ReportMap_ReportElem ((Path_ReportMap_unReportMap :: Path_MRR ReportElem ->
                                                                                                                                                                                                         Path_ReportMap ReportElem) q) x
                                                                                                                      Peek_MRR_ReportElems q
                                                                                                                                           x -> Peek_ReportMap_ReportElems ((Path_ReportMap_unReportMap :: Path_MRR (Order ReportElemID
                                                                                                                                                                                                                           ReportElem) ->
                                                                                                                                                                                                           Path_ReportMap (Order ReportElemID
                                                                                                                                                                                                                                 ReportElem)) q) x
                                                                                                                      Peek_MRR_ReportFlags q
                                                                                                                                           x -> Peek_ReportMap_ReportFlags ((Path_ReportMap_unReportMap :: Path_MRR ReportFlags ->
                                                                                                                                                                                                           Path_ReportMap ReportFlags) q) x
                                                                                                                      Peek_MRR_ReportStandard q
                                                                                                                                              x -> Peek_ReportMap_ReportStandard ((Path_ReportMap_unReportMap :: Path_MRR ReportStandard ->
                                                                                                                                                                                                                 Path_ReportMap ReportStandard) q) x
                                                                                                                      Peek_MRR_ReportStatus q
                                                                                                                                            x -> Peek_ReportMap_ReportStatus ((Path_ReportMap_unReportMap :: Path_MRR ReportStatus ->
                                                                                                                                                                                                             Path_ReportMap ReportStatus) q) x
                                                                                                                      Peek_MRR_ReportValueApproachInfo q
                                                                                                                                                       x -> Peek_ReportMap_ReportValueApproachInfo ((Path_ReportMap_unReportMap :: Path_MRR ReportValueApproachInfo ->
                                                                                                                                                                                                                                   Path_ReportMap ReportValueApproachInfo) q) x
                                                                                                                      Peek_MRR_ReportValueTypeInfo q
                                                                                                                                                   x -> Peek_ReportMap_ReportValueTypeInfo ((Path_ReportMap_unReportMap :: Path_MRR ReportValueTypeInfo ->
                                                                                                                                                                                                                           Path_ReportMap ReportValueTypeInfo) q) x
                                                                                                                      Peek_MRR_EUI q
                                                                                                                                   x -> Peek_ReportMap_EUI ((Path_ReportMap_unReportMap :: Path_MRR (Either URI
                                                                                                                                                                                                            ImageFile) ->
                                                                                                                                                                                           Path_ReportMap (Either URI
                                                                                                                                                                                                                  ImageFile)) q) x
                                                                                                                      Peek_MRR_MEUI q
                                                                                                                                    x -> Peek_ReportMap_MEUI ((Path_ReportMap_unReportMap :: Path_MRR (Maybe (Either URI
                                                                                                                                                                                                                     ImageFile)) ->
                                                                                                                                                                                             Path_ReportMap (Maybe (Either URI
                                                                                                                                                                                                                           ImageFile))) q) x
                                                                                                                      Peek_MRR_MaybeImageFile q
                                                                                                                                              x -> Peek_ReportMap_MaybeImageFile ((Path_ReportMap_unReportMap :: Path_MRR (Maybe ImageFile) ->
                                                                                                                                                                                                                 Path_ReportMap (Maybe ImageFile)) q) x
                                                                                                                      Peek_MRR_ReportImage q
                                                                                                                                           x -> Peek_ReportMap_ReportImage ((Path_ReportMap_unReportMap :: Path_MRR ReportImage ->
                                                                                                                                                                                                           Path_ReportMap ReportImage) q) x
                                                                                                                      Peek_MRR_ReportImages q
                                                                                                                                            x -> Peek_ReportMap_ReportImages ((Path_ReportMap_unReportMap :: Path_MRR (Order ReportImageID
                                                                                                                                                                                                                             ReportImage) ->
                                                                                                                                                                                                             Path_ReportMap (Order ReportImageID
                                                                                                                                                                                                                                   ReportImage)) q) x
                                                                                                                      Peek_MRR_ReadOnlyFilePath q
                                                                                                                                                x -> Peek_ReportMap_ReadOnlyFilePath ((Path_ReportMap_unReportMap :: Path_MRR (ReadOnly ([Char])) ->
                                                                                                                                                                                                                     Path_ReportMap (ReadOnly ([Char]))) q) x
                                                                                                                      Peek_MRR_ReportImageView q
                                                                                                                                               x -> Peek_ReportMap_ReportImageView ((Path_ReportMap_unReportMap :: Path_MRR ReportImageView ->
                                                                                                                                                                                                                   Path_ReportMap ReportImageView) q) x
                                                                                                                      Peek_MRR_ReportView q
                                                                                                                                          x -> Peek_ReportMap_ReportView ((Path_ReportMap_unReportMap :: Path_MRR ReportView ->
                                                                                                                                                                                                         Path_ReportMap ReportView) q) x
                                                                                                                      Peek_MRR_SaneSizeImageSize q
                                                                                                                                                 x -> Peek_ReportMap_SaneSizeImageSize ((Path_ReportMap_unReportMap :: Path_MRR (SaneSize ImageSize) ->
                                                                                                                                                                                                                       Path_ReportMap (SaneSize ImageSize)) q) x
                                                                                                                      Peek_MRR_Item q
                                                                                                                                    x -> Peek_ReportMap_Item ((Path_ReportMap_unReportMap :: Path_MRR Item ->
                                                                                                                                                                                             Path_ReportMap Item) q) x
                                                                                                                      Peek_MRR_MIM q
                                                                                                                                   x -> Peek_ReportMap_MIM ((Path_ReportMap_unReportMap :: Path_MRR (Map ItemFieldName
                                                                                                                                                                                                         Markup) ->
                                                                                                                                                                                           Path_ReportMap (Map ItemFieldName
                                                                                                                                                                                                               Markup)) q) x
                                                                                                                      Peek_MRR_MRR q
                                                                                                                                   x -> Peek_ReportMap_MRR ((Path_ReportMap_unReportMap :: Path_MRR (Map ReportID
                                                                                                                                                                                                         Report) ->
                                                                                                                                                                                           Path_ReportMap (Map ReportID
                                                                                                                                                                                                               Report)) q) x
                                                                                                                      Peek_MRR_CIString q
                                                                                                                                        x -> Peek_ReportMap_CIString ((Path_ReportMap_unReportMap :: Path_MRR CIString ->
                                                                                                                                                                                                     Path_ReportMap CIString) q) x
                                                                                                                      Peek_MRR_URI q
                                                                                                                                   x -> Peek_ReportMap_URI ((Path_ReportMap_unReportMap :: Path_MRR URI ->
                                                                                                                                                                                           Path_ReportMap URI) q) x
                                                                                                                      Peek_MRR_Text q
                                                                                                                                    x -> Peek_ReportMap_Text ((Path_ReportMap_unReportMap :: Path_MRR Text ->
                                                                                                                                                                                             Path_ReportMap Text) q) x
                                                                                                                      Peek_MRR_UserId q
                                                                                                                                      x -> Peek_ReportMap_UserId ((Path_ReportMap_unReportMap :: Path_MRR UserId ->
                                                                                                                                                                                                 Path_ReportMap UserId) q) x
                                                                                                                      Peek_MRR_UUID q
                                                                                                                                    x -> Peek_ReportMap_UUID ((Path_ReportMap_unReportMap :: Path_MRR UUID ->
                                                                                                                                                                                             Path_ReportMap UUID) q) x) (peek y :: Forest (Peek (Map ReportID
                                                                                                                                                                                                                                                     Report))))
                        [] -> error "No Path_ReportMap_unReportMap field found"
                        ps -> error $ ("Multiple Path_ReportMap_unReportMap fields found: " ++ show ps)]
instance IsPathNode CIString
    where data Peek CIString
              = Peek_CIString_JSONText (Path_CIString JSONText) JSONText
              | Peek_CIString_CIString (Path_CIString CIString) CIString
              | Peek_CIString_Text (Path_CIString Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_CIString_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy Text)) :: [Path_CIString Text]
                    in map (\path -> case path of
                                         p@(Path_CIString_View _) -> let [y] = toListOf (toLens p) x :: [Text]
                                                                      in Node (Peek_CIString_Text p y) (forestMap (\v -> case v of
                                                                                                                             Peek_Text_JSONText q
                                                                                                                                                x -> Peek_CIString_JSONText ((Path_CIString_View :: Path_Text JSONText ->
                                                                                                                                                                                                    Path_CIString JSONText) q) x
                                                                                                                             Peek_Text_Text q
                                                                                                                                            x -> Peek_CIString_Text ((Path_CIString_View :: Path_Text Text ->
                                                                                                                                                                                            Path_CIString Text) q) x) (peek y :: Forest (Peek Text)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode URI
    where data Peek URI
              = Peek_URI_URI (Path_URI URI) URI
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode Text
    where data Peek Text
              = Peek_Text_JSONText (Path_Text JSONText) JSONText
              | Peek_Text_Text (Path_Text Text) Text
              deriving (Eq, Show)
          peek x = let paths = filter (\p -> case p of
                                                 Path_Text_View _ -> True
                                                 _ -> False) (pathsOf x (undefined :: Proxy JSONText)) :: [Path_Text JSONText]
                    in map (\path -> case path of
                                         p@(Path_Text_View _) -> let [y] = toListOf (toLens p) x :: [JSONText]
                                                                  in Node (Peek_Text_JSONText p y) (forestMap (\v -> case v of
                                                                                                                         Peek_JSONText_JSONText q
                                                                                                                                                x -> Peek_Text_JSONText ((Path_Text_View :: Path_JSONText JSONText ->
                                                                                                                                                                                            Path_Text JSONText) q) x) (peek y :: Forest (Peek JSONText)))
                                         _ -> error ("doPeekNodesOf: " ++ show path)) paths
instance IsPathNode UserId
    where data Peek UserId
              = Peek_UserId_UserId (Path_UserId UserId) UserId
              deriving (Eq, Show)
          peek _ = []
instance IsPathNode UUID
    where data Peek UUID
              = Peek_UUID_UUID (Path_UUID UUID) UUID
              deriving (Eq, Show)
          peek _ = []
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
instance ToLens (Path_Either (Path_URI ImageFile)
                             (Path_ImageFile ImageFile))
    where type S (Path_Either (Path_URI ImageFile)
                              (Path_ImageFile ImageFile)) = EUI
          type A (Path_Either (Path_URI ImageFile)
                              (Path_ImageFile ImageFile)) = ImageFile
          toLens (Path_Right _) = _Right
instance ToLens (Path_Either (Path_URI EUI) (Path_ImageFile EUI))
    where type S (Path_Either (Path_URI EUI)
                              (Path_ImageFile EUI)) = EUI
          type A (Path_Either (Path_URI EUI) (Path_ImageFile EUI)) = EUI
          toLens _ = id
instance ToLens (Path_Either (Path_URI URI) (Path_ImageFile URI))
    where type S (Path_Either (Path_URI URI)
                              (Path_ImageFile URI)) = EUI
          type A (Path_Either (Path_URI URI) (Path_ImageFile URI)) = URI
          toLens (Path_Left _) = _Left
instance ToLens (Path_Map ItemFieldName (Path_Markup JSONText))
    where type S (Path_Map ItemFieldName (Path_Markup JSONText)) = MIM
          type A (Path_Map ItemFieldName (Path_Markup JSONText)) = JSONText
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ItemFieldName (Path_Markup Markup))
    where type S (Path_Map ItemFieldName (Path_Markup Markup)) = MIM
          type A (Path_Map ItemFieldName (Path_Markup Markup)) = Markup
          toLens (Path_Look k _) = mat k
instance ToLens (Path_Map ItemFieldName (Path_Markup MIM))
    where type S (Path_Map ItemFieldName (Path_Markup MIM)) = MIM
          type A (Path_Map ItemFieldName (Path_Markup MIM)) = MIM
          toLens _ = id
instance ToLens (Path_Map ItemFieldName (Path_Markup Text))
    where type S (Path_Map ItemFieldName (Path_Markup Text)) = MIM
          type A (Path_Map ItemFieldName (Path_Markup Text)) = Text
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report String))
    where type S (Path_Map ReportID (Path_Report String)) = MRR
          type A (Path_Map ReportID (Path_Report String)) = String
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Int64))
    where type S (Path_Map ReportID (Path_Report Int64)) = MRR
          type A (Path_Map ReportID (Path_Report Int64)) = Int64
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Bool))
    where type S (Path_Map ReportID (Path_Report Bool)) = MRR
          type A (Path_Map ReportID (Path_Report Bool)) = Bool
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Double))
    where type S (Path_Map ReportID (Path_Report Double)) = MRR
          type A (Path_Map ReportID (Path_Report Double)) = Double
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Int))
    where type S (Path_Map ReportID (Path_Report Int)) = MRR
          type A (Path_Map ReportID (Path_Report Int)) = Int
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Dimension))
    where type S (Path_Map ReportID (Path_Report Dimension)) = MRR
          type A (Path_Map ReportID (Path_Report Dimension)) = Dimension
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ImageCrop))
    where type S (Path_Map ReportID (Path_Report ImageCrop)) = MRR
          type A (Path_Map ReportID (Path_Report ImageCrop)) = ImageCrop
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ImageSize))
    where type S (Path_Map ReportID (Path_Report ImageSize)) = MRR
          type A (Path_Map ReportID (Path_Report ImageSize)) = ImageSize
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Units))
    where type S (Path_Map ReportID (Path_Report Units)) = MRR
          type A (Path_Map ReportID (Path_Report Units)) = Units
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ImageFile))
    where type S (Path_Map ReportID (Path_Report ImageFile)) = MRR
          type A (Path_Map ReportID (Path_Report ImageFile)) = ImageFile
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Integer))
    where type S (Path_Map ReportID (Path_Report Integer)) = MRR
          type A (Path_Map ReportID (Path_Report Integer)) = Integer
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report JSONText))
    where type S (Path_Map ReportID (Path_Report JSONText)) = MRR
          type A (Path_Map ReportID (Path_Report JSONText)) = JSONText
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Markup))
    where type S (Path_Map ReportID (Path_Report Markup)) = MRR
          type A (Path_Map ReportID (Path_Report Markup)) = Markup
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Permissions))
    where type S (Path_Map ReportID (Path_Report Permissions)) = MRR
          type A (Path_Map ReportID (Path_Report Permissions)) = Permissions
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report UserIds))
    where type S (Path_Map ReportID (Path_Report UserIds)) = MRR
          type A (Path_Map ReportID (Path_Report UserIds)) = UserIds
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report AbbrevPair))
    where type S (Path_Map ReportID (Path_Report AbbrevPair)) = MRR
          type A (Path_Map ReportID (Path_Report AbbrevPair)) = AbbrevPair
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report AbbrevPairs))
    where type S (Path_Map ReportID (Path_Report AbbrevPairs)) = MRR
          type A (Path_Map ReportID (Path_Report AbbrevPairs)) = AbbrevPairs
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Author))
    where type S (Path_Map ReportID (Path_Report Author)) = MRR
          type A (Path_Map ReportID (Path_Report Author)) = Author
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Authors))
    where type S (Path_Map ReportID (Path_Report Authors)) = MRR
          type A (Path_Map ReportID (Path_Report Authors)) = Authors
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Branding))
    where type S (Path_Map ReportID (Path_Report Branding)) = MRR
          type A (Path_Map ReportID (Path_Report Branding)) = Branding
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report MarkupPair))
    where type S (Path_Map ReportID (Path_Report MarkupPair)) = MRR
          type A (Path_Map ReportID (Path_Report MarkupPair)) = MarkupPair
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report MarkupPairs))
    where type S (Path_Map ReportID (Path_Report MarkupPairs)) = MRR
          type A (Path_Map ReportID (Path_Report MarkupPairs)) = MarkupPairs
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Markups))
    where type S (Path_Map ReportID (Path_Report Markups)) = MRR
          type A (Path_Map ReportID (Path_Report Markups)) = Markups
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID
                          (Path_Report MaybeReportIntendedUse))
    where type S (Path_Map ReportID
                           (Path_Report MaybeReportIntendedUse)) = MRR
          type A (Path_Map ReportID
                           (Path_Report MaybeReportIntendedUse)) = MaybeReportIntendedUse
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Report))
    where type S (Path_Map ReportID (Path_Report Report)) = MRR
          type A (Path_Map ReportID (Path_Report Report)) = Report
          toLens (Path_Look k _) = mat k
instance ToLens (Path_Map ReportID (Path_Report ReportElem))
    where type S (Path_Map ReportID (Path_Report ReportElem)) = MRR
          type A (Path_Map ReportID (Path_Report ReportElem)) = ReportElem
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportElems))
    where type S (Path_Map ReportID (Path_Report ReportElems)) = MRR
          type A (Path_Map ReportID (Path_Report ReportElems)) = ReportElems
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportFlags))
    where type S (Path_Map ReportID (Path_Report ReportFlags)) = MRR
          type A (Path_Map ReportID (Path_Report ReportFlags)) = ReportFlags
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportStandard))
    where type S (Path_Map ReportID (Path_Report ReportStandard)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReportStandard)) = ReportStandard
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportStatus))
    where type S (Path_Map ReportID (Path_Report ReportStatus)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReportStatus)) = ReportStatus
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID
                          (Path_Report ReportValueApproachInfo))
    where type S (Path_Map ReportID
                           (Path_Report ReportValueApproachInfo)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReportValueApproachInfo)) = ReportValueApproachInfo
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID
                          (Path_Report ReportValueTypeInfo))
    where type S (Path_Map ReportID
                           (Path_Report ReportValueTypeInfo)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReportValueTypeInfo)) = ReportValueTypeInfo
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report EUI))
    where type S (Path_Map ReportID (Path_Report EUI)) = MRR
          type A (Path_Map ReportID (Path_Report EUI)) = EUI
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report MEUI))
    where type S (Path_Map ReportID (Path_Report MEUI)) = MRR
          type A (Path_Map ReportID (Path_Report MEUI)) = MEUI
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report MaybeImageFile))
    where type S (Path_Map ReportID (Path_Report MaybeImageFile)) = MRR
          type A (Path_Map ReportID
                           (Path_Report MaybeImageFile)) = MaybeImageFile
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportImage))
    where type S (Path_Map ReportID (Path_Report ReportImage)) = MRR
          type A (Path_Map ReportID (Path_Report ReportImage)) = ReportImage
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportImages))
    where type S (Path_Map ReportID (Path_Report ReportImages)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReportImages)) = ReportImages
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReadOnlyFilePath))
    where type S (Path_Map ReportID
                           (Path_Report ReadOnlyFilePath)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReadOnlyFilePath)) = ReadOnlyFilePath
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportImageView))
    where type S (Path_Map ReportID
                           (Path_Report ReportImageView)) = MRR
          type A (Path_Map ReportID
                           (Path_Report ReportImageView)) = ReportImageView
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report ReportView))
    where type S (Path_Map ReportID (Path_Report ReportView)) = MRR
          type A (Path_Map ReportID (Path_Report ReportView)) = ReportView
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report SaneSizeImageSize))
    where type S (Path_Map ReportID
                           (Path_Report SaneSizeImageSize)) = MRR
          type A (Path_Map ReportID
                           (Path_Report SaneSizeImageSize)) = SaneSizeImageSize
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Item))
    where type S (Path_Map ReportID (Path_Report Item)) = MRR
          type A (Path_Map ReportID (Path_Report Item)) = Item
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report MIM))
    where type S (Path_Map ReportID (Path_Report MIM)) = MRR
          type A (Path_Map ReportID (Path_Report MIM)) = MIM
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report MRR))
    where type S (Path_Map ReportID (Path_Report MRR)) = MRR
          type A (Path_Map ReportID (Path_Report MRR)) = MRR
          toLens _ = id
instance ToLens (Path_Map ReportID (Path_Report CIString))
    where type S (Path_Map ReportID (Path_Report CIString)) = MRR
          type A (Path_Map ReportID (Path_Report CIString)) = CIString
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report URI))
    where type S (Path_Map ReportID (Path_Report URI)) = MRR
          type A (Path_Map ReportID (Path_Report URI)) = URI
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report Text))
    where type S (Path_Map ReportID (Path_Report Text)) = MRR
          type A (Path_Map ReportID (Path_Report Text)) = Text
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report UserId))
    where type S (Path_Map ReportID (Path_Report UserId)) = MRR
          type A (Path_Map ReportID (Path_Report UserId)) = UserId
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Map ReportID (Path_Report UUID))
    where type S (Path_Map ReportID (Path_Report UUID)) = MRR
          type A (Path_Map ReportID (Path_Report UUID)) = UUID
          toLens (Path_Look k v) = mat k . toLens v
instance ToLens (Path_Pair (Path_CIString JSONText)
                           (Path_Markup JSONText))
    where type S (Path_Pair (Path_CIString JSONText)
                            (Path_Markup JSONText)) = AbbrevPair
          type A (Path_Pair (Path_CIString JSONText)
                            (Path_Markup JSONText)) = JSONText
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
instance ToLens (Path_Pair (Path_CIString Markup)
                           (Path_Markup Markup))
    where type S (Path_Pair (Path_CIString Markup)
                            (Path_Markup Markup)) = AbbrevPair
          type A (Path_Pair (Path_CIString Markup)
                            (Path_Markup Markup)) = Markup
          toLens (Path_Second _) = _2
instance ToLens (Path_Pair (Path_CIString AbbrevPair)
                           (Path_Markup AbbrevPair))
    where type S (Path_Pair (Path_CIString AbbrevPair)
                            (Path_Markup AbbrevPair)) = AbbrevPair
          type A (Path_Pair (Path_CIString AbbrevPair)
                            (Path_Markup AbbrevPair)) = AbbrevPair
          toLens _ = id
instance ToLens (Path_Pair (Path_CIString CIString)
                           (Path_Markup CIString))
    where type S (Path_Pair (Path_CIString CIString)
                            (Path_Markup CIString)) = AbbrevPair
          type A (Path_Pair (Path_CIString CIString)
                            (Path_Markup CIString)) = CIString
          toLens (Path_First _) = _1
instance ToLens (Path_Pair (Path_CIString Text) (Path_Markup Text))
    where type S (Path_Pair (Path_CIString Text)
                            (Path_Markup Text)) = AbbrevPair
          type A (Path_Pair (Path_CIString Text) (Path_Markup Text)) = Text
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
instance ToLens (Path_Pair (Path_Markup JSONText)
                           (Path_Markup JSONText))
    where type S (Path_Pair (Path_Markup JSONText)
                            (Path_Markup JSONText)) = MarkupPair
          type A (Path_Pair (Path_Markup JSONText)
                            (Path_Markup JSONText)) = JSONText
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
instance ToLens (Path_Pair (Path_Markup Markup)
                           (Path_Markup Markup))
    where type S (Path_Pair (Path_Markup Markup)
                            (Path_Markup Markup)) = MarkupPair
          type A (Path_Pair (Path_Markup Markup)
                            (Path_Markup Markup)) = Markup
          toLens (Path_First _) = _1
          toLens (Path_Second _) = _2
instance ToLens (Path_Pair (Path_Markup MarkupPair)
                           (Path_Markup MarkupPair))
    where type S (Path_Pair (Path_Markup MarkupPair)
                            (Path_Markup MarkupPair)) = MarkupPair
          type A (Path_Pair (Path_Markup MarkupPair)
                            (Path_Markup MarkupPair)) = MarkupPair
          toLens _ = id
instance ToLens (Path_Pair (Path_Markup Text) (Path_Markup Text))
    where type S (Path_Pair (Path_Markup Text)
                            (Path_Markup Text)) = MarkupPair
          type A (Path_Pair (Path_Markup Text) (Path_Markup Text)) = Text
          toLens (Path_First v) = _1 . toLens v
          toLens (Path_Second v) = _2 . toLens v
instance ToLens (Path_OMap AbbrevPairID
                           (Path_Pair (Path_CIString JSONText) (Path_Markup JSONText)))
    where type S (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString JSONText)
                                       (Path_Markup JSONText))) = AbbrevPairs
          type A (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString JSONText)
                                       (Path_Markup JSONText))) = JSONText
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AbbrevPairID
                           (Path_Pair (Path_CIString Markup) (Path_Markup Markup)))
    where type S (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString Markup)
                                       (Path_Markup Markup))) = AbbrevPairs
          type A (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString Markup) (Path_Markup Markup))) = Markup
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AbbrevPairID
                           (Path_Pair (Path_CIString AbbrevPair) (Path_Markup AbbrevPair)))
    where type S (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString AbbrevPair)
                                       (Path_Markup AbbrevPair))) = AbbrevPairs
          type A (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString AbbrevPair)
                                       (Path_Markup AbbrevPair))) = AbbrevPair
          toLens (Path_At k _) = lens_omat k
instance ToLens (Path_OMap AbbrevPairID
                           (Path_Pair (Path_CIString AbbrevPairs) (Path_Markup AbbrevPairs)))
    where type S (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString AbbrevPairs)
                                       (Path_Markup AbbrevPairs))) = AbbrevPairs
          type A (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString AbbrevPairs)
                                       (Path_Markup AbbrevPairs))) = AbbrevPairs
          toLens _ = id
instance ToLens (Path_OMap AbbrevPairID
                           (Path_Pair (Path_CIString CIString) (Path_Markup CIString)))
    where type S (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString CIString)
                                       (Path_Markup CIString))) = AbbrevPairs
          type A (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString CIString)
                                       (Path_Markup CIString))) = CIString
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AbbrevPairID
                           (Path_Pair (Path_CIString Text) (Path_Markup Text)))
    where type S (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString Text) (Path_Markup Text))) = AbbrevPairs
          type A (Path_OMap AbbrevPairID
                            (Path_Pair (Path_CIString Text) (Path_Markup Text))) = Text
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AuthorID (Path_Author JSONText))
    where type S (Path_OMap AuthorID (Path_Author JSONText)) = Authors
          type A (Path_OMap AuthorID (Path_Author JSONText)) = JSONText
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AuthorID (Path_Author Markup))
    where type S (Path_OMap AuthorID (Path_Author Markup)) = Authors
          type A (Path_OMap AuthorID (Path_Author Markup)) = Markup
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap AuthorID (Path_Author Author))
    where type S (Path_OMap AuthorID (Path_Author Author)) = Authors
          type A (Path_OMap AuthorID (Path_Author Author)) = Author
          toLens (Path_At k _) = lens_omat k
instance ToLens (Path_OMap AuthorID (Path_Author Authors))
    where type S (Path_OMap AuthorID (Path_Author Authors)) = Authors
          type A (Path_OMap AuthorID (Path_Author Authors)) = Authors
          toLens _ = id
instance ToLens (Path_OMap AuthorID (Path_Author Text))
    where type S (Path_OMap AuthorID (Path_Author Text)) = Authors
          type A (Path_OMap AuthorID (Path_Author Text)) = Text
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupID (Path_Markup JSONText))
    where type S (Path_OMap MarkupID (Path_Markup JSONText)) = Markups
          type A (Path_OMap MarkupID (Path_Markup JSONText)) = JSONText
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupID (Path_Markup Markup))
    where type S (Path_OMap MarkupID (Path_Markup Markup)) = Markups
          type A (Path_OMap MarkupID (Path_Markup Markup)) = Markup
          toLens (Path_At k _) = lens_omat k
instance ToLens (Path_OMap MarkupID (Path_Markup Markups))
    where type S (Path_OMap MarkupID (Path_Markup Markups)) = Markups
          type A (Path_OMap MarkupID (Path_Markup Markups)) = Markups
          toLens _ = id
instance ToLens (Path_OMap MarkupID (Path_Markup Text))
    where type S (Path_OMap MarkupID (Path_Markup Text)) = Markups
          type A (Path_OMap MarkupID (Path_Markup Text)) = Text
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupPairID
                           (Path_Pair (Path_Markup JSONText) (Path_Markup JSONText)))
    where type S (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup JSONText)
                                       (Path_Markup JSONText))) = MarkupPairs
          type A (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup JSONText)
                                       (Path_Markup JSONText))) = JSONText
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupPairID
                           (Path_Pair (Path_Markup Markup) (Path_Markup Markup)))
    where type S (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup Markup)
                                       (Path_Markup Markup))) = MarkupPairs
          type A (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup Markup) (Path_Markup Markup))) = Markup
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap MarkupPairID
                           (Path_Pair (Path_Markup MarkupPair) (Path_Markup MarkupPair)))
    where type S (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup MarkupPair)
                                       (Path_Markup MarkupPair))) = MarkupPairs
          type A (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup MarkupPair)
                                       (Path_Markup MarkupPair))) = MarkupPair
          toLens (Path_At k _) = lens_omat k
instance ToLens (Path_OMap MarkupPairID
                           (Path_Pair (Path_Markup MarkupPairs) (Path_Markup MarkupPairs)))
    where type S (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup MarkupPairs)
                                       (Path_Markup MarkupPairs))) = MarkupPairs
          type A (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup MarkupPairs)
                                       (Path_Markup MarkupPairs))) = MarkupPairs
          toLens _ = id
instance ToLens (Path_OMap MarkupPairID
                           (Path_Pair (Path_Markup Text) (Path_Markup Text)))
    where type S (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup Text) (Path_Markup Text))) = MarkupPairs
          type A (Path_OMap MarkupPairID
                            (Path_Pair (Path_Markup Text) (Path_Markup Text))) = Text
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem String))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem String)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem String)) = String
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem Bool))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Bool)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem Bool)) = Bool
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem Double))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Double)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem Double)) = Double
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem Dimension))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Dimension)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem Dimension)) = Dimension
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ImageCrop))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ImageCrop)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ImageCrop)) = ImageCrop
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ImageSize))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ImageSize)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ImageSize)) = ImageSize
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem Units))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Units)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem Units)) = Units
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ImageFile))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ImageFile)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ImageFile)) = ImageFile
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem JSONText))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem JSONText)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem JSONText)) = JSONText
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem Markup))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Markup)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem Markup)) = Markup
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ReportElem))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ReportElem)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ReportElem)) = ReportElem
          toLens (Path_At k _) = lens_omat k
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ReportElems))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ReportElems)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ReportElems)) = ReportElems
          toLens _ = id
instance ToLens (Path_OMap ReportElemID (Path_ReportElem EUI))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem EUI)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem EUI)) = EUI
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem MEUI))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem MEUI)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem MEUI)) = MEUI
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem MaybeImageFile))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem MaybeImageFile)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem MaybeImageFile)) = MaybeImageFile
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ReportImage))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ReportImage)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ReportImage)) = ReportImage
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ReportImages))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ReportImages)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ReportImages)) = ReportImages
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem ReportImageView))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem ReportImageView)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem ReportImageView)) = ReportImageView
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID
                           (Path_ReportElem SaneSizeImageSize))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem SaneSizeImageSize)) = ReportElems
          type A (Path_OMap ReportElemID
                            (Path_ReportElem SaneSizeImageSize)) = SaneSizeImageSize
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem Item))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Item)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem Item)) = Item
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem MIM))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem MIM)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem MIM)) = MIM
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem URI))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem URI)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem URI)) = URI
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportElemID (Path_ReportElem Text))
    where type S (Path_OMap ReportElemID
                            (Path_ReportElem Text)) = ReportElems
          type A (Path_OMap ReportElemID (Path_ReportElem Text)) = Text
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage String))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage String)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage String)) = String
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage Bool))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage Bool)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage Bool)) = Bool
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage Double))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage Double)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage Double)) = Double
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage Dimension))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage Dimension)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage Dimension)) = Dimension
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage ImageCrop))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage ImageCrop)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage ImageCrop)) = ImageCrop
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage ImageSize))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage ImageSize)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage ImageSize)) = ImageSize
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage Units))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage Units)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage Units)) = Units
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage ImageFile))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage ImageFile)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage ImageFile)) = ImageFile
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage JSONText))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage JSONText)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage JSONText)) = JSONText
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage Markup))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage Markup)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage Markup)) = Markup
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage EUI))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage EUI)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage EUI)) = EUI
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage MEUI))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage MEUI)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage MEUI)) = MEUI
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage MaybeImageFile))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage MaybeImageFile)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage MaybeImageFile)) = MaybeImageFile
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage ReportImage))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage ReportImage)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage ReportImage)) = ReportImage
          toLens (Path_At k _) = lens_omat k
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage ReportImages))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage ReportImages)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage ReportImages)) = ReportImages
          toLens _ = id
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage ReportImageView))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage ReportImageView)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage ReportImageView)) = ReportImageView
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID
                           (Path_ReportImage SaneSizeImageSize))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage SaneSizeImageSize)) = ReportImages
          type A (Path_OMap ReportImageID
                            (Path_ReportImage SaneSizeImageSize)) = SaneSizeImageSize
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage URI))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage URI)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage URI)) = URI
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_OMap ReportImageID (Path_ReportImage Text))
    where type S (Path_OMap ReportImageID
                            (Path_ReportImage Text)) = ReportImages
          type A (Path_OMap ReportImageID (Path_ReportImage Text)) = Text
          toLens (Path_At k v) = lens_omat k . toLens v
instance ToLens (Path_Author JSONText)
    where type S (Path_Author JSONText) = Author
          type A (Path_Author JSONText) = JSONText
          toLens (Path_Author_authorName _x) = (\f x -> fmap (\y -> x{authorName = y}) (f (authorName x))) . toLens _x
          toLens (Path_Author_authorCredentials _x) = (\f x -> fmap (\y -> x{authorCredentials = y}) (f (authorCredentials x))) . toLens _x
instance ToLens (Path_Author Markup)
    where type S (Path_Author Markup) = Author
          type A (Path_Author Markup) = Markup
          toLens (Path_Author_authorName _x) = \f x -> fmap (\y -> x{authorName = y}) (f (authorName x))
          toLens (Path_Author_authorCredentials _x) = \f x -> fmap (\y -> x{authorCredentials = y}) (f (authorCredentials x))
instance ToLens (Path_Author Author)
    where type S (Path_Author Author) = Author
          type A (Path_Author Author) = Author
          toLens _ = id
instance ToLens (Path_Author Text)
    where type S (Path_Author Text) = Author
          type A (Path_Author Text) = Text
          toLens (Path_Author_authorName _x) = (\f x -> fmap (\y -> x{authorName = y}) (f (authorName x))) . toLens _x
          toLens (Path_Author_authorCredentials _x) = (\f x -> fmap (\y -> x{authorCredentials = y}) (f (authorCredentials x))) . toLens _x
instance ToLens (Path_Bool String)
    where type S (Path_Bool String) = Bool
          type A (Path_Bool String) = String
          toLens (Path_Bool_View _) = viewLens :: Lens' Bool ([Char])
instance ToLens (Path_Bool Bool)
    where type S (Path_Bool Bool) = Bool
          type A (Path_Bool Bool) = Bool
          toLens _ = id
instance ToLens (Path_Bool JSONText)
    where type S (Path_Bool JSONText) = Bool
          type A (Path_Bool JSONText) = JSONText
          toLens (Path_Bool_View v) = (viewLens :: Lens' Bool
                                                         ([Char])) . toLens v
instance ToLens (Path_Branding JSONText)
    where type S (Path_Branding JSONText) = Branding
          type A (Path_Branding JSONText) = JSONText
          toLens (Path_Branding_View v) = (viewLens :: Lens' Branding
                                                             Text) . toLens v
instance ToLens (Path_Branding Branding)
    where type S (Path_Branding Branding) = Branding
          type A (Path_Branding Branding) = Branding
          toLens _ = id
instance ToLens (Path_Branding Text)
    where type S (Path_Branding Text) = Branding
          type A (Path_Branding Text) = Text
          toLens (Path_Branding_View _) = viewLens :: Lens' Branding Text
instance ToLens (Path_CIString JSONText)
    where type S (Path_CIString JSONText) = CIString
          type A (Path_CIString JSONText) = JSONText
          toLens (Path_CIString_View v) = (viewLens :: Lens' CIString
                                                             Text) . toLens v
instance ToLens (Path_CIString CIString)
    where type S (Path_CIString CIString) = CIString
          type A (Path_CIString CIString) = CIString
          toLens _ = id
instance ToLens (Path_CIString Text)
    where type S (Path_CIString Text) = CIString
          type A (Path_CIString Text) = Text
          toLens (Path_CIString_View _) = viewLens :: Lens' CIString Text
instance ToLens (Path_Dimension Dimension)
    where type S (Path_Dimension Dimension) = Dimension
          type A (Path_Dimension Dimension) = Dimension
          toLens _ = id
instance ToLens (Path_Dimension JSONText)
    where type S (Path_Dimension JSONText) = Dimension
          type A (Path_Dimension JSONText) = JSONText
          toLens (Path_Dimension_View _) = viewLens :: Lens' Dimension
                                                             JSONText
instance ToLens (Path_Double String)
    where type S (Path_Double String) = Double
          type A (Path_Double String) = String
          toLens (Path_Double_View _) = viewLens :: Lens' Double ([Char])
instance ToLens (Path_Double Double)
    where type S (Path_Double Double) = Double
          type A (Path_Double Double) = Double
          toLens _ = id
instance ToLens (Path_Double JSONText)
    where type S (Path_Double JSONText) = Double
          type A (Path_Double JSONText) = JSONText
          toLens (Path_Double_View v) = (viewLens :: Lens' Double
                                                           ([Char])) . toLens v
instance ToLens (Path_ImageCrop ImageCrop)
    where type S (Path_ImageCrop ImageCrop) = ImageCrop
          type A (Path_ImageCrop ImageCrop) = ImageCrop
          toLens _ = id
instance ToLens (Path_ImageFile ImageFile)
    where type S (Path_ImageFile ImageFile) = ImageFile
          type A (Path_ImageFile ImageFile) = ImageFile
          toLens _ = id
instance ToLens (Path_ImageSize String)
    where type S (Path_ImageSize String) = ImageSize
          type A (Path_ImageSize String) = String
          toLens (Path_ImageSize_size _x) = (\f x -> fmap (\y -> x{size = y}) (f (size x))) . toLens _x
instance ToLens (Path_ImageSize Double)
    where type S (Path_ImageSize Double) = ImageSize
          type A (Path_ImageSize Double) = Double
          toLens (Path_ImageSize_size _x) = \f x -> fmap (\y -> x{size = y}) (f (size x))
instance ToLens (Path_ImageSize Dimension)
    where type S (Path_ImageSize Dimension) = ImageSize
          type A (Path_ImageSize Dimension) = Dimension
          toLens (Path_ImageSize_dim _x) = \f x -> fmap (\y -> x{dim = y}) (f (dim x))
instance ToLens (Path_ImageSize ImageSize)
    where type S (Path_ImageSize ImageSize) = ImageSize
          type A (Path_ImageSize ImageSize) = ImageSize
          toLens _ = id
instance ToLens (Path_ImageSize Units)
    where type S (Path_ImageSize Units) = ImageSize
          type A (Path_ImageSize Units) = Units
          toLens (Path_ImageSize_units _x) = \f x -> fmap (\y -> x{units = y}) (f (units x))
instance ToLens (Path_ImageSize JSONText)
    where type S (Path_ImageSize JSONText) = ImageSize
          type A (Path_ImageSize JSONText) = JSONText
          toLens (Path_ImageSize_dim _x) = (\f x -> fmap (\y -> x{dim = y}) (f (dim x))) . toLens _x
          toLens (Path_ImageSize_size _x) = (\f x -> fmap (\y -> x{size = y}) (f (size x))) . toLens _x
          toLens (Path_ImageSize_units _x) = (\f x -> fmap (\y -> x{units = y}) (f (units x))) . toLens _x
instance ToLens (Path_Int Int)
    where type S (Path_Int Int) = Int
          type A (Path_Int Int) = Int
          toLens _ = id
instance ToLens (Path_Int64 Int64)
    where type S (Path_Int64 Int64) = Int64
          type A (Path_Int64 Int64) = Int64
          toLens _ = id
instance ToLens (Path_Integer Integer)
    where type S (Path_Integer Integer) = Integer
          type A (Path_Integer Integer) = Integer
          toLens _ = id
instance ToLens (Path_Item String)
    where type S (Path_Item String) = Item
          type A (Path_Item String) = String
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Bool)
    where type S (Path_Item Bool) = Item
          type A (Path_Item Bool) = Bool
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Double)
    where type S (Path_Item Double) = Item
          type A (Path_Item Double) = Double
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Dimension)
    where type S (Path_Item Dimension) = Item
          type A (Path_Item Dimension) = Dimension
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item ImageCrop)
    where type S (Path_Item ImageCrop) = Item
          type A (Path_Item ImageCrop) = ImageCrop
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item ImageSize)
    where type S (Path_Item ImageSize) = Item
          type A (Path_Item ImageSize) = ImageSize
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Units)
    where type S (Path_Item Units) = Item
          type A (Path_Item Units) = Units
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item ImageFile)
    where type S (Path_Item ImageFile) = Item
          type A (Path_Item ImageFile) = ImageFile
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item JSONText)
    where type S (Path_Item JSONText) = Item
          type A (Path_Item JSONText) = JSONText
          toLens (Path_Item_itemName _x) = (\f x -> fmap (\y -> x{itemName = y}) (f (itemName x))) . toLens _x
          toLens (Path_Item_fields _x) = (\f x -> fmap (\y -> x{fields = y}) (f (fields x))) . toLens _x
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Markup)
    where type S (Path_Item Markup) = Item
          type A (Path_Item Markup) = Markup
          toLens (Path_Item_fields _x) = (\f x -> fmap (\y -> x{fields = y}) (f (fields x))) . toLens _x
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item EUI)
    where type S (Path_Item EUI) = Item
          type A (Path_Item EUI) = EUI
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item MEUI)
    where type S (Path_Item MEUI) = Item
          type A (Path_Item MEUI) = MEUI
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item MaybeImageFile)
    where type S (Path_Item MaybeImageFile) = Item
          type A (Path_Item MaybeImageFile) = MaybeImageFile
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item ReportImage)
    where type S (Path_Item ReportImage) = Item
          type A (Path_Item ReportImage) = ReportImage
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item ReportImages)
    where type S (Path_Item ReportImages) = Item
          type A (Path_Item ReportImages) = ReportImages
          toLens (Path_Item_images _x) = \f x -> fmap (\y -> x{images = y}) (f (images x))
instance ToLens (Path_Item ReportImageView)
    where type S (Path_Item ReportImageView) = Item
          type A (Path_Item ReportImageView) = ReportImageView
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item SaneSizeImageSize)
    where type S (Path_Item SaneSizeImageSize) = Item
          type A (Path_Item SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Item)
    where type S (Path_Item Item) = Item
          type A (Path_Item Item) = Item
          toLens _ = id
instance ToLens (Path_Item MIM)
    where type S (Path_Item MIM) = Item
          type A (Path_Item MIM) = MIM
          toLens (Path_Item_fields _x) = \f x -> fmap (\y -> x{fields = y}) (f (fields x))
instance ToLens (Path_Item URI)
    where type S (Path_Item URI) = Item
          type A (Path_Item URI) = URI
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_Item Text)
    where type S (Path_Item Text) = Item
          type A (Path_Item Text) = Text
          toLens (Path_Item_itemName _x) = \f x -> fmap (\y -> x{itemName = y}) (f (itemName x))
          toLens (Path_Item_fields _x) = (\f x -> fmap (\y -> x{fields = y}) (f (fields x))) . toLens _x
          toLens (Path_Item_images _x) = (\f x -> fmap (\y -> x{images = y}) (f (images x))) . toLens _x
instance ToLens (Path_JSONText JSONText)
    where type S (Path_JSONText JSONText) = JSONText
          type A (Path_JSONText JSONText) = JSONText
          toLens _ = id
instance ToLens (Path_Markup JSONText)
    where type S (Path_Markup JSONText) = Markup
          type A (Path_Markup JSONText) = JSONText
          toLens (Path_Markup_markdownText _x) = (\f x -> fmap (\y -> x{markdownText = y}) (f (markdownText x))) . toLens _x
          toLens (Path_Markup_htmlText _x) = (\f x -> fmap (\y -> x{htmlText = y}) (f (htmlText x))) . toLens _x
instance ToLens (Path_Markup Markup)
    where type S (Path_Markup Markup) = Markup
          type A (Path_Markup Markup) = Markup
          toLens _ = id
instance ToLens (Path_Markup Text)
    where type S (Path_Markup Text) = Markup
          type A (Path_Markup Text) = Text
          toLens (Path_Markup_markdownText _x) = \f x -> fmap (\y -> x{markdownText = y}) (f (markdownText x))
          toLens (Path_Markup_htmlText _x) = \f x -> fmap (\y -> x{htmlText = y}) (f (htmlText x))
instance ToLens (Path_MaybeImageFile String)
    where type S (Path_MaybeImageFile String) = MaybeImageFile
          type A (Path_MaybeImageFile String) = String
          toLens (Path_MaybeImageFile_View _) = viewLens :: Lens' (Maybe ImageFile)
                                                                  ([Char])
instance ToLens (Path_MaybeImageFile JSONText)
    where type S (Path_MaybeImageFile JSONText) = MaybeImageFile
          type A (Path_MaybeImageFile JSONText) = JSONText
          toLens (Path_MaybeImageFile_View v) = (viewLens :: Lens' (Maybe ImageFile)
                                                                   ([Char])) . toLens v
instance ToLens (Path_MaybeImageFile MaybeImageFile)
    where type S (Path_MaybeImageFile MaybeImageFile) = MaybeImageFile
          type A (Path_MaybeImageFile MaybeImageFile) = MaybeImageFile
          toLens _ = id
instance ToLens (Path_MaybeReportIntendedUse String)
    where type S (Path_MaybeReportIntendedUse String) = MaybeReportIntendedUse
          type A (Path_MaybeReportIntendedUse String) = String
          toLens (Path_MaybeReportIntendedUse_View _) = viewLens :: Lens' (Maybe ReportIntendedUse)
                                                                          ([Char])
instance ToLens (Path_MaybeReportIntendedUse JSONText)
    where type S (Path_MaybeReportIntendedUse JSONText) = MaybeReportIntendedUse
          type A (Path_MaybeReportIntendedUse JSONText) = JSONText
          toLens (Path_MaybeReportIntendedUse_View v) = (viewLens :: Lens' (Maybe ReportIntendedUse)
                                                                           ([Char])) . toLens v
instance ToLens (Path_MaybeReportIntendedUse MaybeReportIntendedUse)
    where type S (Path_MaybeReportIntendedUse MaybeReportIntendedUse) = MaybeReportIntendedUse
          type A (Path_MaybeReportIntendedUse MaybeReportIntendedUse) = MaybeReportIntendedUse
          toLens _ = id
instance ToLens (Path_Permissions JSONText)
    where type S (Path_Permissions JSONText) = Permissions
          type A (Path_Permissions JSONText) = JSONText
          toLens (Path_Permissions_writers _x) = (\f x -> fmap (\y -> x{writers = y}) (f (writers x))) . toLens _x
          toLens (Path_Permissions_readers _x) = (\f x -> fmap (\y -> x{readers = y}) (f (readers x))) . toLens _x
instance ToLens (Path_Permissions Permissions)
    where type S (Path_Permissions Permissions) = Permissions
          type A (Path_Permissions Permissions) = Permissions
          toLens _ = id
instance ToLens (Path_Permissions UserIds)
    where type S (Path_Permissions UserIds) = Permissions
          type A (Path_Permissions UserIds) = UserIds
          toLens (Path_Permissions_writers _x) = \f x -> fmap (\y -> x{writers = y}) (f (writers x))
          toLens (Path_Permissions_readers _x) = \f x -> fmap (\y -> x{readers = y}) (f (readers x))
instance ToLens (Path_Permissions Text)
    where type S (Path_Permissions Text) = Permissions
          type A (Path_Permissions Text) = Text
          toLens (Path_Permissions_writers _x) = (\f x -> fmap (\y -> x{writers = y}) (f (writers x))) . toLens _x
          toLens (Path_Permissions_readers _x) = (\f x -> fmap (\y -> x{readers = y}) (f (readers x))) . toLens _x
instance ToLens (Path_Permissions UserId)
    where type S (Path_Permissions UserId) = Permissions
          type A (Path_Permissions UserId) = UserId
          toLens (Path_Permissions_owner _x) = \f x -> fmap (\y -> x{owner = y}) (f (owner x))
instance ToLens (Path_ReadOnlyFilePath String)
    where type S (Path_ReadOnlyFilePath String) = ReadOnlyFilePath
          type A (Path_ReadOnlyFilePath String) = String
          toLens (Path_ReadOnlyFilePath_View _) = viewLens :: Lens' (ReadOnly ([Char]))
                                                                    ([Char])
instance ToLens (Path_ReadOnlyFilePath JSONText)
    where type S (Path_ReadOnlyFilePath JSONText) = ReadOnlyFilePath
          type A (Path_ReadOnlyFilePath JSONText) = JSONText
          toLens (Path_ReadOnlyFilePath_View v) = (viewLens :: Lens' (ReadOnly ([Char]))
                                                                     ([Char])) . toLens v
instance ToLens (Path_ReadOnlyFilePath ReadOnlyFilePath)
    where type S (Path_ReadOnlyFilePath ReadOnlyFilePath) = ReadOnlyFilePath
          type A (Path_ReadOnlyFilePath ReadOnlyFilePath) = ReadOnlyFilePath
          toLens _ = id
instance ToLens (Path_Report String)
    where type S (Path_Report String) = Report
          type A (Path_Report String) = String
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Int64)
    where type S (Path_Report Int64) = Report
          type A (Path_Report Int64) = Int64
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Bool)
    where type S (Path_Report Bool) = Report
          type A (Path_Report Bool) = Bool
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Double)
    where type S (Path_Report Double) = Report
          type A (Path_Report Double) = Double
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Int)
    where type S (Path_Report Int) = Report
          type A (Path_Report Int) = Int
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Dimension)
    where type S (Path_Report Dimension) = Report
          type A (Path_Report Dimension) = Dimension
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ImageCrop)
    where type S (Path_Report ImageCrop) = Report
          type A (Path_Report ImageCrop) = ImageCrop
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ImageSize)
    where type S (Path_Report ImageSize) = Report
          type A (Path_Report ImageSize) = ImageSize
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Units)
    where type S (Path_Report Units) = Report
          type A (Path_Report Units) = Units
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ImageFile)
    where type S (Path_Report ImageFile) = Report
          type A (Path_Report ImageFile) = ImageFile
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Integer)
    where type S (Path_Report Integer) = Report
          type A (Path_Report Integer) = Integer
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report JSONText)
    where type S (Path_Report JSONText) = Report
          type A (Path_Report JSONText) = JSONText
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Markup)
    where type S (Path_Report Markup) = Report
          type A (Path_Report Markup) = Markup
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Permissions)
    where type S (Path_Report Permissions) = Report
          type A (Path_Report Permissions) = Permissions
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report UserIds)
    where type S (Path_Report UserIds) = Report
          type A (Path_Report UserIds) = UserIds
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report AbbrevPair)
    where type S (Path_Report AbbrevPair) = Report
          type A (Path_Report AbbrevPair) = AbbrevPair
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report AbbrevPairs)
    where type S (Path_Report AbbrevPairs) = Report
          type A (Path_Report AbbrevPairs) = AbbrevPairs
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Author)
    where type S (Path_Report Author) = Report
          type A (Path_Report Author) = Author
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Authors)
    where type S (Path_Report Authors) = Report
          type A (Path_Report Authors) = Authors
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Branding)
    where type S (Path_Report Branding) = Report
          type A (Path_Report Branding) = Branding
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report MarkupPair)
    where type S (Path_Report MarkupPair) = Report
          type A (Path_Report MarkupPair) = MarkupPair
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report MarkupPairs)
    where type S (Path_Report MarkupPairs) = Report
          type A (Path_Report MarkupPairs) = MarkupPairs
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Markups)
    where type S (Path_Report Markups) = Report
          type A (Path_Report Markups) = Markups
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report MaybeReportIntendedUse)
    where type S (Path_Report MaybeReportIntendedUse) = Report
          type A (Path_Report MaybeReportIntendedUse) = MaybeReportIntendedUse
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Report)
    where type S (Path_Report Report) = Report
          type A (Path_Report Report) = Report
          toLens _ = id
instance ToLens (Path_Report ReportElem)
    where type S (Path_Report ReportElem) = Report
          type A (Path_Report ReportElem) = ReportElem
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportElems)
    where type S (Path_Report ReportElems) = Report
          type A (Path_Report ReportElems) = ReportElems
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportFlags)
    where type S (Path_Report ReportFlags) = Report
          type A (Path_Report ReportFlags) = ReportFlags
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportStandard)
    where type S (Path_Report ReportStandard) = Report
          type A (Path_Report ReportStandard) = ReportStandard
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportStatus)
    where type S (Path_Report ReportStatus) = Report
          type A (Path_Report ReportStatus) = ReportStatus
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportValueApproachInfo)
    where type S (Path_Report ReportValueApproachInfo) = Report
          type A (Path_Report ReportValueApproachInfo) = ReportValueApproachInfo
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportValueTypeInfo)
    where type S (Path_Report ReportValueTypeInfo) = Report
          type A (Path_Report ReportValueTypeInfo) = ReportValueTypeInfo
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report EUI)
    where type S (Path_Report EUI) = Report
          type A (Path_Report EUI) = EUI
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report MEUI)
    where type S (Path_Report MEUI) = Report
          type A (Path_Report MEUI) = MEUI
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report MaybeImageFile)
    where type S (Path_Report MaybeImageFile) = Report
          type A (Path_Report MaybeImageFile) = MaybeImageFile
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportImage)
    where type S (Path_Report ReportImage) = Report
          type A (Path_Report ReportImage) = ReportImage
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportImages)
    where type S (Path_Report ReportImages) = Report
          type A (Path_Report ReportImages) = ReportImages
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReadOnlyFilePath)
    where type S (Path_Report ReadOnlyFilePath) = Report
          type A (Path_Report ReadOnlyFilePath) = ReadOnlyFilePath
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportImageView)
    where type S (Path_Report ReportImageView) = Report
          type A (Path_Report ReportImageView) = ReportImageView
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report ReportView)
    where type S (Path_Report ReportView) = Report
          type A (Path_Report ReportView) = ReportView
          toLens (Path_Report_View _) = viewLens :: Lens' Report ReportView
instance ToLens (Path_Report SaneSizeImageSize)
    where type S (Path_Report SaneSizeImageSize) = Report
          type A (Path_Report SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Item)
    where type S (Path_Report Item) = Report
          type A (Path_Report Item) = Item
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report MIM)
    where type S (Path_Report MIM) = Report
          type A (Path_Report MIM) = MIM
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report CIString)
    where type S (Path_Report CIString) = Report
          type A (Path_Report CIString) = CIString
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report URI)
    where type S (Path_Report URI) = Report
          type A (Path_Report URI) = URI
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report Text)
    where type S (Path_Report Text) = Report
          type A (Path_Report Text) = Text
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report UserId)
    where type S (Path_Report UserId) = Report
          type A (Path_Report UserId) = UserId
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_Report UUID)
    where type S (Path_Report UUID) = Report
          type A (Path_Report UUID) = UUID
          toLens (Path_Report_View v) = (viewLens :: Lens' Report
                                                           ReportView) . toLens v
instance ToLens (Path_ReportElem String)
    where type S (Path_ReportElem String) = ReportElem
          type A (Path_ReportElem String) = String
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem Bool)
    where type S (Path_ReportElem Bool) = ReportElem
          type A (Path_ReportElem Bool) = Bool
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem Double)
    where type S (Path_ReportElem Double) = ReportElem
          type A (Path_ReportElem Double) = Double
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem Dimension)
    where type S (Path_ReportElem Dimension) = ReportElem
          type A (Path_ReportElem Dimension) = Dimension
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem ImageCrop)
    where type S (Path_ReportElem ImageCrop) = ReportElem
          type A (Path_ReportElem ImageCrop) = ImageCrop
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem ImageSize)
    where type S (Path_ReportElem ImageSize) = ReportElem
          type A (Path_ReportElem ImageSize) = ImageSize
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem Units)
    where type S (Path_ReportElem Units) = ReportElem
          type A (Path_ReportElem Units) = Units
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem ImageFile)
    where type S (Path_ReportElem ImageFile) = ReportElem
          type A (Path_ReportElem ImageFile) = ImageFile
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem JSONText)
    where type S (Path_ReportElem JSONText) = ReportElem
          type A (Path_ReportElem JSONText) = JSONText
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
          toLens (Path_ReportElem_elemText _x) = (\f x -> fmap (\y -> x{elemText = y}) (f (elemText x))) . toLens _x
instance ToLens (Path_ReportElem Markup)
    where type S (Path_ReportElem Markup) = ReportElem
          type A (Path_ReportElem Markup) = Markup
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
          toLens (Path_ReportElem_elemText _x) = \f x -> fmap (\y -> x{elemText = y}) (f (elemText x))
instance ToLens (Path_ReportElem ReportElem)
    where type S (Path_ReportElem ReportElem) = ReportElem
          type A (Path_ReportElem ReportElem) = ReportElem
          toLens _ = id
instance ToLens (Path_ReportElem EUI)
    where type S (Path_ReportElem EUI) = ReportElem
          type A (Path_ReportElem EUI) = EUI
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem MEUI)
    where type S (Path_ReportElem MEUI) = ReportElem
          type A (Path_ReportElem MEUI) = MEUI
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem MaybeImageFile)
    where type S (Path_ReportElem MaybeImageFile) = ReportElem
          type A (Path_ReportElem MaybeImageFile) = MaybeImageFile
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem ReportImage)
    where type S (Path_ReportElem ReportImage) = ReportElem
          type A (Path_ReportElem ReportImage) = ReportImage
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem ReportImages)
    where type S (Path_ReportElem ReportImages) = ReportElem
          type A (Path_ReportElem ReportImages) = ReportImages
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem ReportImageView)
    where type S (Path_ReportElem ReportImageView) = ReportElem
          type A (Path_ReportElem ReportImageView) = ReportImageView
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem SaneSizeImageSize)
    where type S (Path_ReportElem SaneSizeImageSize) = ReportElem
          type A (Path_ReportElem SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem Item)
    where type S (Path_ReportElem Item) = ReportElem
          type A (Path_ReportElem Item) = Item
          toLens (Path_ReportElem_elemItem _x) = \f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))
instance ToLens (Path_ReportElem MIM)
    where type S (Path_ReportElem MIM) = ReportElem
          type A (Path_ReportElem MIM) = MIM
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem URI)
    where type S (Path_ReportElem URI) = ReportElem
          type A (Path_ReportElem URI) = URI
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
instance ToLens (Path_ReportElem Text)
    where type S (Path_ReportElem Text) = ReportElem
          type A (Path_ReportElem Text) = Text
          toLens (Path_ReportElem_elemItem _x) = (\f x -> fmap (\y -> x{elemItem = y}) (f (elemItem x))) . toLens _x
          toLens (Path_ReportElem_elemText _x) = (\f x -> fmap (\y -> x{elemText = y}) (f (elemText x))) . toLens _x
instance ToLens (Path_ReportFlags String)
    where type S (Path_ReportFlags String) = ReportFlags
          type A (Path_ReportFlags String) = String
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = (\f x -> fmap (\y -> x{hideEmptyItemFields = y}) (f (hideEmptyItemFields x))) . toLens _x
instance ToLens (Path_ReportFlags Bool)
    where type S (Path_ReportFlags Bool) = ReportFlags
          type A (Path_ReportFlags Bool) = Bool
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = \f x -> fmap (\y -> x{hideEmptyItemFields = y}) (f (hideEmptyItemFields x))
instance ToLens (Path_ReportFlags JSONText)
    where type S (Path_ReportFlags JSONText) = ReportFlags
          type A (Path_ReportFlags JSONText) = JSONText
          toLens (Path_ReportFlags_hideEmptyItemFields _x) = (\f x -> fmap (\y -> x{hideEmptyItemFields = y}) (f (hideEmptyItemFields x))) . toLens _x
instance ToLens (Path_ReportFlags ReportFlags)
    where type S (Path_ReportFlags ReportFlags) = ReportFlags
          type A (Path_ReportFlags ReportFlags) = ReportFlags
          toLens _ = id
instance ToLens (Path_ReportImage String)
    where type S (Path_ReportImage String) = ReportImage
          type A (Path_ReportImage String) = String
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage Bool)
    where type S (Path_ReportImage Bool) = ReportImage
          type A (Path_ReportImage Bool) = Bool
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage Double)
    where type S (Path_ReportImage Double) = ReportImage
          type A (Path_ReportImage Double) = Double
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage Dimension)
    where type S (Path_ReportImage Dimension) = ReportImage
          type A (Path_ReportImage Dimension) = Dimension
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage ImageCrop)
    where type S (Path_ReportImage ImageCrop) = ReportImage
          type A (Path_ReportImage ImageCrop) = ImageCrop
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage ImageSize)
    where type S (Path_ReportImage ImageSize) = ReportImage
          type A (Path_ReportImage ImageSize) = ImageSize
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage Units)
    where type S (Path_ReportImage Units) = ReportImage
          type A (Path_ReportImage Units) = Units
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage ImageFile)
    where type S (Path_ReportImage ImageFile) = ReportImage
          type A (Path_ReportImage ImageFile) = ImageFile
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage JSONText)
    where type S (Path_ReportImage JSONText) = ReportImage
          type A (Path_ReportImage JSONText) = JSONText
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage Markup)
    where type S (Path_ReportImage Markup) = ReportImage
          type A (Path_ReportImage Markup) = Markup
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage EUI)
    where type S (Path_ReportImage EUI) = ReportImage
          type A (Path_ReportImage EUI) = EUI
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage MEUI)
    where type S (Path_ReportImage MEUI) = ReportImage
          type A (Path_ReportImage MEUI) = MEUI
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage MaybeImageFile)
    where type S (Path_ReportImage MaybeImageFile) = ReportImage
          type A (Path_ReportImage MaybeImageFile) = MaybeImageFile
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage ReportImage)
    where type S (Path_ReportImage ReportImage) = ReportImage
          type A (Path_ReportImage ReportImage) = ReportImage
          toLens _ = id
instance ToLens (Path_ReportImage ReportImageView)
    where type S (Path_ReportImage ReportImageView) = ReportImage
          type A (Path_ReportImage ReportImageView) = ReportImageView
          toLens (Path_ReportImage_View _) = viewLens :: Lens' ReportImage
                                                               ReportImageView
instance ToLens (Path_ReportImage SaneSizeImageSize)
    where type S (Path_ReportImage SaneSizeImageSize) = ReportImage
          type A (Path_ReportImage SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage URI)
    where type S (Path_ReportImage URI) = ReportImage
          type A (Path_ReportImage URI) = URI
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImage Text)
    where type S (Path_ReportImage Text) = ReportImage
          type A (Path_ReportImage Text) = Text
          toLens (Path_ReportImage_View v) = (viewLens :: Lens' ReportImage
                                                                ReportImageView) . toLens v
instance ToLens (Path_ReportImageView String)
    where type S (Path_ReportImageView String) = ReportImageView
          type A (Path_ReportImageView String) = String
          toLens (Path_ReportImageView__picSize _x) = (\f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))) . toLens _x
          toLens (Path_ReportImageView__picEditedDeprecated _x) = (\f x -> fmap (\y -> x{_picEditedDeprecated = y}) (f (_picEditedDeprecated x))) . toLens _x
          toLens (Path_ReportImageView__picThumbDeprecated _x) = (\f x -> fmap (\y -> x{_picThumbDeprecated = y}) (f (_picThumbDeprecated x))) . toLens _x
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = (\f x -> fmap (\y -> x{_picPrinterDeprecated = y}) (f (_picPrinterDeprecated x))) . toLens _x
          toLens (Path_ReportImageView__picMustEnlarge _x) = (\f x -> fmap (\y -> x{_picMustEnlarge = y}) (f (_picMustEnlarge x))) . toLens _x
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = (\f x -> fmap (\y -> x{_picEnlargedDeprecated = y}) (f (_picEnlargedDeprecated x))) . toLens _x
instance ToLens (Path_ReportImageView Bool)
    where type S (Path_ReportImageView Bool) = ReportImageView
          type A (Path_ReportImageView Bool) = Bool
          toLens (Path_ReportImageView__picMustEnlarge _x) = \f x -> fmap (\y -> x{_picMustEnlarge = y}) (f (_picMustEnlarge x))
instance ToLens (Path_ReportImageView Double)
    where type S (Path_ReportImageView Double) = ReportImageView
          type A (Path_ReportImageView Double) = Double
          toLens (Path_ReportImageView__picSize _x) = (\f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))) . toLens _x
instance ToLens (Path_ReportImageView Dimension)
    where type S (Path_ReportImageView Dimension) = ReportImageView
          type A (Path_ReportImageView Dimension) = Dimension
          toLens (Path_ReportImageView__picSize _x) = (\f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))) . toLens _x
instance ToLens (Path_ReportImageView ImageCrop)
    where type S (Path_ReportImageView ImageCrop) = ReportImageView
          type A (Path_ReportImageView ImageCrop) = ImageCrop
          toLens (Path_ReportImageView__picCrop _x) = \f x -> fmap (\y -> x{_picCrop = y}) (f (_picCrop x))
instance ToLens (Path_ReportImageView ImageSize)
    where type S (Path_ReportImageView ImageSize) = ReportImageView
          type A (Path_ReportImageView ImageSize) = ImageSize
          toLens (Path_ReportImageView__picSize _x) = (\f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))) . toLens _x
instance ToLens (Path_ReportImageView Units)
    where type S (Path_ReportImageView Units) = ReportImageView
          type A (Path_ReportImageView Units) = Units
          toLens (Path_ReportImageView__picSize _x) = (\f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))) . toLens _x
instance ToLens (Path_ReportImageView ImageFile)
    where type S (Path_ReportImageView ImageFile) = ReportImageView
          type A (Path_ReportImageView ImageFile) = ImageFile
          toLens (Path_ReportImageView__picOriginal _x) = (\f x -> fmap (\y -> x{_picOriginal = y}) (f (_picOriginal x))) . toLens _x
instance ToLens (Path_ReportImageView JSONText)
    where type S (Path_ReportImageView JSONText) = ReportImageView
          type A (Path_ReportImageView JSONText) = JSONText
          toLens (Path_ReportImageView__picSize _x) = (\f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))) . toLens _x
          toLens (Path_ReportImageView__picCaption _x) = (\f x -> fmap (\y -> x{_picCaption = y}) (f (_picCaption x))) . toLens _x
          toLens (Path_ReportImageView__picEditedDeprecated _x) = (\f x -> fmap (\y -> x{_picEditedDeprecated = y}) (f (_picEditedDeprecated x))) . toLens _x
          toLens (Path_ReportImageView__picThumbDeprecated _x) = (\f x -> fmap (\y -> x{_picThumbDeprecated = y}) (f (_picThumbDeprecated x))) . toLens _x
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = (\f x -> fmap (\y -> x{_picPrinterDeprecated = y}) (f (_picPrinterDeprecated x))) . toLens _x
          toLens (Path_ReportImageView__picMustEnlarge _x) = (\f x -> fmap (\y -> x{_picMustEnlarge = y}) (f (_picMustEnlarge x))) . toLens _x
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = (\f x -> fmap (\y -> x{_picEnlargedDeprecated = y}) (f (_picEnlargedDeprecated x))) . toLens _x
instance ToLens (Path_ReportImageView Markup)
    where type S (Path_ReportImageView Markup) = ReportImageView
          type A (Path_ReportImageView Markup) = Markup
          toLens (Path_ReportImageView__picCaption _x) = \f x -> fmap (\y -> x{_picCaption = y}) (f (_picCaption x))
instance ToLens (Path_ReportImageView EUI)
    where type S (Path_ReportImageView EUI) = ReportImageView
          type A (Path_ReportImageView EUI) = EUI
          toLens (Path_ReportImageView__picOriginal _x) = (\f x -> fmap (\y -> x{_picOriginal = y}) (f (_picOriginal x))) . toLens _x
instance ToLens (Path_ReportImageView MEUI)
    where type S (Path_ReportImageView MEUI) = ReportImageView
          type A (Path_ReportImageView MEUI) = MEUI
          toLens (Path_ReportImageView__picOriginal _x) = \f x -> fmap (\y -> x{_picOriginal = y}) (f (_picOriginal x))
instance ToLens (Path_ReportImageView MaybeImageFile)
    where type S (Path_ReportImageView MaybeImageFile) = ReportImageView
          type A (Path_ReportImageView MaybeImageFile) = MaybeImageFile
          toLens (Path_ReportImageView__picEditedDeprecated _x) = \f x -> fmap (\y -> x{_picEditedDeprecated = y}) (f (_picEditedDeprecated x))
          toLens (Path_ReportImageView__picThumbDeprecated _x) = \f x -> fmap (\y -> x{_picThumbDeprecated = y}) (f (_picThumbDeprecated x))
          toLens (Path_ReportImageView__picPrinterDeprecated _x) = \f x -> fmap (\y -> x{_picPrinterDeprecated = y}) (f (_picPrinterDeprecated x))
          toLens (Path_ReportImageView__picEnlargedDeprecated _x) = \f x -> fmap (\y -> x{_picEnlargedDeprecated = y}) (f (_picEnlargedDeprecated x))
instance ToLens (Path_ReportImageView ReportImageView)
    where type S (Path_ReportImageView ReportImageView) = ReportImageView
          type A (Path_ReportImageView ReportImageView) = ReportImageView
          toLens _ = id
instance ToLens (Path_ReportImageView SaneSizeImageSize)
    where type S (Path_ReportImageView SaneSizeImageSize) = ReportImageView
          type A (Path_ReportImageView SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_ReportImageView__picSize _x) = \f x -> fmap (\y -> x{_picSize = y}) (f (_picSize x))
instance ToLens (Path_ReportImageView URI)
    where type S (Path_ReportImageView URI) = ReportImageView
          type A (Path_ReportImageView URI) = URI
          toLens (Path_ReportImageView__picOriginal _x) = (\f x -> fmap (\y -> x{_picOriginal = y}) (f (_picOriginal x))) . toLens _x
instance ToLens (Path_ReportImageView Text)
    where type S (Path_ReportImageView Text) = ReportImageView
          type A (Path_ReportImageView Text) = Text
          toLens (Path_ReportImageView__picCaption _x) = (\f x -> fmap (\y -> x{_picCaption = y}) (f (_picCaption x))) . toLens _x
instance ToLens (Path_ReportIntendedUse String)
    where type S (Path_ReportIntendedUse String) = ReportIntendedUse
          type A (Path_ReportIntendedUse String) = String
          toLens (Path_ReportIntendedUse_View _) = viewLens :: Lens' ReportIntendedUse
                                                                     ([Char])
instance ToLens (Path_ReportIntendedUse JSONText)
    where type S (Path_ReportIntendedUse JSONText) = ReportIntendedUse
          type A (Path_ReportIntendedUse JSONText) = JSONText
          toLens (Path_ReportIntendedUse_View v) = (viewLens :: Lens' ReportIntendedUse
                                                                      ([Char])) . toLens v
instance ToLens (Path_ReportIntendedUse ReportIntendedUse)
    where type S (Path_ReportIntendedUse ReportIntendedUse) = ReportIntendedUse
          type A (Path_ReportIntendedUse ReportIntendedUse) = ReportIntendedUse
          toLens _ = id
instance ToLens (Path_ReportMap String)
    where type S (Path_ReportMap String) = ReportMap
          type A (Path_ReportMap String) = String
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Int64)
    where type S (Path_ReportMap Int64) = ReportMap
          type A (Path_ReportMap Int64) = Int64
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Bool)
    where type S (Path_ReportMap Bool) = ReportMap
          type A (Path_ReportMap Bool) = Bool
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Double)
    where type S (Path_ReportMap Double) = ReportMap
          type A (Path_ReportMap Double) = Double
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Int)
    where type S (Path_ReportMap Int) = ReportMap
          type A (Path_ReportMap Int) = Int
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Dimension)
    where type S (Path_ReportMap Dimension) = ReportMap
          type A (Path_ReportMap Dimension) = Dimension
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ImageCrop)
    where type S (Path_ReportMap ImageCrop) = ReportMap
          type A (Path_ReportMap ImageCrop) = ImageCrop
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ImageSize)
    where type S (Path_ReportMap ImageSize) = ReportMap
          type A (Path_ReportMap ImageSize) = ImageSize
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Units)
    where type S (Path_ReportMap Units) = ReportMap
          type A (Path_ReportMap Units) = Units
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ImageFile)
    where type S (Path_ReportMap ImageFile) = ReportMap
          type A (Path_ReportMap ImageFile) = ImageFile
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Integer)
    where type S (Path_ReportMap Integer) = ReportMap
          type A (Path_ReportMap Integer) = Integer
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap JSONText)
    where type S (Path_ReportMap JSONText) = ReportMap
          type A (Path_ReportMap JSONText) = JSONText
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Markup)
    where type S (Path_ReportMap Markup) = ReportMap
          type A (Path_ReportMap Markup) = Markup
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Permissions)
    where type S (Path_ReportMap Permissions) = ReportMap
          type A (Path_ReportMap Permissions) = Permissions
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap UserIds)
    where type S (Path_ReportMap UserIds) = ReportMap
          type A (Path_ReportMap UserIds) = UserIds
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap AbbrevPair)
    where type S (Path_ReportMap AbbrevPair) = ReportMap
          type A (Path_ReportMap AbbrevPair) = AbbrevPair
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap AbbrevPairs)
    where type S (Path_ReportMap AbbrevPairs) = ReportMap
          type A (Path_ReportMap AbbrevPairs) = AbbrevPairs
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Author)
    where type S (Path_ReportMap Author) = ReportMap
          type A (Path_ReportMap Author) = Author
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Authors)
    where type S (Path_ReportMap Authors) = ReportMap
          type A (Path_ReportMap Authors) = Authors
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Branding)
    where type S (Path_ReportMap Branding) = ReportMap
          type A (Path_ReportMap Branding) = Branding
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MarkupPair)
    where type S (Path_ReportMap MarkupPair) = ReportMap
          type A (Path_ReportMap MarkupPair) = MarkupPair
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MarkupPairs)
    where type S (Path_ReportMap MarkupPairs) = ReportMap
          type A (Path_ReportMap MarkupPairs) = MarkupPairs
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Markups)
    where type S (Path_ReportMap Markups) = ReportMap
          type A (Path_ReportMap Markups) = Markups
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MaybeReportIntendedUse)
    where type S (Path_ReportMap MaybeReportIntendedUse) = ReportMap
          type A (Path_ReportMap MaybeReportIntendedUse) = MaybeReportIntendedUse
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Report)
    where type S (Path_ReportMap Report) = ReportMap
          type A (Path_ReportMap Report) = Report
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportElem)
    where type S (Path_ReportMap ReportElem) = ReportMap
          type A (Path_ReportMap ReportElem) = ReportElem
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportElems)
    where type S (Path_ReportMap ReportElems) = ReportMap
          type A (Path_ReportMap ReportElems) = ReportElems
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportFlags)
    where type S (Path_ReportMap ReportFlags) = ReportMap
          type A (Path_ReportMap ReportFlags) = ReportFlags
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportStandard)
    where type S (Path_ReportMap ReportStandard) = ReportMap
          type A (Path_ReportMap ReportStandard) = ReportStandard
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportStatus)
    where type S (Path_ReportMap ReportStatus) = ReportMap
          type A (Path_ReportMap ReportStatus) = ReportStatus
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportValueApproachInfo)
    where type S (Path_ReportMap ReportValueApproachInfo) = ReportMap
          type A (Path_ReportMap ReportValueApproachInfo) = ReportValueApproachInfo
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportValueTypeInfo)
    where type S (Path_ReportMap ReportValueTypeInfo) = ReportMap
          type A (Path_ReportMap ReportValueTypeInfo) = ReportValueTypeInfo
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap EUI)
    where type S (Path_ReportMap EUI) = ReportMap
          type A (Path_ReportMap EUI) = EUI
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MEUI)
    where type S (Path_ReportMap MEUI) = ReportMap
          type A (Path_ReportMap MEUI) = MEUI
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MaybeImageFile)
    where type S (Path_ReportMap MaybeImageFile) = ReportMap
          type A (Path_ReportMap MaybeImageFile) = MaybeImageFile
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportImage)
    where type S (Path_ReportMap ReportImage) = ReportMap
          type A (Path_ReportMap ReportImage) = ReportImage
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportImages)
    where type S (Path_ReportMap ReportImages) = ReportMap
          type A (Path_ReportMap ReportImages) = ReportImages
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReadOnlyFilePath)
    where type S (Path_ReportMap ReadOnlyFilePath) = ReportMap
          type A (Path_ReportMap ReadOnlyFilePath) = ReadOnlyFilePath
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportImageView)
    where type S (Path_ReportMap ReportImageView) = ReportMap
          type A (Path_ReportMap ReportImageView) = ReportImageView
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap ReportView)
    where type S (Path_ReportMap ReportView) = ReportMap
          type A (Path_ReportMap ReportView) = ReportView
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap SaneSizeImageSize)
    where type S (Path_ReportMap SaneSizeImageSize) = ReportMap
          type A (Path_ReportMap SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Item)
    where type S (Path_ReportMap Item) = ReportMap
          type A (Path_ReportMap Item) = Item
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MIM)
    where type S (Path_ReportMap MIM) = ReportMap
          type A (Path_ReportMap MIM) = MIM
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap MRR)
    where type S (Path_ReportMap MRR) = ReportMap
          type A (Path_ReportMap MRR) = MRR
          toLens (Path_ReportMap_unReportMap _x) = \f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))
instance ToLens (Path_ReportMap ReportMap)
    where type S (Path_ReportMap ReportMap) = ReportMap
          type A (Path_ReportMap ReportMap) = ReportMap
          toLens _ = id
instance ToLens (Path_ReportMap CIString)
    where type S (Path_ReportMap CIString) = ReportMap
          type A (Path_ReportMap CIString) = CIString
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap URI)
    where type S (Path_ReportMap URI) = ReportMap
          type A (Path_ReportMap URI) = URI
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap Text)
    where type S (Path_ReportMap Text) = ReportMap
          type A (Path_ReportMap Text) = Text
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap UserId)
    where type S (Path_ReportMap UserId) = ReportMap
          type A (Path_ReportMap UserId) = UserId
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportMap UUID)
    where type S (Path_ReportMap UUID) = ReportMap
          type A (Path_ReportMap UUID) = UUID
          toLens (Path_ReportMap_unReportMap _x) = (\f x -> fmap (\y -> x{unReportMap = y}) (f (unReportMap x))) . toLens _x
instance ToLens (Path_ReportStandard Int)
    where type S (Path_ReportStandard Int) = ReportStandard
          type A (Path_ReportStandard Int) = Int
          toLens (Path_ReportStandard_unReportStandard _x) = \f x -> fmap (\y -> x{unReportStandard = y}) (f (unReportStandard x))
instance ToLens (Path_ReportStandard ReportStandard)
    where type S (Path_ReportStandard ReportStandard) = ReportStandard
          type A (Path_ReportStandard ReportStandard) = ReportStandard
          toLens _ = id
instance ToLens (Path_ReportStatus String)
    where type S (Path_ReportStatus String) = ReportStatus
          type A (Path_ReportStatus String) = String
          toLens (Path_ReportStatus_View _) = viewLens :: Lens' ReportStatus
                                                                ([Char])
instance ToLens (Path_ReportStatus JSONText)
    where type S (Path_ReportStatus JSONText) = ReportStatus
          type A (Path_ReportStatus JSONText) = JSONText
          toLens (Path_ReportStatus_View v) = (viewLens :: Lens' ReportStatus
                                                                 ([Char])) . toLens v
instance ToLens (Path_ReportStatus ReportStatus)
    where type S (Path_ReportStatus ReportStatus) = ReportStatus
          type A (Path_ReportStatus ReportStatus) = ReportStatus
          toLens _ = id
instance ToLens (Path_ReportValueApproachInfo JSONText)
    where type S (Path_ReportValueApproachInfo JSONText) = ReportValueApproachInfo
          type A (Path_ReportValueApproachInfo JSONText) = JSONText
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = (\f x -> fmap (\y -> x{reportValueApproachName = y}) (f (reportValueApproachName x))) . toLens _x
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = (\f x -> fmap (\y -> x{reportValueApproachDescription = y}) (f (reportValueApproachDescription x))) . toLens _x
instance ToLens (Path_ReportValueApproachInfo Markup)
    where type S (Path_ReportValueApproachInfo Markup) = ReportValueApproachInfo
          type A (Path_ReportValueApproachInfo Markup) = Markup
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = \f x -> fmap (\y -> x{reportValueApproachName = y}) (f (reportValueApproachName x))
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = \f x -> fmap (\y -> x{reportValueApproachDescription = y}) (f (reportValueApproachDescription x))
instance ToLens (Path_ReportValueApproachInfo ReportValueApproachInfo)
    where type S (Path_ReportValueApproachInfo ReportValueApproachInfo) = ReportValueApproachInfo
          type A (Path_ReportValueApproachInfo ReportValueApproachInfo) = ReportValueApproachInfo
          toLens _ = id
instance ToLens (Path_ReportValueApproachInfo Text)
    where type S (Path_ReportValueApproachInfo Text) = ReportValueApproachInfo
          type A (Path_ReportValueApproachInfo Text) = Text
          toLens (Path_ReportValueApproachInfo_reportValueApproachName _x) = (\f x -> fmap (\y -> x{reportValueApproachName = y}) (f (reportValueApproachName x))) . toLens _x
          toLens (Path_ReportValueApproachInfo_reportValueApproachDescription _x) = (\f x -> fmap (\y -> x{reportValueApproachDescription = y}) (f (reportValueApproachDescription x))) . toLens _x
instance ToLens (Path_ReportValueTypeInfo JSONText)
    where type S (Path_ReportValueTypeInfo JSONText) = ReportValueTypeInfo
          type A (Path_ReportValueTypeInfo JSONText) = JSONText
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = (\f x -> fmap (\y -> x{reportValueTypeName = y}) (f (reportValueTypeName x))) . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = (\f x -> fmap (\y -> x{reportValueTypeDescription = y}) (f (reportValueTypeDescription x))) . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = (\f x -> fmap (\y -> x{reportValueTypeDefinition = y}) (f (reportValueTypeDefinition x))) . toLens _x
instance ToLens (Path_ReportValueTypeInfo Markup)
    where type S (Path_ReportValueTypeInfo Markup) = ReportValueTypeInfo
          type A (Path_ReportValueTypeInfo Markup) = Markup
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = \f x -> fmap (\y -> x{reportValueTypeName = y}) (f (reportValueTypeName x))
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = \f x -> fmap (\y -> x{reportValueTypeDescription = y}) (f (reportValueTypeDescription x))
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = \f x -> fmap (\y -> x{reportValueTypeDefinition = y}) (f (reportValueTypeDefinition x))
instance ToLens (Path_ReportValueTypeInfo ReportValueTypeInfo)
    where type S (Path_ReportValueTypeInfo ReportValueTypeInfo) = ReportValueTypeInfo
          type A (Path_ReportValueTypeInfo ReportValueTypeInfo) = ReportValueTypeInfo
          toLens _ = id
instance ToLens (Path_ReportValueTypeInfo Text)
    where type S (Path_ReportValueTypeInfo Text) = ReportValueTypeInfo
          type A (Path_ReportValueTypeInfo Text) = Text
          toLens (Path_ReportValueTypeInfo_reportValueTypeName _x) = (\f x -> fmap (\y -> x{reportValueTypeName = y}) (f (reportValueTypeName x))) . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDescription _x) = (\f x -> fmap (\y -> x{reportValueTypeDescription = y}) (f (reportValueTypeDescription x))) . toLens _x
          toLens (Path_ReportValueTypeInfo_reportValueTypeDefinition _x) = (\f x -> fmap (\y -> x{reportValueTypeDefinition = y}) (f (reportValueTypeDefinition x))) . toLens _x
instance ToLens (Path_ReportView String)
    where type S (Path_ReportView String) = ReportView
          type A (Path_ReportView String) = String
          toLens (Path_ReportView__reportFolder _x) = (\f x -> fmap (\y -> x{_reportFolder = y}) (f (_reportFolder x))) . toLens _x
          toLens (Path_ReportView__reportIntendedUse _x) = (\f x -> fmap (\y -> x{_reportIntendedUse = y}) (f (_reportIntendedUse x))) . toLens _x
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
          toLens (Path_ReportView__reportStatus _x) = (\f x -> fmap (\y -> x{_reportStatus = y}) (f (_reportStatus x))) . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = (\f x -> fmap (\y -> x{_reportRedacted = y}) (f (_reportRedacted x))) . toLens _x
          toLens (Path_ReportView__reportFlags _x) = (\f x -> fmap (\y -> x{_reportFlags = y}) (f (_reportFlags x))) . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = (\f x -> fmap (\y -> x{_reportOrderByItemName = y}) (f (_reportOrderByItemName x))) . toLens _x
          toLens (Path_ReportView__reportDisplayItemName _x) = (\f x -> fmap (\y -> x{_reportDisplayItemName = y}) (f (_reportDisplayItemName x))) . toLens _x
instance ToLens (Path_ReportView Int64)
    where type S (Path_ReportView Int64) = ReportView
          type A (Path_ReportView Int64) = Int64
          toLens (Path_ReportView__reportCreated _x) = \f x -> fmap (\y -> x{_reportCreated = y}) (f (_reportCreated x))
instance ToLens (Path_ReportView Bool)
    where type S (Path_ReportView Bool) = ReportView
          type A (Path_ReportView Bool) = Bool
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = \f x -> fmap (\y -> x{_reportRedacted = y}) (f (_reportRedacted x))
          toLens (Path_ReportView__reportFlags _x) = (\f x -> fmap (\y -> x{_reportFlags = y}) (f (_reportFlags x))) . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = \f x -> fmap (\y -> x{_reportOrderByItemName = y}) (f (_reportOrderByItemName x))
          toLens (Path_ReportView__reportDisplayItemName _x) = \f x -> fmap (\y -> x{_reportDisplayItemName = y}) (f (_reportDisplayItemName x))
instance ToLens (Path_ReportView Double)
    where type S (Path_ReportView Double) = ReportView
          type A (Path_ReportView Double) = Double
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView Int)
    where type S (Path_ReportView Int) = ReportView
          type A (Path_ReportView Int) = Int
          toLens (Path_ReportView__reportStandardsVersion _x) = (\f x -> fmap (\y -> x{_reportStandardsVersion = y}) (f (_reportStandardsVersion x))) . toLens _x
instance ToLens (Path_ReportView Dimension)
    where type S (Path_ReportView Dimension) = ReportView
          type A (Path_ReportView Dimension) = Dimension
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ImageCrop)
    where type S (Path_ReportView ImageCrop) = ReportView
          type A (Path_ReportView ImageCrop) = ImageCrop
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ImageSize)
    where type S (Path_ReportView ImageSize) = ReportView
          type A (Path_ReportView ImageSize) = ImageSize
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView Units)
    where type S (Path_ReportView Units) = ReportView
          type A (Path_ReportView Units) = Units
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ImageFile)
    where type S (Path_ReportView ImageFile) = ReportView
          type A (Path_ReportView ImageFile) = ImageFile
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView Integer)
    where type S (Path_ReportView Integer) = ReportView
          type A (Path_ReportView Integer) = Integer
          toLens (Path_ReportView__reportRevision _x) = \f x -> fmap (\y -> x{_reportRevision = y}) (f (_reportRevision x))
instance ToLens (Path_ReportView JSONText)
    where type S (Path_ReportView JSONText) = ReportView
          type A (Path_ReportView JSONText) = JSONText
          toLens (Path_ReportView__reportFolder _x) = (\f x -> fmap (\y -> x{_reportFolder = y}) (f (_reportFolder x))) . toLens _x
          toLens (Path_ReportView__reportName _x) = (\f x -> fmap (\y -> x{_reportName = y}) (f (_reportName x))) . toLens _x
          toLens (Path_ReportView__reportDate _x) = (\f x -> fmap (\y -> x{_reportDate = y}) (f (_reportDate x))) . toLens _x
          toLens (Path_ReportView__reportContractDate _x) = (\f x -> fmap (\y -> x{_reportContractDate = y}) (f (_reportContractDate x))) . toLens _x
          toLens (Path_ReportView__reportInspectionDate _x) = (\f x -> fmap (\y -> x{_reportInspectionDate = y}) (f (_reportInspectionDate x))) . toLens _x
          toLens (Path_ReportView__reportEffectiveDate _x) = (\f x -> fmap (\y -> x{_reportEffectiveDate = y}) (f (_reportEffectiveDate x))) . toLens _x
          toLens (Path_ReportView__reportAuthors _x) = (\f x -> fmap (\y -> x{_reportAuthors = y}) (f (_reportAuthors x))) . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = (\f x -> fmap (\y -> x{_reportPreparer = y}) (f (_reportPreparer x))) . toLens _x
          toLens (Path_ReportView__reportPreparerEIN _x) = (\f x -> fmap (\y -> x{_reportPreparerEIN = y}) (f (_reportPreparerEIN x))) . toLens _x
          toLens (Path_ReportView__reportPreparerAddress _x) = (\f x -> fmap (\y -> x{_reportPreparerAddress = y}) (f (_reportPreparerAddress x))) . toLens _x
          toLens (Path_ReportView__reportPreparerEMail _x) = (\f x -> fmap (\y -> x{_reportPreparerEMail = y}) (f (_reportPreparerEMail x))) . toLens _x
          toLens (Path_ReportView__reportPreparerWebsite _x) = (\f x -> fmap (\y -> x{_reportPreparerWebsite = y}) (f (_reportPreparerWebsite x))) . toLens _x
          toLens (Path_ReportView__reportAbbrevs _x) = (\f x -> fmap (\y -> x{_reportAbbrevs = y}) (f (_reportAbbrevs x))) . toLens _x
          toLens (Path_ReportView__reportTitle _x) = (\f x -> fmap (\y -> x{_reportTitle = y}) (f (_reportTitle x))) . toLens _x
          toLens (Path_ReportView__reportHeader _x) = (\f x -> fmap (\y -> x{_reportHeader = y}) (f (_reportHeader x))) . toLens _x
          toLens (Path_ReportView__reportFooter _x) = (\f x -> fmap (\y -> x{_reportFooter = y}) (f (_reportFooter x))) . toLens _x
          toLens (Path_ReportView__reportIntendedUse _x) = (\f x -> fmap (\y -> x{_reportIntendedUse = y}) (f (_reportIntendedUse x))) . toLens _x
          toLens (Path_ReportView__reportValueTypeInfo _x) = (\f x -> fmap (\y -> x{_reportValueTypeInfo = y}) (f (_reportValueTypeInfo x))) . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = (\f x -> fmap (\y -> x{_reportValueApproachInfo = y}) (f (_reportValueApproachInfo x))) . toLens _x
          toLens (Path_ReportView__reportClientName _x) = (\f x -> fmap (\y -> x{_reportClientName = y}) (f (_reportClientName x))) . toLens _x
          toLens (Path_ReportView__reportClientAddress _x) = (\f x -> fmap (\y -> x{_reportClientAddress = y}) (f (_reportClientAddress x))) . toLens _x
          toLens (Path_ReportView__reportClientGreeting _x) = (\f x -> fmap (\y -> x{_reportClientGreeting = y}) (f (_reportClientGreeting x))) . toLens _x
          toLens (Path_ReportView__reportItemsOwnerFull _x) = (\f x -> fmap (\y -> x{_reportItemsOwnerFull = y}) (f (_reportItemsOwnerFull x))) . toLens _x
          toLens (Path_ReportView__reportItemsOwner _x) = (\f x -> fmap (\y -> x{_reportItemsOwner = y}) (f (_reportItemsOwner x))) . toLens _x
          toLens (Path_ReportView__reportBriefItems _x) = (\f x -> fmap (\y -> x{_reportBriefItems = y}) (f (_reportBriefItems x))) . toLens _x
          toLens (Path_ReportView__reportInspectionLocation _x) = (\f x -> fmap (\y -> x{_reportInspectionLocation = y}) (f (_reportInspectionLocation x))) . toLens _x
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = (\f x -> fmap (\y -> x{_reportGlossary = y}) (f (_reportGlossary x))) . toLens _x
          toLens (Path_ReportView__reportSources _x) = (\f x -> fmap (\y -> x{_reportSources = y}) (f (_reportSources x))) . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = (\f x -> fmap (\y -> x{_reportLetterOfTransmittal = y}) (f (_reportLetterOfTransmittal x))) . toLens _x
          toLens (Path_ReportView__reportScopeOfWork _x) = (\f x -> fmap (\y -> x{_reportScopeOfWork = y}) (f (_reportScopeOfWork x))) . toLens _x
          toLens (Path_ReportView__reportCertification _x) = (\f x -> fmap (\y -> x{_reportCertification = y}) (f (_reportCertification x))) . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = (\f x -> fmap (\y -> x{_reportLimitingConditions = y}) (f (_reportLimitingConditions x))) . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = (\f x -> fmap (\y -> x{_reportPrivacyPolicy = y}) (f (_reportPrivacyPolicy x))) . toLens _x
          toLens (Path_ReportView__reportPerms _x) = (\f x -> fmap (\y -> x{_reportPerms = y}) (f (_reportPerms x))) . toLens _x
          toLens (Path_ReportView__reportBranding _x) = (\f x -> fmap (\y -> x{_reportBranding = y}) (f (_reportBranding x))) . toLens _x
          toLens (Path_ReportView__reportStatus _x) = (\f x -> fmap (\y -> x{_reportStatus = y}) (f (_reportStatus x))) . toLens _x
          toLens (Path_ReportView__reportRedacted _x) = (\f x -> fmap (\y -> x{_reportRedacted = y}) (f (_reportRedacted x))) . toLens _x
          toLens (Path_ReportView__reportFlags _x) = (\f x -> fmap (\y -> x{_reportFlags = y}) (f (_reportFlags x))) . toLens _x
          toLens (Path_ReportView__reportOrderByItemName _x) = (\f x -> fmap (\y -> x{_reportOrderByItemName = y}) (f (_reportOrderByItemName x))) . toLens _x
          toLens (Path_ReportView__reportDisplayItemName _x) = (\f x -> fmap (\y -> x{_reportDisplayItemName = y}) (f (_reportDisplayItemName x))) . toLens _x
instance ToLens (Path_ReportView Markup)
    where type S (Path_ReportView Markup) = ReportView
          type A (Path_ReportView Markup) = Markup
          toLens (Path_ReportView__reportName _x) = \f x -> fmap (\y -> x{_reportName = y}) (f (_reportName x))
          toLens (Path_ReportView__reportDate _x) = \f x -> fmap (\y -> x{_reportDate = y}) (f (_reportDate x))
          toLens (Path_ReportView__reportContractDate _x) = \f x -> fmap (\y -> x{_reportContractDate = y}) (f (_reportContractDate x))
          toLens (Path_ReportView__reportInspectionDate _x) = \f x -> fmap (\y -> x{_reportInspectionDate = y}) (f (_reportInspectionDate x))
          toLens (Path_ReportView__reportEffectiveDate _x) = \f x -> fmap (\y -> x{_reportEffectiveDate = y}) (f (_reportEffectiveDate x))
          toLens (Path_ReportView__reportAuthors _x) = (\f x -> fmap (\y -> x{_reportAuthors = y}) (f (_reportAuthors x))) . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = \f x -> fmap (\y -> x{_reportPreparer = y}) (f (_reportPreparer x))
          toLens (Path_ReportView__reportPreparerEIN _x) = \f x -> fmap (\y -> x{_reportPreparerEIN = y}) (f (_reportPreparerEIN x))
          toLens (Path_ReportView__reportPreparerAddress _x) = \f x -> fmap (\y -> x{_reportPreparerAddress = y}) (f (_reportPreparerAddress x))
          toLens (Path_ReportView__reportPreparerEMail _x) = \f x -> fmap (\y -> x{_reportPreparerEMail = y}) (f (_reportPreparerEMail x))
          toLens (Path_ReportView__reportPreparerWebsite _x) = \f x -> fmap (\y -> x{_reportPreparerWebsite = y}) (f (_reportPreparerWebsite x))
          toLens (Path_ReportView__reportAbbrevs _x) = (\f x -> fmap (\y -> x{_reportAbbrevs = y}) (f (_reportAbbrevs x))) . toLens _x
          toLens (Path_ReportView__reportTitle _x) = \f x -> fmap (\y -> x{_reportTitle = y}) (f (_reportTitle x))
          toLens (Path_ReportView__reportHeader _x) = \f x -> fmap (\y -> x{_reportHeader = y}) (f (_reportHeader x))
          toLens (Path_ReportView__reportFooter _x) = \f x -> fmap (\y -> x{_reportFooter = y}) (f (_reportFooter x))
          toLens (Path_ReportView__reportValueTypeInfo _x) = (\f x -> fmap (\y -> x{_reportValueTypeInfo = y}) (f (_reportValueTypeInfo x))) . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = (\f x -> fmap (\y -> x{_reportValueApproachInfo = y}) (f (_reportValueApproachInfo x))) . toLens _x
          toLens (Path_ReportView__reportClientName _x) = \f x -> fmap (\y -> x{_reportClientName = y}) (f (_reportClientName x))
          toLens (Path_ReportView__reportClientAddress _x) = \f x -> fmap (\y -> x{_reportClientAddress = y}) (f (_reportClientAddress x))
          toLens (Path_ReportView__reportClientGreeting _x) = \f x -> fmap (\y -> x{_reportClientGreeting = y}) (f (_reportClientGreeting x))
          toLens (Path_ReportView__reportItemsOwnerFull _x) = \f x -> fmap (\y -> x{_reportItemsOwnerFull = y}) (f (_reportItemsOwnerFull x))
          toLens (Path_ReportView__reportItemsOwner _x) = \f x -> fmap (\y -> x{_reportItemsOwner = y}) (f (_reportItemsOwner x))
          toLens (Path_ReportView__reportBriefItems _x) = \f x -> fmap (\y -> x{_reportBriefItems = y}) (f (_reportBriefItems x))
          toLens (Path_ReportView__reportInspectionLocation _x) = \f x -> fmap (\y -> x{_reportInspectionLocation = y}) (f (_reportInspectionLocation x))
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = (\f x -> fmap (\y -> x{_reportGlossary = y}) (f (_reportGlossary x))) . toLens _x
          toLens (Path_ReportView__reportSources _x) = (\f x -> fmap (\y -> x{_reportSources = y}) (f (_reportSources x))) . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = \f x -> fmap (\y -> x{_reportLetterOfTransmittal = y}) (f (_reportLetterOfTransmittal x))
          toLens (Path_ReportView__reportScopeOfWork _x) = \f x -> fmap (\y -> x{_reportScopeOfWork = y}) (f (_reportScopeOfWork x))
          toLens (Path_ReportView__reportCertification _x) = (\f x -> fmap (\y -> x{_reportCertification = y}) (f (_reportCertification x))) . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = (\f x -> fmap (\y -> x{_reportLimitingConditions = y}) (f (_reportLimitingConditions x))) . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = \f x -> fmap (\y -> x{_reportPrivacyPolicy = y}) (f (_reportPrivacyPolicy x))
instance ToLens (Path_ReportView Permissions)
    where type S (Path_ReportView Permissions) = ReportView
          type A (Path_ReportView Permissions) = Permissions
          toLens (Path_ReportView__reportPerms _x) = \f x -> fmap (\y -> x{_reportPerms = y}) (f (_reportPerms x))
instance ToLens (Path_ReportView UserIds)
    where type S (Path_ReportView UserIds) = ReportView
          type A (Path_ReportView UserIds) = UserIds
          toLens (Path_ReportView__reportPerms _x) = (\f x -> fmap (\y -> x{_reportPerms = y}) (f (_reportPerms x))) . toLens _x
instance ToLens (Path_ReportView AbbrevPair)
    where type S (Path_ReportView AbbrevPair) = ReportView
          type A (Path_ReportView AbbrevPair) = AbbrevPair
          toLens (Path_ReportView__reportAbbrevs _x) = (\f x -> fmap (\y -> x{_reportAbbrevs = y}) (f (_reportAbbrevs x))) . toLens _x
instance ToLens (Path_ReportView AbbrevPairs)
    where type S (Path_ReportView AbbrevPairs) = ReportView
          type A (Path_ReportView AbbrevPairs) = AbbrevPairs
          toLens (Path_ReportView__reportAbbrevs _x) = \f x -> fmap (\y -> x{_reportAbbrevs = y}) (f (_reportAbbrevs x))
instance ToLens (Path_ReportView Author)
    where type S (Path_ReportView Author) = ReportView
          type A (Path_ReportView Author) = Author
          toLens (Path_ReportView__reportAuthors _x) = (\f x -> fmap (\y -> x{_reportAuthors = y}) (f (_reportAuthors x))) . toLens _x
instance ToLens (Path_ReportView Authors)
    where type S (Path_ReportView Authors) = ReportView
          type A (Path_ReportView Authors) = Authors
          toLens (Path_ReportView__reportAuthors _x) = \f x -> fmap (\y -> x{_reportAuthors = y}) (f (_reportAuthors x))
instance ToLens (Path_ReportView Branding)
    where type S (Path_ReportView Branding) = ReportView
          type A (Path_ReportView Branding) = Branding
          toLens (Path_ReportView__reportBranding _x) = \f x -> fmap (\y -> x{_reportBranding = y}) (f (_reportBranding x))
instance ToLens (Path_ReportView MarkupPair)
    where type S (Path_ReportView MarkupPair) = ReportView
          type A (Path_ReportView MarkupPair) = MarkupPair
          toLens (Path_ReportView__reportGlossary _x) = (\f x -> fmap (\y -> x{_reportGlossary = y}) (f (_reportGlossary x))) . toLens _x
          toLens (Path_ReportView__reportSources _x) = (\f x -> fmap (\y -> x{_reportSources = y}) (f (_reportSources x))) . toLens _x
instance ToLens (Path_ReportView MarkupPairs)
    where type S (Path_ReportView MarkupPairs) = ReportView
          type A (Path_ReportView MarkupPairs) = MarkupPairs
          toLens (Path_ReportView__reportGlossary _x) = \f x -> fmap (\y -> x{_reportGlossary = y}) (f (_reportGlossary x))
          toLens (Path_ReportView__reportSources _x) = \f x -> fmap (\y -> x{_reportSources = y}) (f (_reportSources x))
instance ToLens (Path_ReportView Markups)
    where type S (Path_ReportView Markups) = ReportView
          type A (Path_ReportView Markups) = Markups
          toLens (Path_ReportView__reportCertification _x) = \f x -> fmap (\y -> x{_reportCertification = y}) (f (_reportCertification x))
          toLens (Path_ReportView__reportLimitingConditions _x) = \f x -> fmap (\y -> x{_reportLimitingConditions = y}) (f (_reportLimitingConditions x))
instance ToLens (Path_ReportView MaybeReportIntendedUse)
    where type S (Path_ReportView MaybeReportIntendedUse) = ReportView
          type A (Path_ReportView MaybeReportIntendedUse) = MaybeReportIntendedUse
          toLens (Path_ReportView__reportIntendedUse _x) = \f x -> fmap (\y -> x{_reportIntendedUse = y}) (f (_reportIntendedUse x))
instance ToLens (Path_ReportView ReportElem)
    where type S (Path_ReportView ReportElem) = ReportView
          type A (Path_ReportView ReportElem) = ReportElem
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ReportElems)
    where type S (Path_ReportView ReportElems) = ReportView
          type A (Path_ReportView ReportElems) = ReportElems
          toLens (Path_ReportView__reportBody _x) = \f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))
instance ToLens (Path_ReportView ReportFlags)
    where type S (Path_ReportView ReportFlags) = ReportView
          type A (Path_ReportView ReportFlags) = ReportFlags
          toLens (Path_ReportView__reportFlags _x) = \f x -> fmap (\y -> x{_reportFlags = y}) (f (_reportFlags x))
instance ToLens (Path_ReportView ReportStandard)
    where type S (Path_ReportView ReportStandard) = ReportView
          type A (Path_ReportView ReportStandard) = ReportStandard
          toLens (Path_ReportView__reportStandardsVersion _x) = \f x -> fmap (\y -> x{_reportStandardsVersion = y}) (f (_reportStandardsVersion x))
instance ToLens (Path_ReportView ReportStatus)
    where type S (Path_ReportView ReportStatus) = ReportView
          type A (Path_ReportView ReportStatus) = ReportStatus
          toLens (Path_ReportView__reportStatus _x) = \f x -> fmap (\y -> x{_reportStatus = y}) (f (_reportStatus x))
instance ToLens (Path_ReportView ReportValueApproachInfo)
    where type S (Path_ReportView ReportValueApproachInfo) = ReportView
          type A (Path_ReportView ReportValueApproachInfo) = ReportValueApproachInfo
          toLens (Path_ReportView__reportValueApproachInfo _x) = \f x -> fmap (\y -> x{_reportValueApproachInfo = y}) (f (_reportValueApproachInfo x))
instance ToLens (Path_ReportView ReportValueTypeInfo)
    where type S (Path_ReportView ReportValueTypeInfo) = ReportView
          type A (Path_ReportView ReportValueTypeInfo) = ReportValueTypeInfo
          toLens (Path_ReportView__reportValueTypeInfo _x) = \f x -> fmap (\y -> x{_reportValueTypeInfo = y}) (f (_reportValueTypeInfo x))
instance ToLens (Path_ReportView EUI)
    where type S (Path_ReportView EUI) = ReportView
          type A (Path_ReportView EUI) = EUI
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView MEUI)
    where type S (Path_ReportView MEUI) = ReportView
          type A (Path_ReportView MEUI) = MEUI
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView MaybeImageFile)
    where type S (Path_ReportView MaybeImageFile) = ReportView
          type A (Path_ReportView MaybeImageFile) = MaybeImageFile
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ReportImage)
    where type S (Path_ReportView ReportImage) = ReportView
          type A (Path_ReportView ReportImage) = ReportImage
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ReportImages)
    where type S (Path_ReportView ReportImages) = ReportView
          type A (Path_ReportView ReportImages) = ReportImages
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ReadOnlyFilePath)
    where type S (Path_ReportView ReadOnlyFilePath) = ReportView
          type A (Path_ReportView ReadOnlyFilePath) = ReadOnlyFilePath
          toLens (Path_ReportView__reportFolder _x) = \f x -> fmap (\y -> x{_reportFolder = y}) (f (_reportFolder x))
instance ToLens (Path_ReportView ReportImageView)
    where type S (Path_ReportView ReportImageView) = ReportView
          type A (Path_ReportView ReportImageView) = ReportImageView
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView ReportView)
    where type S (Path_ReportView ReportView) = ReportView
          type A (Path_ReportView ReportView) = ReportView
          toLens _ = id
instance ToLens (Path_ReportView SaneSizeImageSize)
    where type S (Path_ReportView SaneSizeImageSize) = ReportView
          type A (Path_ReportView SaneSizeImageSize) = SaneSizeImageSize
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView Item)
    where type S (Path_ReportView Item) = ReportView
          type A (Path_ReportView Item) = Item
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView MIM)
    where type S (Path_ReportView MIM) = ReportView
          type A (Path_ReportView MIM) = MIM
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView CIString)
    where type S (Path_ReportView CIString) = ReportView
          type A (Path_ReportView CIString) = CIString
          toLens (Path_ReportView__reportAbbrevs _x) = (\f x -> fmap (\y -> x{_reportAbbrevs = y}) (f (_reportAbbrevs x))) . toLens _x
instance ToLens (Path_ReportView URI)
    where type S (Path_ReportView URI) = ReportView
          type A (Path_ReportView URI) = URI
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
instance ToLens (Path_ReportView Text)
    where type S (Path_ReportView Text) = ReportView
          type A (Path_ReportView Text) = Text
          toLens (Path_ReportView__reportName _x) = (\f x -> fmap (\y -> x{_reportName = y}) (f (_reportName x))) . toLens _x
          toLens (Path_ReportView__reportDate _x) = (\f x -> fmap (\y -> x{_reportDate = y}) (f (_reportDate x))) . toLens _x
          toLens (Path_ReportView__reportContractDate _x) = (\f x -> fmap (\y -> x{_reportContractDate = y}) (f (_reportContractDate x))) . toLens _x
          toLens (Path_ReportView__reportInspectionDate _x) = (\f x -> fmap (\y -> x{_reportInspectionDate = y}) (f (_reportInspectionDate x))) . toLens _x
          toLens (Path_ReportView__reportEffectiveDate _x) = (\f x -> fmap (\y -> x{_reportEffectiveDate = y}) (f (_reportEffectiveDate x))) . toLens _x
          toLens (Path_ReportView__reportAuthors _x) = (\f x -> fmap (\y -> x{_reportAuthors = y}) (f (_reportAuthors x))) . toLens _x
          toLens (Path_ReportView__reportPreparer _x) = (\f x -> fmap (\y -> x{_reportPreparer = y}) (f (_reportPreparer x))) . toLens _x
          toLens (Path_ReportView__reportPreparerEIN _x) = (\f x -> fmap (\y -> x{_reportPreparerEIN = y}) (f (_reportPreparerEIN x))) . toLens _x
          toLens (Path_ReportView__reportPreparerAddress _x) = (\f x -> fmap (\y -> x{_reportPreparerAddress = y}) (f (_reportPreparerAddress x))) . toLens _x
          toLens (Path_ReportView__reportPreparerEMail _x) = (\f x -> fmap (\y -> x{_reportPreparerEMail = y}) (f (_reportPreparerEMail x))) . toLens _x
          toLens (Path_ReportView__reportPreparerWebsite _x) = (\f x -> fmap (\y -> x{_reportPreparerWebsite = y}) (f (_reportPreparerWebsite x))) . toLens _x
          toLens (Path_ReportView__reportAbbrevs _x) = (\f x -> fmap (\y -> x{_reportAbbrevs = y}) (f (_reportAbbrevs x))) . toLens _x
          toLens (Path_ReportView__reportTitle _x) = (\f x -> fmap (\y -> x{_reportTitle = y}) (f (_reportTitle x))) . toLens _x
          toLens (Path_ReportView__reportHeader _x) = (\f x -> fmap (\y -> x{_reportHeader = y}) (f (_reportHeader x))) . toLens _x
          toLens (Path_ReportView__reportFooter _x) = (\f x -> fmap (\y -> x{_reportFooter = y}) (f (_reportFooter x))) . toLens _x
          toLens (Path_ReportView__reportValueTypeInfo _x) = (\f x -> fmap (\y -> x{_reportValueTypeInfo = y}) (f (_reportValueTypeInfo x))) . toLens _x
          toLens (Path_ReportView__reportValueApproachInfo _x) = (\f x -> fmap (\y -> x{_reportValueApproachInfo = y}) (f (_reportValueApproachInfo x))) . toLens _x
          toLens (Path_ReportView__reportClientName _x) = (\f x -> fmap (\y -> x{_reportClientName = y}) (f (_reportClientName x))) . toLens _x
          toLens (Path_ReportView__reportClientAddress _x) = (\f x -> fmap (\y -> x{_reportClientAddress = y}) (f (_reportClientAddress x))) . toLens _x
          toLens (Path_ReportView__reportClientGreeting _x) = (\f x -> fmap (\y -> x{_reportClientGreeting = y}) (f (_reportClientGreeting x))) . toLens _x
          toLens (Path_ReportView__reportItemsOwnerFull _x) = (\f x -> fmap (\y -> x{_reportItemsOwnerFull = y}) (f (_reportItemsOwnerFull x))) . toLens _x
          toLens (Path_ReportView__reportItemsOwner _x) = (\f x -> fmap (\y -> x{_reportItemsOwner = y}) (f (_reportItemsOwner x))) . toLens _x
          toLens (Path_ReportView__reportBriefItems _x) = (\f x -> fmap (\y -> x{_reportBriefItems = y}) (f (_reportBriefItems x))) . toLens _x
          toLens (Path_ReportView__reportInspectionLocation _x) = (\f x -> fmap (\y -> x{_reportInspectionLocation = y}) (f (_reportInspectionLocation x))) . toLens _x
          toLens (Path_ReportView__reportBody _x) = (\f x -> fmap (\y -> x{_reportBody = y}) (f (_reportBody x))) . toLens _x
          toLens (Path_ReportView__reportGlossary _x) = (\f x -> fmap (\y -> x{_reportGlossary = y}) (f (_reportGlossary x))) . toLens _x
          toLens (Path_ReportView__reportSources _x) = (\f x -> fmap (\y -> x{_reportSources = y}) (f (_reportSources x))) . toLens _x
          toLens (Path_ReportView__reportLetterOfTransmittal _x) = (\f x -> fmap (\y -> x{_reportLetterOfTransmittal = y}) (f (_reportLetterOfTransmittal x))) . toLens _x
          toLens (Path_ReportView__reportScopeOfWork _x) = (\f x -> fmap (\y -> x{_reportScopeOfWork = y}) (f (_reportScopeOfWork x))) . toLens _x
          toLens (Path_ReportView__reportCertification _x) = (\f x -> fmap (\y -> x{_reportCertification = y}) (f (_reportCertification x))) . toLens _x
          toLens (Path_ReportView__reportLimitingConditions _x) = (\f x -> fmap (\y -> x{_reportLimitingConditions = y}) (f (_reportLimitingConditions x))) . toLens _x
          toLens (Path_ReportView__reportPrivacyPolicy _x) = (\f x -> fmap (\y -> x{_reportPrivacyPolicy = y}) (f (_reportPrivacyPolicy x))) . toLens _x
          toLens (Path_ReportView__reportPerms _x) = (\f x -> fmap (\y -> x{_reportPerms = y}) (f (_reportPerms x))) . toLens _x
          toLens (Path_ReportView__reportBranding _x) = (\f x -> fmap (\y -> x{_reportBranding = y}) (f (_reportBranding x))) . toLens _x
instance ToLens (Path_ReportView UserId)
    where type S (Path_ReportView UserId) = ReportView
          type A (Path_ReportView UserId) = UserId
          toLens (Path_ReportView__reportPerms _x) = (\f x -> fmap (\y -> x{_reportPerms = y}) (f (_reportPerms x))) . toLens _x
instance ToLens (Path_ReportView UUID)
    where type S (Path_ReportView UUID) = ReportView
          type A (Path_ReportView UUID) = UUID
          toLens (Path_ReportView__reportUUID _x) = \f x -> fmap (\y -> x{_reportUUID = y}) (f (_reportUUID x))
instance ToLens (Path_SaneSizeImageSize String)
    where type S (Path_SaneSizeImageSize String) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize String) = String
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
instance ToLens (Path_SaneSizeImageSize Double)
    where type S (Path_SaneSizeImageSize Double) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize Double) = Double
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
instance ToLens (Path_SaneSizeImageSize Dimension)
    where type S (Path_SaneSizeImageSize Dimension) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize Dimension) = Dimension
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
instance ToLens (Path_SaneSizeImageSize ImageSize)
    where type S (Path_SaneSizeImageSize ImageSize) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize ImageSize) = ImageSize
          toLens (Path_SaneSizeImageSize_View _) = viewLens :: Lens' (SaneSize ImageSize)
                                                                     ImageSize
instance ToLens (Path_SaneSizeImageSize Units)
    where type S (Path_SaneSizeImageSize Units) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize Units) = Units
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
instance ToLens (Path_SaneSizeImageSize JSONText)
    where type S (Path_SaneSizeImageSize JSONText) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize JSONText) = JSONText
          toLens (Path_SaneSizeImageSize_View v) = (viewLens :: Lens' (SaneSize ImageSize)
                                                                      ImageSize) . toLens v
instance ToLens (Path_SaneSizeImageSize SaneSizeImageSize)
    where type S (Path_SaneSizeImageSize SaneSizeImageSize) = SaneSizeImageSize
          type A (Path_SaneSizeImageSize SaneSizeImageSize) = SaneSizeImageSize
          toLens _ = id
instance ToLens (Path_String String)
    where type S (Path_String String) = String
          type A (Path_String String) = String
          toLens _ = id
instance ToLens (Path_String JSONText)
    where type S (Path_String JSONText) = String
          type A (Path_String JSONText) = JSONText
          toLens (Path_String_View _) = viewLens :: Lens' ([Char]) JSONText
instance ToLens (Path_Text JSONText)
    where type S (Path_Text JSONText) = Text
          type A (Path_Text JSONText) = JSONText
          toLens (Path_Text_View _) = viewLens :: Lens' Text JSONText
instance ToLens (Path_Text Text)
    where type S (Path_Text Text) = Text
          type A (Path_Text Text) = Text
          toLens _ = id
instance ToLens (Path_URI URI)
    where type S (Path_URI URI) = URI
          type A (Path_URI URI) = URI
          toLens _ = id
instance ToLens (Path_UUID UUID)
    where type S (Path_UUID UUID) = UUID
          type A (Path_UUID UUID) = UUID
          toLens _ = id
instance ToLens (Path_Units Units)
    where type S (Path_Units Units) = Units
          type A (Path_Units Units) = Units
          toLens _ = id
instance ToLens (Path_Units JSONText)
    where type S (Path_Units JSONText) = Units
          type A (Path_Units JSONText) = JSONText
          toLens (Path_Units_View _) = viewLens :: Lens' Units JSONText
instance ToLens (Path_UserId UserId)
    where type S (Path_UserId UserId) = UserId
          type A (Path_UserId UserId) = UserId
          toLens _ = id
instance ToLens (Path_UserIds JSONText)
    where type S (Path_UserIds JSONText) = UserIds
          type A (Path_UserIds JSONText) = JSONText
          toLens (Path_UserIds_View v) = (viewLens :: Lens' ([UserId])
                                                            Text) . toLens v
instance ToLens (Path_UserIds UserIds)
    where type S (Path_UserIds UserIds) = UserIds
          type A (Path_UserIds UserIds) = UserIds
          toLens _ = id
instance ToLens (Path_UserIds Text)
    where type S (Path_UserIds Text) = UserIds
          type A (Path_UserIds Text) = Text
          toLens (Path_UserIds_View _) = viewLens :: Lens' ([UserId]) Text
instance ToLens (Path_Maybe (Path_Either (Path_URI ImageFile)
                                         (Path_ImageFile ImageFile)))
    where type S (Path_Maybe (Path_Either (Path_URI ImageFile)
                                          (Path_ImageFile ImageFile))) = MEUI
          type A (Path_Maybe (Path_Either (Path_URI ImageFile)
                                          (Path_ImageFile ImageFile))) = ImageFile
          toLens (Path_Just v) = _Just . toLens v
instance ToLens (Path_Maybe (Path_Either (Path_URI EUI)
                                         (Path_ImageFile EUI)))
    where type S (Path_Maybe (Path_Either (Path_URI EUI)
                                          (Path_ImageFile EUI))) = MEUI
          type A (Path_Maybe (Path_Either (Path_URI EUI)
                                          (Path_ImageFile EUI))) = EUI
          toLens (Path_Just _) = _Just
instance ToLens (Path_Maybe (Path_Either (Path_URI MEUI)
                                         (Path_ImageFile MEUI)))
    where type S (Path_Maybe (Path_Either (Path_URI MEUI)
                                          (Path_ImageFile MEUI))) = MEUI
          type A (Path_Maybe (Path_Either (Path_URI MEUI)
                                          (Path_ImageFile MEUI))) = MEUI
          toLens _ = id
instance ToLens (Path_Maybe (Path_Either (Path_URI URI)
                                         (Path_ImageFile URI)))
    where type S (Path_Maybe (Path_Either (Path_URI URI)
                                          (Path_ImageFile URI))) = MEUI
          type A (Path_Maybe (Path_Either (Path_URI URI)
                                          (Path_ImageFile URI))) = URI
          toLens (Path_Just v) = _Just . toLens v
