{-# LANGUAGE CPP, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, StandaloneDeriving, TypeSynonymInstances, TypeFamilies #-}
{-# OPTIONS -fcontext-stack=100 -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-name-shadowing -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Appraisal.Report
    ( ReportFieldLabel(..)
    , Author(..)
    , Authors, AuthorID(..)
    , AuthorFieldLabel(..)
    , ReportValueTypeInfo(..)
    , ReportIntendedUse(..)
    , MaybeReportIntendedUse
    , ReportValueApproachInfo(..)
    , ReportElem(..)
    , ReportElems, ReportElemID(..)
    , ReportStatus(..)
    , ReportFlags(..)
    , Branding(..)
    , AbbrevPairs, AbbrevPairID(..)
    , MarkupPairs, MarkupPairID(..)
    , Markups, MarkupID(..)
    , ReportStandard(..)
    , Report(..)
    , reportURI
    , reportDir
    , reportPath
    , reportBase
    , ReportElemTypeName(..)
    , reportIntendedUseMarkup
    , reportValueSum
    , normalizeReport
    , sortElems
    , reportBrandingLens
    -- * Exports for testing
    , EpochMilli
    , AbbrevPair
    , MarkupPair
    ) where

import Appraisal.Config (Paths(reports), reportsURIPath)
import Appraisal.Currency (addCashValues, CashValue, Priceable(..))
import Appraisal.File (File(File, fileSource, fileChksum, fileMessages), FileSource(TheURI))
import Appraisal.Image (Dimension, ImageCrop, ImageSize, Units)
import Appraisal.ImageFile (ImageFile(ImageFile, imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal), ImageType(..))
#if !__GHCJS__
import Appraisal.IntJS (deriveOrderJS)
#else
import Appraisal.IntJS ()
#endif
import Appraisal.Markup as M (Markup(Html, Markdown), mapChars, rawMarkdown, markupText)
import Appraisal.Maybe (lens_mrs', Maybe'(Just', Nothing'))
import Appraisal.Permissions (Permissions)
import Appraisal.ReportImage (ReportImage, ReportImageID)
import Appraisal.ReportItem (Item(..))
import qualified Appraisal.ReportItem as I (Item(fields), ItemFieldName(ItemDataSheetNumber))
import qualified Appraisal.Utils.Builders as Builders (empty)
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.Debug (trace'')
import Appraisal.Utils.List (spanBy)
import Appraisal.Utils.Text (read)
import Data.Aeson (decode, encode, FromJSON(..), ToJSON(..), Value(String), withText)
import Data.Char (isDigit, toLower)
import Data.Function (on)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.Int (Int64)
import qualified Data.IxSet.Revision as R (Ident(..), Revision(..), RevisionInfo(..))
import Control.Lens (Iso', iso)
import Data.List as List (groupBy, sortBy)
import qualified Data.ListLike as LL
import Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Data.Order as Order (toList, asList)
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(MigrateFrom, migrate))
import Data.Text as Text (empty, groupBy, pack, strip, Text, uncons, unpack)
import qualified Data.UUID.Types as UUID (fromString, toString)
import Data.UUID.Types (UUID)
import qualified Data.UUID.V4 as UUID (nextRandom)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Path.Core (readShowIso, SelfPath)
import Language.Haskell.TH.Path.View (View(ViewType, viewLens))
import Data.UUID.Orphans ()
import Prelude hiding (read)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Web.Routes.TH (derivePathInfo)

#if 0
approachesToValueOldDefault :: Markup
approachesToValueOldDefault =
    -- rawMarkdown "# Approaches to Value #\n\n" <>
    rawMarkdown
          ("For this appraisal three valuation methods were considered " <>
             "(from Soucy and Smith, eds, <em>The Appraisal of Personal Property – " <>
             "Principles, Theories, and Practice Methods for the Professional " <>
             "Appraiser</em>, 1994.). The three valuation methods are:\n\n") <>
    rawMarkdown (" * <bf>Cost Approach to Value</bf> method estimates either the reproduction or " <>
                          "replacement of a property, either new or depreciated.\n\n") <>
    rawMarkdown (" * <bf>Income Approach to Value</bf> method estimates the present worth of " <>
                 "anticipated future benefits of owning income producing " <>
                 "properties or objects.\n\n") <>
    rawMarkdown (" * <bf>Sales Comparison Approach to Value</bf> method estimates value by using " <>
                 "one or more methods that compare the subject to other similar " <>
                 "properties that have been sold in the relevant market with adjustments (up or down) made for all " <>
                 "differences that affect value, such as differences in characteristics of value, in market level, " <>
                 "and in time.\n\n")
#endif

{-
approachesToValueNewDefault :: Markup
approachesToValueNewDefault =
    -- rawMarkdown "# Approaches to Value #\n\n" <>
    rawMarkdown
          ("For this appraisal three valuation methods were considered.  The three valuation methods are (as defined by The American Society of Appraisers Monograph #7: <em>Analyzing the Research</em>, 2010). The three valuation methods are:\n\n") <>
    rawMarkdown (" * <bf>Cost Approach to Value</bf> method estimates either the reproduction or " <>
                          "replacement of a property, either new or depreciated.\n\n") <>
    rawMarkdown (" * <bf>Income Approach to Value</bf> method estimates the present worth of " <>
                          "anticipated future benefits of owning income producing " <>
                          "properties or objects.\n\n") <>
    rawMarkdown (" * <bf>Market Comparison Approach to Value</bf> method estimates value by " <>
                          "comparison with properties sold in the relevant market with " <>
                          "adjustments for all differences that affect value, such as " <>
                          "differences in characteristics of value, in market layer, and " <>
                          "in time exposed to the market in order to arrive at the most " <>
                          "apposite estimate of value.\n\n")
-}

data ReportFieldLabel
    = ReportName
    | ReportDate
    | ReportContractDate
    | ReportInspectionDate
    | ReportEffectiveDate
    | ReportAuthor
    | ReportPreparer
    | ReportPreparerEIN
    | ReportPreparerAddress
    | ReportPreparerEMail
    | ReportPreparerWebsite
    | ReportTitle
    | ReportIntendedUse
    | ReportValueType
    | ReportValueApproach
    | ReportClient
    | ReportClientGreeting
    | ReportItemsOwnerFull
    | ReportItemsOwner
    | ReportBriefItems
    | ReportInspectionLocation
    | ParagraphText
    | ReportElemType
    deriving (Read, Show, Eq)

data Author
    = Author { authorName :: Markup, authorCredentials :: Markup }
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

#if !__GHCJS__
$(deriveOrderJS ''Author)
#else
newtype AuthorID = AuthorID {unAuthorID :: Integer} deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, FromJSON, ToJSON)
instance Enum AuthorID where fromEnum = fromInteger . unAuthorID; toEnum = AuthorID . toInteger
type Authors = Order AuthorID Author
#endif

data AuthorFieldLabel
    = AuthorName
    | AuthorCredentials
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

data ReportValueTypeInfo
    = ReportValueTypeInfo
      { reportValueTypeName :: Markup
      , reportValueTypeDescription :: Markup
      , reportValueTypeDefinition :: Markup
      } deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

{-
data ReportIntendedUse_1
    = SalesAdvisory_1
    | EstatePlanning_1
    | EstateTax_1
    | Insurance_1
    | CharitableDonation_1
    deriving (Read, Show, Eq, Ord, Typeable, Data)
-}

data ReportIntendedUse_2
    = SalesAdvisory_2
    | EstatePlanning_2
    | EstateTax_2
    | Insurance_2
    | CharitableDonation_2
    | EquitableDistribution_2
    | MaritalDissolution_2
    | EstateDivision_2
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data)

data ReportIntendedUse_3
    = SalesAdvisory_3
    | EstatePlanning_3
    | EstateTax_3
    | InsuranceCoverage_3
    | InsuranceClaim_3
    | CharitableDonation_3
    | EquitableDistribution_3
    | MaritalDissolution_3
    | EstateDivision_3
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data, Generic, FromJSON, ToJSON)

#if !__GHCJS__
instance Migrate ReportIntendedUse where
    type MigrateFrom ReportIntendedUse = ReportIntendedUse_3
    migrate SalesAdvisory_3 = SalesAdvisory
    migrate EstatePlanning_3 = EstatePlanning
    migrate EstateTax_3 = EstateTax
    migrate InsuranceCoverage_3 = InsuranceCoverage
    migrate InsuranceClaim_3 = InsuranceClaim
    migrate CharitableDonation_3 = CharitableDonation
    migrate EquitableDistribution_3 = EquitableDistribution
    migrate MaritalDissolution_3 = MaritalDissolution
    migrate EstateDivision_3 = EstateDivision
#endif

data ReportIntendedUse
    = SalesAdvisory
    | EstatePlanning
    | EstateTax
    | InsuranceCoverage
    | InsuranceClaim
    | CharitableDonation
    | EquitableDistribution
    | MaritalDissolution
    | EstateDivision
    | EstateProbate
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data, Generic, FromJSON, ToJSON)

instance View ReportIntendedUse where
    type ViewType ReportIntendedUse = String
    viewLens = readShowIso SalesAdvisory

data ReportValueApproachInfo
    = ReportValueApproachInfo
    { reportValueApproachName :: Markup
    , reportValueApproachDescription :: Markup
    } deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

#if !__GHCJS__
data ReportElem_1
    = ReportItem_1 {elemItem_1 :: Item}
    | ReportParagraph_1 {elemText_1 :: Markup}
    | ReportUndecided_1
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

instance Migrate ReportElem where
    type MigrateFrom ReportElem = ReportElem_1
    -- Change elemText fields marked Html to Markdown, they were changed accidentally
    migrate (ReportItem_1 {elemItem_1 = x}) = ReportItem {elemItem = x}
    migrate (ReportParagraph_1 {elemText_1 = Html x}) = ReportParagraph {elemText = Markdown x}
    migrate (ReportParagraph_1 {elemText_1 = x}) = ReportParagraph {elemText = x}
    migrate ReportUndecided_1 = ReportUndecided
#endif

data ReportElem
    = ReportItem {elemItem :: Item}
    | ReportParagraph {elemText :: Markup}
    | ReportUndecided
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

deriving instance Show (R.Revision R.Ident)
deriving instance Show (R.RevisionInfo R.Ident)

data ReportStatus
    = Draft
    -- ^ This is the current authoritative version of the report
    | Final
    -- ^ The report has been downloaded, and perhaps uploaded to a
    -- different server.
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

-- Does ReportStatus belong in ReportFlags?  Seems more like meta info.
data ReportFlags = ReportFlags {
  hideEmptyItemFields :: Bool
  } deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

data Branding
    = NoLogo
    | Logo ImageFile
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

type EpochMilli = Int64

type MarkupPair = (Markup, Markup)
type AbbrevPair = (CIString, Markup)

#if !__GHCJS__
$(deriveOrderJS ''ReportElem)
$(deriveOrderJS ''MarkupPair)
$(deriveOrderJS ''AbbrevPair)
$(deriveOrderJS ''Markup)
#else
newtype ReportElemID = ReportElemID {unReportElemID :: Integer} deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, FromJSON, ToJSON)
instance Enum ReportElemID where fromEnum = fromInteger . unReportElemID; toEnum = ReportElemID . toInteger
type ReportElems = Order ReportElemID ReportElem

newtype MarkupPairID = MarkupPairID {unMarkupPairID :: Integer} deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, FromJSON, ToJSON)
instance Enum MarkupPairID where fromEnum = fromInteger . unMarkupPairID; toEnum = MarkupPairID . toInteger
type MarkupPairs = Order MarkupPairID MarkupPair

newtype AbbrevPairID = AbbrevPairID {unAbbrevPairID :: Integer} deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, FromJSON, ToJSON)
instance Enum AbbrevPairID where fromEnum = fromInteger . unAbbrevPairID; toEnum = AbbrevPairID . toInteger
type AbbrevPairs = Order AbbrevPairID AbbrevPair

newtype MarkupID = MarkupID {unMarkupID :: Integer} deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, FromJSON, ToJSON)
instance Enum MarkupID where fromEnum = fromInteger . unMarkupID; toEnum = MarkupID . toInteger
type Markups = Order MarkupID Markup
#endif

type MaybeReportIntendedUse = Maybe' ReportIntendedUse

instance View MaybeReportIntendedUse where
    type ViewType MaybeReportIntendedUse = String
    viewLens = lens_mrs'

-- | The ReportStandard type indicates what UI features should be used
-- for a particular report.  I would have made this an enumerated type,
-- but right now the path generation code isn't ready for it.  So,
--    1 - Initial value for existing reports, corresponds to Uniform
--        Standards of Professional Appraisal Practice (USPAP), the 2014-2015
--        Edition.
--    2 - A number of UI changes are implemented for this next version,
--        and new reports will be set to this value.  In particular, the Scope of Work
--        section is no longer included.
data ReportStandard = ReportStandard {unReportStandard :: Int} deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

#if !__GHCJS__
data Report_17
    = Report_17
             { reportFolder_17 :: FilePath
             , reportName_17 :: Markup
             , reportDate_17 :: Markup
             , reportContractDate_17 :: Markup
             , reportInspectionDate_17 :: Markup
             , reportEffectiveDate_17 :: Markup
             , reportAuthors_17 :: Authors
             , reportPreparer_17 :: Markup
             , reportPreparerEIN_17 :: Markup
             , reportPreparerAddress_17 :: Markup
             , reportPreparerEMail_17 :: Markup
             , reportPreparerWebsite_17 :: Markup
             , reportAbbrevs_17 :: AbbrevPairs
             , reportTitle_17 :: Markup
             , reportHeader_17 :: Markup
             , reportFooter_17 :: Markup
             , reportIntendedUse_17 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_17 :: ReportValueTypeInfo
             , reportValueApproachInfo_17 :: ReportValueApproachInfo
             , reportClientName_17 :: Markup
             , reportClientAddress_17 :: Markup
             , reportClientGreeting_17 :: Markup
             , reportItemsOwnerFull_17 :: Markup
             , reportItemsOwner_17 :: Markup
             , reportBriefItems_17 :: Markup
             , reportInspectionLocation_17 :: Markup
             , reportBody_17 :: ReportElems
             , reportGlossary_17 :: MarkupPairs
             , reportSources_17 :: MarkupPairs
             , reportLetterOfTransmittal_17 :: Markup
             , reportScopeOfWork_17 :: Markup
             , reportCertification_17 :: Markups
             , reportLimitingConditions_17 :: Markups
             , reportPrivacyPolicy_17 :: Markup
             , reportPerms_17 :: Permissions
             , reportRevision_17 :: Integer
             , reportCreated_17 :: EpochMilli
             , reportBranding_17 :: Branding
             , reportStatus_17 :: ReportStatus
             , reportRedacted_17 :: Bool
             , reportFlags_17 :: ReportFlags
             , reportUUID_17 :: UUID
             , reportOrderByItemName_17 :: Bool
             , reportDisplayItemName_17 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_18 where
    type MigrateFrom Report_18 = Report_17
    migrate r =
        Report_18
               { reportStandardsVersion_18 = ReportStandard 1
               , reportFolder_18 = reportFolder_17 r
               , reportName_18 = reportName_17 r
               , reportDate_18 = reportDate_17 r
               , reportContractDate_18 = reportContractDate_17 r
               , reportInspectionDate_18 = reportInspectionDate_17 r
               , reportEffectiveDate_18 = reportEffectiveDate_17 r
               , reportAuthors_18 = reportAuthors_17 r
               , reportPreparer_18 = reportPreparer_17 r
               , reportPreparerEIN_18 = reportPreparerEIN_17 r
               , reportPreparerAddress_18 = reportPreparerAddress_17 r
               , reportPreparerEMail_18 = reportPreparerEMail_17 r
               , reportPreparerWebsite_18 = reportPreparerWebsite_17 r
               , reportAbbrevs_18 = reportAbbrevs_17 r
               , reportTitle_18 = reportTitle_17 r
               , reportHeader_18 = reportHeader_17 r
               , reportFooter_18 = reportFooter_17 r
               , reportIntendedUse_18 = reportIntendedUse_17 r
               , reportValueTypeInfo_18 = reportValueTypeInfo_17 r
               , reportValueApproachInfo_18 = reportValueApproachInfo_17 r
               , reportClientName_18 = reportClientName_17 r
               , reportClientAddress_18 = reportClientAddress_17 r
               , reportClientGreeting_18 = reportClientGreeting_17 r
               , reportItemsOwnerFull_18 = reportItemsOwnerFull_17 r
               , reportItemsOwner_18 = reportItemsOwner_17 r
               , reportBriefItems_18 = reportBriefItems_17 r
               , reportInspectionLocation_18 = reportInspectionLocation_17 r
               , reportBody_18 = reportBody_17 r
               , reportGlossary_18 = reportGlossary_17 r
               , reportSources_18 = reportSources_17 r
               , reportLetterOfTransmittal_18 = reportLetterOfTransmittal_17 r
               , reportScopeOfWork_18 = reportScopeOfWork_17 r
               , reportCertification_18 = reportCertification_17 r
               , reportLimitingConditions_18 = reportLimitingConditions_17 r
               , reportPrivacyPolicy_18 = reportPrivacyPolicy_17 r
               , reportPerms_18 = reportPerms_17 r
               , reportRevision_18 = reportRevision_17 r
               , reportCreated_18 = reportCreated_17 r
               , reportBranding_18 = reportBranding_17 r
               , reportStatus_18 = reportStatus_17 r
               , reportRedacted_18 = reportRedacted_17 r
               , reportFlags_18 = reportFlags_17 r
               , reportUUID_18 = reportUUID_17 r
               , reportOrderByItemName_18 = reportOrderByItemName_17 r
               , reportDisplayItemName_18 = reportDisplayItemName_17 r }
#endif

data Report_18
    = Report_18
             { reportFolder_18 :: FilePath
             , reportName_18 :: Markup
             , reportDate_18 :: Markup
             , reportContractDate_18 :: Markup
             , reportInspectionDate_18 :: Markup
             , reportEffectiveDate_18 :: Markup
             , reportAuthors_18 :: Authors
             , reportPreparer_18 :: Markup
             , reportPreparerEIN_18 :: Markup
             , reportPreparerAddress_18 :: Markup
             , reportPreparerEMail_18 :: Markup
             , reportPreparerWebsite_18 :: Markup
             , reportAbbrevs_18 :: AbbrevPairs
             , reportTitle_18 :: Markup
             , reportHeader_18 :: Markup
             , reportFooter_18 :: Markup
             , reportIntendedUse_18 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_18 :: ReportValueTypeInfo
             , reportValueApproachInfo_18 :: ReportValueApproachInfo
             , reportClientName_18 :: Markup
             , reportClientAddress_18 :: Markup
             , reportClientGreeting_18 :: Markup
             , reportItemsOwnerFull_18 :: Markup
             , reportItemsOwner_18 :: Markup
             , reportBriefItems_18 :: Markup
             , reportInspectionLocation_18 :: Markup
             , reportBody_18 :: ReportElems
             , reportGlossary_18 :: MarkupPairs
             , reportSources_18 :: MarkupPairs
             , reportLetterOfTransmittal_18 :: Markup
             , reportScopeOfWork_18 :: Markup
             , reportCertification_18 :: Markups
             , reportLimitingConditions_18 :: Markups
             , reportPrivacyPolicy_18 :: Markup
             , reportPerms_18 :: Permissions
             , reportRevision_18 :: Integer
             , reportCreated_18 :: EpochMilli
             , reportBranding_18 :: Branding
             , reportStatus_18 :: ReportStatus
             , reportRedacted_18 :: Bool
             , reportFlags_18 :: ReportFlags
             , reportUUID_18 :: UUID
             , reportOrderByItemName_18 :: Bool
             , reportDisplayItemName_18 :: Bool
             , reportStandardsVersion_18 :: ReportStandard
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

instance Migrate Report where
    type MigrateFrom Report = Report_18
    migrate r =
        Report { reportFolder = reportFolder_18 r
               , reportName = reportName_18 r
               , reportDate = reportDate_18 r
               , reportContractDate = reportContractDate_18 r
               , reportInspectionDate = reportInspectionDate_18 r
               , reportEffectiveDate = reportEffectiveDate_18 r
               , reportAuthors = reportAuthors_18 r
               , reportPreparer = reportPreparer_18 r
               , reportPreparerEIN = reportPreparerEIN_18 r
               , reportPreparerAddress = reportPreparerAddress_18 r
               , reportPreparerEMail = reportPreparerEMail_18 r
               , reportPreparerWebsite = reportPreparerWebsite_18 r
               , reportAbbrevs = reportAbbrevs_18 r
               , reportTitle = reportTitle_18 r
               , reportHeader = reportHeader_18 r
               , reportFooter = reportFooter_18 r
               , reportIntendedUse = maybe Nothing' Just' (reportIntendedUse_18 r)
               , reportValueTypeInfo = reportValueTypeInfo_18 r
               , reportValueApproachInfo = reportValueApproachInfo_18 r
               , reportClientName = reportClientName_18 r
               , reportClientAddress = reportClientAddress_18 r
               , reportClientGreeting = reportClientGreeting_18 r
               , reportItemsOwnerFull = reportItemsOwnerFull_18 r
               , reportItemsOwner = reportItemsOwner_18 r
               , reportBriefItems = reportBriefItems_18 r
               , reportInspectionLocation = reportInspectionLocation_18 r
               , reportBody = reportBody_18 r
               , reportGlossary = reportGlossary_18 r
               , reportSources = reportSources_18 r
               , reportLetterOfTransmittal = reportLetterOfTransmittal_18 r
               , reportScopeOfWork = reportScopeOfWork_18 r
               , reportCertification = reportCertification_18 r
               , reportLimitingConditions = reportLimitingConditions_18 r
               , reportPrivacyPolicy = reportPrivacyPolicy_18 r
               , reportPerms = reportPerms_18 r
               , reportRevision = reportRevision_18 r
               , reportCreated = reportCreated_18 r
               , reportBranding = reportBranding_18 r
               , reportStatus = reportStatus_18 r
               , reportRedacted = reportRedacted_18 r
               , reportFlags = reportFlags_18 r
               , reportUUID = reportUUID_18 r
               , reportOrderByItemName = reportOrderByItemName_18 r
               , reportDisplayItemName = reportDisplayItemName_18 r
               , reportStandardsVersion = reportStandardsVersion_18 r
               }

data Report
    = Report { reportFolder :: FilePath
             , reportName :: Markup
             , reportDate :: Markup
             , reportContractDate :: Markup
             , reportInspectionDate :: Markup
             , reportEffectiveDate :: Markup
             , reportAuthors :: Authors
             , reportPreparer :: Markup
             , reportPreparerEIN :: Markup
             , reportPreparerAddress :: Markup
             , reportPreparerEMail :: Markup
             , reportPreparerWebsite :: Markup
             , reportAbbrevs :: AbbrevPairs
             , reportTitle :: Markup
             , reportHeader :: Markup
             , reportFooter :: Markup
             , reportIntendedUse :: MaybeReportIntendedUse
             , reportValueTypeInfo :: ReportValueTypeInfo
             , reportValueApproachInfo :: ReportValueApproachInfo
             , reportClientName :: Markup
             , reportClientAddress :: Markup
             , reportClientGreeting :: Markup
             , reportItemsOwnerFull :: Markup
             , reportItemsOwner :: Markup
             , reportBriefItems :: Markup
             , reportInspectionLocation :: Markup
             , reportBody :: ReportElems
             , reportGlossary :: MarkupPairs
             , reportSources :: MarkupPairs
             , reportLetterOfTransmittal :: Markup
             , reportScopeOfWork :: Markup
             , reportCertification :: Markups
             , reportLimitingConditions :: Markups
             , reportPrivacyPolicy :: Markup
             , reportPerms :: Permissions
             , reportRevision :: Integer
             , reportCreated :: EpochMilli
             , reportBranding :: Branding
             , reportStatus :: ReportStatus
             , reportRedacted :: Bool
             , reportFlags :: ReportFlags
             , reportUUID :: UUID
             , reportOrderByItemName :: Bool
             , reportDisplayItemName :: Bool
             , reportStandardsVersion :: ReportStandard
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, FromJSON, ToJSON)

reportURI :: Paths a => a -> Report -> String -> FilePath
reportURI paths report ext =
    reportsURIPath paths </> reportBase report </> reportFile report ext ++ "?" ++ r
    where r = show . reportRevision $ report

reportDir :: Paths a => a -> Report -> FilePath
reportDir paths report = reports paths </> reportBase report

reportPath :: Paths a => a -> Report -> String -> FilePath
reportPath paths report ext = reportDir paths report </> reportFile report ext

-- CB: remove the conditional, always use the report UUID now, because we never look at the existing dirs.
reportBase :: Report -> FilePath
reportBase = UUID.toString . reportUUID

reportFile :: Report -> String -> String
reportFile _report ext = "report" ++ ext

data ReportElemTypeName
    = UndecidedElem
    | ItemElem
    | CommentaryElem
    deriving (Read, Show, Eq, Generic, FromJSON, ToJSON)

reportIntendedUseMarkup :: Report -> Maybe Markup
reportIntendedUseMarkup report =
    case reportIntendedUse report of
      Just' SalesAdvisory -> Just (rawMarkdown "Sales Advisory")
      Just' EstatePlanning -> Just (rawMarkdown "Estate Planning")
      Just' EstateTax -> Just (rawMarkdown "Estate Tax")
      Just' InsuranceCoverage -> Just (rawMarkdown "Insurance Coverage")
      Just' InsuranceClaim -> Just (rawMarkdown "Insurance Claim")
      Just' CharitableDonation -> Just (rawMarkdown "Charitable Donation for Income Tax")
      Just' EquitableDistribution -> Just (rawMarkdown "Equitable Distribution")
      Just' MaritalDissolution -> Just (rawMarkdown "Marital Dissolution")
      Just' EstateDivision -> Just (rawMarkdown "Estate Division")
      Just' EstateProbate -> Just (rawMarkdown "Estate Probate")
      Nothing' -> Nothing

reportValueSum :: Report -> (CashValue, Int, Int)
reportValueSum report =
    addCashValues (map cashValue (catMaybes (map item (toList (reportBody report)))))
    where
      item (ReportItem x) = Just x
      item _ = Nothing

-- |This puts the report into a more standard form before comparing it
-- to the old version.  It is unfortunate to have to do an
-- "everywhere" to strip whitespace, it would be nicer to just strip
-- it from the field values we get from the form, but there is already
-- some in the database and I want to get rid of it.
normalizeReport :: Report -> Report
normalizeReport report =
    report' { reportAbbrevs = uniq . LL.sortBy (compare `on` fst) $ (reportAbbrevs report')
            , reportBody = sortElems (reportBody report')
            , reportGlossary = LL.sortBy (compare `on` (M.mapChars toLower . fst)) (reportGlossary report') }
    where report' = everywhere (mkT Text.strip) report
          uniq = asList f
          f :: Eq a => [(k, (a, b))] -> [(k, (a, b))]
          f = map head . List.groupBy ((==) `on` (fst . snd))

-- Sort the items of the report according to their item number, while
-- preserving the order of the non-item elements relative to the
-- items.  It is assumed that non-items are associated with the item they follow,
-- unless they are at the beginning of the list.
sortElems :: ReportElems -> ReportElems
sortElems xs =
    asList f xs
    where
      f :: [(k, ReportElem)] -> [(k, ReportElem)]
      f = concat . sortBy cmp . spanBy (isItem . snd)
      isItem (ReportItem _) = True
      isItem _ = False
      cmp ((_, ReportItem x) : _) ((_, ReportItem y) : _) =
          case (Map.lookup I.ItemDataSheetNumber (I.fields x), Map.lookup I.ItemDataSheetNumber (I.fields y)) of
            (Just x', Just y') -> compareVersions x' y'
            (Just _, _) -> LT
            (_, Just _) -> GT
            _ -> EQ
      -- The one group that doesn't begin with a ReportItem needs to stay at the beginning.
      cmp ((_, ReportItem _) : _) _ = GT
      cmp _ ((_, ReportItem _) : _) = LT
      cmp _ _ = EQ

data Tagged = Digits Text | Chars Text

-- |This is a simplified version of the debian version number
-- comparison algorithm.  It splits a string into numeric and non
-- numeric stretches, and compares the non-numeric portions lexically
-- and the numeric portions as numbers.
compareVersions :: Markup -> Markup -> Ordering
compareVersions a b = compareVersions' (markupText a) (markupText b)

compareVersions' :: Text.Text -> Text.Text -> Ordering
compareVersions' a b =
    cmp a' b'
    where
      a' :: [Tagged]
      a' = map (tag . Text.strip) (Text.groupBy (\ x y -> isDigit x == isDigit y) a)
      b' :: [Tagged]
      b' = map (tag . Text.strip) (Text.groupBy (\ x y -> isDigit x == isDigit y) b)
      -- Tag the groups
      tag :: Text -> Tagged
      tag t =
          case Text.uncons t of
            Nothing -> Chars Text.empty -- Should not happen
            Just (c, _) | isDigit c -> Digits t
            Just _ -> Chars t
      -- Compare tagged groups
      cmp (Digits x : xs) (Digits y : ys) = case compare (read (Text.unpack x) :: Int) (read (Text.unpack y) :: Int) of EQ -> cmp xs ys; other -> other
      cmp (Chars x : xs) (Chars y : ys) = case compare x y of EQ -> cmp xs ys; other -> other
      cmp (Digits _ : _) (Chars _ : _) = GT
      cmp (Chars _ : _) (Digits _ : _) = LT
      cmp (_ : _) [] = GT
      cmp [] (_ : _) = LT
      cmp _ _ = EQ

reportBrandingLens :: Iso' Branding Text
reportBrandingLens = iso getter setter
  where getter NoLogo = pack ""
        getter (Logo (ImageFile {imageFile = File {fileSource = Just (TheURI sURI)}})) = pack $ sURI
        getter (Logo (ImageFile {imageFile = File {fileSource = Nothing, fileChksum = csum}}))
            | csum == "17e667c2bbe83e098510607571cffc00" =
                pack $ "Thompson & Martinez"
            | csum == "62e7310af0008fa68de56ab9d1b60e8f" =
                pack $ "Thompson & Martinez New"
            | csum == "c3bd1388b41fa5d956e4308ce518a8bd" =
                pack $ "Thompson & Martinez Wide"
            | csum == "cb913fc45e16135fc540a114c25c8a28" =
                pack $ "Goldfield Appraisals"
            | csum == "6ad232e854c6ff80fd2ec11b2d3af21d" =
                pack $ "Thompson Martinez Goldfield"
            | csum == "4ffb5f95b3baf7790a413e768f1fb2b2" =
                pack $ "Goldfield Appraisals 2"
            | csum == "f92d08935f8ba2cee3427b24fb3c263f" =
                pack $ "Goldfield Appraisals 3"
        getter (Logo x) = trace ("Unhandled Logo condition: " ++ show x) (pack $ "Unhandled Logo condition, see DSF")
        setter x =
            case x of
              _ | unpack (Text.strip x) == "Thompson & Martinez" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "17e667c2bbe83e098510607571cffc00", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 348, imageFileHeight = 140, imageFileMaxVal = 255 })
              _ | unpack (Text.strip x) == "Thompson & Martinez New" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "62e7310af0008fa68de56ab9d1b60e8f", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 324, imageFileHeight = 400, imageFileMaxVal = 255 })
              _ | unpack (Text.strip x) == "Thompson & Martinez Wide" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "c3bd1388b41fa5d956e4308ce518a8bd", fileMessages = []}
                                    , imageFileType = PNG, imageFileWidth = 595, imageFileHeight = 114, imageFileMaxVal = 255 })
              _ | unpack (Text.strip x) == "Goldfield Appraisals" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "cb913fc45e16135fc540a114c25c8a28", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 229, imageFileHeight = 90, imageFileMaxVal = 255 })
              _ | unpack (Text.strip x) == "Thompson Martinez Goldfield" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "6ad232e854c6ff80fd2ec11b2d3af21d", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 704, imageFileHeight = 140, imageFileMaxVal = 255 })
              _ | unpack (Text.strip x) == "Goldfield Appraisals 2" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "4ffb5f95b3baf7790a413e768f1fb2b2", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 2250, imageFileHeight = 225, imageFileMaxVal = 255 })
              _ | unpack (Text.strip x) == "Goldfield Appraisals 3" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "f92d08935f8ba2cee3427b24fb3c263f", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 1280, imageFileHeight = 113, imageFileMaxVal = 255 })
              _ -> case reads (unpack x) of
                     [(b,_)] -> b
                     _ -> trace'' ("reportBrandingLens dropping value " ++ unpack x) NoLogo

instance Pretty MarkupID where
    pPrint = text . show . unMarkupID

instance Pretty AuthorID where
    pPrint = text . show . unAuthorID

instance Pretty MarkupPairID where
    pPrint = text . show . unMarkupPairID

instance Pretty AbbrevPairID where
    pPrint = text . show . unAbbrevPairID

instance Pretty ReportElemID where
    pPrint = text . show . unReportElemID

instance SelfPath AbbrevPairID
instance SelfPath AuthorID
instance SelfPath MarkupID
instance SelfPath MarkupPairID
instance SelfPath ReportElemID

instance ToJSON UUID where
  toJSON = String . Text.pack . UUID.toString

instance FromJSON UUID where
  parseJSON = withText "UUID" $ \t ->
         case UUID.fromString (Text.unpack t) of
              Just u -> pure u
              _      -> fail "could not parse UUID"

_testUUID :: IO (Maybe UUID)
_testUUID = UUID.nextRandom >>= return . decode . encode

_testR :: Report
_testR = Builders.empty

#if !__GHCJS__
$(deriveSafeCopy 1 'base ''AuthorID)
$(derivePathInfo ''AuthorID)

$(deriveSafeCopy 1 'base ''Author)
$(deriveSafeCopy 1 'base ''ReportValueTypeInfo)
$(deriveSafeCopy 3 'base ''ReportIntendedUse_3)
$(deriveSafeCopy 4 'extension ''ReportIntendedUse)
$(deriveSafeCopy 1 'base ''ReportValueApproachInfo)
$(deriveSafeCopy 1 'base ''ReportElem_1)
$(deriveSafeCopy 2 'extension ''ReportElem)
$(deriveSafeCopy 17 'base ''Report_17)
$(deriveSafeCopy 18 'extension ''Report_18)
$(deriveSafeCopy 19 'extension ''Report)
$(deriveSafeCopy 1 'base ''ReportStandard)
$(deriveSafeCopy 1 'base ''ReportStatus)
$(deriveSafeCopy 0 'base ''ReportFlags)
$(deriveSafeCopy 1 'base ''Branding)

$(deriveSafeCopy 1 'base ''ReportElemID)
$(deriveSafeCopy 1 'base ''MarkupPairID)
$(deriveSafeCopy 1 'base ''AbbrevPairID)
$(deriveSafeCopy 1 'base ''MarkupID)
$(derivePathInfo ''ReportElemID)
$(derivePathInfo ''MarkupPairID)
$(derivePathInfo ''AbbrevPairID)
$(derivePathInfo ''MarkupID)

-- For debugging and building test suites
$(deriveLiftMany [
   ''Dimension,
   ''ImageCrop,
   ''ImageFile,
   ''ImageSize,
   ''ImageType,
   ''Markup,
   ''Item,
   ''ReportImage,
   ''ReportImageID,
   ''Units
  ])

$(deriveLiftMany [
   ''AbbrevPairID,
   ''Author,
   ''AuthorFieldLabel,
   ''AuthorID,
   ''Branding,
   ''MarkupID,
   ''MarkupPairID,
   ''Report,
   ''ReportElem,
   ''ReportElemID,
   ''ReportElemTypeName,
   ''ReportFieldLabel,
   ''ReportFlags,
   ''ReportIntendedUse,
   ''ReportStandard,
   ''ReportStatus,
   ''ReportValueApproachInfo,
   ''ReportValueTypeInfo])
#endif
