{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, StandaloneDeriving, TypeSynonymInstances, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -fcontext-stack=100 -fno-warn-orphans -fno-warn-missing-signatures -fno-warn-name-shadowing -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -O0 -Wall -fno-warn-orphans #-}
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

import Appraisal.Abbrevs (expandMarkup, runAbbrevsM)
import Appraisal.Config (Paths(reports), reportsURIPath)
import Appraisal.Currency (addCashValues, CashValue, Priceable(..))
import Appraisal.File (File(File, fileSource, fileChksum, fileMessages), FileSource(TheURI))
import Appraisal.ImageFile (ImageFile(ImageFile, imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal), ImageType(..))
import Appraisal.IntJS (deriveOrderJS)
import Appraisal.Markup as M (Markup, mapChars, rawMarkdown, markupText, protectHtml)
import Appraisal.Permissions (Permissions)
import Appraisal.ReportItem (Item(..))
import qualified Appraisal.ReportItem as I (Item(fields), ItemFieldName(ItemDataSheetNumber))
import Appraisal.Unicode (Unicode'(unUnicode'))
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.Debug (trace'')
import Appraisal.Utils.List (spanBy)
import Appraisal.Utils.Text (read)
import Appraisal.Utils.UUID.Internal (UUID'(..))
import qualified Data.UUID.Types as UUID (toString)
import Data.UUID.Types (UUID)
import Data.UUID.Types.Internal (UUID(..))
-- import qualified Data.UUID as UUID (toString)
import Data.Char (isDigit, toLower)
import Data.Function (on)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.Int (Int64)
import qualified Data.IxSet.Revision as R (Ident(..), Revision(..), RevisionInfo(..))
import Control.Lens (Lens', lens)
import Data.List as List (groupBy, sortBy)
import qualified Data.ListLike as LL
import Data.Map as Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(..))
import Data.String (fromString)
import Data.Text as T (Text, groupBy, pack, unpack, strip, uncons, empty)
import Debug.Trace (trace)
import Language.Haskell.TH.Path.Core (lens_mrs, readShowLens)
import Language.Haskell.TH.Path.Graph (SelfPath)
import Language.Haskell.TH.Path.Order as Order (fromList, toList, asList)
import Language.Haskell.TH.Path.View (View(ViewType, viewLens))
import Data.UUID.Orphans ()
import Prelude hiding (read)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Web.Routes.TH (derivePathInfo)

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
    deriving (Read, Show, Eq, Ord, Typeable, Data)

$(deriveOrderJS ''Author)
$(deriveSafeCopy 1 'base ''AuthorID)
$(derivePathInfo ''AuthorID)

data AuthorFieldLabel
    = AuthorName
    | AuthorCredentials
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportValueTypeInfo
    = ReportValueTypeInfo
      { reportValueTypeName :: Markup
      , reportValueTypeDescription :: Markup
      , reportValueTypeDefinition :: Markup
      } deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportIntendedUse_1
    = SalesAdvisory_1
    | EstatePlanning_1
    | EstateTax_1
    | Insurance_1
    | CharitableDonation_1
    deriving (Read, Show, Eq, Ord, Typeable, Data)

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
    deriving (Read, Show, Eq, Ord, Bounded, Enum, Typeable, Data)

instance View ReportIntendedUse where
    type ViewType ReportIntendedUse = String
    viewLens = readShowLens

data ReportValueApproachInfo
    = ReportValueApproachInfo
    { reportValueApproachName :: Markup
    , reportValueApproachDescription :: Markup
    } deriving (Read, Show, Eq, Ord, Typeable, Data)

data ReportElem_0
    = ReportItem_0 Item
    | ReportParagraph_0 Text
    | ReportUndecided_0
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate ReportElem where
    type MigrateFrom ReportElem = ReportElem_0
    migrate (ReportItem_0 x) = ReportItem x
    migrate (ReportParagraph_0 t) = ReportParagraph (protectHtml t)
    migrate ReportUndecided_0 = ReportUndecided

data ReportElem
    = ReportItem {elemItem :: Item}
    | ReportParagraph {elemText :: Markup}
    | ReportUndecided
    deriving (Read, Show, Eq, Ord, Typeable, Data)

deriving instance Show (R.Revision R.Ident)
deriving instance Show (R.RevisionInfo R.Ident)

instance Migrate ReportIntendedUse_2 where
    type MigrateFrom ReportIntendedUse_2 = ReportIntendedUse_1
    migrate r = case r of
      SalesAdvisory_1 -> SalesAdvisory_2
      EstatePlanning_1 -> EstatePlanning_2
      EstateTax_1 -> EstateTax_2
      Insurance_1 -> Insurance_2
      CharitableDonation_1 -> CharitableDonation_2

instance Migrate ReportIntendedUse where
    type MigrateFrom ReportIntendedUse = ReportIntendedUse_2
    migrate r = case r of
      SalesAdvisory_2 -> SalesAdvisory
      EstatePlanning_2 -> EstatePlanning
      EstateTax_2 -> EstateTax
      Insurance_2 -> InsuranceCoverage
      CharitableDonation_2 -> CharitableDonation
      EquitableDistribution_2 -> EquitableDistribution
      MaritalDissolution_2 -> MaritalDissolution
      EstateDivision_2 -> EstateDivision

data ReportStatus
    = Draft
    -- ^ This is the current authoritative version of the report
    | Final
    -- ^ The report has been downloaded, and perhaps uploaded to a
    -- different server.
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- Does ReportStatus belong in ReportFlags?  Seems more like meta info.
data ReportFlags = ReportFlags {
  hideEmptyItemFields :: Bool
  } deriving (Read, Show, Eq, Ord, Typeable, Data)

data Branding
    = NoLogo
    | Logo ImageFile
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data Report_9
    = Report_9
             { reportFolder_9 :: FilePath
             , reportName_9 :: Markup
             , reportDate_9 :: Markup
             , reportContractDate_9 :: Markup
             , reportInspectionDate_9 :: Markup
             , reportEffectiveDate_9 :: Markup
             , reportAuthors_9 :: [Author]
             , reportPreparer_9 :: Markup
             , reportPreparerEIN_9 :: Markup
             , reportPreparerAddress_9 :: Markup
             , reportPreparerEMail_9 :: Unicode'
             , reportPreparerWebsite_9 :: Unicode'
             , reportAbbrevs_9 :: [(CIString, Markup)]
             , reportTitle_9 :: Markup
             , reportIntendedUse_9 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_9 :: ReportValueTypeInfo
             , reportValueApproachInfo_9 :: ReportValueApproachInfo
             , reportClientName_9 :: Markup
             , reportClientAddress_9 :: Markup
             , reportClientGreeting_9 :: Markup
             , reportItemsOwnerFull_9 :: Markup
             , reportItemsOwner_9 :: Markup
             , reportBriefItems_9 :: Markup
             , reportInspectionLocation_9 :: Markup
             , reportBody_9 :: [ReportElem]
             , reportGlossary_9 :: [(Markup, Markup)]
             , reportSources_9 :: [(Markup, Markup)]
             , reportLetterOfTransmittal_9 :: Markup
             , reportScopeOfWork_9 :: Markup
             , reportCertification_9 :: [Markup]
             , reportLimitingConditions_9 :: [Markup]
             , reportPrivacyPolicy_9 :: Markup
             , reportPerms_9 :: Permissions
             , revisionInfo_9 :: R.RevisionInfo R.Ident
             , reportBranding_9 :: Branding
             , reportStatus_9 :: ReportStatus
             , reportFlags_9 :: ReportFlags
             , reportUUID_9 :: UUID'
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

data Report_10
    = Report_10
             { reportFolder_10 :: FilePath
             , reportName_10 :: Markup
             , reportDate_10 :: Markup
             , reportContractDate_10 :: Markup
             , reportInspectionDate_10 :: Markup
             , reportEffectiveDate_10 :: Markup
             , reportAuthors_10 :: [Author]
             , reportPreparer_10 :: Markup
             , reportPreparerEIN_10 :: Markup
             , reportPreparerAddress_10 :: Markup
             , reportPreparerEMail_10 :: Unicode'
             , reportPreparerWebsite_10 :: Unicode'
             , reportAbbrevs_10 :: [(CIString, Markup)]
             , reportTitle_10 :: Markup
             , reportIntendedUse_10 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_10 :: ReportValueTypeInfo
             , reportValueApproachInfo_10 :: ReportValueApproachInfo
             , reportClientName_10 :: Markup
             , reportClientAddress_10 :: Markup
             , reportClientGreeting_10 :: Markup
             , reportItemsOwnerFull_10 :: Markup
             , reportItemsOwner_10 :: Markup
             , reportBriefItems_10 :: Markup
             , reportInspectionLocation_10 :: Markup
             , reportBody_10 :: [ReportElem]
             , reportGlossary_10 :: [(Markup, Markup)]
             , reportSources_10 :: [(Markup, Markup)]
             , reportLetterOfTransmittal_10 :: Markup
             , reportScopeOfWork_10 :: Markup
             , reportCertification_10 :: [Markup]
             , reportLimitingConditions_10 :: [Markup]
             , reportPrivacyPolicy_10 :: Markup
             , reportPerms_10 :: Permissions
             , revisionInfo_10 :: R.RevisionInfo R.Ident
             , reportBranding_10 :: Branding
             , reportStatus_10 :: ReportStatus
             , reportRedacted_10 :: Bool
             , reportFlags_10 :: ReportFlags
             , reportUUID_10 :: UUID'
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_10 where
    type MigrateFrom Report_10 = Report_9
    migrate r =
        Report_10
               { reportFolder_10 = reportFolder_9 r
               , reportName_10 = reportName_9 r
               , reportDate_10 = reportDate_9 r
               , reportContractDate_10 = reportContractDate_9 r
               , reportInspectionDate_10 = reportInspectionDate_9 r
               , reportEffectiveDate_10 = reportEffectiveDate_9 r
               , reportAuthors_10 = reportAuthors_9 r
               , reportPreparer_10 = reportPreparer_9 r
               , reportPreparerEIN_10 = reportPreparerEIN_9 r
               , reportPreparerAddress_10 = reportPreparerAddress_9 r
               , reportPreparerEMail_10 = reportPreparerEMail_9 r
               , reportPreparerWebsite_10 = reportPreparerWebsite_9 r
               , reportAbbrevs_10 = reportAbbrevs_9 r
               , reportTitle_10 = reportTitle_9 r
               , reportIntendedUse_10 = reportIntendedUse_9 r
               , reportValueTypeInfo_10 = reportValueTypeInfo_9 r
               , reportValueApproachInfo_10 = reportValueApproachInfo_9 r
               , reportClientName_10 = reportClientName_9 r
               , reportClientAddress_10 = reportClientAddress_9 r
               , reportClientGreeting_10 = reportClientGreeting_9 r
               , reportItemsOwnerFull_10 = reportItemsOwnerFull_9 r
               , reportItemsOwner_10 = reportItemsOwner_9 r
               , reportBriefItems_10 = reportBriefItems_9 r
               , reportInspectionLocation_10 = reportInspectionLocation_9 r
               , reportBody_10 = reportBody_9 r
               , reportGlossary_10 = reportGlossary_9 r
               , reportSources_10 = reportSources_9 r
               , reportLetterOfTransmittal_10 = reportLetterOfTransmittal_9 r
               , reportScopeOfWork_10 = reportScopeOfWork_9 r
               , reportCertification_10 = reportCertification_9 r
               , reportLimitingConditions_10 = reportLimitingConditions_9 r
               , reportPrivacyPolicy_10 = reportPrivacyPolicy_9 r
               , reportPerms_10 = reportPerms_9 r
               , revisionInfo_10 = revisionInfo_9 r
               , reportBranding_10 = reportBranding_9 r
               , reportStatus_10 = reportStatus_9 r
               , reportRedacted_10 = False
               , reportFlags_10 = reportFlags_9 r
               , reportUUID_10 = reportUUID_9 r }

data Report_11
    = Report_11
             { reportFolder_11 :: FilePath
             , reportName_11 :: Markup
             , reportDate_11 :: Markup
             , reportContractDate_11 :: Markup
             , reportInspectionDate_11 :: Markup
             , reportEffectiveDate_11 :: Markup
             , reportAuthors_11 :: [Author]
             , reportPreparer_11 :: Markup
             , reportPreparerEIN_11 :: Markup
             , reportPreparerAddress_11 :: Markup
             , reportPreparerEMail_11 :: Unicode'
             , reportPreparerWebsite_11 :: Unicode'
             , reportAbbrevs_11 :: [(CIString, Markup)]
             , reportTitle_11 :: Markup
             , reportIntendedUse_11 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_11 :: ReportValueTypeInfo
             , reportValueApproachInfo_11 :: ReportValueApproachInfo
             , reportClientName_11 :: Markup
             , reportClientAddress_11 :: Markup
             , reportClientGreeting_11 :: Markup
             , reportItemsOwnerFull_11 :: Markup
             , reportItemsOwner_11 :: Markup
             , reportBriefItems_11 :: Markup
             , reportInspectionLocation_11 :: Markup
             , reportBody_11 :: [ReportElem]
             , reportGlossary_11 :: [(Markup, Markup)]
             , reportSources_11 :: [(Markup, Markup)]
             , reportLetterOfTransmittal_11 :: Markup
             , reportScopeOfWork_11 :: Markup
             , reportCertification_11 :: [Markup]
             , reportLimitingConditions_11 :: [Markup]
             , reportPrivacyPolicy_11 :: Markup
             , reportPerms_11 :: Permissions
             , revisionInfo_11 :: R.RevisionInfo R.Ident
             , reportBranding_11 :: Branding
             , reportStatus_11 :: ReportStatus
             , reportRedacted_11 :: Bool
             , reportFlags_11 :: ReportFlags
             , reportUUID_11 :: UUID'
             , reportOrderByItemName_11 :: Bool
             , reportDisplayItemName_11 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_11 where
    type MigrateFrom Report_11 = Report_10
    migrate r =
        Report_11
               { reportFolder_11 = reportFolder_10 r
               , reportName_11 = reportName_10 r
               , reportDate_11 = reportDate_10 r
               , reportContractDate_11 = reportContractDate_10 r
               , reportInspectionDate_11 = reportInspectionDate_10 r
               , reportEffectiveDate_11 = reportEffectiveDate_10 r
               , reportAuthors_11 = reportAuthors_10 r
               , reportPreparer_11 = reportPreparer_10 r
               , reportPreparerEIN_11 = reportPreparerEIN_10 r
               , reportPreparerAddress_11 = reportPreparerAddress_10 r
               , reportPreparerEMail_11 = reportPreparerEMail_10 r
               , reportPreparerWebsite_11 = reportPreparerWebsite_10 r
               , reportAbbrevs_11 = reportAbbrevs_10 r
               , reportTitle_11 = reportTitle_10 r
               , reportIntendedUse_11 = reportIntendedUse_10 r
               , reportValueTypeInfo_11 = reportValueTypeInfo_10 r
               , reportValueApproachInfo_11 = reportValueApproachInfo_10 r
               , reportClientName_11 = reportClientName_10 r
               , reportClientAddress_11 = reportClientAddress_10 r
               , reportClientGreeting_11 = reportClientGreeting_10 r
               , reportItemsOwnerFull_11 = reportItemsOwnerFull_10 r
               , reportItemsOwner_11 = reportItemsOwner_10 r
               , reportBriefItems_11 = reportBriefItems_10 r
               , reportInspectionLocation_11 = reportInspectionLocation_10 r
               , reportBody_11 = reportBody_10 r
               , reportGlossary_11 = reportGlossary_10 r
               , reportSources_11 = reportSources_10 r
               , reportLetterOfTransmittal_11 = reportLetterOfTransmittal_10 r
               , reportScopeOfWork_11 = reportScopeOfWork_10 r
               , reportCertification_11 = reportCertification_10 r
               , reportLimitingConditions_11 = reportLimitingConditions_10 r
               , reportPrivacyPolicy_11 = reportPrivacyPolicy_10 r
               , reportPerms_11 = reportPerms_10 r
               , revisionInfo_11 = revisionInfo_10 r
               , reportBranding_11 = reportBranding_10 r
               , reportStatus_11 = reportStatus_10 r
               , reportRedacted_11 = reportRedacted_10 r
               , reportFlags_11 = reportFlags_10 r
               , reportUUID_11 = reportUUID_10 r
               , reportOrderByItemName_11 = True
               , reportDisplayItemName_11 = True }

type EpochMilli = Int64

data Report_12
    = Report_12
             { reportFolder_12 :: FilePath
             , reportName_12 :: Markup
             , reportDate_12 :: Markup
             , reportContractDate_12 :: Markup
             , reportInspectionDate_12 :: Markup
             , reportEffectiveDate_12 :: Markup
             , reportAuthors_12 :: [Author]
             , reportPreparer_12 :: Markup
             , reportPreparerEIN_12 :: Markup
             , reportPreparerAddress_12 :: Markup
             , reportPreparerEMail_12 :: Unicode'
             , reportPreparerWebsite_12 :: Unicode'
             , reportAbbrevs_12 :: [(CIString, Markup)]
             , reportTitle_12 :: Markup
             , reportIntendedUse_12 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_12 :: ReportValueTypeInfo
             , reportValueApproachInfo_12 :: ReportValueApproachInfo
             , reportClientName_12 :: Markup
             , reportClientAddress_12 :: Markup
             , reportClientGreeting_12 :: Markup
             , reportItemsOwnerFull_12 :: Markup
             , reportItemsOwner_12 :: Markup
             , reportBriefItems_12 :: Markup
             , reportInspectionLocation_12 :: Markup
             , reportBody_12 :: [ReportElem]
             , reportGlossary_12 :: [(Markup, Markup)]
             , reportSources_12 :: [(Markup, Markup)]
             , reportLetterOfTransmittal_12 :: Markup
             , reportScopeOfWork_12 :: Markup
             , reportCertification_12 :: [Markup]
             , reportLimitingConditions_12 :: [Markup]
             , reportPrivacyPolicy_12 :: Markup
             , reportPerms_12 :: Permissions
             , reportRevision_12 :: Integer
             , reportCreated_12 :: EpochMilli
             , reportBranding_12 :: Branding
             , reportStatus_12 :: ReportStatus
             , reportRedacted_12 :: Bool
             , reportFlags_12 :: ReportFlags
             , reportUUID_12 :: UUID'
             , reportOrderByItemName_12 :: Bool
             , reportDisplayItemName_12 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_12 where
    type MigrateFrom Report_12 = Report_11
    migrate r =
        Report_12
               { reportFolder_12 = reportFolder_11 r
               , reportName_12 = reportName_11 r
               , reportDate_12 = reportDate_11 r
               , reportContractDate_12 = reportContractDate_11 r
               , reportInspectionDate_12 = reportInspectionDate_11 r
               , reportEffectiveDate_12 = reportEffectiveDate_11 r
               , reportAuthors_12 = reportAuthors_11 r
               , reportPreparer_12 = reportPreparer_11 r
               , reportPreparerEIN_12 = reportPreparerEIN_11 r
               , reportPreparerAddress_12 = reportPreparerAddress_11 r
               , reportPreparerEMail_12 = reportPreparerEMail_11 r
               , reportPreparerWebsite_12 = reportPreparerWebsite_11 r
               , reportAbbrevs_12 = reportAbbrevs_11 r
               , reportTitle_12 = reportTitle_11 r
               , reportIntendedUse_12 = reportIntendedUse_11 r
               , reportValueTypeInfo_12 = reportValueTypeInfo_11 r
               , reportValueApproachInfo_12 = reportValueApproachInfo_11 r
               , reportClientName_12 = reportClientName_11 r
               , reportClientAddress_12 = reportClientAddress_11 r
               , reportClientGreeting_12 = reportClientGreeting_11 r
               , reportItemsOwnerFull_12 = reportItemsOwnerFull_11 r
               , reportItemsOwner_12 = reportItemsOwner_11 r
               , reportBriefItems_12 = reportBriefItems_11 r
               , reportInspectionLocation_12 = reportInspectionLocation_11 r
               , reportBody_12 = reportBody_11 r
               , reportGlossary_12 = reportGlossary_11 r
               , reportSources_12 = reportSources_11 r
               , reportLetterOfTransmittal_12 = reportLetterOfTransmittal_11 r
               , reportScopeOfWork_12 = reportScopeOfWork_11 r
               , reportCertification_12 = reportCertification_11 r
               , reportLimitingConditions_12 = reportLimitingConditions_11 r
               , reportPrivacyPolicy_12 = reportPrivacyPolicy_11 r
               , reportPerms_12 = reportPerms_11 r
               -- We still need to eliminate the previous migrations
               -- before we can eliminate the dependency on the revision package.
               , reportRevision_12 = R.number $ R.revision $ revisionInfo_11 r
               , reportCreated_12 = R.created $ revisionInfo_11 r
               , reportBranding_12 = reportBranding_11 r
               , reportStatus_12 = reportStatus_11 r
               , reportRedacted_12 = reportRedacted_11 r
               , reportFlags_12 = reportFlags_11 r
               , reportUUID_12 = reportUUID_11 r
               , reportOrderByItemName_12 = reportOrderByItemName_11 r
               , reportDisplayItemName_12 = reportDisplayItemName_11 r }

data Report_13
    = Report_13
             { reportFolder_13 :: FilePath
             , reportName_13 :: Markup
             , reportDate_13 :: Markup
             , reportContractDate_13 :: Markup
             , reportInspectionDate_13 :: Markup
             , reportEffectiveDate_13 :: Markup
             , reportAuthors_13 :: [Author]
             , reportPreparer_13 :: Markup
             , reportPreparerEIN_13 :: Markup
             , reportPreparerAddress_13 :: Markup
             , reportPreparerEMail_13 :: Markup
             , reportPreparerWebsite_13 :: Markup
             , reportAbbrevs_13 :: [(CIString, Markup)]
             , reportTitle_13 :: Markup
             , reportIntendedUse_13 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_13 :: ReportValueTypeInfo
             , reportValueApproachInfo_13 :: ReportValueApproachInfo
             , reportClientName_13 :: Markup
             , reportClientAddress_13 :: Markup
             , reportClientGreeting_13 :: Markup
             , reportItemsOwnerFull_13 :: Markup
             , reportItemsOwner_13 :: Markup
             , reportBriefItems_13 :: Markup
             , reportInspectionLocation_13 :: Markup
             , reportBody_13 :: [ReportElem]
             , reportGlossary_13 :: [(Markup, Markup)]
             , reportSources_13 :: [(Markup, Markup)]
             , reportLetterOfTransmittal_13 :: Markup
             , reportScopeOfWork_13 :: Markup
             , reportCertification_13 :: [Markup]
             , reportLimitingConditions_13 :: [Markup]
             , reportPrivacyPolicy_13 :: Markup
             , reportPerms_13 :: Permissions
             , reportRevision_13 :: Integer
             , reportCreated_13 :: EpochMilli
             , reportBranding_13 :: Branding
             , reportStatus_13 :: ReportStatus
             , reportRedacted_13 :: Bool
             , reportFlags_13 :: ReportFlags
             , reportUUID_13 :: UUID'
             , reportOrderByItemName_13 :: Bool
             , reportDisplayItemName_13 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_13 where
    type MigrateFrom Report_13 = Report_12
    migrate r =
        Report_13
               { reportFolder_13 = reportFolder_12 r
               , reportName_13 = reportName_12 r
               , reportDate_13 = reportDate_12 r
               , reportContractDate_13 = reportContractDate_12 r
               , reportInspectionDate_13 = reportInspectionDate_12 r
               , reportEffectiveDate_13 = reportEffectiveDate_12 r
               , reportAuthors_13 = reportAuthors_12 r
               , reportPreparer_13 = reportPreparer_12 r
               , reportPreparerEIN_13 = reportPreparerEIN_12 r
               , reportPreparerAddress_13 = reportPreparerAddress_12 r
               , reportPreparerEMail_13 = protectHtml (unUnicode' (reportPreparerEMail_12 r))
               , reportPreparerWebsite_13 = protectHtml (unUnicode' (reportPreparerWebsite_12 r))
               , reportAbbrevs_13 = reportAbbrevs_12 r
               , reportTitle_13 = reportTitle_12 r
               , reportIntendedUse_13 = reportIntendedUse_12 r
               , reportValueTypeInfo_13 = reportValueTypeInfo_12 r
               , reportValueApproachInfo_13 = reportValueApproachInfo_12 r
               , reportClientName_13 = reportClientName_12 r
               , reportClientAddress_13 = reportClientAddress_12 r
               , reportClientGreeting_13 = reportClientGreeting_12 r
               , reportItemsOwnerFull_13 = reportItemsOwnerFull_12 r
               , reportItemsOwner_13 = reportItemsOwner_12 r
               , reportBriefItems_13 = reportBriefItems_12 r
               , reportInspectionLocation_13 = reportInspectionLocation_12 r
               , reportBody_13 = reportBody_12 r
               , reportGlossary_13 = reportGlossary_12 r
               , reportSources_13 = reportSources_12 r
               , reportLetterOfTransmittal_13 = reportLetterOfTransmittal_12 r
               , reportScopeOfWork_13 = reportScopeOfWork_12 r
               , reportCertification_13 = reportCertification_12 r
               , reportLimitingConditions_13 = reportLimitingConditions_12 r
               , reportPrivacyPolicy_13 = reportPrivacyPolicy_12 r
               , reportPerms_13 = reportPerms_12 r
               , reportRevision_13 = reportRevision_12 r
               , reportCreated_13 = reportCreated_12 r
               , reportBranding_13 = reportBranding_12 r
               , reportStatus_13 = reportStatus_12 r
               , reportRedacted_13 = reportRedacted_12 r
               , reportFlags_13 = reportFlags_12 r
               , reportUUID_13 = reportUUID_12 r
               , reportOrderByItemName_13 = reportOrderByItemName_12 r
               , reportDisplayItemName_13 = reportDisplayItemName_12 r
               }

data Report_14
    = Report_14
             { reportFolder_14 :: FilePath
             , reportName_14 :: Markup
             , reportDate_14 :: Markup
             , reportContractDate_14 :: Markup
             , reportInspectionDate_14 :: Markup
             , reportEffectiveDate_14 :: Markup
             , reportAuthors_14 :: [Author]
             , reportPreparer_14 :: Markup
             , reportPreparerEIN_14 :: Markup
             , reportPreparerAddress_14 :: Markup
             , reportPreparerEMail_14 :: Markup
             , reportPreparerWebsite_14 :: Markup
             , reportAbbrevs_14 :: [(CIString, Markup)]
             , reportTitle_14 :: Markup
             , reportHeader_14 :: Markup
             , reportFooter_14 :: Markup
             , reportIntendedUse_14 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_14 :: ReportValueTypeInfo
             , reportValueApproachInfo_14 :: ReportValueApproachInfo
             , reportClientName_14 :: Markup
             , reportClientAddress_14 :: Markup
             , reportClientGreeting_14 :: Markup
             , reportItemsOwnerFull_14 :: Markup
             , reportItemsOwner_14 :: Markup
             , reportBriefItems_14 :: Markup
             , reportInspectionLocation_14 :: Markup
             , reportBody_14 :: [ReportElem]
             , reportGlossary_14 :: [(Markup, Markup)]
             , reportSources_14 :: [(Markup, Markup)]
             , reportLetterOfTransmittal_14 :: Markup
             , reportScopeOfWork_14 :: Markup
             , reportCertification_14 :: [Markup]
             , reportLimitingConditions_14 :: [Markup]
             , reportPrivacyPolicy_14 :: Markup
             , reportPerms_14 :: Permissions
             , reportRevision_14 :: Integer
             , reportCreated_14 :: EpochMilli
             , reportBranding_14 :: Branding
             , reportStatus_14 :: ReportStatus
             , reportRedacted_14 :: Bool
             , reportFlags_14 :: ReportFlags
             , reportUUID_14 :: UUID'
             , reportOrderByItemName_14 :: Bool
             , reportDisplayItemName_14 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_14 where
    type MigrateFrom Report_14 = Report_13
    migrate r =
        Report_14
               { reportFolder_14 = reportFolder_13 r
               , reportName_14 = reportName_13 r
               , reportDate_14 = reportDate_13 r
               , reportContractDate_14 = reportContractDate_13 r
               , reportInspectionDate_14 = reportInspectionDate_13 r
               , reportEffectiveDate_14 = reportEffectiveDate_13 r
               , reportAuthors_14 = reportAuthors_13 r
               , reportPreparer_14 = reportPreparer_13 r
               , reportPreparerEIN_14 = reportPreparerEIN_13 r
               , reportPreparerAddress_14 = reportPreparerAddress_13 r
               , reportPreparerEMail_14 = reportPreparerEMail_13 r
               , reportPreparerWebsite_14 = reportPreparerWebsite_13 r
               , reportAbbrevs_14 = reportAbbrevs_13 r
               , reportTitle_14 = reportTitle_13 r
               , reportHeader_14 = mempty
               , reportFooter_14 = mempty
               , reportIntendedUse_14 = reportIntendedUse_13 r
               , reportValueTypeInfo_14 = reportValueTypeInfo_13 r
               , reportValueApproachInfo_14 = reportValueApproachInfo_13 r
               , reportClientName_14 = reportClientName_13 r
               , reportClientAddress_14 = reportClientAddress_13 r
               , reportClientGreeting_14 = reportClientGreeting_13 r
               , reportItemsOwnerFull_14 = reportItemsOwnerFull_13 r
               , reportItemsOwner_14 = reportItemsOwner_13 r
               , reportBriefItems_14 = reportBriefItems_13 r
               , reportInspectionLocation_14 = reportInspectionLocation_13 r
               , reportBody_14 = reportBody_13 r
               , reportGlossary_14 = reportGlossary_13 r
               , reportSources_14 = reportSources_13 r
               , reportLetterOfTransmittal_14 = reportLetterOfTransmittal_13 r
               , reportScopeOfWork_14 = reportScopeOfWork_13 r
               , reportCertification_14 = reportCertification_13 r
               , reportLimitingConditions_14 = reportLimitingConditions_13 r
               , reportPrivacyPolicy_14 = reportPrivacyPolicy_13 r
               , reportPerms_14 = reportPerms_13 r
               , reportRevision_14 = reportRevision_13 r
               , reportCreated_14 = reportCreated_13 r
               , reportBranding_14 = reportBranding_13 r
               , reportStatus_14 = reportStatus_13 r
               , reportRedacted_14 = reportRedacted_13 r
               , reportFlags_14 = reportFlags_13 r
               , reportUUID_14 = reportUUID_13 r
               , reportOrderByItemName_14 = reportOrderByItemName_13 r
               , reportDisplayItemName_14 = reportDisplayItemName_13 r
               }

type MarkupPair = (Markup, Markup)
type AbbrevPair = (CIString, Markup)

$(deriveOrderJS ''ReportElem)
$(deriveOrderJS ''MarkupPair)
$(deriveOrderJS ''AbbrevPair)
$(deriveOrderJS ''Markup)

data Report_15
    = Report_15
             { reportFolder_15 :: FilePath
             , reportName_15 :: Markup
             , reportDate_15 :: Markup
             , reportContractDate_15 :: Markup
             , reportInspectionDate_15 :: Markup
             , reportEffectiveDate_15 :: Markup
             , reportAuthors_15 :: Authors
             , reportPreparer_15 :: Markup
             , reportPreparerEIN_15 :: Markup
             , reportPreparerAddress_15 :: Markup
             , reportPreparerEMail_15 :: Markup
             , reportPreparerWebsite_15 :: Markup
             , reportAbbrevs_15 :: AbbrevPairs
             , reportTitle_15 :: Markup
             , reportHeader_15 :: Markup
             , reportFooter_15 :: Markup
             , reportIntendedUse_15 :: Maybe ReportIntendedUse
             , reportValueTypeInfo_15 :: ReportValueTypeInfo
             , reportValueApproachInfo_15 :: ReportValueApproachInfo
             , reportClientName_15 :: Markup
             , reportClientAddress_15 :: Markup
             , reportClientGreeting_15 :: Markup
             , reportItemsOwnerFull_15 :: Markup
             , reportItemsOwner_15 :: Markup
             , reportBriefItems_15 :: Markup
             , reportInspectionLocation_15 :: Markup
             , reportBody_15 :: ReportElems
             , reportGlossary_15 :: MarkupPairs
             , reportSources_15 :: MarkupPairs
             , reportLetterOfTransmittal_15 :: Markup
             , reportScopeOfWork_15 :: Markup
             , reportCertification_15 :: Markups
             , reportLimitingConditions_15 :: Markups
             , reportPrivacyPolicy_15 :: Markup
             , reportPerms_15 :: Permissions
             , reportRevision_15 :: Integer
             , reportCreated_15 :: EpochMilli
             , reportBranding_15 :: Branding
             , reportStatus_15 :: ReportStatus
             , reportRedacted_15 :: Bool
             , reportFlags_15 :: ReportFlags
             , reportUUID_15 :: UUID'
             , reportOrderByItemName_15 :: Bool
             , reportDisplayItemName_15 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_15 where
    type MigrateFrom Report_15 = Report_14
    migrate r =
        Report_15
               { reportFolder_15 = reportFolder_14 r
               , reportName_15 = reportName_14 r
               , reportDate_15 = reportDate_14 r
               , reportContractDate_15 = reportContractDate_14 r
               , reportInspectionDate_15 = reportInspectionDate_14 r
               , reportEffectiveDate_15 = reportEffectiveDate_14 r
               , reportAuthors_15 = Order.fromList $ reportAuthors_14 r
               , reportPreparer_15 = reportPreparer_14 r
               , reportPreparerEIN_15 = reportPreparerEIN_14 r
               , reportPreparerAddress_15 = reportPreparerAddress_14 r
               , reportPreparerEMail_15 = reportPreparerEMail_14 r
               , reportPreparerWebsite_15 = reportPreparerWebsite_14 r
               , reportAbbrevs_15 = Order.fromList $ reportAbbrevs_14 r
               , reportTitle_15 = reportTitle_14 r
               , reportHeader_15 = reportHeader_14 r
               , reportFooter_15 = reportFooter_14 r
               , reportIntendedUse_15 = reportIntendedUse_14 r
               , reportValueTypeInfo_15 = reportValueTypeInfo_14 r
               , reportValueApproachInfo_15 = reportValueApproachInfo_14 r
               , reportClientName_15 = reportClientName_14 r
               , reportClientAddress_15 = reportClientAddress_14 r
               , reportClientGreeting_15 = reportClientGreeting_14 r
               , reportItemsOwnerFull_15 = reportItemsOwnerFull_14 r
               , reportItemsOwner_15 = reportItemsOwner_14 r
               , reportBriefItems_15 = reportBriefItems_14 r
               , reportInspectionLocation_15 = reportInspectionLocation_14 r
               , reportBody_15 = Order.fromList $ reportBody_14 r
               , reportGlossary_15 = Order.fromList $ reportGlossary_14 r
               , reportSources_15 = Order.fromList $ reportSources_14 r
               , reportLetterOfTransmittal_15 = reportLetterOfTransmittal_14 r
               , reportScopeOfWork_15 = reportScopeOfWork_14 r
               , reportCertification_15 = Order.fromList $ reportCertification_14 r
               , reportLimitingConditions_15 = Order.fromList $ reportLimitingConditions_14 r
               , reportPrivacyPolicy_15 = reportPrivacyPolicy_14 r
               , reportPerms_15 = reportPerms_14 r
               , reportRevision_15 = reportRevision_14 r
               , reportCreated_15 = reportCreated_14 r
               , reportBranding_15 = reportBranding_14 r
               , reportStatus_15 = reportStatus_14 r
               , reportRedacted_15 = reportRedacted_14 r
               , reportFlags_15 = reportFlags_14 r
               , reportUUID_15 = reportUUID_14 r
               , reportOrderByItemName_15 = reportOrderByItemName_14 r
               , reportDisplayItemName_15 = reportDisplayItemName_14 r
               }

type MaybeReportIntendedUse = Maybe ReportIntendedUse

instance View MaybeReportIntendedUse where
    type ViewType MaybeReportIntendedUse = String
    viewLens = lens_mrs

data Report_16
    = Report_16
             { reportFolder_16 :: FilePath
             , reportName_16 :: Markup
             , reportDate_16 :: Markup
             , reportContractDate_16 :: Markup
             , reportInspectionDate_16 :: Markup
             , reportEffectiveDate_16 :: Markup
             , reportAuthors_16 :: Authors
             , reportPreparer_16 :: Markup
             , reportPreparerEIN_16 :: Markup
             , reportPreparerAddress_16 :: Markup
             , reportPreparerEMail_16 :: Markup
             , reportPreparerWebsite_16 :: Markup
             , reportAbbrevs_16 :: AbbrevPairs
             , reportTitle_16 :: Markup
             , reportHeader_16 :: Markup
             , reportFooter_16 :: Markup
             , reportIntendedUse_16 :: MaybeReportIntendedUse
             , reportValueTypeInfo_16 :: ReportValueTypeInfo
             , reportValueApproachInfo_16 :: ReportValueApproachInfo
             , reportClientName_16 :: Markup
             , reportClientAddress_16 :: Markup
             , reportClientGreeting_16 :: Markup
             , reportItemsOwnerFull_16 :: Markup
             , reportItemsOwner_16 :: Markup
             , reportBriefItems_16 :: Markup
             , reportInspectionLocation_16 :: Markup
             , reportBody_16 :: ReportElems
             , reportGlossary_16 :: MarkupPairs
             , reportSources_16 :: MarkupPairs
             , reportLetterOfTransmittal_16 :: Markup
             , reportScopeOfWork_16 :: Markup
             , reportCertification_16 :: Markups
             , reportLimitingConditions_16 :: Markups
             , reportPrivacyPolicy_16 :: Markup
             , reportPerms_16 :: Permissions
             , reportRevision_16 :: Integer
             , reportCreated_16 :: EpochMilli
             , reportBranding_16 :: Branding
             , reportStatus_16 :: ReportStatus
             , reportRedacted_16 :: Bool
             , reportFlags_16 :: ReportFlags
             , reportUUID_16 :: UUID'
             , reportOrderByItemName_16 :: Bool
             , reportDisplayItemName_16 :: Bool
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report_16 where
    type MigrateFrom Report_16 = Report_15
    migrate r =
        Report_16
               { reportFolder_16 = reportFolder_15 r
               , reportName_16 = reportName_15 r
               , reportDate_16 = reportDate_15 r
               , reportContractDate_16 = reportContractDate_15 r
               , reportInspectionDate_16 = reportInspectionDate_15 r
               , reportEffectiveDate_16 = reportEffectiveDate_15 r
               , reportAuthors_16 = reportAuthors_15 r
               , reportPreparer_16 = reportPreparer_15 r
               , reportPreparerEIN_16 = reportPreparerEIN_15 r
               , reportPreparerAddress_16 = reportPreparerAddress_15 r
               , reportPreparerEMail_16 = reportPreparerEMail_15 r
               , reportPreparerWebsite_16 = reportPreparerWebsite_15 r
               -- I would remove the "ValueApproachExplanation" abbreviation but for two things.  First,
               -- it could conceivably be in use someplace besides reportValueApproachDescription.
               -- Second, it is not likely to cause any harm in the future.
               , reportAbbrevs_16 = {- filter (/= (pack "ValueApproachExplanation") . fst) $ -} reportAbbrevs_15 r
               , reportTitle_16 = reportTitle_15 r
               , reportHeader_16 = reportHeader_15 r
               , reportFooter_16 = reportFooter_15 r
               , reportIntendedUse_16 = reportIntendedUse_15 r
               , reportValueTypeInfo_16 = reportValueTypeInfo_15 r
               , reportValueApproachInfo_16 =
                   (reportValueApproachInfo_15 r)
                   { reportValueApproachDescription =
                         -- This migration moves the boilerplate for the Approach to Value
                         -- description text out of the appraisalscript code and into the report.
                         approachesToValueOldDefault <>
                         ( -- Expand the customized value of ValueApproachExplanation in the
                           -- input field.  This will end up being a close duplicate of the hard
                           -- coded text we are entering above, but that was already the case.
                           runAbbrevsM (Map.fromList (filter ((== (fromString "ValueApproachExplanation")) . fst) (Order.toList (reportAbbrevs_15 r))))
                                       (expandMarkup return (reportValueApproachDescription (reportValueApproachInfo_15 r)))) }
               , reportClientName_16 = reportClientName_15 r
               , reportClientAddress_16 = reportClientAddress_15 r
               , reportClientGreeting_16 = reportClientGreeting_15 r
               , reportItemsOwnerFull_16 = reportItemsOwnerFull_15 r
               , reportItemsOwner_16 = reportItemsOwner_15 r
               , reportBriefItems_16 = reportBriefItems_15 r
               , reportInspectionLocation_16 = reportInspectionLocation_15 r
               , reportBody_16 = reportBody_15 r
               , reportGlossary_16 = reportGlossary_15 r
               , reportSources_16 = reportSources_15 r
               , reportLetterOfTransmittal_16 = reportLetterOfTransmittal_15 r
               , reportScopeOfWork_16 = reportScopeOfWork_15 r
               , reportCertification_16 = reportCertification_15 r
               , reportLimitingConditions_16 = reportLimitingConditions_15 r
               , reportPrivacyPolicy_16 = reportPrivacyPolicy_15 r
               , reportPerms_16 = reportPerms_15 r
               , reportRevision_16 = reportRevision_15 r
               , reportCreated_16 = reportCreated_15 r
               , reportBranding_16 = reportBranding_15 r
               , reportStatus_16 = reportStatus_15 r
               , reportRedacted_16 = reportRedacted_15 r
               , reportFlags_16 = reportFlags_15 r
               , reportUUID_16 = reportUUID_15 r
               , reportOrderByItemName_16 = reportOrderByItemName_15 r
               , reportDisplayItemName_16 = reportDisplayItemName_15 r
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
             }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

instance Migrate Report where
    type MigrateFrom Report = Report_16
    migrate r =
        Report
        { reportFolder = reportFolder_16 r
        , reportName = reportName_16 r
        , reportDate = reportDate_16 r
        , reportContractDate = reportContractDate_16 r
        , reportInspectionDate = reportInspectionDate_16 r
        , reportEffectiveDate = reportEffectiveDate_16 r
        , reportAuthors = reportAuthors_16 r
        , reportPreparer = reportPreparer_16 r
        , reportPreparerEIN = reportPreparerEIN_16 r
        , reportPreparerAddress = reportPreparerAddress_16 r
        , reportPreparerEMail = reportPreparerEMail_16 r
        , reportPreparerWebsite = reportPreparerWebsite_16 r
        , reportAbbrevs = reportAbbrevs_16 r
        , reportTitle = reportTitle_16 r
        , reportHeader = reportHeader_16 r
        , reportFooter = reportFooter_16 r
        , reportIntendedUse = reportIntendedUse_16 r
        , reportValueTypeInfo = reportValueTypeInfo_16 r
        , reportValueApproachInfo = reportValueApproachInfo_16 r
        , reportClientName = reportClientName_16 r
        , reportClientAddress = reportClientAddress_16 r
        , reportClientGreeting = reportClientGreeting_16 r
        , reportItemsOwnerFull = reportItemsOwnerFull_16 r
        , reportItemsOwner = reportItemsOwner_16 r
        , reportBriefItems = reportBriefItems_16 r
        , reportInspectionLocation = reportInspectionLocation_16 r
        , reportBody = reportBody_16 r
        , reportGlossary = reportGlossary_16 r
        , reportSources = reportSources_16 r
        , reportLetterOfTransmittal = reportLetterOfTransmittal_16 r
        , reportScopeOfWork = reportScopeOfWork_16 r
        , reportCertification = reportCertification_16 r
        , reportLimitingConditions = reportLimitingConditions_16 r
        , reportPrivacyPolicy = reportPrivacyPolicy_16 r
        , reportPerms = reportPerms_16 r
        , reportRevision = reportRevision_16 r
        , reportCreated = reportCreated_16 r
        , reportBranding = reportBranding_16 r
        , reportStatus = reportStatus_16 r
        , reportRedacted = reportRedacted_16 r
        , reportFlags = reportFlags_16 r
        , reportUUID = let Appraisal.Utils.UUID.Internal.UUID a b c d = reportUUID_16 r in Data.UUID.Types.Internal.UUID a b c d
        , reportOrderByItemName = reportOrderByItemName_16 r
        , reportDisplayItemName = reportDisplayItemName_16 r }

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
    deriving (Read, Show, Eq)

reportIntendedUseMarkup :: Report -> Maybe Markup
reportIntendedUseMarkup report =
    case reportIntendedUse report of
      Just SalesAdvisory -> Just (rawMarkdown "Sales Advisory")
      Just EstatePlanning -> Just (rawMarkdown "Estate Tax Planning")
      Just EstateTax -> Just (rawMarkdown "Estate Tax")
      Just InsuranceCoverage -> Just (rawMarkdown "Insurance Coverage")
      Just InsuranceClaim -> Just (rawMarkdown "Insurance Claim")
      Just CharitableDonation -> Just (rawMarkdown "Charitable Donation for Income Tax")
      Just EquitableDistribution -> Just (rawMarkdown "Equitable Distribution")
      Just MaritalDissolution -> Just (rawMarkdown "Marital Dissolution")
      Just EstateDivision -> Just (rawMarkdown "Estate Division")
      Nothing -> Nothing

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
    where report' = everywhere (mkT T.strip) report
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

compareVersions' :: T.Text -> T.Text -> Ordering
compareVersions' a b =
    cmp a' b'
    where
      a' :: [Tagged]
      a' = map (tag . T.strip) (T.groupBy (\ x y -> isDigit x == isDigit y) a)
      b' :: [Tagged]
      b' = map (tag . T.strip) (T.groupBy (\ x y -> isDigit x == isDigit y) b)
      -- Tag the groups
      tag :: Text -> Tagged
      tag t =
          case T.uncons t of
            Nothing -> Chars T.empty -- Should not happen
            Just (c, _) | isDigit c -> Digits t
            Just _ -> Chars t
      -- Compare tagged groups
      cmp (Digits x : xs) (Digits y : ys) = case compare (read (T.unpack x) :: Int) (read (T.unpack y) :: Int) of EQ -> cmp xs ys; other -> other
      cmp (Chars x : xs) (Chars y : ys) = case compare x y of EQ -> cmp xs ys; other -> other
      cmp (Digits _ : _) (Chars _ : _) = GT
      cmp (Chars _ : _) (Digits _ : _) = LT
      cmp (_ : _) [] = GT
      cmp [] (_ : _) = LT
      cmp _ _ = EQ

reportBrandingLens :: Lens' Branding Text
reportBrandingLens = lens getter setter
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
        setter _logo x =
            case x of
              _ | unpack (T.strip x) == "Thompson & Martinez" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "17e667c2bbe83e098510607571cffc00", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 348, imageFileHeight = 140, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Thompson & Martinez New" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "62e7310af0008fa68de56ab9d1b60e8f", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 324, imageFileHeight = 400, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Thompson & Martinez Wide" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "c3bd1388b41fa5d956e4308ce518a8bd", fileMessages = []}
                                    , imageFileType = PNG, imageFileWidth = 595, imageFileHeight = 114, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Goldfield Appraisals" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "cb913fc45e16135fc540a114c25c8a28", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 229, imageFileHeight = 90, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Thompson Martinez Goldfield" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "6ad232e854c6ff80fd2ec11b2d3af21d", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 704, imageFileHeight = 140, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Goldfield Appraisals 2" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "4ffb5f95b3baf7790a413e768f1fb2b2", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 2250, imageFileHeight = 225, imageFileMaxVal = 255 })
              _ | unpack (T.strip x) == "Goldfield Appraisals 3" ->
                    Logo (ImageFile { imageFile = File {fileSource = Nothing, fileChksum = "f92d08935f8ba2cee3427b24fb3c263f", fileMessages = []}
                                    , imageFileType = JPEG, imageFileWidth = 1280, imageFileHeight = 113, imageFileMaxVal = 255 })
              _ -> case reads (unpack x) of
                     [(b,_)] -> b
                     _ -> trace'' ("reportBrandingLens dropping value " ++ unpack x) NoLogo

$(deriveSafeCopy 1 'base ''Author)
$(deriveSafeCopy 1 'base ''ReportValueTypeInfo)
$(deriveSafeCopy 1 'base ''ReportIntendedUse_1)
$(deriveSafeCopy 2 'extension ''ReportIntendedUse_2)
$(deriveSafeCopy 3 'extension ''ReportIntendedUse)
$(deriveSafeCopy 1 'base ''ReportValueApproachInfo)
$(deriveSafeCopy 0 'base ''ReportElem_0)
$(deriveSafeCopy 1 'extension ''ReportElem)
$(deriveSafeCopy 9 'base ''Report_9)
$(deriveSafeCopy 10 'extension ''Report_10)
$(deriveSafeCopy 11 'extension ''Report_11)
$(deriveSafeCopy 12 'extension ''Report_12)
$(deriveSafeCopy 13 'extension ''Report_13)
$(deriveSafeCopy 14 'extension ''Report_14)
$(deriveSafeCopy 15 'extension ''Report_15)
$(deriveSafeCopy 16 'extension ''Report_16)
$(deriveSafeCopy 17 'extension ''Report)
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
