{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, StandaloneDeriving, TypeSynonymInstances, TemplateHaskell, TypeFamilies #-}
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
import Appraisal.ImageFile (ImageFile(ImageFile, imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal), ImageType(..))
import Appraisal.IntJS (deriveOrderJS)
import Appraisal.Markup as M (Markup, mapChars, rawMarkdown, markupText)
import Appraisal.Permissions (Permissions)
import Appraisal.ReportItem (Item(..))
import qualified Appraisal.ReportItem as I (Item(fields), ItemFieldName(ItemDataSheetNumber))
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.Debug (trace'')
import Appraisal.Utils.List (spanBy)
import Appraisal.Utils.Text (read)
import qualified Data.UUID.Types as UUID (toString)
import Data.UUID.Types (UUID)
import Data.Char (isDigit, toLower)
import Data.Function (on)
import Data.Generics (Data, everywhere, mkT, Typeable)
import Data.Int (Int64)
import qualified Data.IxSet.Revision as R (Ident(..), Revision(..), RevisionInfo(..))
import Control.Lens (Lens', lens)
import Data.List as List (groupBy, sortBy)
import qualified Data.ListLike as LL
import Data.Map as Map (lookup)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text as T (Text, groupBy, pack, unpack, strip, uncons, empty)
import Debug.Trace (trace)
import Language.Haskell.TH.Path.Core (lens_mrs, readShowLens)
import Language.Haskell.TH.Path.Graph (SelfPath)
import Language.Haskell.TH.Path.Order as Order (toList, asList)
import Language.Haskell.TH.Path.View (View(ViewType, viewLens))
import Data.UUID.Orphans ()
import Prelude hiding (read)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

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

data ReportElem
    = ReportItem {elemItem :: Item}
    | ReportParagraph {elemText :: Markup}
    | ReportUndecided
    deriving (Read, Show, Eq, Ord, Typeable, Data)

deriving instance Show (R.Revision R.Ident)
deriving instance Show (R.RevisionInfo R.Ident)

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

type EpochMilli = Int64

type MarkupPair = (Markup, Markup)
type AbbrevPair = (CIString, Markup)

$(deriveOrderJS ''ReportElem)
$(deriveOrderJS ''MarkupPair)
$(deriveOrderJS ''AbbrevPair)
$(deriveOrderJS ''Markup)

type MaybeReportIntendedUse = Maybe ReportIntendedUse

instance View MaybeReportIntendedUse where
    type ViewType MaybeReportIntendedUse = String
    viewLens = lens_mrs

-- | The ReportStandard type indicates what UI features should be used
-- for a particular report.  I would have made this an enumerated type,
-- but right now the path generation code isn't ready for it.  So,
--    1 - Initial value for existing reports, corresponds to Uniform
--        Standards of Professional Appraisal Practice (USPAP), the 2014-2015
--        Edition.
--    2 - A number of UI changes are implemented for this next version,
--        and new reports will be set to this value.  In particular, the Scope of Work
--        section is no longer included.
data ReportStandard = ReportStandard {unReportStandard :: Int} deriving (Read, Show, Eq, Ord, Typeable, Data)

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
    deriving (Read, Show, Eq, Ord, Typeable, Data)

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
