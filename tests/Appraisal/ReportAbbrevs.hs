{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-binds #-}
module Appraisal.ReportAbbrevs
    ( abbrevsHtml
    , abbrevsLaTeX
    ) where

import Appraisal.Currency(cashValueWordsHtml, cashValueDigitsHtml, cashValueWordsLaTeX, cashValueDigitsLaTeX)
import Appraisal.Markup as M (Markup, protectMarkdown, rawMarkdown, rawLaTeX, protectLaTeX, rawHtml)
import Appraisal.Report (Report(..), ReportValueTypeInfo(reportValueTypeName), reportIntendedUseMarkup,
                         reportValueTypeDescription, reportValueTypeDefinition,
                         reportValueSum, ReportIntendedUse(InsuranceCoverage,InsuranceClaim))
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.IsText (IsText(fromText))
import Data.Map as Map (Map, fromListWith)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text as T (pack)
import Language.Haskell.TH.Path.Order (toList)
import Text.LaTeX as LTX (pageref, raw)
import Text.LaTeX.Base.Class (commS)

abbrevsLaTeX :: Report -> Map CIString Markup
abbrevsLaTeX r =
    fromListWith (\ old _new -> old) $
     abbrevsCommon r ++
    [(fromString "AuthorSignature", rawLaTeX (commS "authorsignature")),
     (fromString "Newpage", rawLaTeX (commS "newpage")),
     (fromString "Newline", rawLaTeX (raw "\\\\\n")),
     (fromString "Break", rawLaTeX (raw "\n")),
     (fromString "ReportLength", rawLaTeX (pageref (raw "LastPage"))),
     (fromString "TotalValue",
      let (value, _, unappraised) = reportValueSum r in
      cashValueDigitsLaTeX value <>
      case unappraised of
        0 -> mempty
        _ -> protectLaTeX (" with " <> pack (show unappraised) <> " unappraised items")),
     (fromString "TotalValueAsText",
      let (value, _, _) = reportValueSum r in
      cashValueWordsLaTeX value)]

abbrevsHtml :: Report -> Map CIString Markup
abbrevsHtml r =
    fromListWith (\ old _new -> old) $
     abbrevsCommon r ++
     [(fromString "AuthorSignature", rawHtml "<i>(Author Signature)</i>"),
      (fromString "Newpage", rawHtml "<i>(New Page)</i>"),
      (fromString "Newline", rawHtml "<br>"),
      (fromString "Break", rawHtml "<br>"),
      (fromString "ReportLength", rawHtml "<i>(Report Length)</i>"),
      (fromString "TotalValue",
       let (value, _, unappraised) = reportValueSum r in
       cashValueDigitsHtml value <>
       case unappraised of
         0 -> mempty
         _ -> rawHtml (" with " <> pack (show unappraised) <> " unappraised items")),
      (fromString "TotalValueAsText",
       let (value, _, _) = reportValueSum r in
       cashValueWordsHtml value)]

abbrevsCommon :: Report -> [(CIString, Markup)]
abbrevsCommon r =
    -- The values in the abbreviation list come first so they override
    -- the custom values below.
    [(fromString "BriefDescription", reportBriefItems r),
     (fromString "Client", reportItemsOwner r),
     (fromString "ClientName", reportClientName r),
     (fromString "ClientAddress", reportClientAddress r),
     (fromString "Preparer", reportPreparer r),
     (fromString "ContractDate", reportContractDate r),
     (fromString "EffectiveDate", reportEffectiveDate r),
     (fromString "InspectionDate", reportInspectionDate r),
     (fromString "IntendedUse", fromMaybe (rawHtml "(report intended use not selected)") (reportIntendedUseMarkup r)),
     (fromString "ValueType", reportValueTypeName (reportValueTypeInfo r)),
     (fromString "ValueTypeDescription", reportValueTypeDescription (reportValueTypeInfo r)),
     (fromString "ValueTypeLongDescription", reportValueTypeDefinition (reportValueTypeInfo r))
     {- (fromString "ValueApproachExplanation", reportValueApproachDescription (reportValueApproachInfo r)) -} ] ++
     (case reportIntendedUse r of
        Just InsuranceCoverage -> [(fromText "IntendedUseShortDescription", protectMarkdown "obtaining Insurance Coverage")]
        _ -> []) ++
     (case reportIntendedUse r of
        Just InsuranceCoverage -> [(fromText "ReportUsers", rawMarkdown "<bf>" <> reportItemsOwner r <> rawMarkdown " and the Insurance Company</bf>")]
        _ -> []) ++
     (case reportIntendedUse r of
        Just InsuranceClaim -> [(fromText "IntendedUseShortDescription", protectMarkdown "making an Insurance Claim")]
        _ -> []) ++
     (case reportIntendedUse r of
        Just InsuranceClaim -> [(fromText "ReportUsers", rawMarkdown "<bf>" <> reportItemsOwner r <> rawMarkdown " and the Insurance Company</bf>")]
        _ -> []) ++
    (toList (reportAbbrevs r))
