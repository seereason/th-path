{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-unused-binds -fno-warn-unused-do-bind #-}
module Appraisal.ReportLaTeX
    ( reportToLaTeX
    ) where

import Appraisal.Config (imagesDir, Paths)
import Appraisal.Currency (cashValueDigitsLaTeX, Priceable(cashValue))
import Appraisal.File (File(fileChksum))
import qualified Appraisal.Image as I (ImageSize)
import Appraisal.ImageFile (ImageFile(imageFile), imageFileHeight, imageFileWidth)
import Appraisal.LaTeX -- (bold, bulletList, centering, cjk, ColumnSpec(..), cooked, description, heading, longTable, newcommand, nl, renewcommand, setcounter, setlength, table, wideTable)
import Appraisal.LaTeX.Tables (ColumnSpec(..), table, appraisedItemSummary, wideTable)
import Appraisal.LaTeX.Figures (figures, Figure, PicName(PicName))
import Appraisal.LaTeX.Margins (marginSetup, textWidth, textWidthInInches, titleImage)
import Appraisal.Markup as M (blocks, lines, mapChars, Markup, markupText, protectLaTeX, rawMarkdown, strip, strip')
import Appraisal.Report (Author(authorCredentials, authorName), Branding(..), Report(..), ReportElem(ReportItem, ReportParagraph, ReportUndecided), ReportFlags(hideEmptyItemFields), reportIntendedUseMarkup, ReportStatus(..), ReportValueApproachInfo(reportValueApproachName, reportValueApproachDescription), reportValueSum, ReportValueTypeInfo(reportValueTypeName, reportValueTypeDescription, reportValueTypeDefinition), sortElems)
import Appraisal.ReportImage (ReportImage(picSize, picCaption), enlargedSize)
import Appraisal.ReportItem (Item(fields, images), ItemFieldInfo(itemFieldLabel, itemFieldHide), itemFieldMap, ItemFieldName(..))
import Appraisal.Utils.List (terminate)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Control.Monad.Trans (lift)
import Data.Char (toUpper)
import Data.List (intersperse, zip4, groupBy)
import qualified Data.Map as Map (Map, fromList, lookup, mapMaybeWithKey, filterWithKey, toList, elems)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat, mempty)
#endif
import Data.Ratio ((%))
import Data.String (fromString)
import Data.Text as T (pack, strip, Text, unpack)
import Language.Haskell.TH.Path.Order as Order (toList)
import System.FilePath ((</>))
import Text.LaTeX (execLaTeXM, LaTeXM, LaTeX, newpage, lnbk, large3)
import Text.LaTeX.Base.Class (LaTeXC, comm1, commS, liftL)
import Text.LaTeX.Base.Commands (article, ClassOption(FontSize), documentclass, footnotesize, large, raw, small, textbf, texttt, usepackage, vfill, vspace, comment)
import Text.LaTeX.Base.Render as LaTeX (Render(render), rendertex)
import Text.LaTeX.Base.Syntax (LaTeX(TeXComm), Measure(CustomMeasure, In, Pt, Cm), TeXArg(FixArg, OptArg))
import Text.LaTeX.Base.Texy (texy)
import Text.LaTeX.Base.Types (TableSpec(..))
import Text.LaTeX.Packages.Fancyhdr (renewheadrulewidth, lhead, chead, rhead, lfoot, cfoot, rfoot)
import Text.LaTeX.Packages.Graphicx (includegraphics, IGOption(IGWidth, IGHeight, KeepAspectRatio))

reportToLaTeX :: Paths p => p -> (Markup -> LaTeXM ()) -> (ReportImage -> Maybe ImageFile) -> (ReportImage -> Maybe ImageFile) -> Report -> LaTeX
reportToLaTeX ver lfm picPrinter picEnlarged appraisal =
    execLaTeXM $
    do -- (Pt 12) renders as 12.00000pt, and LaTeX wants to see just 12pt
       -- in the documentclass option, so we need a CustomMeasure.
       documentclass [FontSize (CustomMeasure "12pt"){-(Pt 12)-}] article >> nl >> nl
       cjk
       newcommand "effectivedate" (lfm (reportEffectiveDate appraisal)) >> nl
       newcommand "reportdate" (lfm (reportDate appraisal)) >> nl >> nl
       signAndDate (map authorName (filter authorOk (Order.toList (reportAuthors appraisal)))) >> nl
       comment " No section numbering, single level table of contents."
       setcounter "secnumdepth" 0 >> nl
       setcounter "tocdepth" 1 >> nl
       -- This package kills bold and italic fonts under xelatex
       -- usepackage [] "times" >> nl
       usepackage [] "lastpage" >> nl
       usepackage [] "paralist" >> nl
       usepackage [] "graphicx" >> nl
       usepackage [] "float" >> nl
       usepackage ["normalem"] "ulem" >> nl  -- Includes a strikeout elements: sout, xout, and others
       case reportStatus appraisal of
         Draft -> usepackage [] "draftwatermark" >> nl
         Final -> nl
       usepackage [] "longtable" >> nl
       marginSetup
       -- isoentHeaders
       sectionSetup
       fancyhdrSetup
       raw "\\def\\addcontentsline#1#2#3{%\n  \\addtocontents{#1}{\\protect\\contentsline{#2}{#3}{\\thepage \\space of \\pageref{LastPage}}}}" >> nl
       renewcommand "contentsname" (raw "\\vskip 36pt Table of Contents") >> nl >> nl
       raw ("% \\textwidth=" <> LaTeX.render textWidth <> "\n")
       raw "\\parskip 7.2pt" >> nl
       raw "\\parindent 0pt" >> nl >> nl
       captionSetup
       renewcommand "floatpagefraction" (raw ".9") >> nl
       renewcommand "topfraction" (raw ".9") >> nl
       renewcommand "bottomfraction" (raw ".9") >> nl
       renewcommand "textfraction" (raw ".1") >> nl
       raw "\\setcounter{totalnumber}{50}" >> nl
       raw "\\setcounter{topnumber}{50}" >> nl
       raw "\\setcounter{bottomnumber}{50}" >> nl
       raw "\\raggedbottom" >> nl
       raw "\\date{}" >> nl
       raw "\\begin{document}" >> nl
       raw "\\makexeCJKinactive"
       titlePage
       raw "\\pagestyle{fancy}" >> nl
       raw "\\vskip .4in" >> nl
       raw "\\parskip 0ex" >> nl
       raw "\\begin{center}\\tableofcontents\\end{center}" >> nl
       raw "\\parskip 1.5ex " >> nl >> nl
       reportBodyToLaTeX
       appendixImages (concatMap (Order.toList . images) (mapMaybe reportItemOfElem (Order.toList (reportBody appraisal))))
       raw "\\end{document}" >> nl
    where
      sectionSetup =
          do usepackage [] "section" >> nl
             setlength "secpreskp" (Pt 1) >> nl
             setlength "secpstskp" (Pt 1) >> nl
             -- This causes section headings (and above) to be centered
             raw "\\let\\hdpos\\centering\\setcounter{hddepth}{1}" >> nl

      captionSetup :: LaTeXM ()
      captionSetup =
          do usepackage [] "caption"
             comm1 "captionsetup" (raw "labelformat=empty,labelsep=none")
             usepackage [] "subcaption"
             subcaptionsetup (raw "labelformat=empty")
          where
            subcaptionsetup :: LaTeXM () -> LaTeXM ()
            subcaptionsetup = liftL $ \ l -> TeXComm "captionsetup" [OptArg (raw "sub"), FixArg l]

      authorOk :: Author -> Bool
      authorOk a = M.strip (authorName a) /= mempty

      titlePage :: LaTeXM ()
      titlePage =
          do comm1 "thispagestyle" "empty" >> nl
             oldCentering
                       [do nl
                           titleLogo
                           vfill
                           textbf $ large3 $ lfm $ reportTitle appraisal,
                        do nl
                           vspace (In 0.3)
                           large $ raw "Intended Use: " >> intendedUse,
                        do nl
                           large $ raw "Type of Value: " >> valueTypeName,
                        do nl
                           vspace (In 0.3)
                           client
                           vspace (In 0.3)
                           appraisers
                           vspace (In 0.3)
                           dates
                           vfill
                           emails]
             nl
             newpage >> nl

      client :: LaTeXM ()
      client =
          sequence_ (terminate doSep (map doString $ filter (not . (== mempty)) $ xs))
          where
            xs = [protectLaTeX "Client:", reportItemsOwner appraisal] ++
                 M.lines (reportClientName appraisal) ++
                 M.lines (reportClientAddress appraisal)
            doSep = raw " \\\\ "
            doString s = large (lfm s)
            trim = unpack . T.strip . pack

      -- assemble the five appraiser fields into a title page, breaking them up
      -- to simulate having a tab for each appraiser (as we should.)
      appraisers :: LaTeXM ()
      appraisers =
          sequence_ (terminate
                     lnbk
                     (map (\ s -> large (lfm s))
                          ([protectLaTeX "Appraised By:"] ++
                           map authorName (filter authorOk (Order.toList (reportAuthors appraisal))) ++
                           case (zip (M.lines (reportPreparer appraisal)) (M.lines (reportPreparerEIN appraisal) ++ repeat mempty)) of
                             -- If there is only one preparer/ein pair generate two lines,
                             -- otherwise create several "preparer: ein" lines.
                             [] -> []
                             [(a, b)] -> [a, b]
                             pairs -> map (\ (a, b) -> a <> protectLaTeX ": " <> b) pairs)))

      dates :: LaTeXM ()
      dates =
          do large (raw "Date of Inspection: " >> lfm (reportInspectionDate appraisal))
             raw " \\\\ "
             large (raw "Effective Date of Valuation: " >> lfm (reportEffectiveDate appraisal))
             raw " \\\\ "
             large (raw "Date of Report: " >> commS "reportdate")
             raw " \\\\ "

      -- Output the appraiser address, email, and web site.  This is a hack
      -- to handle more than one appraiser in the input forms on the
      -- Appraiser Info tab.  We assume here that the Preparer, EIN, Email,
      -- and Website fields are one line per appraiser, and the address
      -- field is one block per appraiser, with blocks being separated by a
      -- blank line.  We break these up using M.lines and M.blocks, then zip
      -- it all together to reassemble the info for a single appraiser.
      emails :: LaTeXM ()
      emails =
          footnotesize $
            sequence_ $
              intersperse
                (raw "{\\vskip .3cm}")
                (map (\ (p, a, e, w) -> mconcat $ intersperse (lnbk <> raw "\n") ([lfm p] ++ map lfm a ++ [raw "Email: " <> texttt (lfm e)] ++ [raw "Web site: " <> texttt (lfm w)])) appraisers')
          where
            appraisers' :: [(Markup, [Markup], Markup, Markup)]
            appraisers' =
                zip4 (M.lines (reportPreparer appraisal))
                     (M.blocks (reportPreparerAddress appraisal))
                     (M.lines (reportPreparerEMail appraisal))
                     (M.lines (reportPreparerWebsite appraisal))

      fancyhdrSetup :: LaTeXM ()
      fancyhdrSetup =
          do usepackage [] "fancyhdr" >> nl
             headerLogo
             rhead mempty
             lfoot mempty
             cfoot (small . textbf . sequence_ $
                        intersperse lnbk
                                    [ commS "hrulefill"
                                    , texy ("CONFIDENTIAL" :: Text)
                                    , valueTypeName  >> texy (" for " :: Text) >> intendedUse
                                    , texy ("Prepared by " :: Text) >> lfm (reportPreparer appraisal)
                                    , texy ("Effective Date of Valuation: " :: Text) >> lfm (reportEffectiveDate appraisal)
                                    , texy ("Page " :: Text) <> commS "thepage" <> raw " \\ of " <> comm1 "pageref" (texy ("LastPage" :: Text)) ])
             rfoot mempty
             renewheadrulewidth (Pt 0) >> nl

      -- If logo is more than twice as wide as high, put it in the center of
      -- the page header, otherwise left
      headerLogo :: LaTeXM ()
      headerLogo =
          case (reportBranding appraisal) of
            Logo img ->
                if imageFileHeight img % imageFileWidth img < (1 % 2) -- wide
                then do lhead mempty
                        chead (pic img)
                else do lhead (pic img)
                        chead mempty
            NoLogo ->
                do lhead mempty
                   chead mempty

      pic :: ImageFile -> LaTeXM ()
      pic img = includegraphics
                  [IGHeight (Cm 1.8), IGWidth (In 4), KeepAspectRatio True]
                  (imagesDir ver </> fileChksum (imageFile img))
      -- pic img = picture img
      --                 (I.ImageSize { I.dim = I.TheHeight, I.size = 1.8, I.units = I.Cm})
      --                 (PicName (T.pack (imagesDir ver </> fileChksum (imageFile img))))

      titleLogo :: LaTeXM ()
      titleLogo =
          case reportBranding appraisal of
            NoLogo -> return ()
            Logo img -> titleImage ver img

      vskip :: Measure -> LaTeXM ()
      vskip m = do
        commS "vskip"
        raw " "
        rendertex m

      -- raw :: LaTeXC l => Text -> l
      -- commS :: LaTeXC l => String -> l
      -- execLaTeXT :: Monad m => LaTeXM a -> m LaTeX

      parskip :: Measure -> LaTeXM ()
      parskip m =
          do commS "parskip"
             raw " "
             rendertex m
             raw " "

      signAndDate :: [Markup] -> LaTeXM ()
      signAndDate [] = signAndDate [protectLaTeX "(Enter Author Credentials)"]
      signAndDate names =
          newcommand "authorsignature"
                     (do nl
                         vskip (In 0.2) >> nl
                         raw "\\begin{tabular*}{\\textwidth}{l@{\\extracolsep{\\fill}}r}"
                         sequence_ (intersperse doSep (map doName names))
                         raw "\\end{tabular*}"
                         nl) >> nl
          where
            doName :: Markup -> LaTeXM ()
            doName name =
                case mapMaybe M.strip (M.lines name) of
                  [] ->
                      do raw "\\hspace{3in} & \\hspace{1in} \\\\ \\cline{1-1} \\cline{2-2}\n"
                         cooked "(Missing)"
                         raw " & Date \\\\\n"
                  (x : xs) ->
                      do raw "\\hspace{3in} & \\hspace{1in} \\\\ \\cline{1-1} \\cline{2-2}\n"
                         lfm x
                         raw " & Date \\\\\n"
                         mapM_ (\ s -> do lfm s
                                          raw " & \\\\\n") xs
            doSep :: LaTeXM ()
            doSep = raw " & \\\\" >> nl

      reportBodyToLaTeX :: LaTeXM ()
      reportBodyToLaTeX =
          do letterOfTransmittal appraisal
             scopeOfWork appraisal
             typeOfValue appraisal
             intendedUseDescription appraisal
             reportUsers appraisal
             definitionOfValue
             approachesToValue appraisal
             privacyPolicy appraisal
             limitingConditions appraisal
             certification appraisal
             credentials appraisal
             glossary appraisal
             sources appraisal
             conditionDefinitions
             summary appraisal
             reportItems

      letterOfTransmittal :: Report -> LaTeXM ()
      letterOfTransmittal report =
          do hd
             lfm (reportLetterOfTransmittal report)
             sig
          where
            hd = do newpage
                    heading 1 (cooked "Letter of Transmittal")
                    table [LeftColumn, ParColumnTop (texy (In 4))]
                           ([ ([ cooked "Date of Report:", lfm (reportDate report) ] :: [LaTeXM ()])
                            , ([ cooked "Client:", lfm (reportItemsOwner report) ] :: [LaTeXM ()]) ] ++
                            (map (\ x -> [mempty, x])
                                 (map lfm (M.lines (reportClientName report) ++ M.lines (reportClientAddress report))) :: [[LaTeXM ()]]) ++
                            [ ([ cooked "Intended Use:", intendedUse ] :: [LaTeXM ()])
                            , ([ cooked "Type of Value:", valueTypeName ] :: [LaTeXM ()])
                            , ([ cooked "Approach:", valueApproachName ] :: [LaTeXM ()])
                            , ([ cooked "Date of Inspection:", lfm (rawMarkdown "[InspectionDate]")] :: [LaTeXM ()])
                            , ([ cooked "Effective Date of Valuation:", lfm (rawMarkdown "[EffectiveDate]")] :: [LaTeXM ()])
                            , ([ cooked "Inspected By:", lfm (rawMarkdown "[LOTInspectedBy]")] :: [LaTeXM ()]) ])
                    cooked "Dear " >> lfm (reportClientGreeting report) >> cooked ":"
                    nl
                    nl
            sig = do commS "authorsignature"
                     newpage

      scopeOfWork :: Report -> LaTeXM ()
      scopeOfWork report =
          do newpage
             heading 1 (cooked "Scope of Work")
             lfm (reportScopeOfWork report)

      typeOfValue :: Report -> LaTeXM ()
      typeOfValue _report =
          do newpage
             heading 1 (cooked "Type of Value")
             lfm (rawMarkdown ("[TypeOfValueDescription]"))

      intendedUseDescription :: Report -> LaTeXM ()
      intendedUseDescription _report =
          do heading 1 (cooked "Intended Use")
             lfm (rawMarkdown "[IntendedUseDescription]")

      reportUsers :: Report -> LaTeXM ()
      reportUsers _report =
          do heading 1 (cooked "Intended Users of this report")
             lfm (rawMarkdown ("The intended users of this report are: [ReportUsers].  There are no other intended users or uses."))

      definitionOfValue :: LaTeXM ()
      definitionOfValue =
          do heading 1 (cooked "Definition of Value")
             lfm (rawMarkdown "[ValueTypeLongDescription]")

      approachesToValue :: Report -> LaTeXM ()
      approachesToValue r =
          do heading 1 (cooked "Approaches to Value")
             lfm (reportValueApproachDescription . reportValueApproachInfo $ r)
{-
          do heading 1 (cooked "Approaches to Value")
             cooked "For this appraisal three valuation methods were considered (from Soucy and Smith, eds, "
             emph (cooked "The Appraisal of Personal Property – Principles, Theories, and Practice Methods for the Professional Appraiser")
             cooked ", 1994.). The three valuation methods are:"
             bulletList [ do textbf (cooked "Cost Approach to Value")
                             cooked " method estimates either the reproduction or replacement of a property, either new or depreciated."
                        , do textbf (cooked "Income Approach to Value")
                             cooked " method estimates the present worth of anticipated future benefits of owning income producing properties or objects."
{- Retired wording:     , do textbf (cooked "Market Comparison Approach to Value")
                             cooked (" method estimates value by comparison with properties sold in the relevant market with adjustments " <>
                                     "for all differences that affect value, such as differences in characteristics of value, in market " <>
                                     "layer, and in time exposed to the market in order to arrive at the most apposite estimate of value.") -}
                        , do textbf (cooked "Sales Comparison Approach to Value")
                             cooked (" method estimates value by using one or more methods that compare the subject to other similar " <>
                                     "properties that have been sold in the relevant market with adjustments (up or down) made for all " <>
                                     "differences that affect value, such as differences in characteristics of value, in market level, " <>
                                     "and in time.") ]
             lfm (rawMarkdown "[ValueApproachExplanation]")
-}

      privacyPolicy :: Report -> LaTeXM ()
      privacyPolicy report =
          do newpage
             heading 1 (cooked "Privacy Policy")
             lfm (reportPrivacyPolicy report)

      limitingConditions :: Report -> LaTeXM ()
      limitingConditions report =
          do newpage
             heading 1 (cooked "Assumptions and Limiting Conditions")
             cooked "This appraisal has been made subject to the following general assumptions and limiting conditions."
             bulletList (map lfm (Order.toList (reportLimitingConditions report)))

      certification :: Report -> LaTeXM ()
      certification report =
          do newpage
             heading 1 (cooked "Certification")
             lfm (rawMarkdown "The [appraisers] [certify] and [agree] that:")
             bulletList (map lfm (Order.toList (reportCertification report)))
             commS "authorsignature"

      credentials :: Report -> LaTeXM ()
      credentials report =
          do newpage
             heading 1 (cooked "Credentials")
             sequence_ (intersperse newpage (map authorToWiki (Order.toList (reportAuthors report))))
          where
            authorToWiki author = (heading 2 . lfm . authorName $ author) >> (lfm . authorCredentials $ author)

      glossary :: Report -> LaTeXM ()
      glossary report =
          case Order.toList (reportGlossary report) of
            [] -> mempty
            entries ->
                sequence_
                [ newpage
                , heading 1 (cooked "Glossary")
                , description (map (\ (a, b) -> (lfm a, lfm b)) entries) ]

      sources :: Report -> LaTeXM ()
      sources report =
          case Order.toList (reportSources report) of
            [] -> mempty
            sections ->
                do newpage
                   heading 1 (cooked "Bibliography & Authorities Consulted")
                   sequence_ (map (uncurry section) sections)
          where
            section hd entries =
                do heading 3 (lfm hd)
                   preformatted lfm entries
            nonempty = not . (== mempty) . M.strip

      conditionDefinitions :: LaTeXM ()
      conditionDefinitions =
          do newpage
             heading 1 (cooked "Condition Definitions")
             description
                      [ (cooked "Excellent:",
                         cooked "The object's stability and state of preservation (i.e. wear and/or losses to paper, decoration, pigment, chips etc.) exhibits stable condition or integrity; function or artistic intent is structurally sound; no restoration material added and no significant repairs.")
                      , (cooked "Very Good:",
                         cooked "State of preservation is near original condition; object exhibits stabilization/conservation possibly reflecting slight deterioration from original function or artistic intent.  Structural integrity is maintained.  Some repairs are evident and unobtrusive; no restoration, i.e. all original material.")
                      , (cooked "Good:",
                         cooked "State of preservation exhibits clearly its original function and artistic intent, however, the object shows considerable wear.  Stabilization, conservation, restoration reflect original function and maintain artistic intent and structural integrity.  Restoration and repairs are evident; i.e. there may be added material; or, repairs may be needed.")
                      , (cooked "Fair:",
                         cooked "State of preservation shows considerable wear and deterioration yet still indicates the general nature of its original form; the object may need structural stabilization, conservation, restoration and/or repairs.")
                      , (cooked "Poor:",
                         cooked "State of preservation indicates substantial deterioration compromising original form; the object needs structural stabilization, conservation, restoration and/or repairs.") ]

      summary :: Report -> LaTeXM ()
      summary report =
          case length items of
            n | n >= 2 ->
               do newpage
                  heading 1 (cooked "Appraised Item Summary")
                  appraisedItemSummary
                            "Appraised Item Summary"
                            ([ColumnSpec RightColumn ""] ++
                             (if reportDisplayItemName report
                              then [ColumnSpec (ParColumnTop (texy (In 1))) "Item",
                                    ColumnSpec (ParColumnTop (texy (In 2.5))) "Artist",
                                    ColumnSpec (ParColumnTop (texy (In 1.5))) "Type of Object",
                                    ColumnSpec RightColumn "Amount"]
                              else [ColumnSpec (ParColumnTop (texy (In 3))) "Artist",
                                    ColumnSpec (ParColumnTop (texy (In 2))) "Type of Object",
                                    ColumnSpec RightColumn "Amount"]))
                            items
                            -- FIXME: Don't display a sum if any of the entries is unparsable
                            (cooked ("Total of " <> pack (show valued) <>
                                     (if unvalued > 0 then " (of " <> pack (show (valued + unvalued)) <> ")" else "") <>
                                     " items:"),
                             lfm (cashValueDigitsLaTeX value))
            _ -> mempty
          where
            (value, valued, unvalued) = reportValueSum report
            items :: [[LaTeXM ()]]
            items = catMaybes . map doItem . zip ([1..] :: [Int]) . filter isItem . Order.toList . (if reportOrderByItemName report then sortElems else id) . reportBody $ report
            doItem :: (Int, ReportElem) -> Maybe [LaTeXM ()]
            doItem (n, ReportItem item) =
                Just ([ cooked (pack (show n) <> ".") ] ++
                      (if reportDisplayItemName report
                       then [ lfm (maybe mempty id (Map.lookup ItemDataSheetNumber (fields item))) ]
                       else []) ++
                      [ maybe (cooked "–") lfm (Map.lookup ItemArtistOrMaker (fields item))
                      , maybe mempty lfm (Map.lookup ItemTypeOfObject (fields item))
                      , lfm (cashValueDigitsLaTeX (cashValue item)) ])
            doItem _ = Nothing
            isItem (ReportItem {}) = True
            isItem _ = False

      reportItems :: LaTeXM ()
      reportItems =
          (\ x -> evalStateT x 0)$
          do let items = map reportElem $ Order.toList $ (if reportOrderByItemName appraisal then sortElems else id) $ reportBody appraisal
                 items' = lift newpage : intersperse (lift newpage) items
             sequence_ items'

      reportElem :: ReportElem -> StateT Int (LaTeXM) ()
      reportElem (ReportParagraph s) = lift $ lfm s
      reportElem ReportUndecided = lift mempty
      reportElem (ReportItem item) =
          do modify (+ 1)
             n <- get
             lift $ do wideTable
                         [T.pack "l", T.pack "@{\\extracolsep{0pt}}", T.pack "r"]
                         -- \hline \\ [-10pt]
                         [ [ (bold . lfm . M.mapChars toUpper . reportName $ appraisal)
                           , (bold . lfm . M.mapChars toUpper . reportValueTypeName . reportValueTypeInfo $ appraisal) ]
                         , [ (textbf $ (cooked "ITEM #" >>
                                        (if reportDisplayItemName appraisal
                                         then maybe (cooked "???") lfm (Map.lookup ItemDataSheetNumber (fields item))
                                         else cooked (pack (show n)))))
                           , (textbf . lfm . cashValueDigitsLaTeX . cashValue $ item) ] ]
                       -- \hline
                       reportImages textWidthInInches (Order.toList (images item))
                       reportItem item

      reportImages :: Double -> [ReportImage] -> LaTeXM ()
      reportImages _ [] = mempty
      reportImages width xs = figuresFromImages ver width (zip3 xs (map picPrinter xs) (map (Just . picSize) xs))

      reportItem :: Item -> LaTeXM ()
      reportItem item =
          reportItemLaTeX (hideEmptyItemFields (reportFlags appraisal)) (item, lfm (reportValueTypeName (reportValueTypeInfo appraisal) <> protectLaTeX ": "))

      -- instance Conversion (Item, Wiki) Wiki where convert = wikiFromItemPair

      reportItemLaTeX :: Bool -> (Item, LaTeXM ()) -> LaTeXM ()
      reportItemLaTeX hideEmptyFields (item, valueLabel) =
              do table [ParColumnTop (raw ".4\\textwidth"),
                        ParColumnTop (raw ".6\\textwidth")] (Map.elems (Map.mapMaybeWithKey toWiki shortFields))
                 longFields
              where
                shortFields :: Map.Map ItemFieldName ItemFieldInfo
                shortFields = Map.filterWithKey (\ name _ -> not (elem name longFieldNames)) itemFieldMap
                toWiki :: ItemFieldName -> ItemFieldInfo -> Maybe [LaTeXM ()]
                toWiki name info =
                    case (itemFieldHide info, Map.lookup name (fields item)) of
                      (True, _) -> Nothing
                      (_, Nothing) -> Nothing
                      (_, Just s) | s == mempty -> Nothing
                      (_, Just w) -> Just [(bold . cooked . pack . itemFieldLabel $ info), lfm w]
                longFields :: LaTeXM ()
                longFields = mapM_ longField (filter (\ (name, _) -> elem name longFieldNames) (Map.toList itemFieldMap))
                longField :: (ItemFieldName, ItemFieldInfo) -> LaTeXM ()
                longField (ItemCashValue, _) =
                    case Map.lookup ItemCashValue (fields item) of
                      Nothing -> mempty
                      Just w -> heading 4 valueLabel >> lfm w
                longField (name, info) =
                    if hideEmptyFields
                    then
                      case Map.lookup name (fields item) of
                        Nothing -> mempty
                        Just w | w == mempty -> mempty
                        Just w -> (heading 4 . cooked . pack . itemFieldLabel $ info) >> lfm w
                    else
                      case Map.lookup name (fields item) of
                        Nothing -> mempty
                        Just w -> (heading 4 . cooked . pack . itemFieldLabel $ info) >> lfm w
                longFieldNames = [ItemDescription, ItemMarketAnalysis, ItemCashValue]

      intendedUse :: LaTeXM ()
      intendedUse = fieldOrAbbrev reportIntendedUse' "IntendedUse"
          where reportIntendedUse' :: Report -> Markup
                reportIntendedUse' = fromMaybe mempty . reportIntendedUseMarkup

      valueTypeName :: LaTeXM ()
      valueTypeName = fieldOrAbbrev (reportValueTypeName . reportValueTypeInfo) "ValueType"

      valueTypeDescription :: LaTeXM ()
      valueTypeDescription = fieldOrAbbrev (reportValueTypeDescription . reportValueTypeInfo) "ValueTypeDescription"

      valueTypeDefinition :: LaTeXM ()
      valueTypeDefinition = fieldOrAbbrev (reportValueTypeDefinition . reportValueTypeInfo) "ValueTypeLongDescription"

      valueApproachName :: LaTeXM ()
      valueApproachName = fieldOrAbbrev (reportValueApproachName . reportValueApproachInfo) "ValueApproachName"

      -- valueApproachDescription :: LaTeXM ()
      -- valueApproachDescription = fieldOrAbbrev (reportValueApproachDescription . reportValueApproachInfo) "ValueApproachExplanation"

      fieldOrAbbrev :: (Report -> Markup) -> String -> LaTeXM ()
      fieldOrAbbrev access name = maybe (lfm (access appraisal)) lfm (Map.lookup (fromString name) (Map.fromList (Order.toList (reportAbbrevs appraisal))))

      appendixImages :: [ReportImage] -> LaTeXM ()
      appendixImages xs = do
        let enlarged = map picEnlarged xs ++ repeat Nothing
            triples = zip3 xs enlarged (map (fmap enlargedSize) enlarged)
        case filter (\ (_, x, y) -> isJust x && isJust y) triples of
          [] -> return ()
          triples' -> do
            newpage
            heading 1 (raw ("Appendix A - " <> pack (show (length xs)) <> " Enlargements"))
            -- setlength "topmargin" (In (- 1.65))
            -- setlength "evensidemargin" (In (- 0.75))
            -- setlength "oddsidemargin" (In (- 0.75))
            -- setlength "textwidth" (In 8)
            lhead mempty
            chead mempty
            rhead mempty
            lfoot mempty
            cfoot mempty
            rfoot mempty
            figuresFromImages ver textWidthInInches triples'

-- This should call lfm, and that should do the strip
captionText :: ReportImage -> Maybe Text
captionText x = let cap' = T.strip (markupText (picCaption x)) in if cap' == mempty then Nothing else Just cap'

figuresFromImages :: Paths p => p -> Double -> [(ReportImage, Maybe ImageFile, Maybe I.ImageSize)] -> LaTeXM ()
figuresFromImages ver width xs =
    figures width (mapMaybe fig xs)
    where
      fig :: (ReportImage, Maybe ImageFile, Maybe I.ImageSize) -> Maybe Figure
      fig (pic, Just img, Just sz) =
          Just (img, sz, PicName (T.pack (imagesDir ver </> fileChksum (imageFile img))), captionText pic)
      fig _ = Nothing

reportItemOfElem :: ReportElem -> Maybe Item
reportItemOfElem (ReportItem x) = Just x
reportItemOfElem _ = Nothing

-- | Render a preformatted text input box, such as sources.  Newlines
-- are preserved and blank lines are paragraph breaks.
preformatted :: (Markup -> LaTeXM ()) -> Markup -> LaTeXM ()
preformatted lfm entries =
    sequence_ (intersperse (raw "{\\vskip .1cm}")
                           (map sequence_
                                (map (intersperse lnbk)
                                     (map (map lfm)
                                          paras))))
    where
      paras :: [[Markup]]
      paras = filter (not . all  (== mempty))
                     (groupBy (\ a b -> (a == mempty) == (b == mempty))
                              (map M.strip' (M.lines entries)))

texty :: LaTeXC l => Text -> l
texty t = texy (t :: Text)
