{-# LANGUAGE CPP, DeriveDataTypeable, DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}
module Appraisal.Html
    ( latexFromMarkdown
    , latexFromPandoc
    , latexFromHtml
    , latexFromXmlTrees
    , latexFromMarkup
    , latexFromMarkupTest
    , tests
    ) where


import Appraisal.Abbrevs (expandText, MonadAbbrevs, runAbbrevsM', withAbbrevs)
import Appraisal.LaTeX (bulletItem, bulletList', heading, nbsp, orderedList')
import Appraisal.Markup (Markup, foldMarkup, markupText, protectMarkdown, rawMarkdown, rawHtml, rawLaTeX, rawPandoc, rawMarkup)
import Appraisal.Unicode (isCJK)
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.Pandoc (pandocFromMarkdown)
import Data.ByteString.Lazy (toStrict)
import Data.Char (chr)
import Data.Map as Map (Map, fromList)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tree.NTree.TypeDefs (NTree(..))
import Test.HUnit
import Text.LaTeX (execLaTeXM, LaTeX, raw, protectText, textell)
import Text.LaTeX.Base.Class (comm0, fromLaTeX)
import Text.LaTeX.Base.Commands as LaTeX (textbf, emph)
import Text.LaTeX.Base.Texy (texy)
import Text.Pandoc
import Text.XML.HXT.DOM.QualifiedName (localPart)
import Text.XML.HXT.DOM.TypeDefs (XNode(..), XmlTrees)
import Text.XML.HXT.Parser.HtmlParsec (parseHtmlContent)

latexFromPandoc :: Map CIString Markup -> [CIString] -> Pandoc -> LaTeX
latexFromPandoc mp stk = latexFromHtml mp stk . T.pack . writeHtmlString def

latexFromMarkdown :: Map CIString Markup -> [CIString] -> T.Text -> LaTeX
latexFromMarkdown _ _ s | T.null (T.strip s) = nbsp
latexFromMarkdown mp stk s = latexFromPandoc mp stk . pandocFromMarkdown $ s

-- | Treat empty lines as paragraph breaks, replace each line newline
-- with a pair of spaces.  This turns out to break the markdown
-- parser, and when I took it out, all good!  When will I learn to
-- explain why I'm doing things?
{-
fixMarkdown :: T.Text -> T.Text
fixMarkdown t =
    T.intercalate "\n\n" $
      map (T.intercalate "  ") $
        filter (not . T.null . head) $
          groupBy (\ a b -> T.null a == T.null b) $
            map T.strip $
              T.lines $ t
-}

-- Here's a place where I should have made some notes.
tests :: Test
tests =
    TestCase (assertEqual
              "pandocFromMarkdown"
              (pandocFromMarkdown (T.pack "decedent's"))
              (Pandoc (Meta {unMeta = Map.fromList []}) [Para [Str "decedent\8217s"]]))

-- | A state value for the conversion function - if we are in an Inline context we
-- ignore the Html p tag, in Block context we output par.
data Context = Inline | Block

-- | Convert a fragment of Html into LaTeX.
latexFromHtml :: Map CIString Markup -> [CIString] -> T.Text -> LaTeX
latexFromHtml mp stk x = latexFromXmlTrees mp stk (parseHtmlContent (T.unpack x))

latexFromXmlTrees :: Map CIString Markup -> [CIString] -> XmlTrees -> LaTeX
-- The HTML parser tends to return a single paragraph that encloses
-- everything - in LaTeX we generally need to ignore this.
latexFromXmlTrees mp stk [NTree (XTag tag []) xs] | localPart tag == "p" = mconcat $ map (runAbbrevsM' mp stk . latexFromXmlTree Inline) xs
latexFromXmlTrees mp stk xs = mconcat $ map (runAbbrevsM' mp stk . latexFromXmlTree Block) xs

latexFromXmlTree ::  MonadAbbrevs m => Context -> NTree XNode -> m LaTeX
latexFromXmlTree Block (NTree (XTag tag []) xs) | localPart tag == "p" = withAbbrevs $ \mp stk -> mconcat (map (runAbbrevsM' mp stk . latexFromXmlTree Inline) xs) <> execLaTeXM (raw "\n\n")

latexFromXmlTree Inline (NTree (XTag tag []) xs) | localPart tag == "p" = withAbbrevs $ \mp stk -> mconcat (map (runAbbrevsM' mp stk . latexFromXmlTree Inline) xs)

latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "ul" = withAbbrevs $ \mp stk -> execLaTeXM $ bulletList' (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs)
latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "ol" = withAbbrevs $ \mp stk -> execLaTeXM $ orderedList' (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs)
latexFromXmlTree _ (NTree (XTag tag []) xs) | localPart tag == "li" = withAbbrevs $ \mp stk -> execLaTeXM $ bulletItem (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))

latexFromXmlTree _ (NTree (XTag tag []) xs) | localPart tag == "strong" = withAbbrevs $ \mp stk -> execLaTeXM $ textbf (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))
latexFromXmlTree _ (NTree (XTag tag []) xs) | localPart tag == "em" = withAbbrevs $ \mp stk -> execLaTeXM $ emph (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))
latexFromXmlTree _ (NTree (XTag tag []) xs) | localPart tag == "bf" = withAbbrevs $ \mp stk -> execLaTeXM $ textbf (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))

latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "h1" = withAbbrevs $ \mp stk -> execLaTeXM $ heading 1 (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))
latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "h2" = withAbbrevs $ \mp stk -> execLaTeXM $ heading 2 (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))
latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "h3" = withAbbrevs $ \mp stk -> execLaTeXM $ heading 3 (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))
latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "h4" = withAbbrevs $ \mp stk -> execLaTeXM $ heading 4 (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs))
latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "h5" = withAbbrevs $ \mp stk -> execLaTeXM $ heading 5 (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs)) -- Same as h4
latexFromXmlTree _ (NTree (XTag tag _) xs) | localPart tag == "h6" = withAbbrevs $ \mp stk -> execLaTeXM $ heading 6 (sequence_ (map (textell . runAbbrevsM' mp stk . latexFromXmlTree Inline) xs)) -- Same as h4
-- latexFromTag (localPart qname) (execLaTeXM $ sequence_ (map latexFromXmlTree mp stk xs))
-- tag with qualified name and list of attributes (inner node or leaf)
latexFromXmlTree _ x@(NTree (XTag _qname _xs) _ys) = return $ texy ("(Unexpected XTag: " <> T.pack (show x) <> ")")

latexFromXmlTree _ (NTree (XText string) []) = latexFromText (T.pack string)
latexFromXmlTree _ (NTree (XBlob blob) []) = latexFromText (decodeUtf8 (toStrict blob))         -- text represented more space efficient as (utf8?) bytestring (leaf)
latexFromXmlTree _ (NTree (XCharRef n) []) = latexFromText (T.singleton (chr n))
latexFromXmlTree _ (NTree (XEntityRef name) []) = return $ latexFromEntityRef name
latexFromXmlTree _ (NTree (XCmt _) _) = return mempty   -- comment (leaf)
-- FIXME: All these error should be logged
latexFromXmlTree _ x@(NTree (XDTD _dtdelem _attributes) _) = return $ texy ("(Unexpected: " <> T.pack (show x) <> ")")  -- DTD element with assoc list for dtd element features
latexFromXmlTree _ x@(NTree (XError _int _string) _) =       return $ texy ("(Unexpected: " <> T.pack (show x) <> ")")  -- error message with level and text
latexFromXmlTree _ x@(NTree (XCdata _string) _) =            return $ texy ("(Unexpected: " <> T.pack (show x) <> ")")  -- CDATA section (leaf)
latexFromXmlTree _ x@(NTree (XPi _qname _xmltrees) _) =      return $ texy ("(Unexpected: " <> T.pack (show x) <> ")")  -- Processing Instr with qualified name (leaf) with list of attributes. If tag name is xml, attributs are "version", "encoding", "standalone", else attribute list is empty, content is a text child node
latexFromXmlTree _ x@(NTree (XAttr _qname) _) =              return $ texy ("(Unexpected: " <> T.pack (show x) <> ")")  -- attribute with qualified name, the attribute value is stored in children
latexFromXmlTree _ x@(NTree _ _) = return $ texy ("(Unexpected NTree: " <> T.pack (show x) <> ")")

latexFromEntityRef :: String -> LaTeX
latexFromEntityRef name = execLaTeXM $ texy ("(Unexpected entity reference: " <> T.pack (show name) <> ")")

-- | Convert text found in an XText node into LaTeX, performing abbreviation expansion.
latexFromText :: MonadAbbrevs m => T.Text -> m LaTeX
latexFromText t =
    expandText
      (return . latexFromChars)
      (\tag -> return $ latexFromChars (markupText $ protectMarkdown $ "[" <> tag <> "]"))
      (\tag -> return $ latexFromChars (markupText $ protectMarkdown $ "[abbreviation loop: " <> tag <> "]"))
      (\markup -> latexFromMarkup (fixAbbreviationMarkup markup)) -- markup from an expanded tag
      t
    where
      -- Strip off white space surrounding the abbreviation.
      fixAbbreviationMarkup :: Markup -> Markup
      fixAbbreviationMarkup = foldMarkup (rawMarkdown . T.strip) rawHtml rawLaTeX rawPandoc rawMarkup

latexFromChars :: T.Text -> LaTeX
latexFromChars =
    mconcat . map protectCJK . T.groupBy (\ a b -> isCJK a == isCJK b) . protectText . latexFromUnicode
    where
      protectCJK :: T.Text -> LaTeX
      protectCJK text =
          case T.uncons text of
            Nothing -> mempty
            Just (c, _) | isCJK c -> execLaTeXM $ comm0 "makexeCJKactive" >> raw text >> comm0 "makexeCJKinactive"
            Just _ -> execLaTeXM $ raw text

latexFromUnicode :: T.Text -> T.Text
latexFromUnicode t =
    -- (\ r -> trace ("latexFromUnicode:\n old=" ++ show t ++ "\n new=" ++ show r) r) $
    f (T.uncons t)
    where
      f Nothing = T.empty
      f (Just (c, cs)) =
          (case c of
             -- '‘' -> "`"
             -- '’' -> "'"
             -- '“' -> "``"
             -- '”' -> "''"
             '…' -> "..."
             c -> T.singleton c) `T.append` f (T.uncons cs)

-- Belongs elsewhere
latexFromMarkup :: MonadAbbrevs m => Markup -> m LaTeX
latexFromMarkup markup =
    foldMarkup
      -- Slow - pandoc markdown parser uses String rather than Text
      (\markdown -> withAbbrevs $ \mp stk -> latexFromMarkdown mp stk markdown)
      -- Also slow, for the same reason
      (\html -> withAbbrevs $ \mp stk -> latexFromHtml mp stk html)
      (return . fromLaTeX)
      (\pandoc -> withAbbrevs $ \mp stk -> latexFromPandoc mp stk pandoc)
      (\ms -> mapM latexFromMarkup ms >>= return . mconcat)
      markup

-- | latexFromMarkup with a debugging trace.
-- Belongs elsewhere
latexFromMarkupTest :: MonadAbbrevs m => Markup -> m LaTeX
latexFromMarkupTest markup =
    -- latexFromMarkup (trace ("markup: " ++ show markup) markup)
    foldMarkup
      (\markdown -> withAbbrevs $ \mp stk -> latexFromXmlTrees mp stk . parseHtmlContent . T.unpack . T.pack . writeHtmlString def . pandocFromMarkdown $ markdown)
      (\html -> withAbbrevs $ \mp stk -> latexFromHtml mp stk html)
      (return . fromLaTeX)
      (\ pandoc -> withAbbrevs $ \mp stk -> latexFromXmlTrees mp stk . parseHtmlContent . T.unpack . T.pack . writeHtmlString def $ pandoc)
      (\ ms -> mapM latexFromMarkupTest ms >>= return . mconcat)
      markup
