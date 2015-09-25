-- | Type wrapper that causes the template haskell pretty printer to
-- omit module prefixes of names and render each declarations on a
-- single line.  (See also Debian.Pretty in debian-haskell package.)
--
--  * display is now prettyShow
--  * display' is now prettyText
--  * ppDisplay is now ppShow
--  * ppDisplay' is now ppText
{-# LANGUAGE DeriveFunctor, FlexibleContexts #-}
module Appraisal.Pretty
    ( PP(PP, unPP)
    , prettyShow
    , ppPrint
    , ppShow
    ) where

import Text.PrettyPrint.HughesPJClass (Doc, Pretty(pPrint), prettyShow)

-- | If you want to write a Pretty instance for a but the built in
-- instances are interfering, wrap it in a PP and write an instance
-- for PP a.
newtype PP a = PP {unPP :: a} deriving (Functor)

ppPrint :: Pretty (PP a) => a -> Doc
ppPrint = pPrint . PP

ppShow :: Pretty (PP a) => a -> String
ppShow = prettyShow . PP
