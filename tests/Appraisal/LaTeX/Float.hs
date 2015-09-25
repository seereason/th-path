-- | CTAN float package version 1.3d (2001-11-08)
-- http://www.ctan.org/tex-archive/macros/latex/contrib/float
{-# LANGUAGE CPP, OverloadedStrings #-}
module Appraisal.LaTeX.Float
    ( ListEntry(..)
    , float
    , FloatPlacement(..)
    , FloatPlacementHint(..)
    , FloatStyle(..)
    , floatstyle
    , figure
    , subfigure
    , caption
    ) where

import Data.Set (Set, toList)
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Render
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Types
import Prelude

float :: PackageName
float = "float"

data FloatPlacement = FloatPlacementHints (Set FloatPlacementHint) | HereDefinitely deriving (Show, Eq, Ord)

data FloatPlacementHint = TopOfThePage | BottomOfThePage | PageOfFloats | HereIfPossible deriving (Show, Eq, Ord)

instance Render FloatPlacement where
 render (FloatPlacementHints s) = mconcat $ map render (toList s)
 render HereDefinitely = "H"

instance Render FloatPlacementHint where
 render TopOfThePage = "t"
 render BottomOfThePage = "b"
 render PageOfFloats = "p"
 render HereIfPossible = "h"

data FloatStyle = Plain | PlainTop | Boxed | Ruled deriving Show

instance Render FloatStyle where
    render Plain = "plain"
    render PlainTop = "plaintop"
    render Boxed = "boxed"
    render Ruled = "ruled"

floatstyle :: LaTeXC l => FloatStyle -> l
floatstyle style = fromLaTeX (TeXComm "floatstyle" [FixArg $ TeXRaw $ render style])

{-
floatname ::
floatplacement ::
restylefloat ::
listof ::
-}

-- | Figure environment.
figure :: LaTeXC l =>
          Maybe FloatPlacement -- ^ Optional position (really optional?)
       -> l                   -- ^ Figure content
       -> l
figure Nothing  = liftL $ TeXEnv "figure" []
figure (Just p) = liftL $ TeXEnv "figure" [ OptArg $ TeXRaw $ render p ]

subfigure :: LaTeXC l =>
             Measure
          -> l
          -> l
subfigure m = liftL $ TeXEnv "subfigure" [ FixArg $ TeXRaw $ render m ]

data ListEntry
    = ListEntry deriving Show

instance Render ListEntry where
    render ListEntry = "foo"

caption :: LaTeXC l => Maybe ListEntry -> l -> l
caption listEntry =
    liftL $ \ heading -> TeXComm "caption" (maybe [] ((: []) . OptArg . TeXRaw . render) listEntry ++ [FixArg heading])

#if UNUSED
test0 = execLaTeXT centering >>= putStrLn . unpack . render
test1 = execLaTeXT (floatstyle Ruled) >>= putStrLn . unpack . render
test2 = execLaTeXT (figure (Just (FloatPlacementHints (fromList [BottomOfThePage, PageOfFloats]))) (floatstyle Ruled <> raw "foo")) >>= putStrLn . unpack . render
test4 = execLaTeXT (figure (Just HereDefinitely) (centering <> raw "includegraphics" <> comment mempty)) >>= putStrLn . unpack . render
#endif
