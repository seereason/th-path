{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TupleSections, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-orphans #-}
module Appraisal.LaTeX.Tables
    ( ColumnSpec(..)
    , appraisedItemSummary
    , wideTable
    , table
    ) where

import Appraisal.LaTeX (center, texty, nbsp)
import Appraisal.Utils.List (ifEmpty)
import Data.List as L (intersperse)
import Data.Monoid ((<>))
import qualified Data.Text as T (Text)
import Text.LaTeX (raw, lnbk)
import Text.LaTeX.Base.Class (comm0, commS, LaTeXC, liftL)
import Text.LaTeX.Base.Commands as LaTeX (hline, multicolumn, (&))
import Text.LaTeX.Base.Render (renderAppend)
import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv))
import Text.LaTeX.Base.Syntax as LaTeX (LaTeX(..), TeXArg(..))
import Text.LaTeX.Base.Types (TableSpec(..))
import Text.LaTeX.Base.Writer (LaTeXT)

data ColumnSpec
    = ColumnSpec
      { columnFormat :: TableSpec
      , columnHeader :: T.Text
      } deriving Show

table :: Monad m => [TableSpec] -> [[LaTeXT m ()]] -> LaTeXT m ()
table specs rows =
    longtable specs $ do
      nl
      ifEmpty (texty "(no rows)") (sequence_ . map (\ cols -> foldl1 (&) cols >> lnbk >> nl >> nl)) rows
      nl

longtable :: LaTeXC l => [TableSpec] -> l -> l
longtable cols = liftL $ TeXEnv "longtable" [ FixArg $ TeXRaw $ renderAppend cols ]

-- | Build a table summarizing the items in the appraisal.  It may be
-- long enough to cross page boundries.  All the rows will contain the
-- same number of columns, the number of which is either four or five.
-- If five, the extra column is in position 2 and contains the item
-- name.
-- FIXME: This contains business logic
appraisedItemSummary :: Monad m => T.Text -> [ColumnSpec] -> [[LaTeXT m ()]] -> (LaTeXT m (), LaTeXT m ()) -> LaTeXT m ()
appraisedItemSummary caption cols rows (footLabel, footValue) =
    center $ longtable ([VerticalLine] ++ intersperse VerticalLine (map columnFormat cols) ++ [VerticalLine]) $
    do  let hasItemNameColumn = length cols == 5
        hline >> nl >> ((if hasItemNameColumn then (nbsp & "Item") else nbsp) & "Artist" & "Type of Object" & "Amount") >> lnbk >> hline
        endfirsthead >> nl

        multicolumn (length cols) [CenterColumn] (bfseries >> texty (caption <> " (continued)")) >> lnbk >> hline >> nl
        ((if hasItemNameColumn then (nbsp & "Item") else nbsp) & "Artist" & "Type of Object" & "Amount") >> lnbk >> hline
        endhead >> nl

        hline >> multicolumn (length cols) [VerticalLine, RightColumn, VerticalLine] (texty "Continued on next page") >> lnbk >> hline
        endfoot >> nl

        hline >> (multicolumn (length cols - 1) [VerticalLine, LeftColumn, VerticalLine] (bfseries >> footLabel) &
                  multicolumn 1 [VerticalLine, RightColumn, VerticalLine] footValue) >> lnbk >> hline
        endlastfoot >> nl

        nl
        ifEmpty (texty "(no rows)") (sequence_ . map (\ cols' -> sequence_ (L.intersperse (raw " & ") cols') <> lnbk >> nl >> nl)) rows

nl :: LaTeXC l => l
nl = raw "\n"

wideTable :: Monad m => [T.Text] -> [[LaTeXT m ()]] -> LaTeXT m ()
wideTable specs rows =
    do raw "\\begin{tabular*}{\\textwidth}{"
       sequence_ (L.intersperse (raw "@{\\extracolsep{\\fill}}") (map raw specs))
       raw "}\n"
       ifEmpty (raw "(no rows)") (sequence_ . map (\ cols -> sequence_ (L.intersperse (raw " & ") cols) >> lnbk >> nl)) rows
       raw ("\\end{tabular*}\n")

endhead :: LaTeXC l => l
endhead = commS "endhead"
endfirsthead :: LaTeXC l => l
endfirsthead = commS "endfirsthead"
endfoot :: LaTeXC l => l
endfoot = commS "endfoot"
endlastfoot :: LaTeXC l => l
endlastfoot = commS "endlastfoot"
bfseries :: LaTeXC l => l
bfseries = comm0 "bfseries"
