{-# LANGUAGE FlexibleInstances, RankNTypes, ScopedTypeVariables, TypeSynonymInstances #-}
module Common where

import Control.Monad (MonadPlus, msum)
import Data.Generics (Data, everywhere, extT, listify, mkT, Typeable)
import Data.Maybe (catMaybes, isJust)
import Data.Set as Set (Set, fromList)
import Language.Haskell.TH.Path.Graph (SinkType)
import Language.Haskell.TH.Syntax

instance SinkType String
instance SinkType Rational
instance SinkType VarStrictType

-- | Make a template haskell value more human reader friendly, and
-- remove extensions which are different on each run to allow building
-- of test cases.  The result almost certainly won't be compilable,
-- but then again neither was the input.
stripNames :: forall a. Data a => a -> a
stripNames = everywhere (mkT f)
    where
      -- Remove all module qualifiers
      f (Name x _) = Name x NameS

-- | Turn lists of Char into strings:
--      ListE [LitE (CharL 'a'), LitE (CharL 'b'), ...] -> LitE (StringL "ab...")
fixStringLits :: forall a. Data a => a -> a
fixStringLits = everywhere (mkT f)
    where
      f e@(ListE lits) =
          let cs = map litChar lits in
          if all isJust cs then LitE (StringL (catMaybes cs)) else e
      f e = e
      litChar :: Exp -> Maybe Char
      litChar (LitE (CharL c)) = Just c
      litChar _ = Nothing

gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)

depFiles =
    mapM_ addDependentFile [
                       "th-typegraph/Language/Haskell/TH/TypeGraph/Edges.hs",
                       "th-typegraph/Language/Haskell/TH/TypeGraph/TypeGraph.hs",
                       "Language/Haskell/TH/Path/Core.hs",
                       "Language/Haskell/TH/Path/Graph.hs",
                       "Language/Haskell/TH/Path/Instances.hs",
                       "Language/Haskell/TH/Path/Lens.hs",
                       "Language/Haskell/TH/Path/Order.hs",
                       "Language/Haskell/TH/Path/PathType.hs",
                       "Language/Haskell/TH/Path/Types.hs",
                       "Language/Haskell/TH/Path/View.hs",
                       "tests/Appraisal/Abbrevs.hs",
                       "tests/Appraisal/Config.hs",
                       "tests/Appraisal/Currency.hs",
                       "tests/Appraisal/IntJS.hs",
                       "tests/Appraisal/LaTeX.hs",
                       "tests/Appraisal/LaTeX/Margins.hs",
                       "tests/Appraisal/Markdown.hs",
                       "tests/Appraisal/Markup.hs",
                       "tests/Appraisal/Permissions.hs",
                       "tests/Appraisal/Report.hs",
                       "tests/Appraisal/ReportImage.hs",
                       "tests/Appraisal/ReportItem.hs",
                       "tests/Appraisal/ReportMap.hs",
                       "tests/Appraisal/ReportPathInfo.hs",
                       "tests/Appraisal/Unicode.hs",
                       "tests/Appraisal/Utils/CIString.hs",
                       "tests/Appraisal/Utils/Debug.hs",
                       "tests/Appraisal/Utils/IsText.hs",
                       "tests/Appraisal/Utils/List.hs",
                       "tests/Appraisal/Utils/Pandoc.hs",
                       "tests/Appraisal/Utils/Text.hs",
                       "tests/Common.hs"
              ]
