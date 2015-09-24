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
