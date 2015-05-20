{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Language.Haskell.TH.Path.Lens
            ( at
            , mat
            , eitherIso
            , readOnlyLens
            , readShowLens
            , lens_trace
            , lens_mapPair
            , lens_mapFirst
            , lens_mapSecond
            , lens_list
            , lens_Maybe_Monoid
            , lens_Monoid_Maybe
            -- , listLookupLens
            , lens_mrs
            , idLens
            , dummyLens
            , JSONText
            , iso_JSONText
            , gjsonIso
            , gjsonLens
            , textLens
            , IsText(textLens')
            , stringLens
            , lens_UserIds_Text
            ) where

import Debug.Trace

import Control.Category
import Control.Applicative.Error (maybeRead)
import Control.Lens (set, Traversal', Lens')
import Data.Generics (Typeable)
import Control.Lens (_Just, iso, lens, view)
import qualified Data.Map as M (Map, insert, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Happstack.Authenticate.Core (UserId(..))
import Prelude hiding (id, (.))
import Safe (readMay)
import Text.JSON.Generic (Data, decodeJSON, encodeJSON)
import Web.Routes.TH (derivePathInfo)

idLens :: Lens' a a
idLens = iso id id

dummyLens :: b -> Lens' a b
dummyLens v = lens (const v) const

-- Similar to an isomorphism, but fails silently.
-- next round of lenses will fix this.
readShowLens :: (Show a, Read a) => Lens' a String
readShowLens = lens show (\r v ->
                           case maybeRead v of
                             Nothing -> r
                             Just r' -> r')


lens_mrs :: (Show a, Read a) => Lens' (Maybe a) String
lens_mrs = lens getter setter
  where getter Nothing = ""
        getter (Just x) = show x
        setter _ x =
          case reads x of
            [(b,_)] -> Just b
            _ -> Nothing

readOnlyLens :: Lens' a a
readOnlyLens = iso id (error "Lens.readOnlyLens: TROUBLE ignoring write to readOnlyLens")

mat :: forall k a. (Show k, Ord k) => k -> Traversal' (M.Map k a) a
mat k = lens (M.lookup k) (\ mp ma -> maybe mp (\ a -> M.insert k a mp) ma) . _Just

-- A list lens
at :: Integral i => i -> Traversal' [a] a
at ii = lens getter setter . _Just
    where getter :: [a] -> Maybe a
          getter = nth ii
          setter :: [a] -> Maybe a -> [a]
          setter = replace ii

nth :: Integral i => i -> [a] -> Maybe a
nth n _ | n < 0 = Nothing
nth _ [] = Nothing
nth 0 (x : _) = Just x
nth n (_ : xs) = nth (pred n) xs

replace :: Integral i => i -> [b] -> Maybe b -> [b]
replace 0 (_ : xs) (Just x) = x : xs
replace n (_ : xs) x | n > 0 = replace (pred n) xs x
replace _ xs _ = xs

lens_trace :: Show a => String -> Lens' a a
lens_trace s =
    iso getter setter
    where
      getter a = trace ("lens_trace " ++ s ++ " - get " ++ show a) a
      setter b = trace ("lens_trace " ++ s ++ " - set " ++ show b) b

eitherIso :: (a -> Bool) -> Lens' a (Either a a)
eitherIso p = iso (split p) merge
  where split f x | f x = Right x
                  | otherwise = Left x
        merge (Right x) = x
        merge (Left x) = x

lens_mapPair :: forall a b c d. Lens' a c -> Lens' b d -> Lens' (a, b) (c, d)
lens_mapPair lensAC lensBD =
    lens g s
    where
      g :: (a, b) -> (c, d)
      g (a, b) = (view lensAC a, view lensBD b)
      s :: (a, b) -> (c, d) -> (a, b)
      s (a, b) (c, d) = (set lensAC c a, set lensBD d b)

lens_mapFirst :: Lens' a b -> Lens' (a, c) (b, c)
lens_mapFirst l = lens_mapPair l (iso id id)

lens_mapSecond :: Lens' b c -> Lens' (a, b) (a, c)
lens_mapSecond l = lens_mapPair (iso id id) l

-- | Create a list lens from an element lens.  For the setter, extra
-- elements in the C list cause an error (unless elens is an iso.)
-- Extra elements in the B list are ignored.
lens_list :: forall b c. b -> Lens' b c -> Lens' [b] [c]
lens_list def elens =
    lens getter setter
    where
      getter :: [b] -> [c]
      getter bs = map (view elens) bs
      setter :: [b] -> [c] -> [b]
      setter bs cs = map
                       (\ (c, b) -> set elens c b)
                       (zip cs (bs ++ repeat def))

lens_Maybe_Monoid :: (Eq a, Monoid a) => Lens' (Maybe a) a
lens_Maybe_Monoid = lens (maybe mempty id) (\_ v -> if v == mempty then Nothing else Just v)

lens_Monoid_Maybe :: (Eq a, Monoid a) => Lens' a (Maybe a)
lens_Monoid_Maybe = lens (\a -> if a == mempty then Nothing else Just a)
                    (\_ v -> case v of
                               Nothing -> mempty
                               Just z -> z)

_lens_Maybe_Monoid_Tests :: [Bool]
_lens_Maybe_Monoid_Tests = [ Just [i 1,2,3]  == (set lens_Maybe_Monoid [1,2,3] $ (Just [1,2]))
                          , Nothing == (set lens_Maybe_Monoid [] $ (Just [i 1,2]))
                          , Just (Sum $ i 2) == (set lens_Maybe_Monoid (Sum 2) $ (Just (Sum 1)))
                          , Nothing == (set lens_Maybe_Monoid (Sum $ i 0) $ (Just (Sum 1)))
                          ]
  where i :: Int -> Int
        i = id

_lens_Monoid_Maybe_Tests :: [Bool]
_lens_Monoid_Maybe_Tests = [ [i 1,2,3] == ((set lens_Monoid_Maybe (Just [1,2,3])) [1,2])
                          , [] == (set lens_Monoid_Maybe Nothing [i 1,2])
                          ]
  where i :: Int -> Int
        i = id

newtype JSONText = JSONText {unJSONText :: String} deriving (Eq, Ord, Read, Show, Data, Typeable, Monoid)

iso_JSONText :: Lens' JSONText String
iso_JSONText = iso unJSONText JSONText

$(derivePathInfo ''JSONText)
$(deriveSafeCopy 1 'base ''JSONText)

gjsonIso :: Data a => Lens' a JSONText
gjsonIso = iso (JSONText . encodeJSON) (decodeJSON . unJSONText)

gjsonLens :: Data a => Lens' a JSONText
gjsonLens = gjsonIso

textLens :: Lens' Text Text
textLens = iso id id

class IsText a where
    textLens' :: Lens' a Text
    stringLens :: IsText a => Lens' a String
    stringLens = textLens' . iso unpack pack

lens_UserIds_Text :: Lens' [UserId] Text
lens_UserIds_Text = iso (encode') (decode')
    where
      decode' :: Text -> [UserId]
      decode' t =
          catMaybes . map readId . Text.words $ t
          where readId :: Text -> Maybe UserId
                readId = fmap UserId . readMay . unpack

      encode' :: [UserId] -> Text
      encode' uids =
          Text.unwords . map showId $ uids
          where showId = Text.pack . show . _unUserId
