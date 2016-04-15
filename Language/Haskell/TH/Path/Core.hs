{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Haskell.TH.Path.Core
    ( treeMap
    , forestMap
    , camelWords

      -- * Type classes and associated types
    , IsPath(UType, SType, idPath)
    , PathStart(UPeek, upeekCons, upeekPath, upeekValue, UPath, upaths, upeekRow, upeekTree)
    , liftPeek
    , ToLens(toLens)
    , ulens'
    -- , (:.:)(..)
    , U(u, unU')

    -- * Hint classes
    , SinkType
    , HideType
    , SelfPath
    , Describe(describe')
    , describe

    -- * Basic Path Types
    , Path_Pair(..)
    , lens_mapPair
    , lens_mapFirst
    , lens_mapSecond
    , Path_Maybe(..)
    , lens_Maybe_Monoid
    , lens_Monoid_Maybe
    , Path_Map(..)
    , mat
    , Path_List(..)
    , at
    , lens_list
    , Path_Either(..)
    , eitherIso
    , Path_View(..)

    -- * Basic lenses
    , readOnlyLens
    , readShowLens
    , readShowIso
    , lens_trace
    -- , listLookupLens
    , lens_mrs
    , idLens
    , dummyLens
    , textLens
    , IsText(textLens')
    , stringLens
    , lens_UserIds_Text
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Lens hiding (at) -- (set, Traversal', Lens', _Just, iso, lens, view, view)
import Data.Char (isUpper, toUpper)
import Data.Generics (Data, Typeable)
import Data.List as List (groupBy, map)
import Data.Map as Map (Map, insert, lookup)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.Proxy
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Data.Tree (Tree(..), Forest)
import Data.UserId (UserId(..))
import Debug.Trace (trace)
import Language.Haskell.TH.Instances ()
#if !__GHCJS__
import Language.Haskell.TH.Path.Instances ()
#endif
import Prelude hiding (exp)
import Safe (readMay)
import Web.Routes
import Web.Routes.TH (derivePathInfo)
import Text.Parsec.Prim ((<|>))
import GHC.Base (ap)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node x ns) = Node (f x) (forestMap f ns)

forestMap :: (a -> b) -> Forest a -> Forest b
forestMap f = List.map (treeMap f)

-- | Convert a camel case string (no whitespace) into a natural
-- language looking phrase:
--   camelWords "aCamelCaseFOObar123" -> "A Camel Case FOObar123"
camelWords :: String -> String
camelWords s =
    case groupBy (\ a b -> isUpper a == isUpper b) (dropWhile (== '_') s) of -- "aCamelCaseFOObar123"
      (x : xs) -> concat $ capitalize x : map (\ (c : cs) -> if isUpper c then ' ' : c : cs else c : cs) xs
      [] -> ""

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c) : cs

class U univ a where
    u :: a -> univ
    unU' :: univ -> Maybe a

-- | Every path type must have an identity value, such that 'toLens'
-- 'idPath' is just 'id'.
class (Eq p, Ord p, Read p, Show p, Data p, Typeable p) => IsPath p where
    type UType p
    type SType p
    idPath :: p -- ^ The identity value for path type @p@.  Obeys the law
                -- @toLens idPath == iso id id@.

-- | Every path has start and end types and can be converted to a lens.
class ToLens u s where
#if 1
    toLens :: UPath u s -> Traversal' s u
#else
    toTraversal :: UPath u s -> Traversal' s u -- If you don't know what you have use this
    toLens :: UPath u s -> Lens' s u
    toIso :: UPath u s -> Iso' s u
#endif

-- | Return a lens that converts between a universal type value and some @a@.
-- Once you have generated your own Univ type create a specialized version:
-- @
--   ulens :: U Univ a => Iso' Univ a
--   ulens = ulens' Proxy
-- @
-- Then you can compose paths by interspersing ulens between toLens calls.
ulens' :: (U u a) => Proxy u -> Iso' u a
ulens' Proxy = iso (fromJust . unU') u

{-
data (f :.: g) = f :.: g deriving (Eq, Generic, Read, Show)

instance (ToLens f, ToLens g, A f ~ S g {-, B f ~ T g-}) => ToLens (f :.: g) where
  type S (f :.: g) = S f
  -- type T (f :.: g) = T f
  type A (f :.: g) = A g
  -- type B (f :.: g) = B g
  toLens (f :.: g) = toLens f . toLens g
  -- ^ Function to turn a path value of type @p@ into a lens to access
  -- (one of) the @A p@ values in an @S p@.
-}

-- | The term Path used here means a path from a type which proceeds
-- to smaller and smaller components of that type - e.g. from a record
-- to a field of that record, then to an element of a list contained
-- in that field, and so on.  The path proceeds from node to node,
-- where each node is represented by the second type parameter of
-- PathStart, @s@.  The @u@ type parameter is a wrapper type that has
-- a constructor for every @s@ we are allowed to use as a path node.
class (U u s, IsPath (UPath u s)) => PathStart u s where
    type UPath u s
    -- ^ The type @UPath u s@ represents a the beginning of any path
    -- starting at @s@.

    -- It would be nice to make this a data instead of a type synonym,
    -- but some UPath types are already defined - e.g. Path_Pair.
    -- This also means it is impossible to say something like
    -- @instance Describe (UPath Univ Int64)@, we need to find the
    -- actual type.
    upaths :: Proxy u -> (UPath u s -> r -> r) -> r -> s -> r
    -- ^ UPath version of 'paths'
    -- upathRow :: Proxy u -> s -> [UPath u s]
    -- ^ Return the immediate subpaths of s.
    -- upathTree :: Proxy u -> s -> Tree (UPath u s)
    -- ^ Return a tree containing all subpaths of s
    upeekTree :: Proxy u -> s -> Tree (UPeek u s)
    -- ^ Given a value of type @s@, return a tree containing every
    -- 'Peek' that can be reached from it.  The order of the nodes in
    -- the forest reflects the order the elements were encountered
    -- during the traversal.
    upeekRow :: Proxy u -> s -> Tree (UPeek u s)
    -- ^ In this function only one layer of the tree is returned, no
    -- recursive peek calls are made.

    data UPeek u s
    -- ^ 'UPath' version of 'Peek'.
    upeekCons :: UPath u s -> Maybe u -> UPeek u s
    -- ^ Construct a UPeek u s
    upeekPath :: UPeek u s -> UPath u s
    -- ^ Accessor for path field of a Peek type
    upeekValue :: UPeek u s -> Maybe u
    -- ^ Accessor for value field of a Peek type

liftPeek :: (PathStart u s, PathStart u t) => (UPath u s -> UPath u t) -> UPeek u s -> UPeek u t
liftPeek f pk = upeekCons (f (upeekPath pk)) (upeekValue pk)

-- | Nodes along a path can be customized by declaring types to be
-- instances of this class and the ones that follow.  If a type is an
-- instance of 'SinkType', no paths that lead to the internal stucture
-- of the value will be created - the value is considered atomic.
class SinkType a

-- | Like SinkType, but no paths out or into the type will be created.
class HideType a

-- | Types for which
-- a 'SelfPath' instance is declared will be used as their own Path
-- Type.  For example, a UUID or some enumerated type contained in a
-- record could be used directly to reference the object that contains
-- it.
class SelfPath a

-- | Override the default description associated with the type of @a@.
-- The first argument indicates the field of the parent record that
-- contains the @a@ value, if any.
class Describe a where
    describe' :: Maybe String -> a -> Maybe String

describe :: Describe a => a -> Maybe String
describe = describe' Nothing

-- Primitive path types

-- | A path type with constructors to extract either @fst@, @snd@, or
-- the pair itself.
data Path_Pair fstpath sndpath = Path_First fstpath | Path_Second sndpath | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Either leftpath rightpath = Path_Left leftpath | Path_Right rightpath | Path_Either deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Maybe justpath = Path_Just justpath | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Map key valuepath = Path_Look key valuepath | Path_Map deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data) -- No element lookup path - too dangerous, use OMap
-- | A view from a type @s@ to @SType viewpath@.  The Proxy value
-- indicates that we don't need to know anything about a particular
-- @s@ to build or use a @View_Path@.
data Path_View s viewpath = Path_To (Proxy s) viewpath | Path_Self deriving (Eq, Ord, Read, Show, Typeable, Data)

instance (IsPath fstpath, IsPath sndpath, UType fstpath ~ UType sndpath) => IsPath (Path_Pair fstpath sndpath) where
    type UType (Path_Pair fstpath sndpath) = UType fstpath
    type SType (Path_Pair fstpath sndpath) = (SType fstpath, SType sndpath)
    idPath = Path_Pair
instance (IsPath justpath) => IsPath (Path_Maybe justpath) where
    type UType (Path_Maybe justpath) = UType justpath
    type SType (Path_Maybe justpath) = Maybe (SType justpath)
    idPath = Path_Maybe
instance (IsPath leftpath, IsPath rightpath, UType leftpath ~ UType rightpath) => IsPath (Path_Either leftpath rightpath) where
    type UType (Path_Either leftpath rightpath) = UType leftpath
    type SType (Path_Either leftpath rightpath) = Either (SType leftpath) (SType rightpath)
    idPath = Path_Either
instance (Data key, Typeable key, Ord key, Read key, Show key, IsPath valuepath) => IsPath (Path_Map key valuepath) where
    type UType (Path_Map key valuepath) = UType valuepath
    type SType (Path_Map key valuepath) = Map key (SType valuepath)
    idPath = Path_Map
instance (IsPath elttype) => IsPath (Path_List elttype) where
    type UType (Path_List elttype) = UType elttype
    type SType (Path_List elttype) = [SType elttype]
    idPath = Path_List
instance (Data s, IsPath viewpath) => IsPath (Path_View s viewpath) where
    type UType (Path_View s viewpath) = UType viewpath
    type SType (Path_View s viewpath) = s
    idPath = Path_Self

#if !__GHCJS__
$(derivePathInfo ''Path_Pair)
$(derivePathInfo ''Path_List)
$(derivePathInfo ''Path_Map)
$(derivePathInfo ''Path_Either)
$(derivePathInfo ''Path_Maybe)
#if 0
$(derivePathInfo ''Path_View)
#else
instance ({-PathInfo s,-} PathInfo viewpath) => PathInfo (Path_View s viewpath) where
      toPathSegments inp
        = case inp of
            Path_To arg_a15Lg arg_a15Lh
              -> (++)
                   [pack "path_-to"]
                   ((++)
                      (toPathSegments arg_a15Lg)
                      (toPathSegments arg_a15Lh))
            Path_Self -> [pack "path_-self"]
      fromPathSegments
        = (Text.Parsec.Prim.<|>)
            (ap
               (ap
                  ((segment (pack "path_-to"))
                   >> (return Path_To))
                  fromPathSegments)
               fromPathSegments)
            ((segment (pack "path_-self"))
             >> (return Path_Self))
#endif

$(deriveSafeCopy 0 'base ''Path_Pair)
$(deriveSafeCopy 0 'base ''Path_List)
$(deriveSafeCopy 0 'base ''Path_Map)
$(deriveSafeCopy 0 'base ''Path_Either)
$(deriveSafeCopy 0 'base ''Path_Maybe)
$(deriveSafeCopy 0 'base ''Path_View)
#endif

instance (IsPath (Path_Either a b), Describe a, Describe b, Describe (Proxy (SType (Path_Either a b)))) => Describe (Path_Either a b)
    where describe' _f (_p@(Path_Left _wp)) = maybe (describe' _f (Proxy :: Proxy (SType (Path_Either a b)))) Just (describe' Nothing _wp)
          describe' _f (_p@(Path_Right _wp)) = maybe (describe' _f (Proxy :: Proxy (SType (Path_Either a b)))) Just (describe' Nothing _wp)
          describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_Either a b)))
          describe' _ p = error ("Unexpected " ++ (" path: " ++ show p))

instance (IsPath (Path_Pair a b), Describe a, Describe b, Describe (Proxy (SType (Path_Pair a b)))) => Describe (Path_Pair a b)
    where describe' _f (_p@(Path_First _wp)) = maybe (describe' _f (Proxy :: Proxy (SType (Path_Pair a b)))) Just (describe' Nothing _wp)
          describe' _f (_p@(Path_Second _wp)) = maybe (describe' _f (Proxy :: Proxy (SType (Path_Pair a b)))) Just (describe' Nothing _wp)
          describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_Pair a b)))
          describe' _ p = error ("Unexpected path: " ++ show p)

instance (IsPath (Path_Map k v), Describe v, Describe (Proxy (SType (Path_Map k v)))) => Describe (Path_Map k v)
    where describe' _f (Path_Look _ _wp) = maybe (describe' _f (Proxy :: Proxy (SType (Path_Map k v)))) Just (describe' Nothing _wp)
          describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_Map k v)))
          describe' _ p = error ("Unexpected path: " ++ show p)

instance (IsPath (Path_Maybe a), Describe a, Describe (Proxy (SType (Path_Maybe a)))) => Describe (Path_Maybe a)
    where describe' _f (Path_Just _wp) = maybe (describe' _f (Proxy :: Proxy (SType (Path_Maybe a)))) Just (describe' Nothing _wp)
          describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_Maybe a)))
          describe' _ p = error ("Unexpected path: " ++ show p)

instance (IsPath (Path_View s viewpath), Describe (Proxy s),
          IsPath viewpath, Describe viewpath, Describe (Proxy (SType viewpath))) => Describe (Path_View s viewpath)
    where describe' _f (_p@(Path_To Proxy _wp)) =
              maybe (describe' _f (Proxy :: Proxy (SType viewpath)))
                    Just
                    (describe' Nothing _wp)
          describe' f Path_Self = describe' f (Proxy :: Proxy s)

idLens :: Lens' a a
idLens = id

dummyLens :: b -> Lens' a b
dummyLens v = lens (const v) const

-- | A lens that assumes usable round trip Read/Show instances for
-- a.  Similar to an isomorphism, but fails silently.
readShowLens :: (Show a, Read a) => Lens' a String
readShowLens = lens show (\r v ->
                           case maybeRead v of
                             Nothing -> r
                             Just r' -> r')

-- | A readShowLens that really is an Iso - if the read fails the
-- default value is used.
readShowIso :: (Show a, Read a) => a -> Iso' a String
readShowIso def = iso show (\v ->
                                case maybeRead v of
                                  Nothing -> def
                                  Just r' -> r')

-- | A lens for 'Maybe' values whose getter turns Nothing into the
-- empty string and whose setter returns Nothing whenever read fails.
lens_mrs :: (Show a, Read a) => Iso' (Maybe a) String
lens_mrs = iso getter setter
  where getter Nothing = ""
        getter (Just x) = show x
        setter x = maybeRead x

readOnlyLens :: Iso' a a
readOnlyLens = iso id (error "Lens.readOnlyLens: TROUBLE ignoring write to readOnlyLens")

mat :: forall k a. (Show k, Ord k) => k -> Traversal' (Map k a) a
mat k = lens (Map.lookup k) (\ mp ma -> maybe mp (\ a -> Map.insert k a mp) ma) . _Just

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

lens_trace :: Show a => String -> Iso' a a
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
lens_mapFirst l = lens_mapPair l id

lens_mapSecond :: Lens' b c -> Lens' (a, b) (a, c)
lens_mapSecond l = lens_mapPair id l

-- | Create a list lens from an element lens.  For the setter, extra
-- elements in the C list cause an error (unless elens is an iso.)
-- Extra elements in the B list are ignored.
lens_list :: forall b c. b -> Lens' b c -> Lens' [b] [c]
lens_list def' elens =
    lens getter setter
    where
      getter :: [b] -> [c]
      getter bs = List.map (view elens) bs
      setter :: [b] -> [c] -> [b]
      setter bs cs = List.map
                       (\ (c, b) -> set elens c b)
                       (zip cs (bs ++ repeat def'))

lens_Maybe_Monoid :: (Eq a, Monoid a) => Iso' (Maybe a) a
lens_Maybe_Monoid = iso (maybe mempty id) (\v -> if v == mempty then Nothing else Just v)

lens_Monoid_Maybe :: (Eq a, Monoid a) => Iso' a (Maybe a)
lens_Monoid_Maybe = iso (\a -> if a == mempty then Nothing else Just a)
                        (\v -> case v of
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

textLens :: Iso' Text Text
textLens = id

class IsText a where
    textLens' :: Iso' a Text
    stringLens :: IsText a => Iso' a String
    stringLens = textLens' . iso unpack pack

lens_UserIds_Text :: Iso' [UserId] Text
lens_UserIds_Text = iso (encode') (decode')
    where
      decode' :: Text -> [UserId]
      decode' t =
          catMaybes . List.map readId . Text.words $ t
          where readId :: Text -> Maybe UserId
                readId = fmap UserId . readMay . unpack

      encode' :: [UserId] -> Text
      encode' uids =
          Text.unwords . List.map showId $ uids
          where showId = Text.pack . show . _unUserId
