{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
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
    , IsPath(UType, SType, idPath, toLens)
    , PathStart(UPeek, upeekCons, upeekPath, upeekValue, UPath, upeekRow, upeekTree, upeekCol)
    , makeRow
    , makeTrees
    , makeCol
    , makePeek
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
    , Maybe'(Just', Nothing')
    , lens_mrs'
    , idLens
    , dummyLens
    , textLens
    , IsText(textLens')
    , stringLens
    , lens_UserIds_Text
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Lens hiding (at) -- (set, Traversal', Lens', _Just, iso, lens, view, view)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isUpper, toUpper)
import Data.Generics (Data, Typeable)
import Data.List as List (groupBy, map)
import Data.Map as Map (Map, insert, lookup, toList)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.Proxy
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Data.Tree (Tree(..), Forest)
import Data.UserId (UserId(..))
import Debug.Trace (trace)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (deriveLiftMany)
#if !__GHCJS__
import Language.Haskell.TH.Path.Instances ()
#endif
import Language.Haskell.TH.Path.View (View(ViewType), viewLens)
import Prelude hiding (exp)
import Safe (readMay)
import Web.Routes
import Web.Routes.TH (derivePathInfo)
import Text.Parsec.Prim ((<|>))
import GHC.Base (ap)

deriving instance FromJSON (Proxy a)
deriving instance ToJSON (Proxy a)

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
    toLens :: p -> Traversal' (SType p) (UType p)

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
class (U u s, u ~ UType (UPath u s), s ~ SType (UPath u s), IsPath (UPath u s)) => PathStart u s where
    type UPath u s
    -- ^ The type @UPath u s@ represents a the beginning of any path
    -- starting at @s@.

    -- It would be nice to make UPath a data instead of a type synonym,
    -- but some UPath types are already defined - e.g. Path_Pair.
    -- This also means it is impossible to say something like
    -- @instance Describe (UPath Univ Int64)@, we need to find the
    -- actual type.

    data UPeek u s
    -- ^ A data type containing a path and (optionally) the value
    -- found at the end of that path.
    upeekPath :: UPeek u s -> UPath u s
    -- ^ Accessor for path field of a Peek type
    upeekValue :: UPeek u s -> Maybe u
    -- ^ Accessor for value field of a Peek type
    upeekCons :: UPath u s -> Maybe u -> UPeek u s
    -- ^ Construct a UPeek u s
    upeekTree :: Proxy u -> Maybe Int -> s -> Tree (UPeek u s)
    -- ^ Given a value of type @s@, return a tree containing every
    -- 'Peek' that can be reached from it.  The order of the nodes in
    -- the forest reflects the order the elements were encountered
    -- during the traversal.
    upeekRow :: Proxy u -> s -> Tree (UPeek u s)
    -- ^ In this function only one layer of the tree is returned, no
    -- recursive peek calls are made.
    upeekCol :: Proxy u -> UPath u s -> s -> Tree (UPeek u s)
    -- ^ In this function only the nodes along the given path appear
    -- in the result.  That is to say, the result is a degenerate tree
    -- with a single node at each level.
    -- @@
    --   upeekCol Proxy (UPath_ReportView__reportUUID idPath) report ->
    --      Node (UPeek_Report idPath Nothing)
    --           [Node (UPeek_Report (Path_To Proxy idPath) Nothing)
    --                 [Node (UPeek_Report (Path_To Proxy (UPath_ReportView__reportUUID idPath)) (Just uuid))
    --                       []]]
    -- @@

-- | Given a function that lifts a path by one hop (e.g. a constructor
-- such as Path_Left), return the peek(s?) resulting from traversing that hop.
makeRow :: forall s u p q a. (u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s,
                              u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a) =>
             s -> (q -> p) -> [Tree (UPeek u s)]
makeRow x f = forestMap (mapPeek f) (map makePeek (hopValues f x))

-- | Given a function that lifts a path by one hop (e.g. a constructor
-- such as Path_Left), return the peek(s?) resulting from traversing that hop.
makeTrees :: forall s u p q a. (u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s,
                                u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a) =>
             s -> (q -> p) -> [Tree (UPeek u s)]
makeTrees x f = forestMap (mapPeek f) (map (upeekTree Proxy Nothing) (hopValues f x))

-- | Helper function for implementing upeekCol
makeCol :: forall s u p q a. (u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s,
                              u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a) =>
           s -> (q -> p) -> (p -> q) -> p -> [Tree (UPeek u s)]
makeCol x f g p = forestMap (mapPeek f) (map (upeekCol Proxy (g p)) (hopValues f x))

mapPeek :: (PathStart u a, PathStart u s) => (UPath u a -> UPath u s) -> UPeek u a -> UPeek u s
mapPeek f pk = upeekCons (f (upeekPath pk)) (upeekValue pk)

makePeek :: forall u s. PathStart u s => s -> Tree (UPeek u s)
makePeek x = Node (upeekCons idPath (Just (u x))) []

-- | Given a hop function f, return a list values of type a which are one hop away from s.
hopValues :: forall s u p q a.
        (u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a,
         u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s) =>
        (q -> p) -> s -> [a]
hopValues f = toListOf (toLens (f idPath :: p) . ulens' (Proxy :: Proxy u))

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
data Path_Pair fstpath sndpath = Path_First fstpath | Path_Second sndpath | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data Path_Either leftpath rightpath = Path_Left leftpath | Path_Right rightpath | Path_Either deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data Path_Maybe justpath = Path_Just justpath | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data Path_Map key valuepath = Path_Look key valuepath | Path_Map deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON) -- No element lookup path - too dangerous, use OMap
-- | A view from a type @s@ to @SType viewpath@.  The Proxy value
-- indicates that we don't need to know anything about a particular
-- @s@ to build or use a @View_Path@.
data Path_View s viewpath = Path_To (Proxy s) viewpath | Path_Self deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)

instance (IsPath fstpath, IsPath sndpath,
          U (UType fstpath) (SType fstpath, SType sndpath),
          UType fstpath ~ UType sndpath
         ) => IsPath (Path_Pair fstpath sndpath) where
    type UType (Path_Pair fstpath sndpath) = UType fstpath
    type SType (Path_Pair fstpath sndpath) = (SType fstpath, SType sndpath)
    idPath = Path_Pair
    toLens (Path_First p) = _1 . toLens p
    toLens (Path_Second p) = _2 . toLens p
    toLens _ = lens u (\s a -> maybe s id (unU' a))
instance (IsPath justpath,
          U (UType justpath) (Maybe (SType justpath))
         ) => IsPath (Path_Maybe justpath) where
    type UType (Path_Maybe justpath) = UType justpath
    type SType (Path_Maybe justpath) = Maybe (SType justpath)
    idPath = Path_Maybe
    toLens (Path_Just p) = _Just . toLens p
    toLens _ = lens u (\s a -> maybe s id (unU' a))
instance (IsPath leftpath, IsPath rightpath,
          U (UType leftpath) (Either (SType leftpath) (SType rightpath)),
          UType leftpath ~ UType rightpath
         ) => IsPath (Path_Either leftpath rightpath) where
    type UType (Path_Either leftpath rightpath) = UType leftpath
    type SType (Path_Either leftpath rightpath) = Either (SType leftpath) (SType rightpath)
    idPath = Path_Either
    toLens (Path_Left p) = _Left . toLens p
    toLens (Path_Right p) = _Right . toLens p
    toLens _ = lens u (\s a -> maybe s id (unU' a))
instance (Data key, Typeable key, Ord key, Read key, Show key,
          IsPath valuepath,
          U (UType valuepath) (Map key (SType valuepath))
         ) => IsPath (Path_Map key valuepath) where
    type UType (Path_Map key valuepath) = UType valuepath
    type SType (Path_Map key valuepath) = Map key (SType valuepath)
    idPath = Path_Map
    toLens (Path_Look k p) = mat k . toLens p
    toLens _ = lens u (\s a -> maybe s id (unU' a))
instance (IsPath elttype, U (UType elttype) [SType elttype]) => IsPath (Path_List elttype) where
    type UType (Path_List elttype) = UType elttype
    type SType (Path_List elttype) = [SType elttype]
    idPath = Path_List
    toLens _ = lens u (\s a -> maybe s id (unU' a))
instance (Data s, View s, IsPath viewpath,
          ViewType s ~ SType viewpath,
          U (UType viewpath) s,
          U (UType viewpath) (ViewType s)
         ) => IsPath (Path_View s viewpath) where
    type UType (Path_View s viewpath) = UType viewpath
    type SType (Path_View s viewpath) = s
    idPath = Path_Self
    toLens (Path_To _ p) = viewLens . toLens p
    toLens _ = lens u (\s a -> maybe s id (unU' a))

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

instance (p ~ UPath u (Either a b), IsPath p, s ~ (Either a b), u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a,
          r ~ UPath u b, IsPath r, u ~ UType r, PathStart u b
         ) => PathStart u (Either a b) where
    type UPath u (Either a b) = Path_Either (UPath u a) (UPath u b)
    data UPeek u (Either a b) = UPeek_Either (UPath u (Either a b)) (Maybe u) -- deriving (Eq, Show, Generic, FromJSON, ToJSON)
    upeekCons = UPeek_Either
    upeekPath (UPeek_Either p _) = p
    upeekValue (UPeek_Either _ x) = x
    upeekRow _ (x@(Left _)) = Node (upeekCons idPath Nothing) (concat [concatMap (makeRow x) [Path_Left]])
    upeekRow _ (x@(Right _)) = Node (upeekCons idPath Nothing) (concat [concatMap (makeRow x) [Path_Right]])
    upeekTree _ d (x@(Left _)) = case d of
                                     Just 0 -> Node (upeekCons idPath (Just (u x))) []
                                     _ -> Node (upeekCons idPath Nothing) (concat [concatMap (makeTrees x) [Path_Left]])
    upeekTree _ d (x@(Right _)) = case d of
                                      Just 0 -> Node (upeekCons idPath (Just (u x))) []
                                      _ -> Node (upeekCons idPath Nothing) (concat [concatMap (makeTrees x) [Path_Right]])
    upeekCol _ (_p@(Path_Left _q)) (x@(Left _)) = Node (upeekCons idPath Nothing) (makeCol x Path_Left (\(Path_Left p) -> p) _p)
    upeekCol _ _p (x@(Left _)) = Node (upeekCons idPath (Just (u x))) []
    upeekCol _ (_p@(Path_Right _q)) (x@(Right _)) = Node (upeekCons idPath Nothing) (makeCol x Path_Right (\(Path_Right p) -> p) _p)
    upeekCol _ _p (x@(Right _)) = Node (upeekCons idPath (Just (u x))) []
{-
instance (p ~ UPath u (Maybe a), IsPath p, s ~ Maybe a, u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a
         ) => PathStart u (Maybe a) where
    type UPath u (Maybe a) = Path_Maybe (UPath u a)
    data UPeek u (Maybe a) = UPeek_Maybe (UPath u (Maybe a)) (Maybe u) -- deriving (Eq, Show, Generic, FromJSON, ToJSON)
    upeekCons = UPeek_Maybe
    upeekPath (UPeek_Maybe p _) = p
    upeekValue (UPeek_Maybe _ x) = x
    upeekRow _ x = Node (upeekCons idPath Nothing) (concat [concatMap (makeRow x) [Path_Just]])
    upeekTree _ d x = case d of
                        Just 0 -> Node (upeekCons idPath (Just (u x))) []
                        _ -> Node (upeekCons idPath Nothing) (concat [concatMap (makeTrees x) [Path_Just]])
    upeekCol _ (_p@(Path_Just _q)) x = Node (upeekCons idPath Nothing) (makeCol x (\q -> Path_Just q) (\(Path_Just p) -> p) _p)
    upeekCol _ _p x = Node (upeekCons idPath (Just (u x))) []

instance (p ~ UPath u (a, b), IsPath p, s ~ (a, b), u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a,
          r ~ UPath u b, IsPath r, u ~ UType r, PathStart u b
         ) => PathStart u (a, b) where
    type UPath u (a, b) = Path_Pair (UPath u a) (UPath u b)
    data UPeek u (a, b) = UPeek_Pair (UPath u (a, b)) (Maybe u) -- deriving (Eq, Show, Generic, FromJSON, ToJSON)
    upeekCons = UPeek_Pair
    upeekPath (UPeek_Pair p _) = p
    upeekValue (UPeek_Pair _ v) = v
    upeekRow _ x = Node (upeekCons idPath Nothing) (concat [concatMap (makeRow x) [Path_First], concatMap (makeRow x) [Path_Second]])
    upeekTree _ d x = case d of
                          Just 0 -> Node (upeekCons idPath (Just (u x))) []
                          _ -> Node (upeekCons idPath Nothing) (concat [concatMap (makeTrees x) [Path_First], concatMap (makeTrees x) [Path_Second]])
    upeekCol _ (_p@(Path_First _q)) x = Node (upeekCons idPath Nothing) (makeCol x Path_First (\(Path_First p) -> p) _p)
    upeekCol _ (_p@(Path_Second _q)) x = Node (upeekCons idPath Nothing) (makeCol x Path_Second (\(Path_Second p) -> p) _p)
    upeekCol _ _p x = Node (upeekCons idPath (Just (u x))) []
-}

instance (p ~ UPath u (Map k a), IsPath p, s ~ Map k a, u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a
         ) => PathStart u (Map k a) where
    type UPath u (Map k a) = Path_Map k (UPath u a)
    data UPeek u (Map k a) = UPeek_Map (UPath u (Map k a)) (Maybe u) -- deriving (Eq, Show, Generic, FromJSON, ToJSON)
    upeekCons = UPeek_Map
    upeekPath (UPeek_Map p _) = p
    upeekValue (UPeek_Map _ v) = v
    upeekRow _ x = Node (upeekCons idPath Nothing) (concat [concatMap (makeRow x) (map (\(_k, _) -> (Path_Look _k)) (toList x))])
    upeekTree _ (Just 0) x = Node (upeekCons idPath (Just (u x))) []
    upeekTree _ d x = Node (upeekCons idPath Nothing) (concat [concatMap (makeTrees x) (map (\(_k, _) -> Path_Look _k) (toList x))])
    upeekCol _ _p@(Path_Look _k _) x = Node (upeekCons idPath Nothing) (makeCol x (Path_Look _k) (\(Path_Look _ p) -> p) _p)
    upeekCol _ p x = Node (upeekCons idPath (Just (u x))) []

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

-- | Alternative version of Maybe for which we can create alternative instances.
data Maybe' a = Just' a | Nothing' deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, ToJSON, FromJSON)

lens_mrs' :: (Show a, Read a) => Iso' (Maybe' a) String
lens_mrs' = iso getter setter
  where getter Nothing' = ""
        getter (Just' x) = show x
        setter x = maybe Nothing' Just' (maybeRead x)

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
$(deriveSafeCopy 1 'base ''Maybe')
$(deriveLiftMany [''Maybe'])
#endif
