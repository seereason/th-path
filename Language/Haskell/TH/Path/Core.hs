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
    ( camelWords

      -- * Type classes and associated types
    , IsPath(UType, SType, idPath, toLens)
    , PathStart(UPath, upeekRow, upeekTree, upeekCol)
    , Peek(..)
    , makeRow
    , makeTrees
    , makeCol
    , makePeek
    , makePath
    , aOfS
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
import Control.Monad.State (get, put, StateT, runStateT)
import Control.Monad.Trans (lift)
import Data.Aeson hiding (decode')
import Data.Char (isUpper, toUpper)
import Data.Generics (Data, Typeable)
import Data.List as List (groupBy, map)
import Data.Map as Map (Map, insert, lookup, toList, (!))
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.Order hiding (view)
import Data.Proxy
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Data.Tree (Tree(..))
import Data.UserId (UserId(..))
import Debug.Trace (trace)
import Language.Haskell.TH -- (Q, ExpQ, Exp(AppE, VarE, TupE, LitE, InfixE))
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (makeUNamedFieldCon, PathCon(unPathCon))
import Language.Haskell.TH.Path.View (View(ViewType), viewLens)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax (qReify)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Prelude hiding (exp)
import Safe (readMay)
-- import Language.Haskell.TH.Ppr (pprint)
import Web.Routes
import Web.Routes.TH (derivePathInfo)
import Text.Parsec.Prim ((<|>))
import GHC.Base (ap)

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
class (U u s,
       u ~ UType (UPath u s),
       s ~ SType (UPath u s),
       IsPath (UPath u s),
       Eq (UPath u s),
       Ord (UPath u s),
       Read (UPath u s),
       Show (UPath u s),
       Data (UPath u s),
       FromJSON (UPath u s),
       ToJSON (UPath u s)
      ) => PathStart u s where
    type UPath u s
    -- ^ A value of type @UPath u s@ represents some path starting at @s@.
    upeekTree :: Proxy u -> Maybe Int -> s -> Tree (Peek u s)
    -- ^ Given a value of type @s@, return a tree containing every
    -- 'Peek' that can be reached from it.  The order of the nodes in
    -- the forest reflects the order the elements were encountered
    -- during the traversal.
    upeekRow :: Proxy u -> s -> Tree (Peek u s)
    -- ^ In this function only one layer of the tree is returned, no
    -- recursive peek calls are made.
    upeekCol :: Proxy u -> UPath u s -> s -> Tree (Peek u s)
    -- ^ In this function only the nodes along the given path appear
    -- in the result.  That is to say, the result is a degenerate tree
    -- with a single node at each level.
    -- @@
    --   upeekCol Proxy (UPath_ReportView__reportUUID idPath) report ->
    --      Node (Peek idPath Nothing)
    --           [Node (Peek (Path_To Proxy idPath) Nothing)
    --                 [Node (Peek (Path_To Proxy (UPath_ReportView__reportUUID idPath)) (Just uuid))
    --                       []]]
    -- @@

-- | A data type containing a path and (optionally) the value
-- found at the end of that path.
data Peek u s
    = Peek { upeekPath :: UPath u s
           -- ^ Accessor for path field of a Peek type
           , upeekValue :: Maybe u
           -- ^ Accessor for value field of a Peek type
           }

deriving instance (Show u, Show (UPath u s)) => Show (Peek u s)
deriving instance (Read u, Read (UPath u s)) => Read (Peek u s)
deriving instance (Eq u, Eq (UPath u s)) => Eq (Peek u s)
deriving instance (Ord u, Ord (UPath u s)) => Ord (Peek u s)
deriving instance (Data u, Data (UPath u s), Data s, Typeable s) => Data (Peek u s)
deriving instance (Typeable u, Typeable (UPath u s)) => Typeable (Peek u s)
deriving instance Generic (Peek u s)

-- | Given a function that lifts a path by one hop (e.g. a constructor
-- such as Path_Left), return the peek(s?) resulting from traversing that hop.
makeRow :: forall s u p q a. (u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s,
                              u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a) =>
             s -> (q -> p) -> [Tree (Peek u s)]
makeRow x f = List.map (fmap (mapPeek f)) (map makePeek (hopValues f x))

-- | Given a function that lifts a path by one hop (e.g. a constructor
-- such as Path_Left), return the peek(s?) resulting from traversing that hop.
makeTrees :: forall s u p q a. (u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s,
                                u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a) =>
             Maybe Int -> s -> (q -> p) -> [Tree (Peek u s)]
makeTrees d x f = List.map (fmap (mapPeek f)) (map (upeekTree Proxy (fmap pred d)) (hopValues f x))

-- | Helper function for implementing upeekCol
makeCol :: forall s u p q a. (u ~ UType p, s ~ SType p, UPath u s ~ p, PathStart u s,
                              u ~ UType q, a ~ SType q, UPath u a ~ q, PathStart u a) =>
           s -> (q -> p) -> (p -> q) -> p -> [Tree (Peek u s)]
makeCol x f g p = List.map (fmap (mapPeek f)) (map (upeekCol Proxy (g p)) (hopValues f x))

mapPeek :: (PathStart u a, PathStart u s) => (UPath u a -> UPath u s) -> Peek u a -> Peek u s
mapPeek f pk = Peek (f (upeekPath pk)) (upeekValue pk)

makePeek :: forall u s. PathStart u s => s -> Tree (Peek u s)
makePeek x = Node (Peek idPath (Just (u x))) []

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
instance (Data k, Typeable k, Eq k, Ord k, Read k, Show k, Enum k,
          IsPath valuepath,
          U (UType valuepath) (Order k (SType valuepath))
         ) => IsPath (Path_OMap k valuepath) where
    type UType (Path_OMap k valuepath) = UType valuepath
    type SType (Path_OMap k valuepath) = Order k (SType valuepath)
    idPath = Path_OMap
    toLens (Path_At k p) = lens_omat k . toLens p
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

instance (IsPath (Path_OMap k v), Describe v, Describe (Proxy (SType (Path_OMap k v)))) => Describe (Path_OMap k v)
    where describe' _f (_p@(Path_At _k _wp)) = maybe (describe' _f (Proxy :: Proxy (SType (Path_OMap k v)))) Just (describe' Nothing _wp)
          describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_OMap k v)))
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

instance (IsPath (Path_List a), Describe a, Describe (Proxy (SType (Path_List a)))) => Describe (Path_List a)
    where describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_List a)))
          describe' _ p = error ("Unexpected path: " ++ show p)

instance (p ~ UPath u (Either a b), IsPath p, s ~ (Either a b), u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a,
          r ~ UPath u b, IsPath r, u ~ UType r, PathStart u b, U u (Either a b)
         ) => PathStart u (Either a b) where
    type UPath u (Either a b) = Path_Either (UPath u a) (UPath u b)
    upeekRow _ (x@(Left _)) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_Left]])
    upeekRow _ (x@(Right _)) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_Right]])
    upeekTree _ d (x@(Left _)) = case d of
                                     Just 0 -> Node (Peek idPath (Just (u x))) []
                                     _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_Left]])
    upeekTree _ d (x@(Right _)) = case d of
                                      Just 0 -> Node (Peek idPath (Just (u x))) []
                                      _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_Right]])
    upeekCol _ (_p@(Path_Left _q)) (x@(Left _)) = Node (Peek idPath Nothing) (makeCol x Path_Left (\(Path_Left p) -> p) _p)
    upeekCol _ _p (x@(Left _)) = Node (Peek idPath (Just (u x))) []
    upeekCol _ (_p@(Path_Right _q)) (x@(Right _)) = Node (Peek idPath Nothing) (makeCol x Path_Right (\(Path_Right p) -> p) _p)
    upeekCol _ _p (x@(Right _)) = Node (Peek idPath (Just (u x))) []

instance (p ~ UPath u (Maybe a), IsPath p, s ~ Maybe a, u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a, U u (Maybe a)
         ) => PathStart u (Maybe a) where
    type UPath u (Maybe a) = Path_Maybe (UPath u a)
    upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_Just]])
    upeekTree _ d x = case d of
                        Just 0 -> Node (Peek idPath (Just (u x))) []
                        _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_Just]])
    upeekCol _ (_p@(Path_Just _q)) x = Node (Peek idPath Nothing) (makeCol x (\q -> Path_Just q) (\(Path_Just p) -> p) _p)
    upeekCol _ _p x = Node (Peek idPath (Just (u x))) []

instance (p ~ UPath u (a, b), IsPath p, s ~ (a, b), u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a,
          r ~ UPath u b, IsPath r, u ~ UType r, PathStart u b, U u (a, b)
         ) => PathStart u (a, b) where
    type UPath u (a, b) = Path_Pair (UPath u a) (UPath u b)
    upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_First], concatMap (makeRow x) [Path_Second]])
    upeekTree _ d x = case d of
                          Just 0 -> Node (Peek idPath (Just (u x))) []
                          _ -> Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_First], concatMap (makeTrees d x) [Path_Second]])
    upeekCol _ (_p@(Path_First _q)) x = Node (Peek idPath Nothing) (makeCol x Path_First (\(Path_First p) -> p) _p)
    upeekCol _ (_p@(Path_Second _q)) x = Node (Peek idPath Nothing) (makeCol x Path_Second (\(Path_Second p) -> p) _p)
    upeekCol _ _p x = Node (Peek idPath (Just (u x))) []

instance (p ~ UPath u (Map k a), IsPath p, s ~ Map k a, u ~ UType p, s ~ SType p, U u s,
          q ~ UPath u a, IsPath q, u ~ UType q, PathStart u a, U u (Map k a),
          FromJSON k, ToJSON k
         ) => PathStart u (Map k a) where
    type UPath u (Map k a) = Path_Map k (UPath u a)
    upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) (map (\(_k, _) -> (Path_Look _k)) (Map.toList x))])
    upeekTree _ (Just 0) x = Node (Peek idPath (Just (u x))) []
    upeekTree _ d x = Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) (map (\(_k, _) -> Path_Look _k) (Map.toList x))])
    upeekCol _ _p@(Path_Look _k _) x = Node (Peek idPath Nothing) (makeCol x (Path_Look _k) (\(Path_Look _ p) -> p) _p)
    upeekCol _ _p x = Node (Peek idPath (Just (u x))) []

instance (u ~ UType (UPath u a), a ~ SType (UPath u a),
          U u (Order k a), PathStart u a,
          Data k, Ord k, Enum k, Read k, Show k, FromJSON k, ToJSON k
         ) => PathStart u (Order k a) where
    type UPath u (Order k a) = Path_OMap k (UPath u a)
    upeekRow _ (x@_xyz) = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) (map (\(_k, _) -> Path_At _k) (toPairs _xyz))])
    upeekTree _ (Just 0) (x@_xyz) = Node (Peek idPath (Just (u x))) []
    upeekTree _ d (x@_xyz) = Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) (map (\(_k, _) -> Path_At _k) (toPairs _xyz))])
    upeekCol _ (_p@(Path_At _k _q)) (x@_xyz) = Node (Peek idPath Nothing) (makeCol x (Path_At _k) (\(Path_At _ p) -> p) _p)
    upeekCol _ _p (x@_xyz) = Node (Peek idPath (Just (u x))) []

-- | A wrapper used to access the View functionality
data ViewOf s a = ViewOf (Proxy a) s deriving Data

instance (p ~ UPath u s, u ~ UType (UPath u a), u ~ UType (UPath u s), U u (ViewOf s a),
          q ~ UPath u a, a ~ SType (UPath u a), a ~ ViewType (ViewOf s a),
          Data s, Data a, PathStart u a, View (ViewOf s a)
         ) => PathStart u (ViewOf s a) where
    type UPath u (ViewOf s a) = Path_View (ViewOf s a) (UPath u a)
    upeekRow _ x = Node (Peek idPath Nothing) (concat [concatMap (makeRow x) [Path_To Proxy]])
    upeekTree _ (Just 0) x = Node (Peek idPath (Just (u x))) []
    upeekTree _ d x = Node (Peek idPath Nothing) (concat [concatMap (makeTrees d x) [Path_To Proxy]])
    upeekCol _ (_p@(Path_To _ _q)) x = Node (Peek idPath Nothing) (makeCol x (Path_To Proxy) (\(Path_To (Proxy) q) -> q) _p)
    upeekCol _ _p x = Node (Peek idPath (Just (u x))) []

-- | Given a starting type @s@ and an expression of type @s -> a@,
-- return a path builderfunction, i.e. an expression of type @s ->
-- UPath u s@.  When this path is converted to a lens, that lens
-- should look at the exact @a@ that the original expression did.  The
-- expression is built from a limited set of elements - fst and snd
-- for pairs, applications of field accessors, Just to get from a
-- Maybe into the contained value, and so on.
makePath :: forall m. DsMonad m => TypeQ -> TypeQ -> ExpQ -> m Exp
makePath utypeq stypeq expq = runQ (snd <$> aOfS utypeq expq stypeq)

-- | Given an expression of type @s -> a@ (in a very specific format)
-- and a type @s@ type, return the type @a@.
--
-- We extract @a@ from the type with guidance from the expression.
-- Specifically, the innermost layer of the expression tells us how to
-- strip the outermost layer of the type and get closer to @a@.
--
-- So we must first traverse to the bottom of the expression.  When we
-- reach @id@ we return the initial @s@ type.  Then as we exit each
-- layer of the expression we strip a layer of the result type.
aOfS :: TypeQ -> ExpQ -> TypeQ -> Q (Type, Exp)
aOfS _utypeq expq stypeq = do
  exp <- expq
  stype <- stypeq
  -- trace ("aOfS (" ++ pprint1 exp ++ ") (" ++ pprint1 stype ++ ")\n  exp=" ++ show exp ++ "\n  typ=" ++ show stype) (pure ())
  [|idPath|] >>= runStateT (doView exp stype) >>= return . debug exp stype
    where
      debug :: Exp -> Type -> (Type, Exp) -> (Type, Exp)
      debug exp stype (rtype, rexp) =
          trace ("makePath (" ++ pprint1 exp ++ ") (" ++ pprint1 stype ++ ") -> (" ++ pprint1 rexp ++ " :: " ++ pprint1 rtype ++ ")")
                (rtype, rexp)

      doView :: Exp -> Type -> StateT Exp Q Type
      doView exp stype =
          viewInstanceType stype >>=
          maybe (doType exp stype)
                (\atype -> doView exp atype >>= \t -> modify' (\e -> [|Path_To Proxy $e|]) >> return t)
      -- doView stype (Just atype) = makePath utypeq (pure atype) expq >>= \p -> runQ [|Path_To (Proxy :: Proxy $(pure stype)) $(pure p)|]

      doType :: Exp -> Type -> StateT Exp Q Type
      doType exp (ConT tname) = {-trace ("doType1 (" ++ pprint1 exp ++ ") (" ++ pprint1 typ ++ ")") (pure ()) >>-} qReify tname >>= doInfo exp tname >>= lift . expandType
      doType exp@(LamE [VarP _x] _) typ = {-trace ("doType2 (" ++ pprint1 exp ++ ") (" ++ pprint1 typ ++ ")") (pure ()) >>-} doExp exp typ >>= lift . expandType
      doType exp _typ = error $ "Expecting a lambda expression, saw " ++ pprint1 exp

      doInfo :: Exp -> Name -> Info -> StateT Exp Q Type
      doInfo exp tname (TyConI dec) = {-trace ("doInfo1 (" ++ pprint1 exp ++ ") (" ++ show tname ++ ") (" ++ pprint1 dec ++ ")") (pure ()) >>-} doDec exp tname dec
      doInfo exp tname (FamilyI dec _) = {-trace ("doInfo2 (" ++ pprint1 exp ++ ") (" ++ show tname ++ ") (" ++ pprint1 dec ++ ")") (pure ()) >>-} doDec exp tname dec
      doInfo _ _ info = error $ "Unexpected info: " ++ pprint1 info ++ "\n  " ++ show info

      doDec :: Exp -> Name -> Dec -> StateT Exp Q Type
      doDec exp _ (TySynD _ _binds typ) = doView exp typ
      doDec exp tname _ = doExp exp (ConT tname)

      -- Given a lambda expression and the type of its argument,
      -- return the type of its result.  Save the resulting path
      -- expression in the state.  Example:
      --   exp: \x -> fst x
      --   typ: (Int, Char)
      doExp :: Exp -> Type -> StateT Exp Q Type
      -- If the expression is (\x -> x) then return typ.
      doExp (LamE [(VarP x)] (VarE name)) typ | name == x = pure typ
      doExp (LamE [x@(VarP _)] (AppE (VarE fn) exp')) typ
        | fn == 'fst =
            do modify' (\e -> [|Path_First $e|])
               -- Strip off the outer layer of the lambda expression
               -- and compute the result type.  Then return the
               -- application of that outer layer
               t <- doView (LamE [x] exp') typ -- doView (LamE [x] (VarE x)) (Int, Char) -> Int
               -- t needs to be a pair, return the type of fst
               case t of
                 AppT (AppT (TupleT 2) a) _ -> pure a
                 _ -> error "aOfS fst"
        | fn == 'snd =
            do modify' (\e -> [|Path_Second $e|])
               t <- doView (LamE [x] exp') typ
               case t of
                 AppT (AppT (TupleT 2) _) b -> pure b
                 _ -> error $ "aOfS snd - t=" ++ show t
      doExp (LamE [x@(VarP _)] (AppE (AppE (VarE fn) (VarE lns)) exp')) typ
        | fn == 'view && lns == '_Left =
            do modify' (\e -> [|Path_Left $e|])
               t <- doView (LamE [x] exp') typ
               case t of
                 AppT (AppT (ConT c) l) _ | c == ''Either -> pure l
                 _ -> error "aOfS Left"
        | fn == 'view && lns == '_Right =
            do modify' (\e -> [|Path_Right $e|])
               t <- doView (LamE [x] exp') typ
               case t of
                 AppT (AppT (ConT c) _) r | c == ''Either -> pure r
                 _ -> error "aOfS Right"
      doExp (LamE [x@(VarP _)] (AppE (ConE c) exp')) typ
        | c == 'Just =
            do modify' (\e -> [|Path_Just $e|])
               t <- doView (LamE [x] exp') typ
               case t of
                 AppT (ConT name) a | name == ''Maybe -> pure a
                 _ -> error "aOfS Just"
      doExp (LamE [x@(VarP _)] (InfixE (Just exp') (VarE fn) (Just key))) typ
        | fn == '(!) =
            do modify' (\e -> [|Path_Look $(pure key) $e|])
               t <- doView (LamE [x] exp') typ
               case t of
                 AppT (AppT (ConT mp) _k) a | mp == ''Map -> pure a
                 _ -> error "aOfS Map"
      doExp exp@(LamE [x@(VarP _)] (AppE (VarE fname) exp')) typ =
#if MIN_VERSION_template_haskell(2,11,0)
        do info@(VarI _ (AppT (AppT ArrowT rtype@(ConT tname)) ftype) _) <- qReify fname
#else
        do info@(VarI _ (AppT (AppT ArrowT rtype@(ConT tname)) ftype) _ _) <- qReify fname
#endif
           fld <- lift $ fromJust <$> lookupValueName (nameBase (unPathCon (makeUNamedFieldCon tname fname)))
           modify' (\e -> [|$(conE fld) $e|])
           t <- doView (LamE [x] exp') typ -- Should return a @ConT tname@
           if t == rtype then pure ftype else error ("aOfS field - exp=(" ++ pprint1 exp ++ ") typ=(" ++ pprint1 typ ++ ") " ++ show fname ++ "=(" ++ pprint1 info ++ ") t=(" ++ pprint1 t ++ ")")
           -- if t == rtype then pure t else
      doExp exp typ = error $ "aOfS - exp=" ++ pprint1 exp ++ ", typ=" ++ pprint1 typ

      expandType :: Type -> TypeQ
      expandType (ConT tname) = qReify tname >>= expandInfo tname
      expandType typ = pure typ
      expandInfo tname (TyConI dec) = expandDec tname dec
      expandInfo tname (FamilyI dec _) = expandDec tname dec
      expandInfo _tname _info = error $ "Unexpected info"
      expandDec _ (TySynD _ _binds typ) = expandType typ
      expandDec tname _ = conT tname

      -- Customized modify for our state monad.
      modify' :: (ExpQ -> ExpQ) -> StateT Exp Q ()
      modify' f = get >>= \e -> lift (f (pure e)) >>= put {- . (\x -> trace ("new path: " ++ pprint1 x) x) -}

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
