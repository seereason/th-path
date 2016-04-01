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
module Language.Haskell.TH.Path.Core
    ( treeMap
    , forestMap
      -- * Type classes and associated types
    , Paths(paths, Path, peek, peekPath, peekValue, peekCons)
    , IdPath(idPath)
    , PathStart(Peek, peekTree, peekRow)
    , ToLens(S, A, toLens)
    , (:.:)(..)
    , U(u, unU)

    -- * Hint classes
    , SinkType
    , HideType
    , SelfPath
    , Describe(describe')
    , describe
    , fieldStrings

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

    -- * Basic lenses
    , readOnlyLens
    , readShowLens
    , lens_trace
    -- , listLookupLens
    , lens_mrs
    , idLens
    , dummyLens
    , textLens
    , IsText(textLens')
    , stringLens
    , lens_UserIds_Text
#if !__GHCJS__
    , pathTypeNames
#endif
    , camelWords
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Lens hiding (at) -- (set, Traversal', Lens', _Just, iso, lens, view, view)
import Data.Char (isUpper, toUpper)
import Data.Generics (Data, Typeable)
import Data.List as List (groupBy, map)
import qualified Data.Map as M (Map, insert, lookup)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Proxy
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set ({-difference,-} fromList, Set)
import Data.Text as Text (Text, pack, unpack, unwords, words)
import Data.Tree (Tree(Node), Forest)
import Data.UserId (UserId(..))
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (Lift (lift))
import Language.Haskell.TH.Syntax (liftString, qReify)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Prelude hiding (exp)
import Safe (readMay)
import Web.Routes.TH (derivePathInfo)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node x ns) = Node (f x) (forestMap f ns)

forestMap :: (a -> b) -> Forest a -> Forest b
forestMap f = List.map (treeMap f)

class U univ a where
    u :: a -> univ
    unU :: univ -> a

-- | Every path type must have an identity value, such that 'toLens'
-- 'idPath' is just 'id'.
class IdPath p where
    idPath :: p -- ^ The identity value for path type @p@.  Obeys the law
                -- @toLens idPath == iso id id@.

-- | Every path has start and end types and can be converted to a lens.
class ToLens p where
    type S p
    type A p
    toLens :: p -> Traversal' (S p) (A p)

data (f :.: g) = f :.: g deriving (Eq, Generic, Read, Show)

instance (ToLens f, ToLens g, A f ~ S g {-, B f ~ T g-}) => ToLens (f :.: g) where
  type S (f :.: g) = S f
  -- type T (f :.: g) = T f
  type A (f :.: g) = A g
  -- type B (f :.: g) = B g
  toLens (f :.: g) = toLens f . toLens g
  -- ^ Function to turn a path value of type @p@ into a lens to access
  -- (one of) the @A p@ values in an @S p@.

-- | If there are paths that begin from type @s@, the 'peek' function
-- returns all the paths starting from a particular value of type @s@,
-- along with the value found at the end of that path.  The 'Peek'
-- type is constructed to be able to represent this result.
class PathStart u s where
    data Peek u s
    -- ^ 'Peek' is a type function that maps a type to the union of
    -- all paths that start at that type, and (maybe) the value found
    -- by following the path.
    peekTree :: Proxy u -> s -> Forest (Peek u s)
    -- ^ Given a value of type @s@, return a forest containing every
    -- 'Peek' that can be reached from it.  The order of the nodes in
    -- the forest reflects the order the elements were encountered
    -- during the traversal.
    peekRow :: Proxy u -> s -> [Peek u s]
    -- ^ In this function only one layer of the forest is returned, no
    -- recursive peek calls are made.

-- | For any two types @s@ and @a@, there is an instance of @Paths
-- s a@ if there is any path from @s@ to @a@.  The @Path@ type
-- function maps @s@ and @a@ to a path type, and 'paths' returns all
-- possible path values from @s@ to @a@.
--
-- For example, there is one instance of @Paths (Int, Int) Int@.
-- In this case the type function @Path s a@ would return @Path_Pair
-- (Path_Int Int) (Path_Int Int)@. The 'paths' function would return
-- the two possible values of this type: @Path_First Path_Int@ and
-- @Path_Second Path_Int@.  @Path_Pair@ has a third constructor,
-- eponymously named @Path_Pair@, but that is the identity
-- constructor, so it can not represent a path from @(Int, Int)@ to
-- @Int@.
class (PathStart u s, IdPath (Path u s a), ToLens (Path u s a), S (Path u s a) ~ s, A (Path u s a) ~ a) => Paths u s a where
    type Path u s a
    -- ^ Each instance defines this type function which returns the
    -- path type.  Each value of this type represents a different way
    -- of obtaining the @a@ from the @s@.  For example, if @s@ is a
    -- record with two fields of type 'Int', the type @PathType s Int@
    -- would have distinct values for those two fields, and the lenses
    -- returned by 'toLens would access those two fields.
    paths :: Proxy u -> s -> Proxy a -> [Path u s a]
    -- ^ Build the paths corresponding to a particular @s@ value and a
    -- particular @a@ type.  Returns a list because there may be
    -- several @a@ reachable from this @s@.  This function will freak
    -- out if called with types for which there is no instance
    -- @Paths s a@.
    peek :: Path u s a -> s -> Peek u s
    -- ^ Build a 'Peek' @s@ value for a specific path from @s@ to @a@.

    peekPath :: Proxy a -> Peek u s -> Path u s a
    -- ^ Accessor for path field of a Peek type
    peekValue :: Proxy a -> Peek u s -> Maybe a
    -- ^ Accessor for value field of a Peek type
    peekCons :: Path u s a -> Maybe a -> Peek u s
    -- ^ Construct a Peek s

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
    describe' :: Maybe (String, String, Either Int String) -> a -> Maybe String

describe :: Describe a => a -> Maybe String
describe = describe' Nothing

-- | Convert a 'Language.Haskell.TH.TypeGraph.Shape.Field' into the argument used by describe'.
fieldStrings :: (Name, Name, Either Int Name) -> ExpQ
fieldStrings (tname, cname, f) = [|($(liftString (nameBase tname)),
                                    $(liftString (nameBase cname)),
                                    $(either (\i -> [|Left $(lift i)|]) (\n -> [|Right $(liftString (nameBase n))|]) f))|]

-- Primitive path types

-- | A path type with constructors to extract either @fst@, @snd@, or
-- the pair itself.
data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Either a b = Path_Left a | Path_Right b | Path_Either deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Maybe a = Path_Just a | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Map k v = Path_Look k v | Path_Map  deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data) -- No element lookup path - too dangerous, use OMap

instance IdPath (Path_Pair a b) where idPath = Path_Pair
instance IdPath (Path_Maybe a) where idPath = Path_Maybe
instance IdPath (Path_Either a b) where idPath = Path_Either
instance IdPath (Path_Map k v) where idPath = Path_Map
instance IdPath (Path_List a) where idPath = Path_List

#if !__GHCJS__
$(derivePathInfo ''Path_Pair)
$(derivePathInfo ''Path_List)
$(derivePathInfo ''Path_Map)
$(derivePathInfo ''Path_Either)
$(derivePathInfo ''Path_Maybe)

$(deriveSafeCopy 0 'base ''Path_Pair)
$(deriveSafeCopy 0 'base ''Path_List)
$(deriveSafeCopy 0 'base ''Path_Map)
$(deriveSafeCopy 0 'base ''Path_Either)
$(deriveSafeCopy 0 'base ''Path_Maybe)
#endif

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

-- | A lens for 'Maybe' values whose getter turns Nothing into the
-- empty string and whose setter returns Nothing whenever read fails.
lens_mrs :: (Show a, Read a) => Lens' (Maybe a) String
lens_mrs = lens getter setter
  where getter Nothing = ""
        getter (Just x) = show x
        setter _ x = maybeRead x

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
lens_mapFirst l = lens_mapPair l id

lens_mapSecond :: Lens' b c -> Lens' (a, b) (a, c)
lens_mapSecond l = lens_mapPair id l

-- | Create a list lens from an element lens.  For the setter, extra
-- elements in the C list cause an error (unless elens is an iso.)
-- Extra elements in the B list are ignored.
lens_list :: forall b c. b -> Lens' b c -> Lens' [b] [c]
lens_list def elens =
    lens getter setter
    where
      getter :: [b] -> [c]
      getter bs = List.map (view elens) bs
      setter :: [b] -> [c] -> [b]
      setter bs cs = List.map
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

textLens :: Lens' Text Text
textLens = id

class IsText a where
    textLens' :: Lens' a Text
    stringLens :: IsText a => Lens' a String
    stringLens = textLens' . iso unpack pack

lens_UserIds_Text :: Lens' [UserId] Text
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
-- | Find all the names of the path types.
pathTypeNames :: DsMonad m => m (Set Name)
pathTypeNames = do
  (FamilyI (FamilyD TypeFam _pathtype [_,_,_] (Just StarT)) tySynInsts) <- qReify ''Path
  return . {-flip Set.difference primitivePathTypeNames .-} Set.fromList . List.map (\(TySynInstD _ (TySynEqn _ typ)) -> doTySyn typ) $ tySynInsts
    where
      doTySyn (AppT x _) = doTySyn x
      doTySyn (ConT pathTypeName) = pathTypeName
      doTySyn x = error $ "Unexpected type in pathTypeNames: " ++ pprint1 x ++ " (" ++ show x ++ ")"

-- primitivePathTypeNames :: Set Name
-- primitivePathTypeNames = Set.fromList [''Path_Pair, ''Path_List, ''Path_Either, ''Path_Map, ''Path_OMap, ''Path_Maybe]
#endif

-- | Convert a camel case string (no whitespace) into a natural
-- language looking phrase:
--   camelWords3 "aCamelCaseFOObar123" -> "A Camel Case FOObar123"
camelWords :: String -> String
camelWords s =
    case groupBy (\ a b -> isUpper a == isUpper b) (dropWhile (== '_') s) of -- "aCamelCaseFOObar123"
      (x : xs) -> concat $ capitalize x : map (\ (c : cs) -> if isUpper c then ' ' : c : cs else c : cs) xs
      [] -> ""

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c) : cs
