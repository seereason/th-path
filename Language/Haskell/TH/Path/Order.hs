-- | A data structure that combines a 'Map' @k@ @v@ with a list of
-- @k@, representing the element order.  This means the @[k]@ can be
-- reordered without invalidating any @k@ values that might be in use.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.TH.Path.Order
    ( Order(..)
#if !__GHCJS__
    , deriveOrder
#endif
    -- * Query
    , view
    , view'
    , toPairs
    , toList
    , find
    -- * Construction
    , init
    , empty
    , fromPairs
    , fromList
    -- * Modification
    , putItem
    , insert
    , permute
    , deleteItem
    , appendItem
    , insertItems
    , asList
    -- * Lens
    , lens_omat
    -- * Path Type
    , Path_OMap(Path_OMap, Path_At)
    ) where

import Control.Lens (Traversal', _Just, lens)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List as List (elem, foldl, foldl', foldr, filter, partition)
import qualified Data.ListLike as LL
import Data.Map as Map (Map, (!))
import qualified Data.Map as Map
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import Data.Tree (Tree(Node))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Core (Describe(..), IsPath(..), makeCol, makeRow, makeTrees, PathStart(..), Peek(..), U(u, unU'))
-- import Language.Haskell.TH.Path.GHCJS (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.TypeGraph.Prelude ({-some Lift instances?-})
import Prelude hiding (init)
import Web.Routes.TH (derivePathInfo)

data Order k v =
    Order { elems :: Map k v
          -- ^ Return the key-value map
          , order :: [k]
          -- ^ Return the list of keys in order.
          , next :: k
          -- ^ Next available key
          }
    deriving (Data, Typeable, Generic)

init :: Enum k => k
init = toEnum 1            -- Yeah, that's right, 1.  F**k zeroth elements.

-- | Return the an empty Order.
empty :: (Ord k, Enum k) => Order k v     -- ^ The empty Order
empty = Order {elems = mempty, order = mempty, next = init}

-- | Update the value of an existing item
putItem :: (Ord k, Enum k) => k -> v -> Order k v -> Order k v
putItem k a m =
    m {elems = Map.alter f k (elems m)}
        where f Nothing = (error "putItem: bad key")
              f (Just _) = Just a

-- | Partition an Order into the element at k and the Order
-- containing the remaining elements.
view :: (Ord k, Enum k) => k -> Order k v -> Maybe (v, Order k v) -- like Data.Set.minView
view k m = case Map.lookup k (elems m) of
             Nothing -> Nothing
             Just x -> Just (x, m {elems = Map.delete k (elems m), order = filter (/= k) (order m)})

-- | Build an order from a list of (key, value) pairs.  No
-- uniqueness check of the keys is performed.
fromPairs :: (Ord k, Enum k) => [(k, v)] -> Order k v
fromPairs prs =
    let ks = map fst prs in
    Order { elems = Map.fromList prs
          , order = ks
          , next = succ (foldl1 max ks) }

instance (Ord k, Show k, Show v) => Show (Order k v) where
    show o = "(fromPairs (" ++ show (map (\k -> (k, elems o ! k)) (order o)) ++ "))"

-- | Put a new element at the end of the Order, returning a pair
-- containing the new Order and the new key.
insert :: (Ord k, Enum k) => v -> Order k v -> (Order k v, k)
insert a m = let k = next m in (m {next = succ k, elems = Map.insert k a (elems m), order = order m ++ [k]}, k)

-- | Replace the current ordering with the given key list.  The
-- result is a triple: (new, missing, invalid).  Missing pairs are
-- those not mentioned in the new list, invalid are those
-- mentioned but not present in the old Order.
permute :: (Ord k, Enum k) => [k] -> Order k v -> (Order k v, [(k, v)], [k])
permute neworder m =
    reorder $ collect $ sanitize
    where
      -- Make sure the new key order doesn't have any unknown keys
      -- sanitize :: ([k], [k]) -- (known, invalid)
      sanitize = List.partition (`List.elem` (order m)) neworder
      -- Collect the values that are missing from the new key order
      -- collect :: ([k], [k]) -> ([k], [k], [k]) -- (present, missing, invalid)
      collect (valid, invalid) =
          let deleted = List.filter (not . (`List.elem` invalid)) (order m) in (valid, deleted, invalid)
      -- Reorder the OrderMap according to the now safe permutation,
      -- also return the portion of the OrderMap not mentioned in the
      -- new order and the list of invalid keys.
      -- reorder :: ([k], [k], [k]) -> (OrderMap k a, OrderMap k a, [k])
      reorder (valid, _missing, invalid) =
          let (validmap, missingmap) = Map.partitionWithKey (\ k _ -> List.elem k valid) (elems m) in
          (m {elems = validmap, order = valid},
           (Map.toList missingmap),
           invalid)

instance (Ord k, Enum k) => Monoid (Order k m) where
    mempty = empty
    mappend a b = foldr (\ x m -> fst (insert x m)) a (toList b)

-- Not sure how correct these three instances are in the presence of
-- randomly allocated keys and the like.
instance (Ord k, Enum k, Eq a) => Eq (Order k a) where
    a == b = toList a == toList b

instance (Ord k, Enum k, Eq a, Ord a) => Ord (Order k a) where
    compare a b = compare (toList a) (toList b)

instance (Ord k, Enum k, Read a) => Read (Order k a) where
    -- readsPrec :: Int -> String -> [(OrderMap k a, String)]
    readsPrec _ s = let l = (read s :: [a]) in [(fromList l, "")]

instance (Ord k, Enum k, Monoid (Order k a)) => LL.ListLike (Order k a) a where
    uncons m =
        case order m of
          [] -> Nothing
          (hd : tl) -> Just (elems m ! hd, m {order = tl, elems = Map.delete hd (elems m), next = next m})
    null = null . order
    singleton x = fst $ insert x empty
    head m = case order m of
               (hd : _) -> elems m ! hd
               _ -> error "OrderMap.head"
    tail m = case order m of
               (hd : tl) -> m {order = tl, elems = Map.delete hd (elems m), next = next m}
               _ -> error "OrderMap.tail"

instance (Ord k, Enum k, Monoid (Order k a)) => LL.FoldableLL (Order k a) a where
    foldl f r0 xs = List.foldl f r0 (toList xs)
    foldr f r0 xs = List.foldr f r0 (toList xs)

-- | Remove the element at k if present.
deleteItem :: (Ord k, Enum k) => k -> Order k v -> Order k v
deleteItem k m = maybe m snd (view k m)

-- | Put a new element at the end of the order, allocating a new key
-- for it.
appendItem :: (Ord k, Enum k) => v -> Order k v -> Order k v
appendItem x = fst . insert x

insertItems :: forall k v. (Ord k, Enum k) => Order k v -> [v] -> ([k], Order k v)
insertItems om xs =
    foldr f ([], om) (reverse xs)
    where
      f x (ks, om') = let (om'', k) = insert x om' in ((k : ks), om'')

-- | Return the keys and values of the order.
toPairs :: (Ord k, Enum k) => Order k v -> [(k, v)]
toPairs m = map (\ k -> (k, (elems m) ! k)) (order m)

-- | Return only the values of the order, discarding the keys.
toList :: (Ord k, Enum k) => Order k v -> [v]
toList = map snd . toPairs

-- | Build an order from a list of values, allocating new all keys.
fromList :: (Ord k, Enum k) => [v] -> Order k v
fromList xs = foldl' (flip appendItem) empty xs

-- | Perform an operation on a of an Order's (key, value) pairs,
-- reassembling the resulting pairs into a new Order.
asList :: (Ord k, Enum k) => ([(k, v)] -> [(k, v)]) -> Order k v -> Order k v
asList f om = fromPairs . f . toPairs $ om

-- | Find the first value (along with the associated key) that
-- satisfies the predicate.
find :: forall k v. (Ord k, Enum k) => (v -> Bool) -> Order k v -> Maybe (k, v)
find p m =
    find' (order m)
    where
      find' :: [k] -> Maybe (k, v)
      find' [] = Nothing
      find' (k : more) =
          case Map.lookup k (elems m) of
            Nothing -> find' more
            Just x | not (p x) -> find' more
            Just x -> Just (k, x)

-- | Build a lens to focus on the k element of an Order.
lens_omat :: (Ord k, Enum k) => k -> Traversal' (Order k v) v
lens_omat k = lens getter setter . _Just
    where
      getter s = Map.lookup k (elems s)
      setter s a = maybe s (\ a' -> putItem k a' s) a

-- | Like view, but discards the remainder list
view' :: (Ord k, Enum k) => k -> Order k v -> v
view' i m = maybe (error "Order.view'") fst (view i m)

data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, FromJSON, ToJSON)
instance (Data k, Typeable k, Eq k, Ord k, Read k, Show k, Enum k,
          IsPath valuepath,
          U (UType valuepath) (Order k (SType valuepath))
         ) => IsPath (Path_OMap k valuepath) where
    type UType (Path_OMap k valuepath) = UType valuepath
    type SType (Path_OMap k valuepath) = Order k (SType valuepath)
    idPath = Path_OMap
    toLens (Path_At k p) = lens_omat k . toLens p
    toLens _ = lens u (\s a -> maybe s id (unU' a))

instance (IsPath (Path_OMap k v), Describe v, Describe (Proxy (SType (Path_OMap k v)))) => Describe (Path_OMap k v)
    where describe' _f (_p@(Path_At _k _wp)) = maybe (describe' _f (Proxy :: Proxy (SType (Path_OMap k v)))) Just (describe' Nothing _wp)
          describe' f p | p == idPath = describe' f (Proxy :: Proxy (SType (Path_OMap k v)))
          describe' _ p = error ("Unexpected path: " ++ show p)

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

#if !__GHCJS__
-- | Given the name of a type such as AbbrevPair, generate declarations
-- @@
--     newtype AbbrevPairID = AbbrevPairID {unAbbrevPairID :: IntJS} deriving (Eq, Ord, Read, Show, Data, Typeable)
--     type AbbrevPairs = Order AbbrevPairID AbbrevPair
--     instance Enum AbbrevPairID where
--       toEnum = AbbrevPairID . toEnum
--       fromEnum = fromEnum . unAbbrevPairID
-- @@
deriveOrder :: TypeQ -> Name -> [Name] -> Q [Dec]
deriveOrder ityp t supers = do
  let idname = mkName (nameBase t ++ "ID")
      unname = mkName ("un" ++ nameBase t ++ "ID")
      mpname = mkName (nameBase t ++ "s")
  idtype <- newtypeD (cxt []) idname [] (recC idname [varStrictType unname (strictType notStrict ityp) ]) ([''Eq, ''Ord, ''Read, ''Show, ''Data, ''Typeable] ++ supers)
  insts <- [d| instance Enum $(conT idname) where
                 toEnum = $(conE idname) . toEnum
                 fromEnum = fromEnum . $(varE unname) |]
  -- It would be nice to build the PathInfo instance for idname, but a
  -- call to derivePathInfo would try to reify it, and its too soon
  -- for that.
  omtype <- tySynD mpname [] [t|Order $(conT idname) $(conT t)|]
  return $ [idtype, omtype] ++ insts

instance (Ord k, Enum k, SafeCopy k, SafeCopy a) => SafeCopy (Order k a) where
    putCopy m = contain $ do safePut (elems m)
                             safePut (order m)
                             safePut (next m)
    getCopy = contain $ do elems_ <- safeGet
                           order_ <- safeGet
                           next_ <- safeGet
                           return $ Order {elems = elems_, order = order_, next = next_}

$(derivePathInfo ''Path_OMap)
$(deriveSafeCopy 0 'base ''Path_OMap)
$(deriveLiftMany [''Order])
#endif
