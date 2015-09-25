{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, RankNTypes, ScopedTypeVariables #-}
module Appraisal.Utils.Twins
    ( gzip
    , extQ2
    , extM2
    , gContinue
    ) where

import Data.Generics (Typeable, GenericQ, GenericM, gzipWithM, toConstr, cast)
import Data.Maybe (fromMaybe)
-- import Debug.Trace (trace)

-- |Generic zip to do some IO based on the differences between the
-- two versions.  The query function returns Nothing when the two
-- arguments should be traversed into, 
gzip :: Monad m => GenericQ (GenericQ Bool) -> GenericQ (GenericM m) -> GenericQ (GenericM m)
gzip f m x y = 
    -- Do we need to traverse?  This will return Nothing until we reach a primitive type.
    case f x y of
      True -> if toConstr x == toConstr y then gzipWithM (gzip f m) x y else return y
      False -> m x y -- do the IO required for changing x to y
    where
      -- t1 y = trace ("gzip: y=" ++ take 100 (gshow y)) y
      -- t2 flag = trace ("gzip: f x y :: " ++ dataTypeName (dataTypeOf y) ++ " = " ++ take 100 (gshow y) ++ " -> " ++ show flag) flag
      -- t3 result = trace ("gzip: m x y") result

extQ2 :: forall a b d e r. (Typeable a, Typeable b, Typeable d, Typeable e) =>
         (a -> b -> r) -> (d -> e -> r) -> a -> b -> r
extQ2 d q x y = fromMaybe (d x y) $ cast x >>= \x' -> cast y >>= \ y' -> Just (q x' y')

extM2 :: forall a b d e m. (Monad m, Typeable a, Typeable b, Typeable d, Typeable e) =>
         (a -> b -> m b) -> (d -> e -> m e) -> a -> b -> m b
extM2 d q x y =
    case (cast x, cast y) of
      (Just x', Just y') -> 
          do z' <- q x' y'
             case cast z' of
               Just z -> return z
               Nothing -> d x y
      _ -> d x y

-- Normally, we should continue to traverse any non-primitive database.
gContinue :: GenericQ (GenericQ Bool)
gContinue _a _b = True
