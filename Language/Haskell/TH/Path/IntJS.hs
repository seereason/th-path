{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.IntJS
    ( IntJS
    , ToIntJS(intJS)
    ) where

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import Data.Data (Data)
import Data.Int (Int32)
import Control.Lens (Traversal', _Just, lens)
import Data.List as List (partition, elem, foldl, foldl', foldr, filter)
import qualified Data.ListLike as LL
import Data.Map as Map (Map, (!))
import qualified Data.Map as Map
import Data.Monoid (Monoid(mempty, mappend))
import Data.SafeCopy (SafeCopy(..), contain, safeGet, safePut)
import Data.Text as Text (null, pack)
import Data.Text.Read (decimal, signed)
import Data.Typeable (Typeable)
import Language.Haskell.TH
import Language.Javascript.JMacro (ToJExpr(toJExpr), JExpr(ValExpr), JVal(JInt))
import Prelude hiding (init, succ)
import qualified Prelude (succ)
import Web.Routes.PathInfo

-- | Javascript can't handle Int64, so until there is an Int53 in
-- haskell this will have to do.
type IntJS = Int32

instance ToJExpr IntJS where
    toJExpr = ValExpr . JInt . fromIntegral

class ToIntJS k where
    intJS :: k -> IntJS

instance ToIntJS IntJS where
    intJS = id

instance PathInfo IntJS where
  toPathSegments i = [pack $ show i]
  fromPathSegments = pToken (const "IntJS") checkInt
   where checkInt txt =
           case signed decimal txt of
             (Left _e) -> Nothing
             (Right (n, r))
                 | Text.null r -> Just n
                 | otherwise -> Nothing
