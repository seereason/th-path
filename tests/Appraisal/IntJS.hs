{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Appraisal.IntJS
    ( IntJS
    , ToIntJS(intJS)
#if !__GHCJS__
    , deriveOrderJS
#endif
    , JSONText(..)
    , iso_JSONText
    , gjsonIso
    , gjsonLens
    ) where

import Control.Lens (iso, Iso')
import Data.Aeson (ToJSON(toJSON), FromJSON(parseJSON))
import Data.Generics (Typeable)
import Data.Int (Int32)
import Data.Map as Map (fromList, Map, toList)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Text as Text (null, pack)
import Data.Text.Read (decimal, signed)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Order (fromPairs, Order, toPairs)
#if !__GHCJS__
import Language.Haskell.TH.Path.Order (deriveOrder)
#endif
import Language.Javascript.JMacro (ToJExpr(toJExpr), JExpr(ValExpr), JVal(JInt))
import Prelude hiding (init, succ)
import Text.JSON.Generic (Data, decodeJSON, encodeJSON)
import Web.Routes.PathInfo
import Web.Routes.TH (derivePathInfo)

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

instance (ToJSON k, ToJSON v) => ToJSON (Map k v) where
    toJSON mp = toJSON (Map.toList mp)

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Map k v) where
    parseJSON mp = Map.fromList <$> parseJSON mp

instance (Ord k, Enum k, ToJSON k, ToJSON a) => ToJSON (Order k a) where
  toJSON = toJSON . toPairs

instance (Ord k, Enum k, FromJSON k, FromJSON a) => FromJSON (Order k a) where
  parseJSON = fmap fromPairs . parseJSON

#if !__GHCJS__
deriveOrderJS :: Name -> Q [Dec]
deriveOrderJS t = do
  decs <- deriveOrder [t|IntJS|] t [''Generic, ''FromJSON, ''ToJSON]
  let idname = mkName (nameBase t ++ "ID")
      unname = mkName ("un" ++ nameBase t ++ "ID")
  insts <- [d| instance ToIntJS $(conT idname) where intJS = $(varE unname) |]
  return $ decs ++ insts
#endif

newtype JSONText = JSONText {unJSONText :: String} deriving (Eq, Ord, Read, Show, Data, Typeable, Monoid, Generic, FromJSON, ToJSON)

iso_JSONText :: Iso' JSONText String
iso_JSONText = iso unJSONText JSONText

#if !__GHCJS__
$(derivePathInfo ''JSONText)
$(deriveSafeCopy 1 'base ''JSONText)
#endif

gjsonIso :: Data a => Iso' a JSONText
gjsonIso = iso (JSONText . encodeJSON) (decodeJSON . unJSONText)

gjsonLens :: Data a => Iso' a JSONText
gjsonLens = gjsonIso
