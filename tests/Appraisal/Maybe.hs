{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Appraisal.Maybe
    ( Maybe'(Just', Nothing')
    , lens_mrs'
    ) where

import Control.Applicative.Error (maybeRead)
import Control.Lens (Iso', iso)
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift as TH (deriveLiftMany)

-- | Alternative version of Maybe for which we can create alternative instances.
data Maybe' a = Just' a | Nothing' deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, ToJSON, FromJSON)

lens_mrs' :: (Show a, Read a) => Iso' (Maybe' a) String
lens_mrs' = iso getter setter
  where getter Nothing' = ""
        getter (Just' x) = show x
        setter x = maybe Nothing' Just' (maybeRead x)

$(deriveSafeCopy 1 'base ''Maybe')
$(deriveLiftMany [''Maybe'])
