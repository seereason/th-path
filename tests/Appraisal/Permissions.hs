{-# LANGUAGE CPP, DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Appraisal.Permissions
    ( Permissions(..)
    , PermissionStatus(..)
    , Ownable(..)
    , UserIds
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Generics (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.UserId (UserId(..))
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift (deriveLiftMany)

newtype UserId_0 = UserId_0 { _unUserId_0 :: Integer } deriving (Eq, Ord, Read, Show, Data, Typeable)
#if !__GHCJS__
$(deriveSafeCopy 0 'base ''UserId_0)
#endif

type UserIds = [UserId]

-- | We need to store UserIds here rather than Usernames in case
-- the user changes their name, or a black hat changes their name
-- to match some user whose data they want to steal.
data Permissions
    = Permissions { owner :: UserId
                  , writers :: UserIds   -- Sets would be preferable, but I think maybe they are causing problems
                  , readers :: UserIds }
    deriving (Read, Show, Eq, Ord, Typeable, Data, Generic, ToJSON, FromJSON)

data PermissionStatus
    = NotLoggedIn
    | TakeOwnership Bool
    | ShowPermissions Permissions
    deriving (Read, Show, Eq, Ord, Typeable, Data)

class Ownable a where
    getPermissions :: a -> Permissions
    putPermissions :: Permissions -> a -> a

#if !__GHCJS__
$(deriveSafeCopy 2 'base ''Permissions)
$(deriveLiftMany [''Permissions, ''UserId])
#endif
