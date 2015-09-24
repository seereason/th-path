{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Appraisal.Permissions
    ( Permissions(..)
    , PermissionStatus(..)
    , Ownable(..)
    , UserIds
    ) where

import Data.Generics (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(..))
import Data.UserId (UserId(..))

newtype UserId_0 = UserId_0 { _unUserId_0 :: Integer } deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''UserId_0)

-- | We need to store UserIds here rather than Usernames in case
-- the user changes their name, or a black hat changes their name
-- to match some user whose data they want to steal.
data Permissions_1
    = Permissions_1 { _owner_1 :: UserId_0
                    , _writers_1 :: [UserId_0]   -- Sets would be preferable, but I think maybe they are causing problems
                    , _readers_1 :: [UserId_0] }
    deriving (Read, Show, Eq, Ord, Typeable, Data)
$(deriveSafeCopy 1 'base ''Permissions_1)

type UserIds = [UserId]

-- | We need to store UserIds here rather than Usernames in case
-- the user changes their name, or a black hat changes their name
-- to match some user whose data they want to steal.
data Permissions
    = Permissions { owner :: UserId
                  , writers :: UserIds   -- Sets would be preferable, but I think maybe they are causing problems
                  , readers :: UserIds }
    deriving (Read, Show, Eq, Ord, Typeable, Data)

$(deriveSafeCopy 2 'extension ''Permissions)

instance Migrate Permissions where
    type MigrateFrom Permissions = Permissions_1
    migrate (Permissions_1 o w r) = Permissions (old2new o) (map old2new w) (map old2new r)
        where
          old2new (UserId_0 n) = UserId n

data PermissionStatus
    = NotLoggedIn
    | TakeOwnership Bool
    | ShowPermissions Permissions
    deriving (Read, Show, Eq, Ord, Typeable, Data)

class Ownable a where
    getPermissions :: a -> Permissions
    putPermissions :: Permissions -> a -> a
