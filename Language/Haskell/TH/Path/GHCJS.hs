-- | Provide dummy versions of some TH functions when using ghcjs.
{-# LANGUAGE CPP #-}
module Language.Haskell.TH.Path.GHCJS
    ( module Data.SafeCopy
    , module Web.Routes.TH
#if __GHCJS__
    , deriveSafeCopy
    , derivePathInfo
#endif
    ) where

#if __GHCJS__
import Data.SafeCopy hiding (deriveSafeCopy)
import Web.Routes.TH hiding (derivePathInfo)

deriveSafeCopy :: Name -> Name -> Q [Dec]
deriveSafeCopy _ _ = pure []

derivePathInfo :: Name -> Q [Dec]
derivePathInfo _ = pure []
#else
import Data.SafeCopy
import Web.Routes.TH
#endif
