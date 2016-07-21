-- | Instances that don't depend on anything else in this package.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
module Language.Haskell.TH.Path.Instances () where

import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (pack)
import Language.Haskell.TH.Ppr (Ppr(ppr))
import Language.Haskell.TH.PprLib (ptext)
import Language.Haskell.TH.Instances ()
import Web.Routes

#if !MIN_VERSION_aeson(0,11,0)
import Data.Aeson hiding (decode')
import Data.Aeson.Types (typeMismatch)

-- Backport the JSON instances from aeson-0.11.
instance ToJSON (Proxy a) where
   toJSON _ = Null
   {-# INLINE toJSON #-}

instance FromJSON (Proxy a) where
    {-# INLINE parseJSON #-}
    parseJSON Null = pure Proxy
    parseJSON v    = typeMismatch "Proxy" v
#endif

instance Ppr () where
    ppr () = ptext "()"

#if !__GHCJS__
-- $(derivePathInfo ''Proxy)
instance {-PathInfo t =>-} PathInfo (Proxy t) where
    toPathSegments inp = case inp of Proxy -> [pack "proxy"]
    fromPathSegments = segment (pack "proxy") >> return Proxy
$(deriveSafeCopy 0 'base ''Proxy)
#endif
