{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      : Appraisal.Data.UUID
-- Copied to create a Data instance.  Rejected by upstream.


Module      : Data.UUID
Copyright   : (c) 2008,2012 Antoine Latter

License     : BSD-style

Maintainer  : aslatter@gmail.com
Stability   : experimental
Portability : portable


This library is useful for comparing, parsing and
printing Universally Unique Identifiers.
See <http://en.wikipedia.org/wiki/UUID> for the general idea.
See <http://tools.ietf.org/html/rfc4122> for the specification.

* Random UUIDs may be generated using 'Data.UUID.V4.nextRandom' or
your favorite instance of 'System.Random.Random'.

* We have an implementation of generating a UUID from the hardware
MAC address and current system time in "Data.UUID.V1".

* For name-based generation of UUIDs using SHA-1 hashing see
"Data.UUID.V5".
-}

module Appraisal.Utils.UUID(UUID'
                          ,toString
                          ,fromString
                          ,toByteString
                          ,fromByteString
                          ,toWords
                          ,fromWords
                          ,null
                          ,nil
                          ) where

import Appraisal.Utils.UUID.Internal
import Data.Text as T hiding (null)
import Prelude hiding (null)
import Web.Routes.PathInfo

-- Everything is really implemented in Data.UUID.Internal,
-- but I don't want to export the constructors out of the
-- package.

instance PathInfo UUID' where
  toPathSegments = (:[]) . T.pack . toString
  fromPathSegments = pToken (const "UUID") checkUUID
    where checkUUID txt = fromString (T.unpack txt)
