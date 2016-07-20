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

module Language.Haskell.TH.Path.Order () where

import Control.Lens (lens)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Order
import Data.Proxy (Proxy(Proxy))
import Data.Tree (Tree(Node))
import Data.Typeable (Typeable)
import Language.Haskell.TH.Path.Core (Describe(..), IsPath(..), makeCol, makeRow, makeTrees, PathStart(..), Peek(..), U(u, unU'))
-- import Language.Haskell.TH.Path.GHCJS (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import Language.Haskell.TH.TypeGraph.Prelude ({-some Lift instances?-})
import Prelude hiding (init)

