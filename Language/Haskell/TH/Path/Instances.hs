-- | Orphanage.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Instances () where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.States (MonadStates(getPoly, putPoly))
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (WriterT)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set.Extra as Set (Set)
import Data.Text (pack)
import Language.Haskell.TH.Context (ContextM, InstMap)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.TypeGraph.Expand (ExpandMap)
import Web.Routes

instance (Monad m, MonadStates InstMap m) => MonadStates InstMap (StateT (Set s) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates ExpandMap m) => MonadStates ExpandMap (StateT (Set s) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates String m) => MonadStates String (StateT (Set s) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance ContextM m => ContextM (StateT (Set a) m)
instance ContextM m => ContextM (ReaderT t m)

instance (Monoid w, ContextM m) => ContextM (WriterT w m)

#if !__GHCJS__
-- $(derivePathInfo ''Proxy)
instance {-PathInfo t =>-} PathInfo (Proxy t) where
    toPathSegments inp = case inp of Proxy -> [pack "proxy"]
    fromPathSegments = segment (pack "proxy") >> return Proxy
$(deriveSafeCopy 0 'base ''Proxy)
#endif
