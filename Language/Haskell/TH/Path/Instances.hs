-- | Orphanage.

{-# OPTIONS_GHC -fno-warn-orphans #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Instances () where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.States (MonadStates(getPoly, putPoly))
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (WriterT)
import Data.Set.Extra as Set (Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.TypeGraph.Expand (ExpandMap)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

instance (Monad m, MonadStates InstMap m) => MonadStates InstMap (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates ExpandMap m) => MonadStates ExpandMap (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates String m) => MonadStates String (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates InstMap m) => MonadStates InstMap (StateT (Set TGVSimple) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates ExpandMap m) => MonadStates ExpandMap (StateT (Set TGVSimple) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates String m) => MonadStates String (StateT (Set TGVSimple) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance ContextM m => ContextM (StateT (Set Name) m)
instance ContextM m => ContextM (StateT (Set TGVSimple) m)

instance ContextM m => ContextM (ReaderT t m)

instance (Monoid w, ContextM m) => ContextM (WriterT w m)
