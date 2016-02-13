-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the Path types, and the universal path type.

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
module Language.Haskell.TH.Path.Decs
    ( allDecs
    ) where

import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, execWriterT)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Decs.IsPath (peekDecs)
import Language.Haskell.TH.Path.Decs.Lens (lensDecs)
import Language.Haskell.TH.Path.Decs.PathsOf (pathDecs)
import Language.Haskell.TH.Path.Decs.PathType (pathTypeDecs)
import Language.Haskell.TH.Path.Decs.ToLens (toLensDecs)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathStarts, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

allDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
allDecs = execWriterT $ allPathStarts >>= mapM_ doNode

doNode :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  peekDecs v
  toLensDecs v
  -- generate the IsPath instance declarations
  pathDecs v
  -- generate the lenses
  lensDecs v
  pathTypeDecs v
