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
    ( derivePaths
    , allDecs
    ) where

import Control.Monad.Writer (MonadWriter, execWriterT)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Decs.HasPaths (pathDecs)
import Language.Haskell.TH.Path.Decs.IsPathStart (peekDecs)
import Language.Haskell.TH.Path.Decs.Lens (lensDecs)
import Language.Haskell.TH.Path.Decs.PathTypeDecs (pathTypeDecs)
import Language.Haskell.TH.Path.Decs.ToLens (toLensDecs)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathStarts, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

derivePaths :: [TypeQ] -> TypeQ -> Q [Dec]
derivePaths topTypes thisType =
    runTypeGraphT (execWriterT . doType =<< runQ thisType) =<< sequence topTypes

allDecs :: forall m. (TypeGraphM m) => m [Dec]
allDecs = execWriterT $ allPathStarts >>= mapM_ doNode

doType :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => Type -> m ()
doType t = tgvSimple t >>= doNode

doNode :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  lensDecs v      -- generate lenses using makeClassyFor
  pathTypeDecs v  -- generate Path types and the HasIdPath instances
  toLensDecs v    -- generate ToLens instances
  pathDecs v      -- generate HasPaths instances
  peekDecs v      -- generate IsPathStart instances
