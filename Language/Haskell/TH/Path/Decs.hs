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
    , allDecsToFile
    ) where

import Control.Exception as E (catch, IOException, throw, try)
import Control.Monad.Writer (MonadWriter, execWriterT)
import Data.List (sort)
import Data.Monoid ((<>))
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Decs.HasPaths (pathDecs)
import Language.Haskell.TH.Path.Decs.IsPathStart (peekDecs)
import Language.Haskell.TH.Path.Decs.Lens (lensDecs)
import Language.Haskell.TH.Path.Decs.PathTypeDecs (pathTypeDecs)
import Language.Haskell.TH.Path.Decs.ToLens (toLensDecs)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames, pprint1, pprintW)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathStarts, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

derivePaths :: [TypeQ] -> TypeQ -> Q [Dec]
derivePaths topTypes thisType =
    runTypeGraphT (execWriterT . doType =<< runQ thisType) =<< sequence topTypes

allDecs :: forall m. (TypeGraphM m) => m [Dec]
allDecs = execWriterT $ allPathStarts >>= mapM_ doNode

allDecsToFile :: [TypeQ] -> Maybe FilePath -> Maybe FilePath -> FilePath -> Q [Dec]
allDecsToFile st hd tl dest = do
  runQ $ maybe (pure ()) addDependentFile hd
  runQ $ maybe (pure ()) addDependentFile tl
  hdText <- runQ $ runIO $ maybe (pure mempty) readFile hd
  tlText <- runQ $ runIO $ maybe (pure mempty) readFile tl
  old <- runQ $ runIO (try (readFile dest) >>=
                       either (\(e :: IOException) -> case isDoesNotExistError e of
                                                 True -> pure Nothing
                                                 False -> throw e) (pure . Just))
  st' <- runQ $ sequence st
  decs <- runTypeGraphT allDecs st'
  let code = (unlines . map pprintW . sort . map friendlyNames) decs
      removeFileMaybe :: FilePath -> IO ()
      removeFileMaybe path =
          try (removeFile path) >>=
          either (\(e :: IOException) -> case isDoesNotExistError e of
                                           True -> pure ()
                                           False -> throw e) pure
      new = hdText <> code <> tlText
  case Just new == old of
    True -> runQ $ runIO $ do
      removeFileMaybe dest
      removeFileMaybe $ dest <> ".new"
      writeFile dest new
    False -> runQ $ runIO $ do
      writeFile (dest <> ".new") new
      error $ "Generated " <> dest <> ".new does not match existing " <> dest
  pure decs

doType :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => Type -> m ()
doType t = tgvSimple t >>= maybe (error $ "doType: No node for " ++ pprint1 t) doNode

doNode :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  lensDecs v      -- generate lenses using makeClassyFor
  pathTypeDecs v  -- generate Path types and the HasIdPath instances
  toLensDecs v    -- generate ToLens instances
  pathDecs v      -- generate HasPaths instances
  peekDecs v      -- generate IsPathStart instances
