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

import Control.Exception as E (IOException, throw, try)
import Control.Monad.Writer (MonadWriter, execWriterT, runWriterT)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Set as Set (toList)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasTypeQ(asTypeQ), tells)
import Language.Haskell.TH.Path.Core (U(u, unU))
import Language.Haskell.TH.Path.Decs.Lens (lensDecs)
import Language.Haskell.TH.Path.Decs.Paths (pathDecs)
import Language.Haskell.TH.Path.Decs.PathStart (peekDecs)
import Language.Haskell.TH.Path.Decs.PathTypeDecs (pathTypeDecs)
import Language.Haskell.TH.Path.Decs.ToLens (toLensDecs)
import Language.Haskell.TH.Path.Decs.UPath (upathTypeDecs)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames, pprint1, pprintW)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathStarts, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

-- instance TypeGraphM m => TypeGraphM (StateT Int m)

derivePaths :: TypeQ -> [TypeQ] -> TypeQ -> Q [Dec]
derivePaths utype topTypes thisType =
    runTypeGraphT (execWriterT . doType utype =<< runQ thisType) =<< sequence topTypes

allDecs :: forall m. (TypeGraphM m) => ([Dec] -> [Dec]) -> m [Dec]
allDecs order = do
  (uname, udecs) <- runWriterT doUniv
  moreDecs <- execWriterT (allPathStarts >>= mapM_ (doNode uname))
  return $ order (udecs <> moreDecs)

doUniv :: (TypeGraphM m, MonadWriter [Dec] m) => m TypeQ
doUniv = do
  uname <- runQ $ newName "Univ"
  types <- (map asTypeQ . Set.toList) <$> allPathStarts
  cons <- mapM (\(typ, n) -> do
                  ucon <- runQ $ newName ("U" ++ show n)
                  tells [newName "a" >>= \a ->
                         instanceD (cxt []) [t|U $(conT uname) $typ|] [funD 'u [clause [] (normalB (conE ucon)) []],
                                                                       funD 'unU [clause [conP ucon [varP a]] (normalB (varE a)) []]]]
                  return $ normalC ucon [strictType notStrict typ])
               (zip types ([1..] :: [Int]))
  tells [dataD (pure []) uname [] cons []]
  return $ conT uname

allDecsToFile :: ([Dec] -> [Dec]) -> [TypeQ] -> Maybe FilePath -> Maybe FilePath -> FilePath -> [FilePath] -> Q [Dec]
allDecsToFile order st hd tl dest deps = do
  runQ $ mapM_ addDependentFile $ catMaybes [hd, tl] ++ deps
  hdText <- runQ $ runIO $ maybe (pure mempty) readFile hd
  tlText <- runQ $ runIO $ maybe (pure mempty) readFile tl
  old <- runQ $ runIO (try (readFile dest) >>=
                       either (\(e :: IOException) -> case isDoesNotExistError e of
                                                 True -> pure Nothing
                                                 False -> throw e) (pure . Just))
  st' <- runQ $ sequence st
  decs <- runTypeGraphT (allDecs order) st'
  let code = (unlines . map pprintW . {-sort .-} map friendlyNames) decs
      removeFileMaybe :: FilePath -> IO ()
      removeFileMaybe path =
          try (removeFile path) >>=
          either (\(e :: IOException) -> case isDoesNotExistError e of
                                           True -> pure ()
                                           False -> throw e) pure
      new = hdText <> code <> tlText
  case maybe True (== new) old of
    True -> runQ $ runIO $ do
      removeFileMaybe dest
      removeFileMaybe $ dest <> ".new"
      writeFile dest new
    False -> runQ $ runIO $ do
      writeFile (dest <> ".new") new
      error $ "Generated " <> dest <> ".new does not match existing " <> dest
  pure decs

doType :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> Type -> m ()
doType utype t = tgvSimple t >>= maybe (error $ "doType: No node for " ++ pprint1 t) (doNode utype)

doNode :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
doNode utype v = do
  pathTypeDecs v  -- generate Path types and the IdPath instances
  upathTypeDecs utype v  -- generate Path types and the IdPath instances
  lensDecs v      -- generate lenses using makeClassyFor
  pathDecs utype v      -- generate HasPaths instances
  peekDecs utype v      -- generate PathStart instances
  toLensDecs utype v    -- generate ToLens instances
