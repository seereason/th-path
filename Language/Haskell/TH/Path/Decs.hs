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
    , writePaths
    ) where

import Control.Exception as E (IOException, throw, try)
import Control.Lens (Iso', makeClassyFor)
import Control.Monad.Writer (MonadWriter, execWriterT, runWriterT, tell)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Set as Set (toList)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasTypeQ(asTypeQ), telld, tells)
import Language.Haskell.TH.Path.Core (U(u, unU'), ulens')
import Language.Haskell.TH.Path.Decs.PathStart (peekDecs)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Syntax (addDependentFile, Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames, pprint1, pprintW)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathStarts)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, typeNames)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

derivePaths :: [TypeQ] -> Q [Dec]
derivePaths st = do
  st' <- runQ $ sequence st
  runTypeGraphT allDecs st'

allDecs :: forall m. (TypeGraphM m) => m [Dec]
allDecs = do
  (uname, udecs) <- runWriterT doUniv
  moreDecs <- execWriterT (allPathStarts >>= mapM_ (doNode uname))
  return $ (udecs <> moreDecs)

doUniv :: (TypeGraphM m, MonadWriter [Dec] m) => m TypeQ
doUniv = do
  uname <- runQ $ newName "Univ"
  types <- (map asTypeQ . Set.toList) <$> allPathStarts
  cons <- mapM (\(typ, n) -> do
                  ucon <- runQ $ newName ("U" ++ show n)
                  tells [newName "a" >>= \a ->
                         instanceD (cxt []) [t|U $(conT uname) $typ|] [funD 'u [clause [] (normalB (conE ucon)) []],
                                                                       funD 'unU' [clause [conP ucon [varP a]] (normalB [|Just $(varE a)|]) [],
                                                                                   clause [wildP] (normalB [|Nothing|]) []]]]
                  return $ normalC ucon [strictType notStrict typ])
               (zip types ([1..] :: [Int]))
  tells [dataD (pure []) uname [] cons [''Eq, ''Show, ''Data, ''Typeable]]
  telld [d| ulens :: U $(conT uname) a => Iso' $(conT uname) a
            ulens = ulens' Proxy |]
  return $ conT uname

writePaths :: Maybe FilePath -> Maybe FilePath -> FilePath -> [FilePath] -> [Dec] -> Q [Dec]
writePaths hd tl dest deps decs = do
  runQ $ mapM_ addDependentFile $ catMaybes [hd, tl] ++ deps
  hdText <- runQ $ runIO $ maybe (pure mempty) readFile hd
  tlText <- runQ $ runIO $ maybe (pure mempty) readFile tl
  old <- runQ $ runIO (try (readFile dest) >>=
                       either (\(e :: IOException) -> case isDoesNotExistError e of
                                                 True -> pure Nothing
                                                 False -> throw e) (pure . Just))
  let code = (unlines . map (pprintW 250) . {-sort .-} map friendlyNames) decs
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

-- doType :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> Type -> m ()
-- doType utype t = tgvSimple t >>= maybe (error $ "doType: No node for " ++ pprint1 t) (doNode utype)

doNode :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
doNode utype v = do
  lensDecs v           -- generate lenses using makeClassyFor
  peekDecs utype v     -- generate IsPath and PathStart instances
  -- uLensDecs utype v    -- generate ToLens instances for UPath types

-- | Make lenses for a type with the names described by fieldLensNamePair, which is a little
-- different from the namer used in th-typegraph (for historical reasons I guess.)
lensDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
lensDecs v = mapM makePathLens (toList (typeNames v)) >>= tell . concat
    where
      makePathLens :: ContextM m => Name -> m [Dec]
      makePathLens tname = qReify tname >>= execWriterT . doInfo
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec (NewtypeD _ tname _ _ _) = do
        pairs <- lensNamePairs fieldLensNamePair tname
        tell =<< runQ (makeClassyFor (className tname) (lensName tname) pairs tname)
      doDec (DataD _ tname _ _ _) = do
        pairs <- lensNamePairs fieldLensNamePair tname
        tell =<< runQ (makeClassyFor (className tname) (lensName tname) pairs tname)
      doDec _ = return ()
      className tname = "Has" ++ nameBase tname
      lensName tname = "lens_" ++ uncap (nameBase tname)
      uncap :: String -> String
      uncap (n : ame) = toLower n : ame
      uncap "" = ""

-- | Version of fieldLensName suitable for use as argument to
-- findNames below.
fieldLensNamePair :: Name -> Name -> Name -> (String, String)
fieldLensNamePair tname _cname fname = (nameBase fname, nameBase (fieldLensNameOld tname fname))

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)
