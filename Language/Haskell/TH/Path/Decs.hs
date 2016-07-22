-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the Path types, and the universal path type.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
module Language.Haskell.TH.Path.Decs
    ( derivePaths
    , writePaths
    , testAndWritePaths
    ) where

import Control.Exception as E (IOException, throw, try)
import Control.Lens (Iso', makeClassyFor)
import Control.Monad.Writer (MonadWriter, execWriterT, runWriterT, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Set as Set (toList)
import Data.UUID.Types (UUID)
import Data.UUID.Orphans (showUUID)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Path.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Common (HasType(asType), HasTypeQ(asTypeQ), telld, tells)
import Language.Haskell.TH.Path.Core (U(u, unU'), ulens', SinkType)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Peek (peekDecs)
import Language.Haskell.TH.Path.View (View)
import Language.Haskell.TH.Syntax (addDependentFile, Quasi(qReify))
import Language.Haskell.TH.Path.Lens (lensNamePairs)
import Language.Haskell.TH.Path.Prelude (friendlyNames, pprint1, pprintW)
import Language.Haskell.TH.Path.TypeGraph (allPathStarts)
import Language.Haskell.TH.Path.Vertex (TGVSimple, typeNames)
import System.Directory (removeFile, renameFile)
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
  x <- runQ $ newName "x"
  -- Sort these so the U constructors don't change any more than necessary.
  types <- (map asTypeQ . sortBy (compare `on` (pprint1 . asType)) . Set.toList) <$> allPathStarts
  info <- mapM (\(typeq, n) -> do
                  typ <- runQ typeq
                  simple <- (not . null) <$> reifyInstancesWithContext ''SinkType [typ]
                  hasview <- (not . null) <$> reifyInstancesWithContext ''View [typ]
                  (,,,) <$> pure typ
                        <*> runQ (newName ("U" ++ show n))
                        <*> pure simple
                        <*> pure hasview
               ) (zip types ([1..] :: [Int]))
  mapM_ (\(typ, ucon, _, _) ->
             tells [do a <- newName "a"
                       instanceD (cxt []) [t|U $(conT uname) $(pure typ)|]
                                 [funD 'u [clause [] (normalB (conE ucon)) []],
                                  funD 'unU' [clause [conP ucon [varP a]] (normalB [|Just $(varE a)|]) [],
                                              clause [wildP] (normalB [|Nothing|]) []]
                                 ]]) info
  tells [dataD (pure []) uname []
               (map (\(typ, ucon, _, _) -> normalC ucon [strictType notStrict (pure typ)]) info)
               [''Eq, ''Ord, ''Read, ''Data, ''Typeable, ''Generic, ''FromJSON, ''ToJSON],
         funD (mkName "uMatch") (map (\(_, ucon, _, _) -> clause [conP ucon [wildP], conP ucon [wildP]] (normalB [|True|]) []) info ++
                                 [clause [wildP, wildP] (normalB [|False|]) []]),
         funD (mkName "uSimple") (map (\(_, ucon, simple, _) -> clause [conP ucon [wildP]] (normalB (lift simple)) []) info),
         funD (mkName "uView") (map (\(_, ucon, _, hasview) -> clause [conP ucon [wildP]] (normalB (lift hasview)) []) info),
         instanceD (cxt []) [t|Show $(conT uname)|]
                   [funD 'show (map (\(typ, ucon, _, _) ->
                                         clause
                                           [conP ucon [varP x]]
                                           -- Work around the broken Show instance in UUID
                                           (normalB [|"(u (" ++ $(if typ == ConT ''UUID
                                                                  then [|showUUID $(varE x)|]
                                                                  else [|show $(varE x)|])
                                                             ++ $(lift (" :: " ++ pprint1 typ ++ ") :: " ++ nameBase uname ++ ")"))|])

                                           []) info)]
        ]
  telld [d| ulens :: U $(conT uname) a => Iso' $(conT uname) a
            ulens = ulens' Proxy |]
  return $ conT uname

writePaths :: String -> String -> FilePath -> [FilePath] -> [Dec] -> Q [Dec]
writePaths hdText tlText dest deps decs = do
  new <- paths hdText tlText deps decs
  runIO $ removeFileMaybe (dest <> "~")
  runIO $ renameFileMaybe dest (dest <> "~")
  runIO $ removeFileMaybe dest
  runIO $ writeFile dest new
  return decs

-- | Write the new paths file if it doesn't exist.  If it does, make
-- sure the new text matches the old using testPaths.
testAndWritePaths :: String -> String -> FilePath -> [FilePath] -> [Dec] -> Q [Dec]
testAndWritePaths hdText tlText dest deps decs = do
  new <- paths hdText tlText deps decs
  runIO $ testPaths new dest
  return decs

paths :: String -> String -> [FilePath] -> [Dec] -> Q String
paths hdText tlText deps decs = do
  runQ $ mapM_ addDependentFile deps
  let code = (unlines . map (pprintW 250) . {-sort .-} map friendlyNames) decs
  return $ hdText <> code <> tlText

-- | See if the new Paths code matches the old, if not write it to a
-- file with the suffix ".new" and throw an error so the new code can
-- be inspected and checked in.
testPaths :: String -> FilePath -> IO ()
testPaths new dest = do
  old <- try (readFile dest) >>=
         either (\(e :: IOException) -> case isDoesNotExistError e of
                                          True -> pure Nothing
                                          False -> throw e) (pure . Just)
  removeFileMaybe $ dest <> ".new"
  case old of
    Nothing -> writeFile dest new -- No old version
    Just x | x /= new -> -- Old version differs
               do writeFile (dest <> ".new") new
                  error $ "Generated " <> dest <> ".new does not match existing " <> dest
    _ -> pure () -- Old version matches

removeFileMaybe :: FilePath -> IO ()
removeFileMaybe path =
    try (removeFile path) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> pure ()
                                     False -> throw e) pure

renameFileMaybe :: FilePath -> FilePath -> IO ()
renameFileMaybe oldpath newpath =
    try (renameFile oldpath newpath) >>=
    either (\(e :: IOException) -> case isDoesNotExistError e of
                                     True -> pure ()
                                     False -> throw e) pure

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
