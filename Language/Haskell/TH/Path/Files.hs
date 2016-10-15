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
module Language.Haskell.TH.Path.Files
    ( writePaths
    , testAndWritePaths
    ) where

import Control.Exception as E (IOException, throw, try)
import Data.Monoid ((<>))
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames, pprintW)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (removeFile, renameFile)
import System.IO.Error (isDoesNotExistError)

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
