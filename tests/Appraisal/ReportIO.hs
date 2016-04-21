{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
module Appraisal.ReportIO
    ( validateImages
    , Result(..)
    , reportOutput
    , reportUprightImages
    , reportPrinterImages
    , reportThumbImages
    , reportEnlargedImages
    , reportEditedImages
    ) where

--import Debug.Trace

import Appraisal.Abbrevs (runAbbrevsM)
import Appraisal.Cache (MonadCache, cacheInsert)
import Appraisal.Config (Paths(images))
import Appraisal.File (MonadFileCacheTop, fileChksum)
import Appraisal.Html (latexFromMarkup)
import Appraisal.ImageCache (ImageCacheState, ImageKey, runImageCacheIO, fileCachePath')
import Appraisal.ImageFile (ImageFile(imageFile, imageFileType), extension)
import Appraisal.Report (Report(..), reportPath)
import Appraisal.ReportAbbrevs (abbrevsLaTeX)
import Appraisal.ReportImage (ReportImage(..))
import Appraisal.ReportImageCache (uprightKey, printedKey, enlargedKey, thumbKey, editedKey)
import Appraisal.ReportLaTeX(reportToLaTeX)
import Appraisal.Utils.ErrorWithIO (logException, ensureLink)
import Appraisal.Utils.Generics (gFind)
--import Appraisal.Utils.Twins (extM2, extQ2, gContinue, gzip)
import Control.Exception (IOException, tryJust, catchJust)
import Control.Monad (guard, when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as U
import Data.Generics (everywhereM, mkM)
import Data.List(isInfixOf)
import Data.Map as Map (lookup, fromList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.URI (URI)
import System.Directory(removeFile, doesFileExist)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath(splitFileName, dropTrailingPathSeparator, (</>), (<.>))
import System.IO.Error (isDoesNotExistError, isAlreadyInUseError)
import System.Log.Logger (Priority(DEBUG), logM)
import System.Process (CreateProcess(cwd), proc, showCommandForUser)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)
import System.Unix.FilePath ((<++>))
import Text.LaTeX (render, textell)

-- | Make sure all the required files for building the report exist in
-- the image directory.
validateImages :: (MonadCatch m, MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m) =>
                  Report -> m Report
validateImages report =
    everywhereM (mkM validateReportImage) report
    where
      validateReportImage :: (MonadCatch m, MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m) =>
                             ReportImage -> m ReportImage
      validateReportImage x =
          validatePicImage (picOriginal x) >>
          --validateImageFile (picEdited' x) >>
          --validateImageFile (picThumb' x) >>
          -- validateImageFile (picPrinter' x) >>
          -- validateImageFile (picEnlarged' x) >>
          return x
      validatePicImage :: (MonadCatch m, MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m) =>
                          Maybe (Either URI ImageFile) -> m (Maybe (Either URI ImageFile))
      validatePicImage (Just (Right x)) = validateImageFile (Just x) >>= return . maybe Nothing (Just . Right)
      validatePicImage x = return x

validateImageFile :: (MonadCatch m, MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m) => Maybe ImageFile -> m (Maybe ImageFile)
validateImageFile (Just x) =
    do path <- fileCachePath' x
       testFile "image" path
       liftIO $ ensureLink (fileChksum (imageFile x)) (path ++ extension (imageFileType x))
       testFile "link" (path ++ extension (imageFileType x))
       return (Just x)
    where
      testFile :: (MonadCatch m, MonadError IOException m, MonadIO m) => String -> FilePath -> m ()
      testFile message path =
          do ok <- liftIO $ doesFileExist path
             -- We've checked that the file exists, we could check
             -- that the checksum is correct, but that seems like a
             -- lot of overhead.
             -- validateFile (imageFile x)
             when (not ok) ($logException $ fail (message ++ " missing: " ++ show path))
validateImageFile x = return x

{-
reportInput :: Paths a => a -> [(String, String)] -> Maybe Report -> Report -> ErrorWithIO Report
reportInput ver _pairs oldReport report = $logException "Appraisal.Report.IO.reportInput" $
    io (createDirectoryIfMissing True (reportDir ver report)) >> getFiles report
    where
      -- Traverse the old and new reports and look for changes,
      -- performing any IO required to generate images and the like.
      getFiles :: Report -> ErrorWithIO Report
      -- Polymorphic parameter magic around update here.
      getFiles report = gzip continue (\ a b -> update ver a b) oldReport (Just report) >>= return . fromJust

update :: forall paths a b. (Paths paths, Data a, Data b) => paths -> a -> b -> ErrorWithIO b
update ver a b = (gIO2 `extM2` updateImagePair ver) a b

-- |The default update operation is to simply return the new value
-- without performing any IO operations.
gIO2 :: GenericQ (GenericM ErrorWithIO)
gIO2 _a b = return b

-- | examine how the ReportImage has changed and call suitable IO
-- operations.  Normally, the IO operation that modified the
-- reportImage should do the necessary updates, but we can at least
-- see if any derived image fields are set to Nothing and update them.
updateImagePair :: Paths a => a -> ReportImage -> ReportImage -> ErrorWithIO ReportImage
updateImagePair ver stored edited =
    case picImage' edited of
      Just (Left uri) -> rebuildOriginal ver (picCrop edited) (picSize edited) (Left uri) edited
      _ | picCrop stored /= picCrop edited -> rebuildDerived ver (picCrop edited) (picSize edited) edited
        | picSize stored /= picSize edited -> rebuildSizes ver (picSize edited) edited
        | picMustEnlarge edited && isNothing (picEnlarged' edited) -> rebuildEnlarged ver edited
      _ -> return edited

continue :: GenericQ (GenericQ Bool)
continue a b = (gContinue `extQ2` stringContinue `extQ2` textContinue `extQ2` imageContinue) a b

-- Some continue functions for specific types

stringContinue :: String -> String -> Bool
stringContinue _ _ = False

textContinue :: T.Text -> T.Text -> Bool
textContinue _ _ = False

imageContinue :: ReportImage -> ReportImage -> Bool
imageContinue _ _ = False
-}

reportUprightImages ::  (MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                        Report -> m (ReportImage -> Maybe ImageFile)
reportUprightImages = reportImageFn uprightKey

reportPrinterImages :: (MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                       Report -> m (ReportImage -> Maybe ImageFile)
reportPrinterImages = reportImageFn printedKey

reportThumbImages :: (MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                     Report -> m (ReportImage -> Maybe ImageFile)
reportThumbImages = reportImageFn thumbKey

reportEditedImages :: (MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                      Report -> m (ReportImage -> Maybe ImageFile)
reportEditedImages = reportImageFn editedKey

reportImageFn :: (MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                 (ReportImage -> ImageKey) -> Report -> m (ReportImage -> Maybe ImageFile)
reportImageFn keyfn report = do
  let imgs = gFind report :: [ReportImage]
  mp <- mapM cacheInsert (map keyfn imgs) >>= return . fromList . zip imgs
  return (`Map.lookup` mp)

-- | Filter out the images that don't need to be enlarged
reportEnlargedImages :: (MonadCache ImageKey ImageFile m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) =>
                        Report -> m (ReportImage -> Maybe ImageFile)
reportEnlargedImages report = do
  let imgs = filter picMustEnlarge (gFind report :: [ReportImage])
  mp <- mapM cacheInsert (map enlargedKey imgs) >>= return . fromList . zip imgs
  return (`Map.lookup` mp)

reportOutput :: (Paths p, MonadCatch m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) => p -> ImageCacheState -> Report -> m Result
reportOutput ver st report =
    writeHaskell report >> writeLatex report >> validateImages' report >>= runLatex
    where
      writeHaskell :: (MonadError IOException m, MonadIO m) => Report -> m ()
      writeHaskell r =
          liftIO (logM "reportOutput" DEBUG "writeHaskell" >> replaceFile hsPath (T.pack (show r)))
          where hsPath = reportPath ver r ".hs"
      writeLatex :: (MonadCatch m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) => Report -> m ()
      writeLatex r =
          do picPrinter <- runImageCacheIO (reportPrinterImages report) (images ver) st
             picEnlarged <- runImageCacheIO (reportEnlargedImages report) (images ver) st
             let latex = render $ reportToLaTeX ver doMarkup picPrinter picEnlarged r
             liftIO (logM "reportOutput" DEBUG "writeLaTeX" >> replaceFile ltxPath latex)
          where
            ltxPath = reportPath ver r ".ltx"
            doMarkup markup = textell $ runAbbrevsM (abbrevsLaTeX report) (latexFromMarkup markup)
      validateImages' :: (MonadCatch m, MonadError IOException m, MonadFileCacheTop m, MonadIO m, Functor m) => Report -> m Report
      validateImages' report' =
          runImageCacheIO (validateImages report') (images ver) st

      runLatex :: (MonadError IOException m, MonadIO m) => Report -> m Result
      runLatex r = do
        liftIO $ logM "reportOutput" DEBUG "runLatex"
        liftIO $ removeFileMaybe auxPath
        runLatex' (dropTrailingPathSeparator dir) name r
          where auxPath = reportPath ver r ".aux"
                path = reportPath ver r ""
                (dir, name) = splitFileName path

removeFileMaybe :: FilePath -> IO ()
removeFileMaybe path = catchJust (guard . isDoesNotExistError) (removeFile path) return

replaceFile :: FilePath -> T.Text -> IO ()
replaceFile path text = removeFileMaybe path >> T.writeFile path text

-- | For non-critical files, don't have a cow if things go wrong
_tryToReplaceFile :: FilePath -> T.Text -> IO ()
_tryToReplaceFile path text = catchJust (guard . isAlreadyInUseError) (replaceFile path text) (\ () -> return ())

data Result = Ok | Again | Fatal deriving Eq

readFileBinary :: FilePath -> IO String
readFileBinary path = U.readFile path >>= return . U.unpack

-- Run latex at least twice, and repeatedly up to five times until all the log tests pass.
runLatex' :: (MonadError IOException m, MonadIO m) => FilePath -> String -> Report -> m Result
runLatex' dir name _report =
    run Again >> run Again >>= run >>= run >>= run >>= finish
    where
      finish Fatal = liftIO (readFileBinary logPath) >>= error -- Throw an error containing the whole log file (!)
      finish result = return result
      run Again = tryLatexOnce dir name >> liftIO (readFileBinary logPath) >>= return . runTests
      run x = return x
      runTests text =
          case map (\ f -> f text) [test1, test2] of
            xs | any (== Fatal) xs -> Fatal
               | any (== Again) xs -> Again
               | True -> Ok
      test1 text = if isInfixOf "LaTeX Warning: There were undefined references." text then Again else Ok
      test2 text = if isInfixOf "Fatal error occurred, no output PDF file produced!" text then Fatal else Ok
      logPath = dir <++> name ++ ".log"

tryLatexOnce :: (MonadError IOException m, MonadIO m) => FilePath -> String -> m (B.ByteString, B.ByteString)
tryLatexOnce dir name =
    do (code, out, err) <- runLatexOnce dir name
       case (code, isInfixOf "Fatal error occurred" (U.unpack out)) of
         (ExitFailure 1, True) ->
             -- Things to try when latex won't run - just one at the moment.  It
             -- may happen that the .aux file gets corrupted, so remove that.
             liftIO (tryJust (guard . isDoesNotExistError) (removeFile (dir </> name <.> "aux"))) >>=
             either (\ () -> runLatexOnce dir name >>= \ (_, out', err') -> return (out', err')) (\ () -> return (out, err))
         _ -> return (out, err)

runLatexOnce :: (MonadError IOException m, MonadIO m) => FilePath -> String -> m (ExitCode, B.ByteString, B.ByteString)
runLatexOnce dir name =
    liftIO (readCreateProcessWithExitCode ((proc cmd args) {cwd = Just dir}) B.empty) >>= \ result@(code, out, err) ->
    case code of
      ExitSuccess -> return result
      -- LaTeX returns error code 1 for most warning conditions.
      ExitFailure 1 -> return result
      _ -> error $ showCommandForUser cmd args ++ " -> " ++ show code ++ "\nstdout:\n " ++ indent " 1> " out ++ "\nstderr:\n " ++ indent " 2> " err
    where
      cmd = "xelatex"
      args = ["-interaction=nonstopmode", name <.> "ltx"]
      indent pre s = unlines $ map (pre ++) $ lines $ U.unpack s
