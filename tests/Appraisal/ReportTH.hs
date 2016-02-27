-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -dshow-passes #-}
module Appraisal.ReportTH where

--import Appraisal.File (URI)
--import Appraisal.Image
--import Appraisal.ImageFile
--import Appraisal.IntJS
--import Appraisal.Markup (Markup(..))
--import Appraisal.Permissions
--import Appraisal.Report
--import Appraisal.ReportImage (MEUI)
import Appraisal.ReportInstances (startTypes)
--import Appraisal.ReportItem
--import Appraisal.ReportMap (ReportMap(ReportMap), ReportID)
--import Appraisal.Utils.CIString (CIString(..))
--import Control.Lens (Lens', Traversal', iso, _Just, _1, _2, _Left, _Right)
import Control.Monad (filterM)
--import Control.Monad.Writer (execWriterT)
import Data.ByteString.UTF8 as UTF8 (toString)
import Data.FileEmbed (embedFile)
--import Data.Function (on)
--import Data.Generics (Data, Typeable)
--import Data.Int (Int64)
import Data.List (isSuffixOf)
--import Data.Map (Map)
import Data.Monoid ((<>))
--import Data.Text (Text)
--import Data.UserId (UserId)
--import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Language.Haskell.TH (Dec, runIO)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
--import Language.Haskell.TH.PprLib (Doc, ptext, vcat)
--import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.Decs (allDecs)
--import Language.Haskell.TH.Path.Order (lens_omat)
--import Language.Haskell.TH.Path.View (View(viewLens))
import Language.Haskell.TH.Syntax (addDependentFile)
--import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
--import System.FilePath.Extra (compareSaveAndReturn, changeError)

--import Data.List as List (map)
--import Control.Exception as E (catch, IOException, throw, try)
--import Data.ListLike as LL (hPutStr, ListLikeIO, readFile, writeFile)
--import GHC.IO.Exception (ioe_description)
import Data.List (sort)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames, pprintW)
import Prelude hiding (readFile)
--import System.Directory (removeFile)
--import qualified System.IO as IO
--import System.IO.Error (isDoesNotExistError)
--import System.Posix.Files (getFdStatus, fileMode, setFdMode, unionFileModes, ownerReadMode, groupReadMode, otherReadMode)
import System.Posix.Files (getSymbolicLinkStatus, isRegularFile)
--import System.Posix.IO (handleToFd, closeFd)

decs :: [Dec]
decs = $(do let regular path = runIO $ isRegularFile <$> getSymbolicLinkStatus path
            addDependentFile "tests/ReportHead.hs"
            addDependentFile "tests/ReportDecs.hs"
            let dir s = runIO (getDirectoryContents s) >>=
                        filterM regular . map (s </>) . filter (isSuffixOf ".hs") >>=
                        mapM_ (addDependentFile)
            mapM_ dir ["Language/Haskell/TH/Path", "Language/Haskell/TH/Path/Decs"]
            decs' <- startTypes >>= runTypeGraphT allDecs -- >>= lift
            let code = (unlines . map (pprintW . friendlyNames) . sort) decs'
            let hdr = UTF8.toString $(embedFile "tests/ReportHead.hs")
                old = UTF8.toString $(embedFile "tests/ReportDecs.hs")
                new = hdr <> code
            case (new == old) of
              True -> lift decs'
              False ->
                  do runIO (writeFile "tests/ReportDecs.hs.new" new)
                     error "Generated tests/ReportDecs.hs does not match existing")
