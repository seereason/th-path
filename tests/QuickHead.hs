{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-incomplete-patterns #-}

import Appraisal.File
import Appraisal.Image
import Appraisal.ImageFile
import Appraisal.IntJS
import Appraisal.LaTeX
import Appraisal.Markup
import Appraisal.Permissions
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.ReportInstances
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID(..), ReportMap(..), MRR)
import Appraisal.Utils.CIString (CIString(..))
import Control.Lens (iso, _Just, _1, _2, _Left, _Right, Lens', toListOf, Traversal', view)
import Control.Monad.Readers (askPoly)
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (Map, toList)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node), Forest)
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Language.Haskell.TH (pprint, runQ, runIO)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Decs (allDecsToFile)
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(Path_At), toPairs)
import Language.Haskell.TH.Path.View (View(viewLens))
import Language.Haskell.TH.TypeGraph.TypeGraph (TypeGraph)
import Network.URI (URI(URI), URIAuth)
import System.Exit
import Test.HUnit hiding (Path)
import Text.LaTeX (LaTeX)

