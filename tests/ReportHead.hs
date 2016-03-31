-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-incomplete-patterns #-}
module ReportDecs where

import Appraisal.File (File)
import Appraisal.Image
import Appraisal.ImageFile
import Appraisal.IntJS
import Appraisal.Markup (Markup(..))
import Appraisal.Permissions
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.ReportInstances
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID(..), ReportMap(..), MRR)
import Appraisal.Utils.CIString (CIString(..))
import Control.Lens (iso, _Just, _1, _2, _Left, _Right, Lens', toListOf, Traversal')
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (toList)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node), Forest)
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Language.Haskell.TH.Path.Common (view')
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(Path_At), toPairs)
import Language.Haskell.TH.Path.View (View(viewLens))
import Network.URI (URI(URI), URIAuth)

