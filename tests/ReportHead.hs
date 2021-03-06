-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Appraisal.Maybe
import Appraisal.Permissions
import Appraisal.Report
import Appraisal.ReportImage
import Appraisal.ReportInstances
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID(..), ReportMap(..), MRR)
import Appraisal.Utils.CIString (CIString(..))
import Control.Lens (Iso', iso, lens, Lens', Traversal')
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics (Data, Typeable)
import Data.Int (Int64)
import Data.Map (toList)
import Data.Maybe (fromMaybe)
import Data.Order (Path_OMap(Path_At), toPairs)
import Data.Proxy
import Data.Text (Text)
import Data.Tree (Tree(Node))
import Data.UserId (UserId(UserId))
import Data.UUID (UUID)
import Data.UUID.Orphans (showUUID)
import GHC.Generics (Generic)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.View (View(viewLens))
import Network.URI (URI(URI), URIAuth)

