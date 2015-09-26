-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- The generated toLens instances will have incomplete patterns where
-- we tried to generate a clause but we found no path to the goal type.
module Appraisal.ReportPaths where

import Control.Monad.States (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.List (sort)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#endif
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID)
import Appraisal.ReportPathInfo
import Data.UUID.Orphans ()
import Language.Haskell.TH (Dec, pprint, runQ, runIO)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.Instances (pathInstances)
import Language.Haskell.TH.Path.Lens (pathLenses)
import Language.Haskell.TH.Path.Types (pathTypes)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import System.FilePath.Extra (compareSaveAndReturn, changeError)
import Web.Routes.TH (derivePathInfo)

import Data.List (sortBy)
import Data.Function (on)

types :: [Dec]
types = $(do depFiles >> startTypes >>= (runTypeGraphT . execWriterT) pathTypes >>= lift)

lenses :: [Dec]
lenses = $(do depFiles >> startTypes >>= (runTypeGraphT . execWriterT) pathLenses >>= lift)

instances :: [Dec]
instances = $(do depFiles >> startTypes >>= (runTypeGraphT . execWriterT) pathInstances >>= lift)

$(derivePathInfo ''Maybe)
$(derivePathInfo ''ItemFieldName)
$(derivePathInfo ''ReportID)
