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

import Control.Monad.Writer (execWriterT)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#endif
import Appraisal.ReportItem
import Appraisal.ReportMap (ReportID)
import Appraisal.ReportPathInfo
import Data.Function (on)
import Data.List (sortBy)
import Data.UUID.Orphans ()
import Language.Haskell.TH (Dec)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.Path.Instances (pathInstances)
import Language.Haskell.TH.Path.Lens (pathLenses)
import Language.Haskell.TH.Path.Types (pathTypeDecs)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import Web.Routes.TH (derivePathInfo)

$(derivePathInfo ''Maybe)
$(derivePathInfo ''ItemFieldName)
$(derivePathInfo ''ReportID)

decs :: [Dec]
decs = $(depFiles >>
         startTypes >>=
         runTypeGraphT (do types <- execWriterT pathTypeDecs >>= return . sortBy (compare `on` show) . map friendlyNames
                           lenses <- execWriterT pathLenses >>= return . sortBy (compare `on` show) . map friendlyNames
                           instances <- execWriterT pathInstances >>= return . sortBy (compare `on` show) . map friendlyNames
                           return (types ++ lenses ++ instances)) >>=
         lift)
