{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-orphans #-}
module Appraisal.Orphans where

import Data.Map (Map)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.TypeGraph.Prelude ()
