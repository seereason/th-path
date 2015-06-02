-- | A library to facilitate the creation of lenses into a data
-- structure.
module Language.Haskell.TH.Path
    ( -- * Path Classes
      Path(toLens)
    , PathType
    , idPath
    -- * Primitive Path Types
    , Path_Pair(..)
    , Path_Either(..)
    , Path_Maybe(..)
    , Path_Map(..)
    , Path_OMap(..)
    , Path_List(..)
    -- * Hint classes
    , SinkType
    , SelfPath
    , View(viewLens, ViewType)
    , viewInstanceType
    -- * Template Haskell
    , pathInstances
    , pathTypes
    ) where

import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Instances
import Language.Haskell.TH.Path.Prune (SinkType)
import Language.Haskell.TH.Path.Types
import Language.Haskell.TH.Path.View
