{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Core
    ( pathTypeNameFromTypeName
    , pathTypeNames
    , bestPathTypeName
    , pathConNameOfField
    , fieldLensName

    -- * Basic Path Types
    , Path_Pair(..)
    , Path_Maybe(..)
    , Path_Map(..)
    , Path_OMap(..)
    , Path_List(..)
    -- * Hints
    , LensHint(..)
    , Field

    -- * Hint classes
    , SelfPath
    ) where

import Control.Lens -- (makeLenses, over, view)
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set (map, minView, Set)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib (ptext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.TypeGraph.Core (Field)
import Language.Haskell.TH.TypeGraph.Expand (E(E))
import Language.Haskell.TH.TypeGraph.Hints (HasVertexHints(hasVertexHints), VertexHint(..))
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype, syns, typeNames)
import Prelude hiding (exp)
import Web.Routes.TH (derivePathInfo)

-- Primitive path types

data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)

-- data SomePath = SomePath deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Maybe a = Path_Maybe a deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Map k v = Path_Map k v  deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_List a = Path_Elems deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data) -- FIXME - Path_OMap constructor should be removed

$(derivePathInfo ''Path_Pair)
$(derivePathInfo ''Path_List)
$(derivePathInfo ''Path_Map)
$(derivePathInfo ''Either)
$(derivePathInfo ''Path_Maybe)
$(derivePathInfo ''Path_OMap)

$(deriveSafeCopy 0 'base ''Path_Pair)
$(deriveSafeCopy 0 'base ''Path_List)
$(deriveSafeCopy 0 'base ''Path_Map)
$(deriveSafeCopy 0 'base ''Path_Maybe)
$(deriveSafeCopy 0 'base ''Path_OMap)

data LensHint
    = Substitute Exp Type
    -- ^ Replace the automatically generated pathToLens
    -- function with this.  The expression is the lens to use,
    -- and the second element of the pair is the 'b' Type of
    -- the lens.
    | Normal'
    -- ^ no effect
    | VertexHint VertexHint
    deriving (Eq, Ord)

instance HasVertexHints LensHint where
    hasVertexHints (VertexHint x) = return [x]
    hasVertexHints (Substitute _exp typ) = return [Divert typ]
    hasVertexHints _ = return []

instance Default LensHint where
    def = Normal'

instance Show LensHint where
    show (Substitute exp typ) = "Substitute (" ++ pprint exp ++ ") (" ++ pprint typ ++ ")"
    show Normal' = "Normal'"
    show (VertexHint x) = "VertexHint " ++ show x

instance Ppr LensHint where
    ppr = ptext . show

-- | Instances of this class will be used as their own Path Type.  For
-- example, a UUID or some enumerated type contained in a record
-- could be used directly to reference the object that contains it.
class SelfPath a

-- Naming conventions

pathTypeNameFromTypeName :: Name -> Name
pathTypeNameFromTypeName tname = mkName $ "Path_" ++ nameBase tname

pathTypeNames :: TypeGraphVertex -> Set Name
pathTypeNames = Set.map pathTypeNameFromTypeName . typeNames

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: TypeGraphVertex -> Maybe (Name, Set Name)
bestPathTypeName v =
    case view etype v of
      E (ConT tname) -> Just (pathTypeNameFromTypeName tname, Set.map pathTypeNameFromTypeName (view syns v))
      _ -> Set.minView (Set.map pathTypeNameFromTypeName (view syns v))

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TypeGraphVertex -> Maybe Name
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ mkName $ "Path_" ++ nameBase tname ++ "_" ++ nameBase fname') (_field key)

fieldLensName :: Name -> Name -> Name
fieldLensName tname fname' = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname')
