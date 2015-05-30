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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Core
    ( -- * The Path type class
      Path(toLens)
    , PathType

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

    -- * instance queries
    , pathTypeNames

    -- * Naming conventions
    , pathTypeNameFromTypeName
    , pathTypeNames'
    , bestPathTypeName
    , pathConNameOfField
    , fieldLensName
    ) where

import Control.Lens -- (makeLenses, over, view)
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.List as List (map)
import Data.Monoid (Monoid(mempty, mappend))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set (difference, fromList, map, minView, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.PprLib (ptext)
import Language.Haskell.TH.Syntax (qReify)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.TypeGraph.Core (Field, pprint')
import Language.Haskell.TH.TypeGraph.Expand (E(E))
import Language.Haskell.TH.TypeGraph.Hints (HasVertexHints(hasVertexHints), VertexHint(..))
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype, syns, typeNames)
import Prelude hiding (exp)
import Web.Routes.TH (derivePathInfo)

-- | Instances of @Path s a@ include a 'PathType' which describes all
-- the different ways to obtain an value of type @a@ (called the goal
-- type in some places) from an @s@, and a 'toLens' function that
-- allows you to turn a PathType value into a lens.
class Path s a where
    type PathType s a
    toLens :: PathType s a -> Traversal' s a

-- instance OrderKey k => Path (Order k a) a where
--     type PathType (Order k a) a = (Path_OMap k a)
--     toLens (Path_At k a) = lens_omat k . toLens a

-- Primitive path types

data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)

-- data SomePath = SomePath deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Maybe a = Path_Just a | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Map k v = Path_Look k v | Path_Map  deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data) -- No element lookup path - too dangerous, use OMap

data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data) -- FIXME - Path_OMap constructor should be removed

primitivePathTypeNames :: Set Name
primitivePathTypeNames = Set.fromList [''Path_Pair, ''Path_List, ''Either, ''Path_Map, ''Path_OMap, ''Path_Maybe]

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
    | VertexHint VertexHint
    deriving (Eq, Ord)

-- This type is due for removal, so here is a semi-useful Monoid
-- instance to satisfy the simpleEdges function.
instance Monoid LensHint where
    mempty = VertexHint Normal
    mappend x@(Substitute _ _) _ = x
    mappend _ x@(Substitute _ _) = x
    mappend (VertexHint Normal) x = x
    mappend x _ = x

instance HasVertexHints LensHint where
    hasVertexHints (VertexHint x) = return [x]
    hasVertexHints (Substitute _exp typ) = return [Divert typ]

instance Default LensHint where
    def = VertexHint Normal

instance Show LensHint where
    show (Substitute exp typ) = "Substitute (" ++ pprint exp ++ ") (" ++ pprint typ ++ ")"
    show (VertexHint x) = "VertexHint " ++ show x

instance Ppr LensHint where
    ppr = ptext . show

-- | Instances of this class will be used as their own Path Type.  For
-- example, a UUID or some enumerated type contained in a record
-- could be used directly to reference the object that contains it.
class SelfPath a

-- | Find all the names of the (non-primitive) path types.
pathTypeNames :: DsMonad m => m (Set Name)
pathTypeNames = do
  (FamilyI (FamilyD TypeFam _pathtype [_,_] (Just StarT)) tySynInsts) <- qReify ''PathType
  return . flip Set.difference primitivePathTypeNames . Set.fromList . List.map (\(TySynInstD _ (TySynEqn _ typ)) -> doTySyn typ) $ tySynInsts
    where
      doTySyn (AppT x _) = doTySyn x
      doTySyn (ConT pathTypeName) = pathTypeName
      doTySyn x = error $ "Unexpected PathType: " ++ pprint' x ++ " (" ++ show x ++ ")"

-- Naming conventions

pathTypeNameFromTypeName :: Name -> Name
pathTypeNameFromTypeName tname = mkName $ "Path_" ++ nameBase tname

pathTypeNames' :: TypeGraphVertex -> Set Name
pathTypeNames' = Set.map pathTypeNameFromTypeName . typeNames

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
