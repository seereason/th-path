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
    , IdPath(idPath)

    -- * Basic Path Types
    , Path_Pair(..)
    , Path_Maybe(..)
    , Path_Map(..)
    , Path_OMap(..)
    , Path_List(..)
    , Path_Either(..)

    -- * Hint classes
    , SelfPath

    -- * instance queries
    , pathTypeNames

    -- * Naming conventions
    , pathTypeNameFromTypeName
    , pathTypeNames'
    , bestPathTypeName
    , pathConNameOfField
    ) where

import Control.Lens -- (makeLenses, over, view)
import Data.Generics (Data, Typeable)
import Data.List as List (map)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set (delete, difference, fromList, map, null, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Syntax (qReify)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.Vertex (bestType, TypeGraphVertex, TGV, field, typeNames)
import Prelude hiding (exp)
import Web.Routes.TH (derivePathInfo)

-- | If there is an instance of 'Path' for a pair of types @s@ and
-- @a@, that means there is at least one way to obtain an @a@ from an
-- @s@.
class Path s a where
    type PathType s a
    -- ^ Each instance defines this type function which returns the
    -- path type.  Each value of this type represents a different way
    -- of obtaining the @a@ from the @s@.  For example, if @s@ is a
    -- record with two fields of type 'Int', the type @PathType s Int@
    -- would have distinct values for those two fields, and the lenses
    -- returned by 'toLens' would access those two fields.
    toLens :: PathType s a -> Traversal' s a
    -- ^ Function to turn a PathType into a lens to access (one of)
    -- the @a@ values.

class IdPath s where
    idPath :: s -- ^ The identity path for type s.  @toLens idPath@
                -- returns @iso id id@.

-- instance OrderKey k => Path (Order k a) a where
--     type PathType (Order k a) a = (Path_OMap k a)
--     toLens (Path_At k a) = lens_omat k . toLens a

-- Primitive path types

-- | A path type with constructors to extract either @fst@, @snd@, or
-- the pair itself.
data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Either a b = Path_Left a | Path_Right b | Path_Either deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Maybe a = Path_Just a | Path_Maybe deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_Map k v = Path_Look k v | Path_Map  deriving (Eq, Ord, Read, Show, Typeable, Data)
data Path_List a = Path_List deriving (Eq, Ord, Read, Show, Typeable, Data) -- No element lookup path - too dangerous, use OMap
data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data)

instance IdPath (Path_Pair a b) where idPath = Path_Pair
instance IdPath (Path_Maybe a) where idPath = Path_Maybe
instance IdPath (Path_Either a b) where idPath = Path_Either
instance IdPath (Path_Map k v) where idPath = Path_Map
instance IdPath (Path_List a) where idPath = Path_List
instance IdPath (Path_OMap k a) where idPath = Path_OMap

primitivePathTypeNames :: Set Name
primitivePathTypeNames = Set.fromList [''Path_Pair, ''Path_List, ''Path_Either, ''Path_Map, ''Path_OMap, ''Path_Maybe]

$(derivePathInfo ''Path_Pair)
$(derivePathInfo ''Path_List)
$(derivePathInfo ''Path_Map)
$(derivePathInfo ''Path_Either)
$(derivePathInfo ''Path_Maybe)
$(derivePathInfo ''Path_OMap)

$(deriveSafeCopy 0 'base ''Path_Pair)
$(deriveSafeCopy 0 'base ''Path_List)
$(deriveSafeCopy 0 'base ''Path_Map)
$(deriveSafeCopy 0 'base ''Path_Either)
$(deriveSafeCopy 0 'base ''Path_Maybe)
$(deriveSafeCopy 0 'base ''Path_OMap)

-- | Types for which
-- a 'SelfPath' instance is declared will be used as their own Path
-- Type.  For example, a UUID or some enumerated type contained in a
-- record could be used directly to reference the object that contains
-- it.
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

pathTypeNames' :: TypeGraphVertex v => v -> Set Name
pathTypeNames' = Set.map pathTypeNameFromTypeName . typeNames

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: TypeGraphVertex v => v -> Maybe (Name, Set Name)
bestPathTypeName v =
    case (bestType v, typeNames v) of
      (ConT tname, names) -> Just (pathTypeNameFromTypeName tname, Set.map pathTypeNameFromTypeName (Set.delete tname names))
      (_t, s) | Set.null s -> Nothing
      (_t, _s) -> error "bestPathTypeName - unexpected name"

pathTypeNameFromTypeName :: Name -> Name
pathTypeNameFromTypeName tname = mkName $ "Path_" ++ nameBase tname

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TGV -> Maybe Name
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ mkName $ "Path_" ++ nameBase tname ++ "_" ++ nameBase fname') (key ^. field)
