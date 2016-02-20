-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Common
    ( bestPathTypeName
    , allPathTypeNames
    , bestTypeName
    , clauses
    , fieldLensNamePair
    , HasTypeQ(asTypeQ)
    , HasType(asType)
    , HasName(asName)
    , HasCon(asCon)
    , HasConQ(asConQ)
    , PathCon(..)
    , ModelType(ModelType)
    , PathType
    , makePathType
    , makeHopCon
    , makePathCon
    , makeFieldCon
    ) where

import Control.Lens hiding (cons, Strict)
import Data.List as List (map)
import Data.Set as Set (map, minView, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.TypeGraph.Expand (E, unE)
import Language.Haskell.TH.TypeGraph.Vertex (etype, field, MayHaveField(fieldOf), syns, TGV, TGVSimple, TypeGraphVertex, typeNames, vsimple)

-- Naming conventions

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: (TypeGraphVertex v, HasName v) => v -> PathType Name
bestPathTypeName = makePathType . bestTypeName

allPathTypeNames :: TypeGraphVertex v => v -> Set (PathType Name)
allPathTypeNames v = Set.map (makePathType . ModelType) (typeNames v)

bestTypeName :: (TypeGraphVertex v, HasName v) => v -> ModelType Name
bestTypeName = ModelType . asName
{-
    case bestType v of
      ConT tname -> Just (ModelType tname)
      _ -> maybe Nothing (Just . fst) (minView (Set.map ModelType $ typeNames v))
-}

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)

-- | Version of fieldLensName suitable for use as argument to
-- findNames below.
fieldLensNamePair :: Name -> Name -> Name -> (String, String)
fieldLensNamePair tname _cname fname = (nameBase fname, nameBase (fieldLensNameOld tname fname))

class ToPat x where
    toPat :: x -> PatQ

instance ToPat (Strict, Type) where
    toPat _ = wildP

instance ToPat (Name, Strict, Type) where
    toPat _ = wildP

-- | Extract the template haskell Clauses
class Clauses x where
    clauses :: x -> [ClauseQ]

instance Clauses Dec where
    clauses (FunD _ xs) = List.map pure xs
    clauses _ = error "No clauses"

instance Clauses a => Clauses [a] where
    clauses = concatMap clauses

-- Conversions

-- | Model as in Model-View-Controller.
newtype HasName a => ModelType a = ModelType {unModelType :: a} deriving (Eq, Ord, Show) -- e.g. AbbrevPair
newtype HasName a => PathType a = PathType {unPathType :: a} deriving (Eq, Ord, Show) -- e.g. Path_AbbrevPair
newtype HasName a => PathCon a = PathCon {unPathCon :: a} deriving (Eq, Ord, Show) -- e.g. Path_UserIds_View
newtype HasName a => HopCon a = HopCon {unHopCon :: a} deriving (Eq, Ord, Show) -- e.g. Hop_AbbrevPairs_Markup

class HasTypeQ a where asTypeQ :: a -> TypeQ
class HasType a where asType :: a -> Type
class HasCon a where asCon :: a -> Exp
class HasConQ a where asConQ :: a -> ExpQ
class HasName a where asName :: a -> Name

instance HasName Name where asName = id
instance HasName a => HasName (ModelType a) where asName = asName . unModelType
instance HasName a => HasType (ModelType a) where asType = ConT . asName . unModelType
instance HasName a => HasTypeQ (ModelType a) where asTypeQ = conT . asName . unModelType
instance HasName a => HasName (PathType a) where asName = asName . unPathType
instance HasName a => HasType (PathType a) where asType = ConT . asName . unPathType
instance HasName a => HasTypeQ (PathType a) where asTypeQ = conT . asName . unPathType
instance HasName a => HasCon (PathType a) where asCon = ConE . asName . unPathType
                               -- ^ There is always a self path constructor that has
                               -- the same name as the type.  But there are others
                               -- too, so this is not really safe.
instance HasName a => HasConQ (PathType a) where asConQ = conE . asName . unPathType
instance HasName a => HasName (PathCon a) where asName = asName . unPathCon
instance HasName a => HasCon (PathCon a) where asCon = ConE . asName . unPathCon
instance HasName a => HasConQ (PathCon a) where asConQ = conE . asName . unPathCon
instance HasName a => HasName (HopCon a) where asName = asName . unHopCon
instance HasName a => HasCon (HopCon a) where asCon = ConE . asName . unHopCon
instance HasName a => HasConQ (HopCon a) where asConQ = conE . asName . unHopCon

-- Temporary
instance HasName TGVSimple where
    asName x = case view (etype . unE) x of
                 ConT name -> name
                 _ -> case minView (view syns x) of
                        Just (name, _) -> name
                        Nothing -> error $ "HasName " ++ pprint (view (etype . unE) x)
-- Temporary
instance HasName TGV where
    asName = asName . view vsimple

instance HasType TGVSimple where asType = asType . view etype
instance HasType TGV where asType = asType . view vsimple
instance HasType (E Type) where asType = view unE
instance HasType Type where asType = id
instance HasTypeQ TGVSimple where asTypeQ = pure . asType
instance HasTypeQ TGV where asTypeQ = pure . asType
instance HasTypeQ (E Type) where asTypeQ = pure . asType
instance HasTypeQ Type where asTypeQ = pure

makePathType :: HasName a => ModelType a -> PathType Name
makePathType (ModelType a) = PathType (mkName ("Path_" ++ nameBase (asName a)))

makeHopCon :: (TypeGraphVertex s, HasName s, TypeGraphVertex a, MayHaveField a, HasName a) => s -> a -> HopCon Name
makeHopCon s g =
    HopCon (mkName ("Hop_" ++
                    (let ModelType n = bestTypeName s in nameBase n) ++
                    "_" ++
                    (case fieldOf g of
                      Just (_, _, fname) -> either show nameBase fname
                      Nothing -> let ModelType n = bestTypeName g in nameBase n)))
{-
makeHopCon :: ModelType -> TGV -> HopCon
makeHopCon (ModelType s) g = HopCon (mkName ("Hop_" ++ nameBase s ++ "_" ++ case g ^. field of
                                                                              Just (_, _, fname) -> either show nameBase fname
                                                                              Nothing -> case bestTypeName g of
                                                                                           Just (ModelType tname) -> nameBase tname
                                                                                           Nothing -> error "makeHopCon"))
-}
makePathCon :: HasName a => PathType a -> String -> PathCon Name
makePathCon (PathType p) a = PathCon $ mkName $ nameBase (asName p) ++ "_" ++ a

-- pathTypeNameFromTypeName :: ModelType -> PathType
-- pathTypeNameFromTypeName = makePathType

-- | Path type constructor for the field described by key in the parent type named tname.
makeFieldCon :: TGV -> Maybe (PathCon Name)
makeFieldCon key =
    case key ^. field of
      Nothing -> Nothing
      Just (tname, _, Right fname) -> Just $ makePathCon (makePathType (ModelType tname)) (nameBase fname)
      Just (tname, _, Left fpos) -> Just $ makePathCon (makePathType (ModelType tname)) (show fpos)
