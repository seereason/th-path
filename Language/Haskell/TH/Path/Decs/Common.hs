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
module Language.Haskell.TH.Path.Decs.Common
    ( treeMap
    , forestMap
    , bestNames
    , bestPathTypeName
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
    , makePeekType
    , makePeekCon
    , makePathCon
    , makeFieldCon
    ) where

import Control.Lens hiding (cons, Strict)
import Data.List as List (map)
import Data.Set as Set (delete, minView)
import Data.Set.Extra as Set (map, Set)
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.TypeGraph.Expand (E, unE)
import Language.Haskell.TH.TypeGraph.Vertex (etype, field, TGV, TGVSimple, TypeGraphVertex(bestType), typeNames, vsimple)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Node x ns) = Node (f x) (forestMap f ns)

forestMap :: (a -> b) -> Forest a -> Forest b
forestMap f = List.map (treeMap f)

-- Naming conventions

bestNames :: TypeGraphVertex v => v -> Maybe (ModelType, PathType, Set PathType)
bestNames v =
    case (bestTypeName v, bestPathTypeName v) of
      (Just tname, Just (pname, pnames)) -> Just (tname, pname, pnames)
      _ -> Nothing

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: TypeGraphVertex v => v -> Maybe (PathType, Set PathType)
bestPathTypeName v =
    case (bestType v, typeNames v) of
      (ConT tname, names) -> Just (makePathType (ModelType tname), Set.map (makePathType . ModelType) (Set.delete tname names))
      (_t, s) | null s -> Nothing
      (_t, _s) -> error "bestPathTypeName - unexpected name"

bestTypeName :: TypeGraphVertex v => v -> Maybe ModelType
bestTypeName v =
    case bestType v of
      ConT tname -> Just (ModelType tname)
      _ -> maybe Nothing (Just . fst) (minView (Set.map ModelType $ typeNames v))

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
newtype ModelType = ModelType {unModelType :: Name} deriving (Eq, Ord, Show) -- e.g. AbbrevPair
newtype PathType = PathType {unPathType :: Name} deriving (Eq, Ord, Show) -- e.g. Path_AbbrevPair
newtype PathCon = PathCon {unPathCon :: Name} deriving (Eq, Ord, Show) -- e.g. Path_UserIds_View
newtype PeekType = PeekType {unPeekType :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs
newtype PeekCon = PeekCon {unPeekCon :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs_Markup

class HasTypeQ a where asTypeQ :: a -> TypeQ
class HasType a where asType :: a -> Type
class HasCon a where asCon :: a -> Exp
class HasConQ a where asConQ :: a -> ExpQ
class HasName a where asName :: a -> Name

instance HasName ModelType where asName = unModelType
instance HasType ModelType where asType = ConT . unModelType
instance HasTypeQ ModelType where asTypeQ = conT . unModelType
instance HasName PathType where asName = unPathType
instance HasType PathType where asType = ConT . unPathType
instance HasTypeQ PathType where asTypeQ = conT . unPathType
instance HasCon PathType where asCon = ConE . unPathType
                               -- ^ There is always a self path constructor that has
                               -- the same name as the type.  But there are others
                               -- too, so this is not really safe.
instance HasConQ PathType where asConQ = conE . unPathType
instance HasName PathCon where asName = unPathCon
instance HasCon PathCon where asCon = ConE . unPathCon
instance HasConQ PathCon where asConQ = conE . unPathCon
instance HasName PeekType where asName = unPeekType
instance HasType PeekType where asType = ConT . unPeekType
instance HasTypeQ PeekType where asTypeQ = conT . unPeekType
instance HasName PeekCon where asName = unPeekCon
instance HasCon PeekCon where asCon = ConE . unPeekCon
instance HasConQ PeekCon where asConQ = conE . unPeekCon

instance HasType TGVSimple where asType = asType . view etype
instance HasType TGV where asType = asType . view vsimple
instance HasType (E Type) where asType = view unE
instance HasType Type where asType = id
instance HasTypeQ TGVSimple where asTypeQ = pure . asType
instance HasTypeQ TGV where asTypeQ = pure . asType
instance HasTypeQ (E Type) where asTypeQ = pure . asType
instance HasTypeQ Type where asTypeQ = pure

makePathType :: ModelType -> PathType
makePathType (ModelType a) = PathType (mkName ("Path_" ++ nameBase a))

makePeekType :: ModelType -> PeekType
makePeekType (ModelType s) = PeekType (mkName ("Peek_" ++ nameBase s))

makePeekCon :: ModelType -> ModelType -> PeekCon
makePeekCon (ModelType s) (ModelType g) = PeekCon (mkName ("Peek_" ++ nameBase s ++ "_" ++ nameBase g))

makePathCon :: PathType -> String -> PathCon
makePathCon (PathType p) a = PathCon $ mkName $ nameBase p ++ "_" ++ a

-- pathTypeNameFromTypeName :: ModelType -> PathType
-- pathTypeNameFromTypeName = makePathType

-- | Path type constructor for the field described by key in the parent type named tname.
makeFieldCon :: TGV -> Maybe PathCon
makeFieldCon key = maybe Nothing (\ (tname, _, Right fname') -> Just $ makePathCon (makePathType (ModelType tname)) (nameBase fname')) (key ^. field)
