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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.Common
    ( treeMap
    , forestMap
    , bestNames
    , bestPathTypeName
    , bestTypeName
    , clauses
    , fieldLensNameOld
    , fieldLensNamePair
    , makePathLens
    , pathConNameOfField
    , pathTypeNameFromTypeName
    , HasTypeQ(asTypeQ)
    , HasType(asType)
    , HasName(asName)
    , HasCon(asCon)
    , HasConQ(asConQ)
    , PathCon(..)
    , ModelType(ModelType)
    , PathType
    , makePathType
    , makePathValueType
    , makePathValueCon
    , makePathCon
    ) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Writer (execWriterT, tell)
import Data.Char (toLower)
import Data.List as List (map)
import Data.Set as Set (delete, minView)
import Data.Set.Extra as Set (map, Set)
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Expand (E, unE)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
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
      (ConT tname, names) -> Just (pathTypeNameFromTypeName (ModelType tname), Set.map (pathTypeNameFromTypeName . ModelType) (Set.delete tname names))
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

uncap :: String -> String
uncap (n : ame) = toLower n : ame
uncap "" = ""

class ToPat x where
    toPat :: x -> PatQ

instance ToPat (Strict, Type) where
    toPat _ = wildP

instance ToPat (Name, Strict, Type) where
    toPat _ = wildP

-- | Extract the template haskell Clauses
class Clauses x where
    clauses :: x -> [ClauseQ]

{-
pvTreeClauses key gkey _ptyp
    | view etype key == view etype gkey =
        tell [clause [wildP] (normalB [|undefined|]) []]
pvTreeClauses key gkey ptyp =
    tell [clause [wildP] (normalB [|undefined|]) []]

pvName :: TGVSimple -> TGVSimple -> Name
pvName t v =
    let Just tname = bestTypeName t
        Just vname = bestTypeName v in
    mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase vname)

-- | Change the s type of a PV value
pvLift :: Name -> Name -> Exp -> Exp
pvLift old new (AppE (AppE (ConE pv)
                           (AppE (ConE p) a)) x)
    | not (isPrefixOf pvPrefix (nameBase pv)) || pname /= nameBase p = error "pvLift"
    | otherwise =
        AppE (AppE (ConE (mkName ("PV_" ++ nameBase new ++ "_" ++ drop (length pvPrefix) (nameBase pv))))
                   (AppE (ConE (mkName ("Path_" ++ nameBase new))) a)) x
    where
      pvPrefix = "PV_" ++ nameBase old ++ "_"
      pname = ("Path_" ++ nameBase old)
-}

-- | Make lenses for a type with the names described by fieldLensNamePair, which is a little
-- different from the namer used in th-typegraph (for historical reasons I guess.)
makePathLens :: Quasi m => Name -> m [Dec]
makePathLens tname =
    -- runQ (runIO (putStrLn ("makePathLens " ++ nameBase tname))) >>
    qReify tname >>= execWriterT . doInfo
    where
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec (NewtypeD {}) = lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec (DataD {}) =    lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec _ = return ()

instance Clauses Dec where
    clauses (FunD _ xs) = List.map pure xs
    clauses _ = error "No clauses"

instance Clauses a => Clauses [a] where
    clauses = concatMap clauses

-- Conversions

newtype ModelType = ModelType {unModelType :: Name} deriving (Eq, Ord, Show) -- e.g. AbbrevPair
newtype PathType = PathType {unPathType :: Name} deriving (Eq, Ord, Show) -- e.g. Path_AbbrevPair
newtype PathCon = PathCon {unPathCon :: Name} deriving (Eq, Ord, Show) -- e.g. Path_UserIds_View
newtype PathValueType = PathValueType {unPathValueType :: Name} deriving (Eq, Ord, Show) -- e.g. PV_AbbrevPairs
newtype PathValueCon = PathValueCon {unPathValueCon :: Name} deriving (Eq, Ord, Show) -- e.g. PV_AbbrevPairs_Markup

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
instance HasCon PathType where asCon = ConE . unPathType -- There is always a self path constructor that has the same name as the type
instance HasConQ PathType where asConQ = conE . unPathType
instance HasName PathCon where asName = unPathCon
instance HasCon PathCon where asCon = ConE . unPathCon
instance HasConQ PathCon where asConQ = conE . unPathCon
instance HasName PathValueType where asName = unPathValueType
instance HasType PathValueType where asType = ConT . unPathValueType
instance HasTypeQ PathValueType where asTypeQ = conT . unPathValueType
instance HasName PathValueCon where asName = unPathValueCon
instance HasCon PathValueCon where asCon = ConE . unPathValueCon
instance HasConQ PathValueCon where asConQ = conE . unPathValueCon

instance HasType TGVSimple where asType = asType . view etype
instance HasType TGV where asType = asType . view vsimple
instance HasType (E Type) where asType = view unE
instance HasTypeQ TGVSimple where asTypeQ = pure . asType
instance HasTypeQ TGV where asTypeQ = pure . asType
instance HasTypeQ (E Type) where asTypeQ = pure . asType

makePathType :: ModelType -> PathType
makePathType (ModelType a) = PathType (mkName ("Path_" ++ nameBase a))

makePathValueType :: ModelType -> PathValueType
makePathValueType (ModelType s) = PathValueType (mkName ("PV_" ++ nameBase s))

makePathValueCon :: ModelType -> ModelType -> PathValueCon
makePathValueCon (ModelType s) (ModelType g) = PathValueCon (mkName ("PV_" ++ nameBase s ++ "_" ++ nameBase g))

makePathCon :: PathType -> String -> PathCon
makePathCon (PathType p) a = PathCon $ mkName $ nameBase p ++ "_" ++ a

pathTypeNameFromTypeName :: ModelType -> PathType
pathTypeNameFromTypeName = makePathType

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TGV -> Maybe PathCon
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ makePathCon (makePathType (ModelType tname)) (nameBase fname')) (key ^. field)
