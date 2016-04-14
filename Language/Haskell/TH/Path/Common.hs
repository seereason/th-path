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
    ( bestTypeName
    , clauses
    , HasTypeQ(asTypeQ)
    , HasType(asType)
    , HasName(asName)
    , HasCon(asCon)
    , HasConQ(asConQ)
    , PathCon(..)
    , ModelType(ModelType)
    , PathType
    , makeUPathType
    , makeHopCon
    , makeUFieldCon
    , uncurry3
    , tells
    , telld
    , mconcatQ
    , tagExp
    ) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Bool (bool)
import Data.List as List (map)
import Data.Set as Set (minView)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Syntax (lift, Quasi)
import Language.Haskell.TH.TypeGraph.Expand (E, unE)
import Language.Haskell.TH.TypeGraph.TypeGraph (HasTGV(asTGV))
import Language.Haskell.TH.TypeGraph.Vertex (bestType, bestTypeQ, etype, field, syns,
                                             TGV', TGVSimple', TGV, TGVSimple, vsimple)

-- Naming conventions

-- bestTypeName :: (TypeGraphVertex v, HasName v) => v -> ModelType Name
bestTypeName :: HasName v => v -> ModelType Name
bestTypeName = ModelType . asName

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
newtype ModelType a = ModelType {unModelType :: a} deriving (Eq, Ord, Show) -- e.g. AbbrevPair
newtype PathType a = PathType {unPathType :: a} deriving (Eq, Ord, Show) -- e.g. Path_AbbrevPair
newtype PathCon a = PathCon {unPathCon :: a} deriving (Eq, Ord, Show) -- e.g. Path_UserIds_View
newtype HopCon a = HopCon {unHopCon :: a} deriving (Eq, Ord, Show) -- e.g. Hop_AbbrevPairs_Markup

class HasTypeQ a where asTypeQ :: a -> TypeQ
class HasType a where asType :: a -> Type
class HasCon a where asCon :: a -> Exp
class HasConQ a where asConQ :: a -> ExpQ
class HasName a where asName :: a -> Name

instance HasName Name where asName = id
instance HasName TyVarBndr where
    asName (PlainTV x) = x
    asName (KindedTV x _) = x
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
    asName (v, s) =
        case asType s of
          ConT name -> name
          _ -> case minView (view syns s) of
                 Just (name, _) -> name
                 Nothing -> mkName ("S" ++ show v)
instance HasName TGV where
    asName (v, t) =
        let s = view vsimple t in
        case asType s of
          ConT name -> name
          _ -> case minView (view syns s) of
                 Just (name, _) -> name
                 Nothing -> mkName ("T" ++ show v)

instance HasType TGVSimple where asType = bestType . snd -- asType . snd
instance HasType TGV where asType = bestType . snd -- asType . snd
instance HasType TGVSimple' where asType = asType . view etype
instance HasType TGV' where asType = asType . view vsimple
instance HasType (E Type) where asType = view unE
instance HasType Type where asType = id
instance HasTypeQ TGVSimple where asTypeQ = bestTypeQ -- asTypeQ . snd
instance HasTypeQ TGV where asTypeQ = bestTypeQ -- asTypeQ . snd
instance HasTypeQ TGVSimple' where asTypeQ = pure . asType
instance HasTypeQ TGV' where asTypeQ = pure . asType
instance HasTypeQ (E Type) where asTypeQ = pure . asType
instance HasTypeQ Type where asTypeQ = pure

makeUPathType :: HasName a => ModelType a -> PathType Name
makeUPathType (ModelType a) = PathType (mkName ("UPath_" ++ nameBase (asName a)))

makeHopCon :: TGVSimple -> TGV -> HopCon Name
makeHopCon s a =
    HopCon (mkName ("Hop_" ++
                    (nameBase (asName s)) ++
                    "_" ++
                    (case view (_2 . field) a of
                      Just (_, _, fname) -> either show nameBase fname
                      Nothing -> nameBase (asName a))))

-- | Path type constructor for the field described by key in the parent type named tname.
makeUFieldCon :: TGV -> Maybe (PathCon Name)
makeUFieldCon key =
    case asTGV key ^. field of
      Nothing -> Nothing
      Just (tname, _, Right fname) -> Just $ makePathCon (makeUPathType (ModelType tname)) (nameBase fname)
      Just (tname, _, Left fpos) -> Just $ makePathCon (makeUPathType (ModelType tname)) (show fpos)

makePathCon :: HasName a => PathType a -> String -> PathCon Name
makePathCon (PathType p) a = PathCon $ mkName $ nameBase (asName p) ++ "_" ++ a

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

telld :: (Quasi m, MonadWriter [a] m) => Q [a] -> m ()
telld ds = runQ ds >>= tell

tells :: (Quasi m, MonadWriter [a] m) => [Q a] -> m ()
tells ds = telld (sequence ds)

mconcatQ :: [ExpQ] -> ExpQ
mconcatQ [] = [| mempty |]
mconcatQ [x] = x
mconcatQ xs = [|mconcat $(listE xs)|]

-- | Insert a string into an expression by applying an id function
tagExp :: String -> ExpQ -> ExpQ
tagExp s e = [|bool (undefined $(lift s)) $e True|]
