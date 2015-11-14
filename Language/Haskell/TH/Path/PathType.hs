{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.PathType
    ( pathType
    , pathType'
    , pathTypeCall

    -- * Naming conventions
    , pathTypeNameFromTypeName
    , bestPathTypeName
    , pathConNameOfField
    ) where

import Data.List as List (map)
import Data.Set as Set (delete, map, null, Set)
import Prelude hiding (exp)

import Control.Lens hiding (cons)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Control.Monad.States (MonadStates)
import Data.Foldable
import Data.List as List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Context (InstMap)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (PathType, Path_OMap, Path_List, Path_Map, Path_Pair, Path_Maybe, Path_Either)
import Language.Haskell.TH.Path.Graph (FoldPathControl(..), foldPath)
import Language.Haskell.TH.TypeGraph.Expand (E(E, unE), ExpandMap)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (TypeGraph, reachableFromSimple)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestType, TypeGraphVertex, TGV, field, typeNames, TGVSimple, vsimple, etype)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)

-- | Given a type, generate the corresponding path type.
pathType :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates ExpandMap m, MonadStates InstMap m) =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key =
  foldPath control key
    where
          -- Nest the definition of control so it can see this binding of hints,
          -- and call pathType' with a modified hint list.
      control =
              FoldPathControl
                { simplef = let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
                , substf = \_lns _styp ->
                    -- This is safe because hint types are now required to have a name
                    let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
                , pathyf = return $ unE $ view etype key
                , namedf = \tname -> runQ $ [t|$(conT (pathTypeNameFromTypeName tname)) $gtyp|]
                , maybef = \etyp -> do
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_Maybe $(return epath)|]
                , listf = \etyp -> do
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_List $(return epath)|]
                , orderf = \ityp etyp -> do
                    ipath <- vert ityp >>= pathType gtyp
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_OMap $(return ipath) $(return epath)|]
                , mapf = \ktyp vtyp -> do
                    kpath <- vert ktyp >>= pathType gtyp
                    vpath <- vert vtyp >>= pathType gtyp
                    runQ [t| Path_Map $(return kpath) $(return vpath)|]
                , pairf = \ftyp styp -> do
                    fpath <- vert ftyp >>= pathType gtyp
                    spath <- vert styp >>= pathType gtyp
                    runQ [t| Path_Pair $(return fpath) $(return spath) |]
                , eitherf = \ltyp rtyp -> do
                    lpath <- vert ltyp >>= pathType gtyp
                    rpath <- vert rtyp >>= pathType gtyp
                    runQ [t| Path_Either $(return lpath) $(return rpath)|]
                , otherf = do
                    ks <- reachableFromSimple key
                    error $ "pathType otherf: " ++ pprint' key ++ "\n" ++
                            intercalate "\n  " ("reachable from:" : List.map pprint' (toList ks))
                }

      vert typ = askPoly >>= \(ti :: TypeInfo) -> runReaderT (typeVertex (E typ)) ti

-- | pathType for the simplified vertex
pathType' :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates ExpandMap m, MonadStates InstMap m) => TypeQ -> TGV -> m Type
pathType' gtyp key = pathType gtyp (view vsimple key)

-- | Call the type function PathType.
pathTypeCall :: (DsMonad m, MonadReaders TypeGraph m) =>
                TypeQ           -- ^ The goal type - possibly a type variable
             -> TGV -- ^ The type to convert to a path type
             -> m Type
pathTypeCall gtyp key = runQ [t|PathType $(let (E typ) = view (vsimple . etype) key in return typ) $gtyp|]

-- Naming conventions

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TGV -> Maybe Name
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ mkName $ "Path_" ++ nameBase tname ++ "_" ++ nameBase fname') (key ^. field)

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
