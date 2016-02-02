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
module Language.Haskell.TH.Path.Decs.PathType where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Data.Foldable as Foldable
import Data.List as List (intercalate, map)
import Data.Map as Map (Map)
import Data.Maybe (isJust)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (asTypeQ, bestPathTypeName, ModelType(ModelType), pathTypeNameFromTypeName)
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), unE)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (reachableFromSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple)

-- | Given a type, compute the corresponding path type.
pathType :: (MonadReaders TypeGraph m, MonadReaders TypeInfo m, ContextM m) =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = do
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype key in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ]
  viewType <- viewInstanceType (view etype key)
  case view (etype . unE) key of
    _ | selfPath -> return $ view (etype . unE) key
      | simplePath -> let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(asTypeQ pname) $gtyp|]
      | isJust viewType ->
          let Just (pname, _syns) = bestPathTypeName key in
          runQ [t|$(asTypeQ pname) $gtyp|]
    ConT tname ->
        runQ $ [t|$(asTypeQ (pathTypeNameFromTypeName (ModelType tname))) $gtyp|]
    AppT (AppT mtyp ityp) etyp
        | mtyp == ConT ''Order ->
            do ipath <- vert ityp >>= pathType gtyp
               epath <- vert etyp >>= pathType gtyp
               runQ [t|Path_OMap $(return ipath) $(return epath)|]
    AppT ListT etyp ->
        do epath <- vert etyp >>= pathType gtyp
           runQ [t|Path_List $(return epath)|]
    AppT (AppT t3 ktyp) vtyp
        | t3 == ConT ''Map ->
            do kpath <- vert ktyp >>= pathType gtyp
               vpath <- vert vtyp >>= pathType gtyp
               runQ [t| Path_Map $(return kpath) $(return vpath)|]
    AppT (AppT (TupleT 2) ftyp) styp ->
        do fpath <- vert ftyp >>= pathType gtyp
           spath <- vert styp >>= pathType gtyp
           runQ [t| Path_Pair $(return fpath) $(return spath) |]
    AppT t1 vtyp
        | t1 == ConT ''Maybe ->
            do epath <- vert vtyp >>= pathType gtyp
               runQ [t|Path_Maybe $(return epath)|]
    AppT (AppT t3 ltyp) rtyp
        | t3 == ConT ''Either ->
            do lpath <- vert ltyp >>= pathType gtyp
               rpath <- vert rtyp >>= pathType gtyp
               runQ [t| Path_Either $(return lpath) $(return rpath)|]
    _ -> do ks <- reachableFromSimple key
            error $ "pathType otherf: " ++ pprint' key ++ "\n" ++
                    intercalate "\n  " ("reachable from:" : List.map pprint' (Foldable.toList ks))

    where
      vert typ = askPoly >>= \(ti :: TypeInfo) -> runReaderT (typeVertex (E typ)) ti
