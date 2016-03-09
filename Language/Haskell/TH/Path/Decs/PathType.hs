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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.PathType
    ( pathType
    ) where

import Data.Foldable as Foldable
import Data.List as List (intercalate, map)
import Data.Map as Map (Map)
import Data.Maybe (isJust)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asType, asTypeQ, bestPathTypeName, ModelType(ModelType),
                                        makePathType)
import Language.Haskell.TH.Path.Core (SelfPath, SinkType,
                                      Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (reachableFromSimple, simplify, tgv, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

pathTypeControl :: (TypeGraphM m) => TypeQ -> TGVSimple -> Control m Type Type Type
pathTypeControl gtyp key =
    Control
    { _doSimple = pure (asType key)
    , _doSelf = runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    , _doView = \_ -> runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    , _doOrder = undefined
{-
        \ityp w ->
            do ipath <- tgvSimple ityp >>= pathType gtyp
               epath <- simplify w >>= pathType gtyp
               runQ [t|Path_OMap $ityp $(return epath)|]
-}
    , _doMap = undefined
    , _doPair = undefined
    , _doMaybe =
        \w -> do
          w' <- simplify w
          epath <- pathType gtyp w'
          runQ [t|Path_Maybe $(pure epath)|]
    , _doEither = undefined
    , _doField = undefined
    , _doConcs = undefined
    , _doSyn = undefined
    , _doAlts = undefined
    }

-- | Given a type, compute the corresponding path type.
pathType :: forall m. TypeGraphM m =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = pathType' (pathTypeControl gtyp key) gtyp key

pathType' :: forall m. TypeGraphM m =>
             Control m Type Type Type
          -> TypeQ
          -> TGVSimple -- ^ The type to convert to a path type
          -> m Type
pathType' control gtyp key = do
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType key]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType key]
  viewTypeMaybe <- viewInstanceType (asType key)
  case asType key of
    _ | selfPath -> return $ asType key
      | simplePath -> runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
      | isJust viewTypeMaybe ->
          do let Just viewType = viewTypeMaybe
             w <- tgvSimple viewType >>= tgv Nothing
             _doView control w
    ConT tname ->
        runQ $ [t|$(asTypeQ (makePathType (ModelType tname))) $gtyp|]
    AppT (AppT mtyp ityp) etyp
        | mtyp == ConT ''Order ->
            do ipath <- tgvSimple ityp >>= pathType gtyp
               epath <- tgvSimple etyp >>= pathType gtyp
               runQ [t|Path_OMap $(return ipath) $(return epath)|]
{-
            do w <- tgvSimple etyp >>= tgv Nothing
               _doOrder control w
-}
    AppT ListT etyp ->
        do epath <- tgvSimple etyp >>= pathType gtyp
           runQ [t|Path_List $(return epath)|]
    AppT (AppT t3 ktyp) vtyp
        | t3 == ConT ''Map ->
            do kpath <- tgvSimple ktyp >>= pathType gtyp
               vpath <- tgvSimple vtyp >>= pathType gtyp
               runQ [t| Path_Map $(return kpath) $(return vpath)|]
    AppT (AppT (TupleT 2) ftyp) styp ->
        do fpath <- tgvSimple ftyp >>= pathType gtyp
           spath <- tgvSimple styp >>= pathType gtyp
           runQ [t| Path_Pair $(return fpath) $(return spath) |]
    AppT t1 vtyp
        | t1 == ConT ''Maybe ->
            do w <- tgvSimple vtyp >>= tgv Nothing
               _doMaybe control w
    AppT (AppT t3 ltyp) rtyp
        | t3 == ConT ''Either ->
            do lpath <- tgvSimple ltyp >>= pathType gtyp
               rpath <- tgvSimple rtyp >>= pathType gtyp
               runQ [t| Path_Either $(return lpath) $(return rpath)|]
    _ -> do ks <- reachableFromSimple key
            error $ "pathType otherf: " ++ pprint1 key ++ "\n" ++
                    intercalate "\n  " ("reachable from:" : List.map pprint1 (Foldable.toList ks))
