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
import Language.Haskell.TH.Path.Common (asName, asType, asTypeQ, bestPathTypeName, ModelType(ModelType),
                                        makePathType)
import Language.Haskell.TH.Path.Core (SelfPath, SinkType,
                                      Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (reachableFromSimple, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

pathTypeControl :: (TypeGraphM m) => TypeQ -> TGVSimple -> Control m () () Type
pathTypeControl gtyp key =
    Control
    { _doSelf = pure $ asType key
    , _doSimple = runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    , _doView = \_ -> runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    , _doOrder =
        \ityp etyp ->
            do ipath <- tgvSimple ityp >>= pathType gtyp
               epath <- tgvSimple etyp >>= pathType gtyp
               runQ [t|Path_OMap $(return ipath) $(return epath)|]
{-
        \ityp w ->
            do ipath <- tgvSimple ityp >>= pathType gtyp
               epath <- simplify w >>= pathType gtyp
               runQ [t|Path_OMap $ityp $(return epath)|]
-}
    , _doMap =
        \ktyp vtyp ->
            do kpath <- tgvSimple ktyp >>= pathType gtyp
               vpath <- tgvSimple vtyp >>= pathType gtyp
               runQ [t| Path_Map $(return kpath) $(return vpath)|]
    , _doList =
        \etyp ->
            do epath <- tgvSimple etyp >>= pathType gtyp
               runQ [t|Path_List $(return epath)|]
    , _doPair =
        \ftyp styp ->
            do fpath <- tgvSimple ftyp >>= pathType gtyp
               spath <- tgvSimple styp >>= pathType gtyp
               runQ [t| Path_Pair $(return fpath) $(return spath) |]
    , _doMaybe =
        \typ ->
            do w' <- tgvSimple typ
               epath <- pathType gtyp w'
               runQ [t|Path_Maybe $(pure epath)|]
    , _doEither =
        \ltyp rtyp ->
            do lpath <- tgvSimple ltyp >>= pathType gtyp
               rpath <- tgvSimple rtyp >>= pathType gtyp
               runQ [t| Path_Either $(return lpath) $(return rpath) |]
    , _doField = \_ _ -> pure ()
    , _doConcs = \_ _ -> pure ()
    , _doSyn =
        \name typ ->
            error $ "PathType _doSyn " ++ show name ++ " " ++ show typ
    , _doAlts =
        \_ -> runQ $ [t|$(asTypeQ (makePathType (ModelType (asName key)))) $gtyp|]
    }

-- | Given a type, compute the corresponding path type.
pathType :: forall m. TypeGraphM m =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = pathType' (pathTypeControl gtyp key) gtyp key

pathType' :: forall m. TypeGraphM m =>
             Control m () () Type
          -> TypeQ
          -> TGVSimple -- ^ The type to convert to a path type
          -> m Type
pathType' control gtyp key = do
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType key]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType key]
  viewTypeMaybe <- viewInstanceType (asType key)
  case asType key of
    _ | selfPath -> _doSelf control
      | simplePath -> _doSimple control
      | isJust viewTypeMaybe ->
          do let Just viewType = viewTypeMaybe
             _doView control viewType
    ConT tname ->
        runQ $ [t|$(asTypeQ (makePathType (ModelType tname))) $gtyp|]
    AppT (AppT mtyp ityp) etyp
        | mtyp == ConT ''Order ->
            _doOrder control ityp etyp
    AppT ListT etyp ->
        _doList control etyp
    AppT (AppT t3 ktyp) vtyp
        | t3 == ConT ''Map ->
            _doMap control ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp ->
        _doPair control ftyp styp
    AppT t1 vtyp
        | t1 == ConT ''Maybe ->
            _doMaybe control vtyp
    AppT (AppT t3 ltyp) rtyp
        | t3 == ConT ''Either ->
            _doEither control ltyp rtyp
    _ -> do ks <- reachableFromSimple key
            error $ "pathType otherf: " ++ pprint1 key ++ "\n" ++
                    intercalate "\n  " ("reachable from:" : List.map pprint1 (Foldable.toList ks))
