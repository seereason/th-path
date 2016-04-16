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
    ( upathType
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasName(asName), asType, asTypeQ, ModelType(ModelType), makeUPathType, PathType)
import Language.Haskell.TH.Path.Core (Path_List, Path_Map, Path_Pair, Path_Maybe, Path_Either, Path_View)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

-- | Given a type, compute the corresponding path type.
upathType :: forall m. TypeGraphM m =>
             TGVSimple -- ^ The type to convert to a path type
          -> m Type
upathType v = doNode (upathTypeControl v) v

upathTypeControl :: (TypeGraphM m) => TGVSimple -> Control m () () Type
upathTypeControl v =
    Control
    { _doSelf = pure $ asType v
    , _doSimple = runQ (asTypeQ (bestUPathTypeName v))
    , _doView =
        \w -> do ptype <- upathType w
                 runQ [t|Path_View $(asTypeQ v) $(asTypeQ ptype)|]
    , _doOrder =
        \ityp etyp ->
            do epath <- upathType etyp
               runQ [t|Path_OMap $(pure ityp) $(pure epath)|]
    , _doMap =
        \ktyp vtyp ->
            do vpath <- upathType vtyp
               runQ [t| Path_Map $(pure ktyp) $(pure vpath)|]
    , _doList =
        \etyp ->
            do epath <- upathType etyp
               runQ [t|Path_List $(return epath)|]
    , _doPair =
        \ftyp styp ->
            do fpath <- upathType ftyp
               spath <- upathType styp
               runQ [t| Path_Pair $(return fpath) $(return spath) |]
    , _doMaybe =
        \typ ->
            do epath <- upathType typ
               runQ [t|Path_Maybe $(pure epath)|]
    , _doEither =
        \ltyp rtyp ->
            do lpath <- upathType ltyp
               rpath <- upathType rtyp
               runQ [t| Path_Either $(return lpath) $(return rpath) |]
    , _doField = \_ _ -> pure ()
    , _doConcs = \_ _ -> pure ()
    , _doSyn =
        \tname _typ ->
            runQ $ (asTypeQ (makeUPathType (ModelType tname)))
    , _doAlts =
        \_ -> runQ $ (asTypeQ (makeUPathType (ModelType (asName v))))
    , _doSyns = \r0 _rs -> pure r0
    }

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestUPathTypeName :: HasName v => v -> PathType Name
bestUPathTypeName = makeUPathType . ModelType . asName
