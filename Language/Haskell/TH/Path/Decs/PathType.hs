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

import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asName, asType, asTypeQ, bestPathTypeName, ModelType(ModelType),
                                        makePathType)
import Language.Haskell.TH.Path.Core (Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple)

-- | Given a type, compute the corresponding path type.
pathType :: forall m. TypeGraphM m =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = doNode (pathTypeControl gtyp key) key

pathTypeControl :: (TypeGraphM m) => TypeQ -> TGVSimple -> Control m () () Type
pathTypeControl gtyp key =
    Control
    { _doSelf = pure $ asType key
    , _doSimple = runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    , _doView = \_ -> runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    , _doOrder =
        \ityp etyp ->
            do epath <- pathType gtyp etyp
               runQ [t|Path_OMap $(pure ityp) $(pure epath)|]
    , _doMap =
        \ktyp vtyp ->
            do vpath <- pathType gtyp vtyp
               runQ [t| Path_Map $(pure ktyp) $(pure vpath)|]
    , _doList =
        \etyp ->
            do epath <- pathType gtyp etyp
               runQ [t|Path_List $(return epath)|]
    , _doPair =
        \ftyp styp ->
            do fpath <- pathType gtyp ftyp
               spath <- pathType gtyp styp
               runQ [t| Path_Pair $(return fpath) $(return spath) |]
    , _doMaybe =
        \typ ->
            do epath <- pathType gtyp typ
               runQ [t|Path_Maybe $(pure epath)|]
    , _doEither =
        \ltyp rtyp ->
            do lpath <- pathType gtyp ltyp
               rpath <- pathType gtyp rtyp
               runQ [t| Path_Either $(return lpath) $(return rpath) |]
    , _doField = \_ _ -> pure ()
    , _doConcs = \_ _ -> pure ()
    , _doSyn =
        \tname _typ ->
            runQ $ [t|$(asTypeQ (makePathType (ModelType tname))) $gtyp|]
    , _doAlts =
        \_ -> runQ $ [t|$(asTypeQ (makePathType (ModelType (asName key)))) $gtyp|]
    , _doSyns = \r0 _rs -> pure r0
    }
