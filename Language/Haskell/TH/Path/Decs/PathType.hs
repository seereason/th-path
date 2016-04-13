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
    , upathTypeDecs
    ) where

import Control.Lens (_2, view)
import Control.Monad.Writer (MonadWriter)
import Data.Data (Data, Typeable)
import Data.Set.Extra as Set (map)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, HasName(asName), asType, asTypeQ, ModelType(ModelType),
                                        makeUFieldCon, makePathCon, makeUPathType, PathCon, PathType, telld, tells)
import Language.Haskell.TH.Path.Core (IsPath(..), Path_List, Path_Map, Path_Pair, Path_Maybe, Path_Either)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.TypeGraph.Expand (unE)
import Language.Haskell.TH.TypeGraph.Shape (Field)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, typeNames)

-- | Given a type, compute the corresponding path type.
upathType :: forall m. TypeGraphM m =>
             TGVSimple -- ^ The type to convert to a path type
          -> m Type
upathType key = doNode (upathTypeControl key) key

upathTypeControl :: (TypeGraphM m) => TGVSimple -> Control m () () Type
upathTypeControl key =
    Control
    { _doSelf = pure $ asType key
    , _doSimple = runQ (asTypeQ (bestUPathTypeName key))
    , _doView = \_ -> runQ (asTypeQ (bestUPathTypeName key))
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
        \_ -> runQ $ (asTypeQ (makeUPathType (ModelType (asName key))))
    , _doSyns = \r0 _rs -> pure r0
    }

upathTypeDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
upathTypeDecs utype v = doNode (upathTypeDecControl utype v) v

upathTypeDecControl :: (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> Control m [ConQ] (PatQ, [[ConQ]]) ()
upathTypeDecControl utype v =
    let pname = bestUPathTypeName v in
    Control
    { _doSimple = do
        dec <- runQ $ dataD (pure []) (asName pname) [] [normalC (asName pname) []] supers
        -- e.g. data UPath_Int = Path_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
        -- trace ("doSimple " ++ show v ++ " -> " ++ pprint1 dec) (return ())
        tells [pure dec]
        telld [d|instance IsPath $(asTypeQ pname) where
                    type UType $(asTypeQ pname) = $utype
                    type SType $(asTypeQ pname) = $(asTypeQ v)
                    idPath = $(asConQ pname)|]
    , _doSelf = pure ()
    , _doView =
        \skey -> do
          ptype <- upathType skey
          tells [dataD (pure []) (asName pname) []
                       [ normalC (asName (makePathCon pname "View")) [strictType notStrict (pure ptype)]
                       , normalC (asName pname) []
                       ] supers]
          telld [d|instance IsPath $(asTypeQ pname) where
                      type UType $(asTypeQ pname) = $utype
                      type SType $(asTypeQ pname) = $(asTypeQ v)
                      idPath = $(asConQ pname)|]

    , _doOrder = \_ _ -> pure ()
    , _doMap = \_ _ -> pure ()
    , _doList = \_ -> pure ()
    , _doPair = \_ _ -> pure ()
    , _doMaybe = \_ -> pure ()
    , _doEither = \_ _ -> pure ()
    , _doField =
        \fld typ ->
            case fld of
              (_tname, _cname, Right _fname) ->
                  do (pcname, ptype) <- fieldUPathType fld typ
                     pure [normalC (asName pcname) [strictType notStrict (return ptype)]]
              (_tname, _cname, Left _fname) -> pure []
    , _doConcs =
        \xpat pcons -> pure (xpat, pcons)
    , _doAlts =
        \pairs ->
            mapM_ (\pname' ->
                       do tells [dataD (cxt []) (asName pname') [] (concat (concatMap snd pairs) ++ [normalC (asName pname') []]) supers]
                          telld [d|instance IsPath $(asTypeQ pname') where
                                      type UType $(asTypeQ pname') = $utype
                                      type SType $(asTypeQ pname') = $(asTypeQ v)
                                      idPath = $(asConQ pname')|])
                  (Set.map makeUPathType . Set.map ModelType . typeNames $ v)
    , _doSyn =
        \tname stype ->
            if ConT tname == view (_2 . etype . unE) v
            then pure ()
            else do
              skey <- tgvSimple' stype
              ptype <- upathType skey
              dec <- runQ $ tySynD (asName (makeUPathType (ModelType tname))) [] (pure ptype)
              tells [pure dec]
    , _doSyns = \() _ -> pure ()
    }

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestUPathTypeName :: HasName v => v -> PathType Name
bestUPathTypeName = makeUPathType . ModelType . asName

fieldUPathType :: TypeGraphM m => Field -> Type -> m (PathCon Name, Type)
fieldUPathType fld typ =
    do skey' <- tgvSimple' typ
       key' <- tgv (Just fld) skey'
       let Just pcname = makeUFieldCon key'
       ptype <- upathType skey'
       return (pcname, ptype)

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]
