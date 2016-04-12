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
module Language.Haskell.TH.Path.Decs.UPath
    ( upathTypeDecs
    ) where

import Control.Lens (_2, view)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable
import Data.Set.Extra as Set (delete, map)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (allUPathTypeNames, asConQ, asName, asTypeQ, bestUPathTypeName, ModelType(ModelType),
                                        makeUFieldCon, makePathCon, makeUPathType, PathCon, telld, tells)
import Language.Haskell.TH.Path.Core (IdPath(idPath))
import Language.Haskell.TH.Path.Decs.PathType (upathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.TypeGraph.Expand (unE)
import Language.Haskell.TH.TypeGraph.Shape (Field)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, typeNames)

upathTypeDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
upathTypeDecs v = do
  let control = upathTypeDecControl v
      pname = bestUPathTypeName v
      psyns = Set.delete pname (allUPathTypeNames v)
  -- It would be nice if this code lived inside upathTypeDecControl somewhere.
  runQ (mapM (\psyn -> tySynD (asName psyn) [] (asTypeQ pname)) (toList psyns)) >>= tell
  doNode control v

upathTypeDecControl :: (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> Control m [ConQ] (PatQ, [[ConQ]]) ()
upathTypeDecControl v =
    let pname = bestUPathTypeName v in
    Control
    { _doSimple = do
        dec <- runQ $ dataD (pure []) (asName pname) [] [normalC (asName pname) []] supers
        -- e.g. data UPath_Int = Path_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
        -- trace ("doSimple " ++ show v ++ " -> " ++ pprint1 dec) (return ())
        tells [pure dec]
        telld [d|instance IdPath $(asTypeQ pname) where idPath = $(asConQ pname)|]
    , _doSelf = pure ()
    , _doView =
        \skey -> do
          ptype <- upathType skey
          tells [dataD (pure []) (asName pname) []
                       [ normalC (asName (makePathCon pname "View")) [strictType notStrict (pure ptype)]
                       , normalC (asName pname) []
                       ] supers]
          telld [d|instance IdPath $(asTypeQ pname) where idPath = $(asConQ pname)|]

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
                          telld [d|instance IdPath $(asTypeQ pname') where idPath = $(asConQ pname')|])
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

fieldUPathType :: TypeGraphM m => Field -> Type -> m (PathCon Name, Type)
fieldUPathType fld typ =
    do skey' <- tgvSimple' typ
       key' <- tgv (Just fld) skey'
       let Just pcname = makeUFieldCon key'
       ptype <- case typ of
                  ConT ftname -> runQ $ asTypeQ (makeUPathType (ModelType ftname))
                         -- It would be nice to use pathTypeCall (varT a) key' here, but
                         -- it can't infer the superclasses for (PathType Foo a) - Ord,
                         -- Read, Data, etc.
                  _ -> upathType skey'
       return (pcname, ptype)

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]
