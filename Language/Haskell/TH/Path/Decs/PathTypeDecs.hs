-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

{-# LANGUAGE CPP #-}
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
module Language.Haskell.TH.Path.Decs.PathTypeDecs
    ( pathTypeDecs
    ) where

import Control.Lens (_2, view)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable
import Data.Set.Extra as Set (delete, map)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (allPathTypeNames, asConQ, asName, asTypeQ, bestPathTypeName, ModelType(ModelType),
                                        makeFieldCon, makePathCon, makePathType, telld, tells)
import Language.Haskell.TH.Path.Core (IdPath(idPath))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.TypeGraph.Expand (unE)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, typeNames)

pathTypeDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathTypeDecs v = do
  a <- runQ $ newName "_a"
  let control = pathTypeDecControl v a
      pname = bestPathTypeName v
      psyns = Set.delete pname (allPathTypeNames v)
  -- It would be nice if this code lived inside pathTypeDecControl somewhere.
  runQ (mapM (\psyn -> tySynD (asName psyn) [PlainTV a] (appT (asTypeQ pname) (varT a))) (toList psyns)) >>= tell
  doNode control v

pathTypeDecControl :: (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> Name -> Control m [ConQ] (PatQ, [[ConQ]]) ()
pathTypeDecControl v a =
    let pname = bestPathTypeName v in
    Control
    { _doSimple = do
        dec <- runQ $ dataD (pure []) (asName pname) [PlainTV a] [normalC (asName pname) []] supers
        -- e.g. data Path_Int a = Path_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
        -- trace ("doSimple " ++ show v ++ " -> " ++ pprint1 dec) (return ())
        tells [pure dec]
        telld [d|instance IdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|]
    , _doSelf = pure ()
    , _doView =
        \skey -> do
          ptype <- pathType (varT a) skey
          tells [dataD (pure []) (asName pname) [PlainTV a]
                       [ normalC (asName (makePathCon pname "View")) [strictType notStrict (pure ptype)]
                       , normalC (asName pname) []
                       ] supers]
          telld [d|instance IdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|]

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
                  do skey' <- tgvSimple' typ
                     key' <- tgv (Just fld) skey'
                     let Just pcname = makeFieldCon key'
                     ptype <- case typ of
                                ConT ftname -> runQ $ appT (asTypeQ (makePathType (ModelType ftname))) (varT a)
                                -- It would be nice to use pathTypeCall (varT a) key' here, but
                                -- it can't infer the superclasses for (PathType Foo a) - Ord,
                                -- Read, Data, etc.
                                _ -> pathType (varT a) skey'
                     case ptype of
                       TupleT 0 -> pure []
                       -- Given the list of clauses for a field's path type, create new
                       -- constructor for the field in the parent record and alter the
                       -- clauses to match expressions wrapped in this new constructor.
                       _ -> pure [normalC (asName pcname) [strictType notStrict (return ptype)]]
              (_tname, _cname, Left _fname) -> pure []
    , _doConcs =
        \xpat pcons -> pure (xpat, pcons)
    , _doAlts =
        \pairs ->
            mapM_ (\pname ->
                       do tells [dataD (cxt []) (asName pname) [PlainTV a] (concat (concatMap snd pairs) ++ [normalC (asName pname) []]) supers]
                          telld [d|instance IdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|])
                  (Set.map makePathType . Set.map ModelType . typeNames $ v)
    , _doSyn =
        \tname stype ->
            if ConT tname == view (_2 . etype . unE) v
            then pure ()
            else do
              skey <- tgvSimple' stype
              ptype <- pathType (varT a) skey
              dec <- runQ $ tySynD (asName (makePathType (ModelType tname))) [PlainTV a] {-(appT (asTypeQ pname) (varT a))-} (pure ptype)
              tells [pure dec]
    }

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]
