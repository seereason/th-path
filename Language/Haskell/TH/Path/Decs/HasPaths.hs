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
module Language.Haskell.TH.Path.Decs.HasPaths (pathDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.List as List (concatMap, map)
import Data.Map as Map (toList)
import Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, HasName(asName), makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), HasPaths(..), ToLens(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (testIsPath, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doTGVSimple)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys)
import Language.Haskell.TH.TypeGraph.Vertex (field, TGVSimple, TypeGraphVertex(bestType))

pathDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathDecs v =
    pathKeys v >>= Set.mapM_ (pathDecs' v)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: (TypeGraphM m, MonadWriter [Dec] m) =>
             TGVSimple -> TGVSimple -> m ()
pathDecs' v gkey = do
  ptyp <- pathType (pure (bestType gkey)) v
  x <- runQ (newName "s")
  g <- runQ (newName "g")
  poc <- case v == gkey of
           True -> pure [clause [wildP, wildP] (normalB [| [idPath] |]) []]
           False -> execWriterT (doTGVSimple (hasPathControl v gkey g x) v)
  when (not (null poc))
       (tells [ instanceD (pure []) [t|HasPaths $(pure (bestType v)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType v), pure (bestType gkey)] (pure ptyp))
                , funD 'pathsOf poc
                ]])

hasPathControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TGVSimple -> TGVSimple -> Name -> Name -> Control m (Type, ExpQ)
hasPathControl v gkey g x =
    Control { _doView =
                \w -> do
                  let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                  pure (asType w, [|map (\a' -> ($(asConQ pcname) {-:: Path $(asTypeQ w) $(asTypeQ gkey) -> Path $(asTypeQ v) $(asTypeQ gkey)-}, a'))
                                        (toListOf (toLens ($(asConQ pcname) (idPath :: Path $(asTypeQ w) $(asTypeQ w)))) $(varE x)) |])
            , _doOrder =
                \w -> do
                  pure (asType w, [| map (\(idx, val) -> (Path_At idx, val)) (toPairs $(varE x)) |])
            , _doMap =
                \w -> do
                  pure (asType w, [| map (\(idx, val) -> (Path_Look idx, val)) (Map.toList $(varE x)) |])
            , _doPair =
                \f s -> do
                  pure ((asType f, [| [(Path_First, fst $(varE x))] |]),
                        (asType s, [| [(Path_Second, snd $(varE x))] |]))
            , _doMaybe =
                \w -> do
                  pure (asType w, [| case $(varE x) of Nothing -> []; Just a' -> [(Path_Just, a')]|])
            , _doEither =
                \l r ->
                    do pure ((asType l, [| case $(varE x) of Left a' -> [(Path_Left, a')]; Right _ -> []|]),
                             (asType r, [| case $(varE x) of Left _ -> []; Right a' -> [(Path_Right, a')]|]))
            , _doField =
                \f ->
                    case view (_2 . field) f of
                      Just (_tname, _cname, Right fname) ->
                          pure (asType f, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x)))] |])
                      Just (_tname, _cname, Left fpos) ->
                          pure (asType f, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show fpos))),
                                               $(do p <- newName "p"
                                                    lamE (replicate (fpos-1) wildP ++ [varP p] ++ replicate (2-fpos) wildP) (varE p)) $(varE x))] |])
                      Nothing -> error "Not a field"
            , _doAlt =
                \(xpat, concs) -> do
                  exps <- mapM (\(typ, asList) ->
                                    do isPath <- testIsPath typ gkey
                                       case isPath of
                                         False -> pure [| [] |]
                                         True -> pure [| List.concatMap
                                                           (\(p, a') -> (List.map p (pathsOf (a' :: $(pure typ)) $(varE g) {-:: [Path $(pure typ) $(asTypeQ gkey)]-})) {-:: [Path $(pure styp) $(asTypeQ gkey)]-})
                                                           ($asList {-:: [(Path $(pure typ) $(asTypeQ gkey) -> Path $(pure styp) $(asTypeQ gkey), $(pure typ))]-}) |])
                               concs
                  tell [clause [asP' x xpat, varP g] (normalB (mconcatQ exps)) []]
            }
