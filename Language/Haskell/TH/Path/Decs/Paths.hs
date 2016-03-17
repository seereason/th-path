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
module Language.Haskell.TH.Path.Decs.Paths (pathDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.List as List (concatMap, map)
import Data.Map as Map (toList)
import Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, HasName(asName), makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (IdPath(idPath), Paths(..), ToLens(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (testIsPath, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doType, finishConc, finishEither, finishPair)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys', simplify)
import Language.Haskell.TH.TypeGraph.Vertex (TGV, TGVSimple, TypeGraphVertex(bestType))

pathDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TGV -> m ()
pathDecs v =
    pathKeys' v >>= Set.mapM_ (pathDecs' v)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: (TypeGraphM m, MonadWriter [Dec] m) =>
             TGV -> TGVSimple -> m ()
pathDecs' v gkey = do
  v' <- simplify v
  ptyp <- pathType (pure (bestType gkey)) v'
  x <- runQ (newName "_s")
  g <- runQ (newName "_g")
  poc <- case v' == gkey of
           True -> pure [clause [wildP, wildP] (normalB [| [idPath] |]) []]
           False -> execWriterT (doType (hasPathControl v gkey g x) v)
  when (not (null poc))
       (tells [ instanceD (pure []) [t|Paths $(pure (bestType v)) $(pure (bestType gkey))|]
                [ tySynInstD ''FromTo (tySynEqn [pure (bestType v), pure (bestType gkey)] (pure ptyp))
                , funD 'paths poc
                ]])

hasPathControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TGV -> TGVSimple -> Name -> Name -> Control m (Type, ExpQ) () ()
hasPathControl v gkey g x =
    let control = hasPathControl v gkey g x in
    Control { _doSimple = pure ()
            , _doSelf = pure ()
            , _doView =
                \w -> do
                  let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                  alt <- _doConcs control wildP
                             [(asType w, [|map (\a' -> ($(asConQ pcname) {-:: FromTo $(asTypeQ w) $(asTypeQ gkey) -> FromTo $(asTypeQ v) $(asTypeQ gkey)-}, a'))
                                               (toListOf (toLens ($(asConQ pcname) (idPath :: FromTo $(asTypeQ w) $(asTypeQ w)))) $(varE x)) |])]
                  _doAlts control [alt]
            , _doOrder =
                \_i w -> do
                  finishConc control (asType w, [| map (\(idx, val) -> (Path_At idx, val)) (toPairs $(varE x)) |])
            , _doMap =
                \_i w -> do
                  finishConc control (asType w, [| map (\(idx, val) -> (Path_Look idx, val)) (Map.toList $(varE x)) |])
            , _doList =
                \_e -> pure ()
            , _doPair =
                \f s -> finishPair control
                                   (asType f, [| [(Path_First, fst $(varE x))] |])
                                   (asType s, [| [(Path_Second, snd $(varE x))] |])
            , _doMaybe =
                \w -> do
                  finishConc control (asType w, [| case $(varE x) of Nothing -> []; Just a' -> [(Path_Just, a')]|])
            , _doEither =
                \l r ->
                    do let lconc = (asType l, [| case $(varE x) of Left a' -> [(Path_Left, a')]; Right _ -> []|])
                           rconc = (asType r, [| case $(varE x) of Left _ -> []; Right a' -> [(Path_Right, a')]|])
                       finishEither control lconc rconc
                       -- lalt <- _doConcs control (conP 'Left [wildP]) [lconc]
                       -- ralt <- _doConcs control (conP 'Right [wildP]) [rconc]
                       -- _doAlts control [lalt, ralt]
{-
                    do pure ((asType l, [| case $(varE x) of Left a' -> [(Path_Left, a')]; Right _ -> []|]),
                             (asType r, [| case $(varE x) of Left _ -> []; Right a' -> [(Path_Right, a')]|]))
-}
            , _doField =
                \fld typ ->
                    case fld of
                      (_tname, _cname, Right fname) ->
                          pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x)))] |])
                      (_tname, _cname, Left fpos) ->
                          pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show fpos))),
                                               $(do p <- newName "p"
                                                    lamE (replicate (fpos-1) wildP ++ [varP p] ++ replicate (2-fpos) wildP) (varE p)) $(varE x))] |])
            , _doConcs =
                \xpat concs -> do
                  exps <- mapM (\(typ, asList) ->
                                    do isPath <- testIsPath typ gkey
                                       case isPath of
                                         False -> pure [| [] |]
                                         True -> pure [| List.concatMap
                                                           (\(p, a') -> (List.map p (paths (a' :: $(pure typ)) $(varE g) {-:: [FromTo $(pure typ) $(asTypeQ gkey)]-})) {-:: [FromTo $(pure styp) $(asTypeQ gkey)]-})
                                                           ($asList {-:: [(FromTo $(pure typ) $(asTypeQ gkey) -> FromTo $(pure styp) $(asTypeQ gkey), $(pure typ))]-}) |])
                               concs
                  tell [clause [asP' x xpat, varP g] (normalB (mconcatQ exps)) []]
            , _doSyn =
                \_tname _typ -> pure ()
            , _doAlts = \_ -> pure ()
            }
