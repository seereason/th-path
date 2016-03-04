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
import Data.Map as Map (lookup, toList)
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, HasName(asName), makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), HasPaths(..), ToLens(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (Control(..), doTGVSimple)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, pathKeys, tgvSimple')
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
  -- x <- runQ (newName "s")
  g <- runQ (newName "g")
  poc <- case v == gkey of
           True -> pure [clause [wildP, wildP] (normalB [| [idPath] |]) []]
#if 1
           False -> execWriterT (doTGVSimple (hasPathControl v gkey g) v)
#else
           False -> execWriterT (pathsOfClauses (hasPathControl v gkey s g) v gkey x g)
#endif
  when (not (null poc))
       (tells [ instanceD (pure []) [t|HasPaths $(pure (bestType v)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType v), pure (bestType gkey)] (pure ptyp))
                , funD 'pathsOf poc
                ]])

hasPathControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TGVSimple -> TGVSimple -> Name -> Control m (Type, ExpQ) ExpQ
hasPathControl v gkey g =
    Control { _doView =
                \x w -> do
                  let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                  pure [(wildP, [(asType w,
                                  [|map (\a' -> ($(asConQ pcname) {-:: Path $(asTypeQ w) $(asTypeQ gkey) -> Path $(asTypeQ v) $(asTypeQ gkey)-}, a'))
                                        (toListOf (toLens ($(asConQ pcname) (idPath :: Path $(asTypeQ w) $(asTypeQ w)))) $(varE x)) |])])]
            , _doOrder =
                \x w -> do
                  pure [(wildP, [(asType w, [| map (\(idx, val) -> (Path_At idx, val)) (toPairs $(varE x)) |])])]
            , _doMap =
                \x w -> do
                  pure [(wildP, [(asType w, [| map (\(idx, val) -> (Path_Look idx, val)) (Map.toList $(varE x)) |])])]
            , _doPair =
                \x f s -> do
                  pure [(wildP, [(asType f, [| [(Path_First, fst $(varE x))] |]),
                                 (asType s, [| [(Path_Second, snd $(varE x))] |])])]
            , _doMaybe =
                \x w -> do
                  pure [(wildP, [(asType w, [| case $(varE x) of Nothing -> []; Just a' -> [(Path_Just, a')]|])])]
            , _doEither =
                \x l r ->
                    do pure [(conP 'Left [wildP], [(asType l, [| case $(varE x) of Left a' -> [(Path_Left, a')]; Right _ -> []|])]),
                             (conP 'Right [wildP], [(asType r, [| case $(varE x) of Left _ -> []; Right a' -> [(Path_Right, a')]|])])]
            , _doField =
                \x f ->
                    case view (_2 . field) f of
                      Just (_tname, _cname, Right fname) ->
                          pure (asType f, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x)))] |])
                      Just (_tname, _cname, Left fpos) ->
                          pure (asType f, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show fpos))),
                                               $(do p <- newName "p"
                                                    lamE (replicate (fpos-1) wildP ++ [varP p] ++ replicate (2-fpos) wildP) (varE p)) $(varE x))] |])
                      Nothing -> error "Not a field"
            , _doConc =
                \_x (typ, asList) ->
                    do isPath <- testIsPath typ gkey
                       case isPath of
                         False -> pure [| [] |]
                         True -> pure [| List.concatMap
                                           (\(p, a') -> (List.map p (pathsOf (a' :: $(pure typ)) $(varE g) {-:: [Path $(pure typ) $(asTypeQ gkey)]-})) {-:: [Path $(pure styp) $(asTypeQ gkey)]-})
                                           ($asList {-:: [(Path $(pure typ) $(asTypeQ gkey) -> Path $(pure styp) $(asTypeQ gkey), $(pure typ))]-}) |]
            , _doAlt =
                \xpat exps ->
                    tell [clause [xpat, varP g] (normalB (mconcatQ exps)) []]
            }

-- | See if there is a path from typ to gkey.  We need to avoid
-- building expressions for non-existant paths because they will cause
-- "no Path instance" errors.
testIsPath :: TypeGraphM m => Type -> TGVSimple -> m Bool
testIsPath typ gkey = do
  mkey <- tgvSimple' typ
  case mkey of
    Nothing -> pure False
    Just v -> (maybe False (Set.member gkey) . Map.lookup v) <$> allPathKeys
