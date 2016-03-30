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
module Language.Haskell.TH.Path.Decs.Paths (pathDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.List as List (concatMap, map)
import Data.Map as Map (toList)
import Data.Proxy (Proxy(Proxy))
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, bestPathTypeName, HasName(asName),
                                        makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (Describe(..), IdPath(idPath), Paths(..), PathStart(Peek), ToLens(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathStart (makePeekCon)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Decs.PathTypeDecs (fieldPathType)
import Language.Haskell.TH.Path.Graph (testIsPath, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode, finishConcs)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, TypeGraphVertex(bestType))

pathDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathDecs v =
    pathKeys v >>= Set.mapM_ (pathDecs' v)

data ClauseType
    = PathClause ClauseQ
    | DescClause ClauseQ
    | PeekClause ClauseQ
    | PeekPathClause ClauseQ
    | PeekValueClause ClauseQ
    | PeekConsClause ClauseQ

partitionClauses :: [ClauseType] -> ([ClauseQ], [ClauseQ], [ClauseQ], [ClauseQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], [], [], [], []) xs
    where
      f (PathClause c) (pcs, dcs, pkcs, ppcs, pvcs, pccs) = (c : pcs, dcs, pkcs, ppcs, pvcs, pccs)
      f (DescClause c) (pcs, dcs, pkcs, ppcs, pvcs, pccs) = (pcs, c : dcs, pkcs, ppcs, pvcs, pccs)
      f (PeekClause c) (pcs, dcs, pkcs, ppcs, pvcs, pccs) = (pcs, dcs, c : pkcs, ppcs, pvcs, pccs)
      f (PeekPathClause c) (pcs, dcs, pkcs, ppcs, pvcs, pccs) = (pcs, dcs, pkcs, c: ppcs, pvcs, pccs)
      f (PeekValueClause c) (pcs, dcs, pkcs, ppcs, pvcs, pccs) = (pcs, dcs, pkcs, ppcs, c : pvcs, pccs)
      f (PeekConsClause c) (pcs, dcs, pkcs, ppcs, pvcs, pccs) = (pcs, dcs, pkcs, ppcs, pvcs, c : pccs)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: (TypeGraphM m, MonadWriter [Dec] m) =>
             TGVSimple -> TGVSimple -> m ()
pathDecs' v gkey = do
  ptyp <- pathType (asTypeQ gkey) v
  x <- runQ (newName "_s")
  g <- runQ (newName "_g")
  (pcs, dcs, pkcs, ppcs, pvcs, pccs) <-
      partitionClauses <$>
      case v == gkey of
        True -> pure ([PathClause $ clause [wildP, wildP] (normalB [| [idPath] |]) [],
                       PeekClause $ clause [wildP, wildP] (normalB [| undefined "idpeek" :: Peek $(asTypeQ v) |]) []] ++
                      peekAccessors v gkey)
        False -> execWriterT (doNode (hasPathControl v gkey g x) v)
  when (not (null pcs))
       (tells [instanceD (pure []) [t|Paths $(pure (bestType v)) $(pure (bestType gkey))|]
                 [ tySynInstD ''Path (tySynEqn [pure (bestType v), pure (bestType gkey)] (pure ptyp))
                 , funD 'paths pcs
                 , funD 'peek (case pkcs of [] -> [clause [wildP, wildP] (normalB [| undefined "idpeek" :: Peek $(asTypeQ v) |]) []]
                                            _ -> pkcs)
                 , funD 'peekPath ppcs
                 , funD 'peekValue pvcs
                 , funD 'peekCons pccs ]])
  when (not (null dcs))
       (tells [instanceD (pure []) [t|Describe $(asTypeQ v) $(asTypeQ gkey)|]
                [ funD 'describe' dcs ]])

hasPathControl :: (TypeGraphM m, MonadWriter [ClauseType] m) => TGVSimple -> TGVSimple -> Name -> Name -> Control m (Type, ExpQ) () ()
hasPathControl v gkey g x =
    let control = hasPathControl v gkey g x in
    Control { _doSimple =
                  tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doSimple" :: Peek $(asTypeQ v) |]) []]
            , _doSelf = pure ()
            , _doView =
                \w -> do
                  let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                  tell [PeekClause $ clause [conP (asName pcname) [wildP], wildP] (normalB [| undefined "doView1" :: Peek $(asTypeQ v) |]) [],
                        PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doView2" :: Peek $(asTypeQ v) |]) []]
                  alt <- _doConcs control wildP
                             [(asType w, [|map (\a' -> ($(asConQ pcname) {-:: Path $(asTypeQ w) $(asTypeQ gkey) -> Path $(asTypeQ v) $(asTypeQ gkey)-}, a'))
                                               (toListOf (toLens ($(asConQ pcname) (idPath :: Path $(asTypeQ w) $(asTypeQ w)))) $(varE x)) |])]
                  _doAlts control [alt]
            , _doOrder =
                \_i w -> do
                  finishConcs control [(wildP, [(asType w, [| map (\(idx, val) -> (Path_At idx, val)) (toPairs $(varE x)) |])])]
            , _doMap =
                \_i w -> do
                  finishConcs control [(wildP, [(asType w, [| map (\(idx, val) -> (Path_Look idx, val)) (Map.toList $(varE x)) |])])]
            , _doList =
                \_e -> pure ()
            , _doPair =
                \f s -> finishConcs control
                                   [(wildP, [(asType f, [| [(Path_First, fst $(varE x))] |]),
                                             (asType s, [| [(Path_Second, snd $(varE x))] |])])]
            , _doMaybe =
                \w -> do
                  finishConcs control [(wildP, [(asType w, [| case $(varE x) of Nothing -> []; Just a' -> [(Path_Just, a')]|])])]
            , _doEither =
                \l r ->
                    do let lconc = (asType l, [| case $(varE x) of Left a' -> [(Path_Left, a')]; Right _ -> []|])
                           rconc = (asType r, [| case $(varE x) of Left _ -> []; Right a' -> [(Path_Right, a')]|])
                       finishConcs control [(conP 'Left [wildP], [lconc]), (conP 'Right [wildP], [rconc])]
            , _doField =
                \fld typ ->
                    case fld of
                      (_tname, _cname, Right fname) ->
                          do (pcname, _) <- fieldPathType (error "unused name") fld typ
                             tell [PeekClause $ clause [conP (asName pcname) [wildP], wildP] (normalB [| undefined "doField" :: Peek $(asTypeQ v) |]) []]
                             pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x)))] |])
{-
                          do fkey <- tgvSimple' typ
                             key' <- tgv (Just fld) fkey
                             tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doView2" :: Peek $(asTypeQ v) |]) []]
                             pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x)))] |])
-}
                      (_tname, _cname, Left fpos) ->
                          pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show fpos))),
                                               $(do p <- newName "p"
                                                    lamE (replicate (fpos-1) wildP ++ [varP p] ++ replicate (2-fpos) wildP) (varE p)) $(varE x))] |])
            , _doConcs =
                \xpat concs -> do
                  exps <- concat <$>
                          mapM (\(typ, asList) ->
                                    do isPath <- testIsPath typ gkey
                                       {- let _nextPathType = [t|Path $(pure typ) $(asTypeQ gkey)|]
                                              _thisPathType = [t|Path $(asTypeQ v) $(asTypeQ gkey)|] -}
                                       case isPath of
                                         False -> pure []
                                         True -> pure [ [| List.concatMap
                                                             (\(p, a') -> (List.map p (paths (a' :: $(pure typ)) $(varE g) {-:: [$_nextPathType]-})) {-:: [$_thisPathType]-})
                                                             ($asList {-:: [($_nextPathType -> $_thisPathType, $(pure typ))]-}) |] ])
                               concs
                  tell [ PathClause $ clause [asP' x xpat, varP g] (normalB (mconcatQ exps)) []
                        -- We need a clause for every constructor in the path type
                       -- , PeekClause $ newName "_p" >>= \p -> clause [varP p, asP' x xpat] (normalB [| undefined :: Peek $(asTypeQ v)|]) []
                       ]
            , _doSyn =
                \_tname _typ -> pure ()
            , _doAlts =
                \_ -> do
                  keys <- pathKeys v
                  when (Set.member gkey keys) (tell $ peekAccessors v gkey)
            , _doSyns = \() _ -> pure ()
            }

peekAccessors :: TGVSimple -> TGVSimple -> [ClauseType]
peekAccessors v gkey =
    [PeekPathClause $
       newName "_p" >>= \p ->
       clause [conP 'Proxy [], conP (asName (makePeekCon (ModelType (asName v)) (ModelType (asName gkey)))) [varP p, wildP]]
              (normalB [| $(varE p) :: Path $(asTypeQ v) $(asTypeQ gkey)|])
              [],
     PeekValueClause $
       newName "_x" >>= \x ->
       clause [conP 'Proxy [], conP (asName (makePeekCon (ModelType (asName v)) (ModelType (asName gkey)))) [wildP, varP x]]
              (normalB [| $(varE x) :: Maybe $(asTypeQ gkey)|])
              [],
     PeekConsClause $
       newName "_x" >>= \x ->
       newName "_p" >>= \p ->
       clause [varP p, varP x]
              (normalB [| $(conE (asName (makePeekCon (ModelType (asName v)) (ModelType (asName gkey)))))
                             $(varE p) $(varE x) :: Peek $(asTypeQ v)|])
              []]
