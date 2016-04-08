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
import Data.List as List (concatMap)
import Data.Map as Map (toList)
import Data.Proxy (Proxy(Proxy))
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, bestPathTypeName, HasName(asName),
                                        makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (Describe(..), IdPath(idPath), PathsOld(..), PathStart(PeekOld), ToLens(..),
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), view')
import Language.Haskell.TH.Path.Decs.PathStart (makePeekCon)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Decs.PathTypeDecs (fieldPathType)
import Language.Haskell.TH.Path.Graph (testIsPath, TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode, finishConcs)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, TypeGraphVertex(bestType))

pathDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
pathDecs utype v =
    pathKeys v >>= Set.mapM_ (pathDecs' utype v)

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
             TypeQ -> TGVSimple -> TGVSimple -> m ()
pathDecs' utype v gkey = do
  ptyp <- pathType (asTypeQ gkey) v
  x' <- runQ (newName "_s")
  g <- runQ (newName "_g")
  (pcs, dcs, pkcs, ppcs, pvcs, pccs) <-
      partitionClauses <$>
      case v == gkey of
        True -> pure ([PathClause $ do f <- newName "f"
                                       r0 <- newName "r0"
                                       clause [conP 'Proxy [], wildP, wildP, varP f, varP r0] (normalB [| $(varE f) idPath $(varE r0) |]) []] ++
                      peekAccessors utype v gkey)
        False -> execWriterT (doNode (hasPathControl utype v gkey g x') v)
  when (not (null pcs))
       (tells [instanceD (pure []) [t|PathsOld $utype $(pure (bestType v)) $(pure (bestType gkey))|]
                 [ tySynInstD ''PathOld (tySynEqn [utype, pure (bestType v), pure (bestType gkey)] (pure ptyp))
                 , funD 'pathsOld pcs
                 , funD 'peekOld
                            (case pkcs of
                                 (_ : _) -> pkcs
                                 _ | v == gkey ->
                                       [do p <- newName "p"
                                           x <- newName "x"
                                           clause [varP p, varP x] (normalB [| peekConsOld $(varE p) (Just $(varE x)) :: PeekOld $utype $(asTypeQ v) |]) []]
                                 [] -> [clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "no clauses" :: PeekOld $utype $(asTypeQ v) |]) []])
                 , funD 'peekPathOld ppcs
                 , funD 'peekValueOld pvcs
                 , funD 'peekConsOld pccs ]])
  when (not (null dcs))
       (tells [instanceD (pure []) [t|Describe $(asTypeQ v) $(asTypeQ gkey)|]
                [ funD 'describe' dcs ]])

hasPathControl :: (TypeGraphM m, MonadWriter [ClauseType] m) => TypeQ ->  TGVSimple -> TGVSimple -> Name -> Name -> Control m (Type, ExpQ) () ()
hasPathControl utype v gkey g x' =
    let control = hasPathControl utype v gkey g x' in
    Control { -- Why doesn't this ever get called?
              _doSimple = tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doSimple" :: PeekOld $utype $(asTypeQ v) |]) []]
            , _doSelf = tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doSelf" :: PeekOld $utype $(asTypeQ v) |]) []]
            , _doView =
                \w -> do
                  let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                  tell [ PeekClause $ do
                           p <- newName "p"
                           x <- newName "x"
                           clause [asP p (conP (asName pcname) [wildP]), varP x]
                                  (normalB [| peekConsOld $(varE p) (Just (view' (toLens $(varE p)) $(varE x))) :: PeekOld $utype $(asTypeQ v) |])
                                  []
{-
                       , PeekClause $ do
                           p <- newName "p"
                           x <- newName "x"
                           clause [varP p, varP x] (normalB [| peekCons $(varE p) (Just $(varE x)) :: Peek $utype $(asTypeQ v) |]) []
-}
{-
                           clause [asP p (conP (asName (bestPathTypeName v)) []), varP x]
                                  (normalB [| peekCons $(varE p) (Just (view' (toLens $(varE p)) $(varE x))) :: Peek $utype $(asTypeQ v) |])
                                  []
-}
                       ]
                  alt <- _doConcs control wildP
                             [(asType w, [|map (\a' -> ($(asConQ pcname) :: PathOld $utype $(asTypeQ w) $(asTypeQ gkey) -> PathOld $utype $(asTypeQ v) $(asTypeQ gkey), a'))
                                               (toListOf (toLens ($(asConQ pcname) (idPath :: PathOld $utype $(asTypeQ w) $(asTypeQ w)))) $(varE x')) |])]
                  _doAlts control [alt]
            , _doOrder =
                \_i w -> do
                  -- tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doOrder" :: Peek $utype $(asTypeQ v) |]) []]
                  finishConcs control [(wildP, [(asType w, [| map (\(idx, val) -> (Path_At idx, val)) (toPairs $(varE x')) |])])]
            , _doMap =
                \_i w -> do
                  -- tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doMap" :: Peek $utype $(asTypeQ v) |]) []]
                  finishConcs control [(wildP, [(asType w, [| map (\(idx, val) -> (Path_Look idx, val)) (Map.toList $(varE x')) |])])]
            , _doList =
                \_e -> pure () -- tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doList" :: Peek $utype $(asTypeQ v) |]) []]
            , _doPair =
                \f s -> -- tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doPair" :: Peek $utype $(asTypeQ v) |]) []] >>
                        finishConcs control
                                   [(wildP, [(asType f, [| [(Path_First, fst $(varE x'))] |]),
                                             (asType s, [| [(Path_Second, snd $(varE x'))] |])])]
            , _doMaybe =
                \w -> do
                  -- tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doMaybe" :: Peek $utype $(asTypeQ v) |]) []]
                  finishConcs control [(wildP, [(asType w, [| case $(varE x') of Nothing -> []; Just a' -> [(Path_Just, a')]|])])]
            , _doEither =
                \l r -> do
                  -- tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doEither" :: Peek $utype $(asTypeQ v) |]) []]
                  let lconc = (asType l, [| case $(varE x') of Left a' -> [(Path_Left, a')]; Right _ -> []|])
                      rconc = (asType r, [| case $(varE x') of Left _ -> []; Right a' -> [(Path_Right, a')]|])
                  finishConcs control [(conP 'Left [wildP], [lconc]), (conP 'Right [wildP], [rconc])]
            , _doField =
                \fld typ ->
                    case fld of
                      (_tname, _cname, Right fname) ->
                          do (pcname, _) <- fieldPathType (error "unused name") fld typ
                             -- Is there a path from typ to g?
                             fkey <- tgvSimple' typ
                             -- fkey' <- tgv (Just fld) fkey
                             fkeys <- pathKeys fkey
                             when (Set.member gkey fkeys)
                                  (tell [PeekClause $ do
                                           p <- newName "p"
                                           x <- newName "x"
                                           clause [asP p (conP (asName pcname) [wildP]), varP x]
                                                  (normalB [| peekConsOld $(varE p) (Just (view' (toLens $(varE p)) $(varE x))) :: PeekOld $utype $(asTypeQ v) |]) []])
                             pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x')))] |])
                      (_tname, _cname, Left fpos) ->
                          tell [PeekClause $ clause [conP (asName (bestPathTypeName v)) [], wildP] (normalB [| undefined "doAnonField" :: PeekOld $utype $(asTypeQ v) |]) []] >>
                          pure (typ, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show fpos))),
                                               $(do p <- newName "p"
                                                    lamE (replicate (fpos-1) wildP ++ [varP p] ++ replicate (2-fpos) wildP) (varE p)) $(varE x'))] |])
            , _doConcs =
                \xpat concs -> do
                  let _thisPathType = [t|PathOld $utype $(asTypeQ v) $(asTypeQ gkey)|]
                  exps <- concat <$>
                          mapM (\(typ, asList) ->
                                    do isPath <- testIsPath typ gkey
                                       let _nextPathType = [t|PathOld $utype $(pure typ) $(asTypeQ gkey)|]
                                       case isPath of
                                         False -> pure []
                                         True -> pure [ [| List.concatMap
                                                             -- (\(p, a') -> (List.map p (paths (Proxy :: Proxy $utype) (a' :: $(pure typ)) $(varE g) :: [$_nextPathType])) :: [$_thisPathType])
                                                             (\(p, a') -> pathsOld (Proxy :: Proxy $utype) (a' :: $(pure typ)) $(varE g) (\npt r -> p npt : r) [])
                                                             ($asList :: [($_nextPathType -> $_thisPathType, $(pure typ))]) |] ])
                               concs
                  tell [PathClause $ do f <- newName "f"
                                        r0 <- newName "r0"
                                        clause [conP 'Proxy [], asP' x' xpat, varP g, varP f, varP r0] (normalB [|foldr $(varE f) $(varE r0) ($(mconcatQ exps) :: [$_thisPathType])|]) []]
            , _doSyn =
                \_tname _typ -> tell [PeekClause $ do
                                        p <- newName "p"
                                        x <- newName "x"
                                        clause [varP p, varP x]
                                               (normalB [| peekConsOld $(varE p) (Just (view' (toLens $(varE p)) $(varE x))) :: PeekOld $utype $(asTypeQ v) |]) []]
            , _doAlts =
                \_ -> do
                  keys <- pathKeys v
                  when (Set.member gkey keys) (tell $ peekAccessors utype v gkey)
            , _doSyns = \() _ -> pure ()
            }

peekAccessors :: TypeQ -> TGVSimple -> TGVSimple -> [ClauseType]
peekAccessors utype v gkey =
    [PeekPathClause $
       newName "_p" >>= \p ->
       clause [conP 'Proxy [], conP (asName (makePeekCon (ModelType (asName v)) (ModelType (asName gkey)))) [varP p, wildP]]
              (normalB [| $(varE p) :: PathOld $utype $(asTypeQ v) $(asTypeQ gkey)|])
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
                             $(varE p) $(varE x) :: PeekOld $utype $(asTypeQ v)|])
              []]
