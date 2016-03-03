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
import Data.Generics (Data, everywhere, mkT)
import Data.List as List (concatMap, map)
import Data.Map as Map (fromList, lookup, Map, toList)
import Data.Maybe (isJust)
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, bestTypeName, HasName(asName), makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), HasPaths(..), ToLens(..), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, pathKeys, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, TypeGraphVertex(bestType))

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
  s <- runQ (newName "s")
  g <- runQ (newName "g")
  poc <- execWriterT (pathsOfClauses v gkey s g)
  when (not (null poc))
       (tells [ instanceD (pure []) [t|HasPaths $(pure (bestType v)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType v), pure (bestType gkey)] (pure ptyp))
                , funD 'pathsOf poc
                ]])

-- | Build an expression whose value is a list of paths from type S to
-- type A
pathsOfClauses :: forall m alt conc. (TypeGraphM m, MonadWriter [ClauseQ] m, conc ~ (Type, ExpQ), alt ~ (PatQ, [conc])) =>
                  TGVSimple -- ^ the type whose clauses we are generating
               -> TGVSimple -- ^ the goal type key
               -> Name -- ^ s
               -> Name -- ^ g
               -> m ()
pathsOfClauses v gkey s g =
  do -- the corresponding path type - first type parameter of ToLens
     -- ptyp <- pathType (pure (bestType gkey)) v
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case asType v of
       _ | v == gkey -> tell [clause [wildP, wildP] (normalB [| [idPath] |]) []]
         | selfPath -> pure ()
         | simplePath -> pure ()
         | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                let tname = bestTypeName v
                let pcname = makePathCon (makePathType tname) "View"
                -- Get the value as transformed by the view lens
                doAlt id
                      (wildP,
                       [(viewtyp,
                         [|map (\a' -> ($(asConQ pcname) {-:: Path $(pure viewtyp) $(asTypeQ gkey) -> Path $(asTypeQ v) $(asTypeQ gkey)-}, a'))
                               (toListOf (toLens ($(asConQ pcname) (idPath :: Path $(asTypeQ viewtyp) $(asTypeQ viewtyp)))) $(varE s)) |])])
       typ -> doType typ []
    where
      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType (ConT tname) [_ityp, vtyp]
          | tname == ''Order =
              -- Return a path for each element of an order, assuming
              -- there is a path from the element type to the goal.
              doAlt id (wildP, doOrder s vtyp)

      doType (ConT tname) [_ktyp, vtyp]
          | tname == ''Map =
              doAlt id (wildP, doMap s vtyp)
      doType (TupleT 2) [ftyp, styp] =
          doAlt id (wildP, doPair s ftyp styp)
      doType (ConT tname) [etyp]
          | tname == ''Maybe =
              doAlt id (wildP, [(etyp, doMaybe s)])
      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do doAlt id (conP 'Left [wildP], [(ltyp, doLeft s)])
                 doAlt id (conP 'Right [wildP], [(rtyp, doRight s)])
      doType (ConT tname) tps = doName tname tps

      doType _ _ = error $ "pathsOfExpr - unexpected type" {-++ pprint (asTGVSimple v)-}

      doName :: Name -> [Type] -> m ()
      doName tname tps = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m ()
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo _ _ = pure ()
      doDec :: [Type] -> Dec -> m ()
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx tname binds cons _supers) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
            subst = substG bindings
        doCons subst tname cons
      doDec _ dec = error $ "Unexpected Dec: " ++ pprint dec

      doCons :: (Type -> Type) -> Name -> [Con] -> m ()
      doCons _subst _tname [] = error "No constructors"
      doCons subst _tname cons = Prelude.mapM_ (doCon subst) cons

      doCon :: (Type -> Type) -> Con -> m ()
      doCon subst (ForallC _binds _cx con) = doCon subst con -- Should probably do something here
      doCon subst (InfixC lhs cname rhs) = doAlt subst (infixP wildP cname wildP, doInfixC v s lhs rhs)
      doCon subst (NormalC cname binds) = doAlt subst (recP cname [], doNormalC v s binds)
      doCon subst (RecC cname vbinds) = doAlt subst (recP cname [], zip (map (view _3) vbinds) (map (doNamedField v s) vbinds))
      doAlt :: (Type -> Type) -> alt -> m ()
      doAlt subst (spat, pairs) =
          mapM (doConc subst) pairs >>= \exps -> tell [clause [asP s spat, varP g] (normalB (mconcatQ exps)) []]

      doConc :: (Type -> Type)
             -> conc
             -> m ExpQ
      doConc subst (atyp', asList) = do
        let atyp = subst atyp'
        isPath <- testIsPath atyp gkey
        case isPath of
          False -> pure [| [] |]
          True -> pure [| List.concatMap (\(p, a') -> (List.map p (pathsOf (a' :: $(pure atyp)) $(varE g) {-:: [Path $(pure atyp) $(asTypeQ gkey)]-})) {-:: [Path $(pure styp) $(asTypeQ gkey)]-})
                                         ($asList {-:: [(Path $(pure atyp) $(asTypeQ gkey) -> Path $(pure styp) $(asTypeQ gkey), $(pure atyp))]-}) |]

doOrder :: forall t. Name -> t -> [(t, ExpQ)]
doOrder s vtyp = [(vtyp, [| map (\(i, v) -> (Path_At i, v)) (toPairs $(varE s)) |])]

doMap :: forall t. Name -> t -> [(t, ExpQ)]
doMap s vtyp = [(vtyp, [| map (\(i, v) -> (Path_Look i, v)) (Map.toList $(varE s)) |])]

doPair :: forall t. Name -> t -> t -> [(t, ExpQ)]
doPair s ftyp styp = [(ftyp, [| [(Path_First, fst $(varE s))] |]),
                      (styp, [| [(Path_Second, snd $(varE s))] |])]

doMaybe :: Name -> ExpQ
doMaybe s = [| case $(varE s) of Nothing -> []; Just a' -> [(Path_Just, a')]|]

doLeft :: Name -> ExpQ
doLeft s = [| case $(varE s) of Left a' -> [(Path_Left, a')]; Right _ -> []|]

doRight :: Name -> ExpQ
doRight s = [| case $(varE s) of Left _ -> []; Right a' -> [(Path_Right, a')]|]

doNormalC :: forall a. HasName a => a -> Name -> [(Strict, Type)] -> [(Type, ExpQ)]
doNormalC v s binds =
    map (\((_, ftype), pos) ->
             (ftype, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show pos))),
                          ($(do p <- newName "p"
                                lamE (replicate (pos-1) wildP ++ [varP p] ++ replicate (length binds - pos) wildP) (varE p)) $(varE s)))] |]))
        (zip binds ([1..] :: [Int]))

doNamedField :: forall a. HasName a => a -> Name -> (Name, Strict, Type) -> ExpQ
doNamedField v s (fname, _, _) = [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE s)))] |]

doInfixC :: forall a. HasName a => a -> Name -> (Strict, Type) -> (Strict, Type) -> [(Type, ExpQ)]
doInfixC v s lhs rhs =
    map (\((_, ftype), pos) ->
             (ftype, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show pos))),
                          ($(do p <- newName "p"
                                lamE (replicate (pos-1) wildP ++ [varP p] ++ replicate (2-pos) wildP) (varE p)) $(varE s)))] |]))
        (zip [lhs, rhs] ([1..] :: [Int]))

substG :: Data a => Map Name Type -> a -> a
substG bindings typ = everywhere (mkT (subst1 bindings)) typ

subst1 :: Map Name Type -> Type -> Type
subst1 bindings t@(VarT name) = maybe t id (Map.lookup name bindings)
subst1 _ t = t

-- | See if there is a path from typ to gkey.  We need to avoid
-- building expressions for non-existant paths because they will cause
-- "no Path instance" errors.
testIsPath :: TypeGraphM m => Type -> TGVSimple -> m Bool
testIsPath typ gkey = do
  mkey <- tgvSimple' typ
  case mkey of
    Nothing -> pure False
    Just v -> (maybe False (Set.member gkey) . Map.lookup v) <$> allPathKeys
