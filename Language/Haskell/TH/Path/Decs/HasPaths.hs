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
import Control.Monad.Writer (MonadWriter, tell)
import Data.Generics (everywhere, mkT)
import Data.List as List (concatMap, map)
import Data.Map as Map (fromList, lookup, Map, toList)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, bestTypeName, HasName(asName), makePathCon, makePathType, ModelType(ModelType))
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), HasPaths(..), ToLens(..), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, pathKeys, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, TypeGraphVertex(bestType))

pathDecs :: (TypeGraphM m, MonadWriter [Dec] m) =>
            TGVSimple -> m ()
pathDecs v =
    pathKeys v >>= Set.mapM_ (pathDecs' v)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: (TypeGraphM m, MonadWriter [Dec] m) =>
             TGVSimple -> TGVSimple -> m ()
pathDecs' key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  s <- runQ (newName "s")
  a <- runQ (newName "a")
  poe <- pathsOfExprs key gkey (varE s) (varE a)
  when (poe /= ListE []) $
       (runQ $ sequence
            [ instanceD (pure []) [t|HasPaths $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'pathsOf [clause [varP s, varP a] (normalB (pure poe)) []]
                ]]) >>= tell

-- | Build an expression whose value is a list of paths from type S to
-- type A
pathsOfExprs :: forall m. TypeGraphM m =>
                TGVSimple -- ^ the type whose clauses we are generating
             -> TGVSimple -- ^ the goal type key
             -> ExpQ -- ^ S
             -> ExpQ -- ^ Proxy A
             -> m Exp
pathsOfExprs key gkey s a =
  do -- the corresponding path type - first type parameter of ToLens
     -- ptyp <- pathType (pure (bestType gkey)) key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType key]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType key]
     viewTypeMaybe <- viewInstanceType (asType key)
     case asType key of
       _ | key == gkey -> runQ [| [idPath] |]
         | selfPath -> runQ [| [] |]
         | simplePath -> runQ [| [] |]
         | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                let tname = bestTypeName key
                let pcname = makePathCon (makePathType tname) "View"
                -- Get the value as transformed by the view lens
                doClause (asType key) viewtyp gkey s a [|\s' ->  map (\a' -> ($(asConQ pcname) {-:: Path $(pure viewtyp) $(asTypeQ gkey) -> Path $(asTypeQ key) $(asTypeQ gkey)-}, a'))
                                                                     (toListOf (toLens ($(asConQ pcname) (idPath :: Path $(asTypeQ viewtyp) $(asTypeQ viewtyp)))) s') |]
       typ -> doType typ []
    where
      doType (ConT tname) [_ktyp, vtyp]
          | tname == ''Map =
              doClause (asType key) vtyp gkey s a [|map (\(i, v) -> (Path_Look i, v)) . Map.toList|]
      doType (ConT tname) [_ityp, vtyp]
          | tname == ''Order =
              -- Return a path for each element of an order, assuming
              -- there is a path from the element type to the goal.
              doClause (asType key) vtyp gkey s a [|map (\(i, v) -> (Path_At i, v)) . toPairs|]

      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do lexp <- doClause (asType key) ltyp gkey s a [|\s' -> case s' of Left a' -> [(Path_Left, a')]; Right _ -> []|]
                 rexp <- doClause (asType key) rtyp gkey s a [|\s' -> case s' of Left _ -> []; Right a' -> [(Path_Right, a')]|]
                 runQ [| $(pure lexp) <> $(pure rexp) |]
      doType (ConT tname) [etyp]
          | tname == ''Maybe =
              doClause (asType key) etyp gkey s a [|\s' -> case s' of Nothing -> []; Just a' -> [(Path_Just, a')]|]
      doType (TupleT 2) [ftyp, styp] =
          do fexp <- doClause (asType key) ftyp gkey s a [|\x -> [(Path_First, fst x)]|]
             sexp <- doClause (asType key) styp gkey s a [|\x -> [(Path_Second, snd x)]|]
             runQ [| $(pure fexp) <> $(pure sexp) |]
      doType (ConT tname) tps = doName tname tps

      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType _ _ = runQ [|error $ "pathsOfExpr - unexpected type" {-++ pprint (asTGVSimple key)-}|]

      doName :: Name -> [Type] -> m Exp
      doName tname tps = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m Exp
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo _ _ = runQ [| [] |]
      doDec :: [Type] -> Dec -> m Exp
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx _tname binds cons _supers) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
        matches <- mapM (doCon bindings) cons
        runQ (caseE s (map pure matches))
      doDec _bindings dec = error $ "Unexpected Dec: " ++ pprint dec
      doCon :: Map Name Type -> Con -> m Match
      doCon bindings (ForallC _binds _cx con) = doCon bindings con -- Should probably do something here
      doCon _bindings (InfixC _lhs cname _rhs) =
           runQ $ match (infixP wildP cname wildP) (normalB [| [] |]) [] -- Implement
      doCon _bindings (NormalC cname []) = runQ $ match (conP cname []) (normalB [| [] |]) []
      doCon bindings (NormalC cname binds) = do
        let subst t@(VarT name) = maybe t id (Map.lookup name bindings)
            subst t = t
        ps <- runQ $ mapM (\(_, n) -> newName ("p" ++ show n)) (zip binds ([1..] :: [Int]))
        fs <- mapM (\((_, ftype), pos, _p) ->
                        do let ftype' = (everywhere (mkT subst) ftype)
                           cl <- doClause (asType key) ftype' gkey s a [|\x -> [($(asConQ (makePathCon (makePathType (ModelType (asName key))) (show pos))),
                                                                                 ($(do p <- newName "p"
                                                                                       lamE (replicate (pos-1) wildP ++ [varP p] ++ replicate (length binds - pos) wildP) (varE p)) x))]|]
                           pure [pure cl])
                   (zip3 binds ([1..] :: [Int]) ps)
        runQ $ match (conP cname (map varP ps)) (normalB [|mconcat $(listE (concat fs))|] ) []
      doCon bindings (RecC cname vbinds) = do
        let subst t@(VarT name) = maybe t id (Map.lookup name bindings)
            subst t = t
        fs <- mapM (\(fname, _, ftype) ->
                        do let ftype' = everywhere (mkT subst) ftype
                           cl <- doClause (asType key) ftype' gkey s a [|\x -> [($(asConQ (makePathCon (makePathType (ModelType (asName key))) (nameBase fname))), ($(varE fname) x))]|]
                           pure [pure cl])
                   vbinds
        runQ $ match (recP cname []) (normalB [|mconcat $(listE (concat fs))|]) []

doClause :: forall m. TypeGraphM m =>
            Type
         -> Type
         -> TGVSimple
         -> ExpQ -- ^ S
         -> ExpQ -- ^ Proxy A
         -> ExpQ -- ^ The list of (path, value) pairs we will turn into paths
         -> m Exp
doClause _styp atyp gkey s a asList = do
  isPath <- testIsPath atyp gkey
  case isPath of
    False -> runQ [| [] |]
    True -> runQ [| List.concatMap (\(p, a') -> (List.map p (pathsOf (a' :: $(pure atyp)) $a {-:: [Path $(pure atyp) $(asTypeQ gkey)]-})) {-:: [Path $(pure styp) $(asTypeQ gkey)]-})
                                   (($asList $s) {-:: [(Path $(pure atyp) $(asTypeQ gkey) -> Path $(pure styp) $(asTypeQ gkey), $(pure atyp))]-}) |]

-- | See if there is a path from typ to gkey.  We need to avoid
-- building expressions for non-existant paths because they will cause
-- "no Path instance" errors.
testIsPath :: (TypeGraphM m, s ~ TGVSimple) => Type -> s -> m Bool
testIsPath typ gkey = do
  mkey <- tgvSimple' typ
  case mkey of
    Nothing -> pure False
    Just key -> (maybe False (Set.member gkey) . Map.lookup key) <$> allPathKeys
