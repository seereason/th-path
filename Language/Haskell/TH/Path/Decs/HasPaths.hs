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
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Generics (everywhere, mkT)
import Data.List as List (concatMap, map)
import Data.Map as Map (fromList, lookup, Map, toList)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, bestTypeName, HasName(asName), HasType, HasTypeQ, makePathCon, makePathType, ModelType(ModelType))
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), HasPaths(..), ToLens(..), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, HasTGVSimple, pathKeys, tgvSimple', TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, TypeGraphVertex(bestType))

pathDecs :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
            TGVSimple -> m ()
pathDecs v =
    pathKeys v >>= Set.mapM_ (pathDecs' v)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
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
pathsOfExprs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
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
                vIsPath <- testIsPath viewtyp gkey
                let tname = bestTypeName key
                let pcname = makePathCon (makePathType tname) "View"
                -- Get the value as transformed by the view lens
                if vIsPath
                then runQ [| concatMap (\s' ->  List.map $(asConQ pcname) (pathsOf s' $a :: [Path $(pure viewtyp) $(asTypeQ gkey)]))
                                       (toListOf (toLens ($(asConQ pcname) idPath :: Path $(asTypeQ key) $(pure viewtyp))) $s :: [$(pure viewtyp)]) |]
                else runQ [| [] |]
       typ -> doType typ []
    where
      doType (ConT tname) [_ktyp, vtyp]
          | tname == ''Map =
               do vIsPath <- testIsPath vtyp gkey
                  if vIsPath
                  then (doClause vtyp gkey s a
                                         [|Path_Look . fst|]
                                         [|snd|]
                                         [|Map.toList|])
                  else runQ [| [] |]
      doType (ConT tname) [ityp, vtyp]
          | tname == ''Order =
               -- Return a path for each element of an order, assuming
               -- there is a path from the element type to the goal.
               do vIsPath <- testIsPath vtyp gkey
                  if vIsPath
                  then doClause vtyp gkey s a
                                         [|Path_At . fst|]
                                         [|snd|]
                                         [|toPairs :: Order $(pure ityp) $(pure vtyp) -> [($(pure ityp), $(pure vtyp))]|]
                  else runQ [| [] |]

      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do lIsPath <- testIsPath ltyp gkey
                 rIsPath <- testIsPath rtyp gkey
                 lexp <- if lIsPath then doClause ltyp gkey s a [|\_ -> Path_Left|] [|id|] [|\s -> case s of Left a -> [a]; Right _ -> []|] else runQ [| [] |]
                 rexp <- if rIsPath then doClause rtyp gkey s a [|\_ -> Path_Right|] [|id|] [|\s -> case s of Left _ -> []; Right a -> [a]|] else runQ [| [] |]
                 runQ [| $(pure lexp) <> $(pure rexp) |]
      doType (ConT tname) [etyp]
          | tname == ''Maybe =
              doClause etyp gkey s a [|\_ -> Path_Just|] [|id|] [|\s -> case s of Nothing -> []; Just a -> [a]|]
      doType (TupleT 2) [ftyp, styp] =
          do fIsPath <- testIsPath ftyp gkey
             sIsPath <- testIsPath styp gkey
             fexp <- if fIsPath then doClause ftyp gkey s a [|\_ -> Path_First|] [|fst|] [|(: [])|] else runQ [| [] |]
             sexp <- if sIsPath then doClause styp gkey s a [|\_ -> Path_Second|] [|snd|] [|(: [])|] else runQ [| [] |]
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
      doCon bindings (NormalC cname []) = runQ $ match (conP cname []) (normalB [| [] |]) []
      doCon bindings (NormalC cname binds) = do
        let subst t@(VarT name) = maybe t id (Map.lookup name bindings)
            subst t = t
        ps <- runQ $ mapM (\(_, n) -> newName ("p" ++ show n)) (zip binds ([1..] :: [Int]))
        fs <- mapM (\((_, ftype), pos, p) ->
                        do fIsPath <- testIsPath (everywhere (mkT subst) ftype) gkey
                           case fIsPath of
                             False -> pure []
                             True -> pure [ [| map $(asConQ (makePathCon (makePathType (ModelType (asName key))) (show pos)))
                                                   (pathsOf $(varE p) $a) |] ])
                   (zip3 binds ([1..] :: [Int]) ps)
        runQ $ match (conP cname (map varP ps)) (normalB [|mconcat $(listE (concat fs))|] ) []
      doCon bindings (RecC cname vbinds) = do
        let subst t@(VarT name) = maybe t id (Map.lookup name bindings)
            subst t = t
        fs <- mapM (\(fname, _, ftype) ->
                        do fIsPath <- testIsPath (everywhere (mkT subst) ftype) gkey
                           case fIsPath of
                             False -> pure []
                             True -> pure [ [| map $(asConQ (makePathCon (makePathType (ModelType (asName key))) (nameBase fname)))
                                                   (pathsOf ($(varE fname) $s) $a) |] ])
                   vbinds
        runQ $ match (recP cname []) (normalB [|mconcat $(listE (concat fs))|]) []

instance HasName TyVarBndr where
    asName (PlainTV x) = x
    asName (KindedTV x _) = x

doClause :: forall m s. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, HasType s, Ord s, HasTGVSimple s, TypeGraphVertex s, HasTypeQ s, HasName s) =>
            Type
         -> s
         -> ExpQ -- ^ S
         -> ExpQ -- ^ Proxy A
         -> ExpQ -- ^ Build a path
         -> ExpQ -- ^ Build a value
         -> ExpQ -- ^ The list of values we will turn into paths
         -> m Exp
doClause vtyp gkey s a toPath toVal asList = do
  runQ [| List.concatMap (\pv -> List.map ($toPath pv) (pathsOf ($toVal pv) $a {-:: [Path $(pure vtyp) $(asTypeQ gkey)]-})) ($asList $s) |]

-- | See if there is a path from typ to gkey.  We need to avoid
-- building expressions for non-existant paths because they will cause
-- "no Path instance" errors.
testIsPath :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, s ~ TGVSimple) => Type -> s -> m Bool
testIsPath typ gkey = do
  mkey <- tgvSimple' typ
  case mkey of
    Nothing -> pure False
    Just key -> (maybe False (Set.member gkey) . Map.lookup key) <$> allPathKeys
