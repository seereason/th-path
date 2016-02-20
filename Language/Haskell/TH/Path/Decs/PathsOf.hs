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
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.PathsOf (pathDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.State (evalStateT)
import Control.Monad.States (getPoly, modifyPoly, MonadStates)
import Control.Monad.Writer (MonadWriter, tell)
import Data.List as List (concatMap, map)
import Data.Map as Map (lookup, Map, toList)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Set.Extra as Set (insert, mapM_, member, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asTypeQ, bestTypeName, makePathCon, makePathType, ModelType(ModelType))
import Language.Haskell.TH.Path.Core (IsPathEnd(idPath), IsPath(..), ToLens(..), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Expand (unE, expandType)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, pathKeys, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, TGVSimple, TypeGraphVertex(bestType))

pathDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathDecs v =
    pathKeys v >>= Set.mapM_ (pathDecs' v)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
            TGVSimple -> TGVSimple -> m ()
pathDecs' key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  s <- runQ (newName "s")
  a <- runQ (newName "a")
  poe <- evalStateT (pathsOfExprs key gkey (varE s) (varE a)) (mempty :: Set TGVSimple)
  when (poe /= ListE []) $
       (runQ $ sequence
            [ instanceD (pure []) [t|IsPath $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'pathsOf [clause [varP s, varP a] (normalB (pure poe)) []]
                ]]) >>= tell

-- | Build an expression whose value is a list of paths from type S to
-- type A
pathsOfExprs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates (Set TGVSimple) m) =>
                TGVSimple -- ^ the type whose clauses we are generating
             -> TGVSimple -- ^ the goal type key
             -> ExpQ -- ^ S
             -> ExpQ -- ^ Proxy A
             -> m Exp
pathsOfExprs key gkey s a =
  do -- the corresponding path type - first type parameter of ToLens
     -- ptyp <- pathType (pure (bestType gkey)) key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [view (etype . unE) key]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [view (etype . unE) key]
     viewType <- viewInstanceType (view etype key)
     case view (etype . unE) key of
       _ | key == gkey -> runQ [| [idPath] |]
         | selfPath -> runQ [| [] |]
         | simplePath -> runQ [| [] |]
         | isJust viewType ->
             do let Just vtyp = viewType
                vIsPath <- testIsPath vtyp gkey
                let tname = bestTypeName key
                let pcname = makePathCon (makePathType tname) "View"
                -- Get the value as transformed by the view lens
                if vIsPath
                then runQ [| let p = $(asConQ pcname) idPath :: Path $(asTypeQ key) $(pure vtyp)
                                 [s'] = toListOf (toLens p) $s :: [$(pure vtyp)] in
                             List.map $(asConQ pcname) (pathsOf s' $a ::
                                                            [Path $(pure vtyp) $(pure (view (etype . unE) gkey))]) |]
                else runQ [| [] |]
       ConT tname ->
           doName tname
       AppT (AppT mtyp ityp) vtyp
           | mtyp == ConT ''Order ->
               -- Return a path for each element of an order, assuming
               -- there is a path from the element type to the goal.
               do vIsPath <- testIsPath vtyp gkey
                  if vIsPath
                  then doClause vtyp gkey s a
                                         [|Path_At . fst|]
                                         [|snd|]
                                         [|toPairs :: Order $(pure ityp) $(pure vtyp) -> [($(pure ityp), $(pure vtyp))]|]
                  else runQ [| [] |]
       AppT ListT _etyp -> runQ [| [] |]
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do vIsPath <- testIsPath vtyp gkey
                  if vIsPath
                  then (doClause vtyp gkey s a
                                         [|Path_Look . fst|]
                                         [|snd|]
                                         [|Map.toList|])
                  else runQ [| [] |]
       AppT (AppT (TupleT 2) ftyp) styp ->
           do fIsPath <- testIsPath ftyp gkey
              sIsPath <- testIsPath styp gkey
              fexp <- if fIsPath then doClause ftyp gkey s a [|\_ -> Path_First|] [|fst|] [|(: [])|] else runQ [| [] |]
              sexp <- if sIsPath then doClause styp gkey s a [|\_ -> Path_Second|] [|snd|] [|(: [])|] else runQ [| [] |]
              runQ [| $(pure fexp) <> $(pure sexp) |]
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               doClause etyp gkey s a [|\_ -> Path_Just|] [|id|] [|maybe [] (: [])|]
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do -- Are there paths from the left type to a?  This is
                  -- the test we use in pathInstanceDecs, but using it
                  -- here is kind of a hack.
                  lIsPath <- testIsPath ltyp gkey
                  rIsPath <- testIsPath rtyp gkey
                  lexp <- if lIsPath then doClause ltyp gkey s a [|\_ -> Path_Left|] [|id|] [|either (: []) (const [])|] else runQ [| [] |]
                  rexp <- if rIsPath then doClause rtyp gkey s a [|\_ -> Path_Right|] [|id|] [|either (const []) (: [])|] else runQ [| [] |]
                  runQ [| $(pure lexp) <> $(pure rexp) |]
       _ -> runQ [|error $ "pathsOfExpr - unexpected type: " ++ pprint key|]
    where
      doName :: Name -> m Exp
      doName tname = do
        ns <- getPoly
        case Set.member key ns of
          True -> runQ [| [] |]
          False -> modifyPoly (Set.insert key) >> qReify tname >>= doInfo
      doInfo :: Info -> m Exp
      doInfo (TyConI dec) = doDec dec
      doInfo _ = runQ [| [] |]
      doDec :: Dec -> m Exp
      doDec (NewtypeD cx tname binds con supers) = doDec (DataD cx tname binds [con] supers)
      doDec (DataD _cx _tname _binds cons _supers) = do
        matches <- mapM doCon cons
        runQ (caseE s (map pure matches))
      doDec dec = error $ "Unexpected Dec: " ++ pprint dec
      doCon :: Con -> m Match
      doCon (ForallC _binds _cx con) = doCon con -- Should probably do something here
      doCon (InfixC lhs cname rhs) =
           runQ $ match (infixP wildP cname wildP) (normalB [| [] |]) [] -- Implement
      doCon (NormalC cname binds) = do
        fIsPath <- mapM (\(_, ftype) -> testIsPath ftype gkey) binds
        let ns = map snd (zip binds ([1..] :: [Int]))
        ps <- runQ $ mapM (\n -> newName ("p" ++ show n)) ns
        let Just tname = bestName key
        let doField (_, False, _, _) = []
            doField (_, True, n, p) =
                [ [| map $(asConQ (makePathCon (makePathType (ModelType tname)) (show n)))
                         (pathsOf $(varE p) $a) |] ]
        runQ $ match (conP cname (map varP ps)) (normalB (listE (concatMap doField (zip4 binds fIsPath ns ps)))) []
      doCon (RecC cname vbinds) = do
        fIsPath <- mapM (\(_, _, ftype) -> testIsPath ftype gkey) vbinds
        let ns = map snd (zip vbinds ([1..] :: [Int]))
        let Just tname = bestName key
        let doField (_, False, _) = []
            doField ((fname, _, _), _, _) =
                [ [| map $(asConQ (makePathCon (makePathType (ModelType tname)) (nameBase fname)))
                         (pathsOf ($(varE fname) $s) $a) |] ]
        runQ $ match (recP cname []) (normalB [|mconcat $(listE (concatMap doField (zip3 vbinds fIsPath ns)))|]) []

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 l1 l2 l3 l4 = map (\((a, b), (c, d)) -> (a, b, c, d)) (zip (zip l1 l2) (zip l3 l4))

doClause :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
            Type
         -> TGVSimple
         -> ExpQ -- ^ S
         -> ExpQ -- ^ Proxy A
         -> ExpQ -- ^ Build a path
         -> ExpQ -- ^ Build a value
         -> ExpQ -- ^ The list of values we will turn into paths
         -> m Exp
doClause vtyp gkey s a toPath toVal asList = do
  runQ [| List.concatMap (\pv -> List.map ($toPath pv) (pathsOf ($toVal pv) $a :: [Path $(pure vtyp) $(asTypeQ gkey)])) ($asList $s) |]

-- | See if there is a path from typ to gkey.  We need to avoid
-- building expressions for non-existant paths because they will cause
-- "no Path instance" errors.
testIsPath :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Type -> TGVSimple -> m Bool
testIsPath typ gkey = do
  key <- expandType typ >>= typeVertex
  (maybe False (Set.member gkey) . Map.lookup key) <$> allPathKeys
