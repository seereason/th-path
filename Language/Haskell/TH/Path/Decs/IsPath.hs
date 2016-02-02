-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

{-# OPTIONS -Wall -fno-warn-unused-imports #-}
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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.IsPath (doIsPathNode) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad as List ( mapM )
import Control.Monad.Reader (runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Control.Monad.States (MonadStates(getPoly, putPoly), modifyPoly)
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (MonadWriter, execWriterT, tell, WriterT)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable (mapM_)
import Data.Foldable as Foldable
import Data.List as List (concatMap, intercalate, isPrefixOf, map)
import Data.Map as Map (Map, toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Set as Set (delete, minView)
import Data.Set.Extra as Set (insert, map, member, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IsPathType(idPath), IsPathNode(PVType, pvNodes), IsPath(..), Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (bestPathTypeName, bestTypeName, clauses, fieldLensNameOld, pathConNameOfField, pvName, forestMap)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType, viewLens)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify), VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), unE, ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, allPathStarts, goalReachableSimple, reachableFromSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, field, TGV, TGVSimple, syns, TypeGraphVertex(bestType), typeNames, vsimple)

doIsPathNode :: forall m. (MonadWriter [Dec] m, ContextM m, MonadReaders TypeInfo m, MonadReaders TypeGraph m) =>
                TGVSimple -> m ()
doIsPathNode v =
    let typ = view (etype . unE) v in
    case bestTypeName v of
      Just tname -> do
        (pnc :: [ClauseQ]) <- {-evalStateT-} (pvNodeClauses v) {-mempty-}
        -- (ptc :: [ClauseQ]) <- {-evalStateT-} (pvTreeClauses v) {-mempty-}
        runQ (instanceD (cxt []) (appT (conT ''IsPathNode) (pure typ))
                [tySynInstD ''PVType (tySynEqn [pure typ] (conT (mkName ("PV_" ++ nameBase tname)))),
{-
                 funD 'pvTree (case ptc of
                                 [] -> [clause [wildP] (normalB [|error "no pvTree clauses"|]) []]
                                 _ -> ptc),
-}
                 funD 'pvNodes (case pnc of
                                 [] -> [clause [wildP] (normalB [| [] {- error $(litE (stringL ("no pvNode clauses for " ++ pprint v)))-} |]) []]
                                 _ -> pnc)]) >>= tell . (: [])
      Nothing -> return ()

subforest = []

-- | Clauses of the pvNodes function.  Like pathsOf, but returns a
-- PVType instead of an IsPath.
--
--    f x = exp :: [PVType]
--
-- where PVType values are of the form
--
--    PV_<s>_<a> (PathType <s> <a>) <a>
pvNodeClauses :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
                 TGVSimple -> {-StateT (Set Name)-} m [ClauseQ]
pvNodeClauses v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
     viewType <- viewInstanceType (view etype v)
     case view (etype . unE) v of
       _ | selfPath -> return []
         | simplePath -> return []
         | isJust viewType ->
             doPVNodesOfView v (fromJust viewType)
       ConT tname -> doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               doPVNodesOfOrder v vtyp
       AppT ListT _etyp -> error "list" {- return [clause [wildP] (normalB [|error "list"|]) []]-}
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               doPVNodesOfMap v vtyp
       AppT (AppT (TupleT 2) ftyp) styp ->
           mappend <$> doPVNodeOf v ftyp 'Path_First <*> doPVNodeOf v styp 'Path_Second
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               doPVNodeOf v etyp 'Path_Just
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               mappend <$> doPVNodeOf v ltyp 'Path_Left <*> doPVNodeOf v rtyp 'Path_Right
       _ -> return []
    where
      doName :: Name -> m [ClauseQ]
      doName tname = qReify tname >>= doInfo
      doInfo :: Info -> m [ClauseQ]
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return []
      doDec :: Dec -> m [ClauseQ]
      doDec (NewtypeD cx tname binds con supers) = runQ (newName "x") >>= doCons tname [con]
      doDec (DataD cx tname binds cons supers) = runQ (newName "x") >>= doCons tname cons
      -- concat <$> mapM doCon cons
      doCons :: Name -> [Con] -> Name -> m [ClauseQ]
      doCons tname [] x = error "No constructors"
      doCons tname [ForallC _ _ con] x = doCons tname [con] x
      doCons tname [RecC cname vsts] x = do
        flds <- mapM (doField tname x) vsts
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [NormalC cname sts] x = do
        flds <- mapM (doField' tname x) sts
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [InfixC lhs cname rhs] x = do
        flds <- mapM (doField' tname x) [lhs, rhs]
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname cons x = concat <$> mapM (doCon tname x) cons
      -- If we have multiple constructors, only generate values for
      -- the one that matches
      doCon :: Name -> Name -> Con -> m [ClauseQ]
      doCon tname x (ForallC _ _ con) = doCon tname x con
      -- doCon ''Markup _ con@(Markdown {}) = runQ [d|_f (Markdown {}) = doFields ''Markup con
      --                                              _f (Html {}) = doFields ''Markup con
      --                                              _f _ = [] |]
      -- doCon ''Markup _ (Html {htmlText :: Text}) = ...
      doCon tname x (RecC cname vsts) = doMatchingFields tname x cname vsts
      doCon tname x (NormalC cname sts) = doMatchingFields' tname x cname sts
      doCon tname x (InfixC lhs cname rhs) = doMatchingFields' tname x cname [lhs, rhs]

      doMatchingFields :: Name -> Name -> Name -> [(Name, Strict, Type)] -> m [ClauseQ]
      doMatchingFields tname x cname vsts = do
        flds <- mapM (doField tname x) vsts
        return [clause [asP x (recP cname [])] (normalB (listE (List.map pure flds))) []]
      doMatchingFields' :: Name -> Name -> Name -> [(Strict, Type)] -> m [ClauseQ]
      doMatchingFields' tname x cname sts = do
        flds <- mapM (doField' tname x) sts
        return [clause [asP x (recP cname [])] (normalB (listE (List.map pure flds))) []]

      doField :: Name -> Name -> (Name, Strict, Type) -> m Exp
      doField tname x (fname, _, ftype) = do
        f <- expandType ftype >>= typeVertex
        f' <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
        let p = pathConNameOfField f'
        case p of
          Just n -> doPVNodesOfField x v f' n
          _ -> runQ [|error $(litE (stringL ("doField " ++ show p)))|]
{-
        case (bestName f, p) of
          (Nothing, _) -> runQ [|error $(litE (stringL ("doField " ++ pprint fname)))|]
          (Just _, Just n) -> doPVNodesOfField x v f' n
          -- (Just _, Just n) -> runQ [| Node ($(conE (pvName v f)) ($(conE n) idPath) ($(varE fname) $(varE x))) subforest |]
          -- runQ [|conE (mkName $(litE (stringL ("Path_" ++ nameBase tname ++ "_" ++ nameBase fname)))) idPath|]
-}
      doField' :: Name -> Name -> (Strict, Type) -> m Exp
      doField' tname x (_, ftype) = do
        f <- expandType ftype >>= typeVertex
        -- Anonymous fields are not supported.
        runQ [|error $(litE (stringL ("doField' " ++ pprint ftype)))|]

-- | Build a value of type PV_AbbrevPair -> PV_AbbrevPairs
shim :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> TGVSimple -> Name -> ExpQ -> m Exp
shim w v pvname pcname =
    do -- p <- runQ $ newName "p"
       -- x <- runQ $ newName "x"
       -- let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
       --     wname = fromMaybe (error $ "No name for " ++ pprint w) (bestTypeName w)
       -- case (nameBase wname, nameBase tname) of
       --   _ ->
       doNode v w
         -- _ -> [|error $(litE (stringL ("Cannot convert PV_" ++ nameBase wname ++ " -> PV_" ++ nameBase tname ++ " using " ++ nameBase pvname ++ " and " ++ nameBase pcname))) |]
    where
      doNode :: TGVSimple -> TGVSimple -> m Exp
      doNode v w = do
        p <- runQ $ newName "p"
        x <- runQ $ newName "x"
        gs <- pathKeys w
        matches <- concat <$> (mapM (doPair p x v w) (Foldable.toList gs))
        runQ [| \pv -> $(caseE [|pv|] (List.map pure matches)) |]
      doPair :: Name -> Name -> TGVSimple -> TGVSimple -> TGVSimple -> m [Match]
      doPair p x v w g = do
        pathlift <- shim2 pcname v w g
        case (bestName v, bestName w, bestName g) of
          (Just vn, Just wn, Just gn) ->
              sequence
                [runQ $ match (conP (mkName ("PV_" ++ nameBase wn ++ "_" ++ nameBase gn)) [varP p, varP x])
                              (normalB [|$(conE (mkName ("PV_" ++ nameBase vn ++ "_" ++ nameBase gn)))
                                         ($(pure pathlift) $(varE p)) $(varE x)|])
                              []]
          _ -> pure []
{-
          let Just (vp, _) = bestPathTypeName v in
          case (bestName v, bestName w) of
            (Just vn, wn) ->
-}

shim2 :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => ExpQ -> TGVSimple -> TGVSimple -> TGVSimple -> m Exp
shim2 pcname v w g = do
  case (bestName v, bestName w) of
    (Just vn, Just wn) ->
        runQ [|$pcname :: $(conT (mkName ("Path_" ++ nameBase wn))) $(pure (view (etype . unE) g)) ->
                          $(conT (mkName ("Path_" ++ nameBase vn))) $(pure (view (etype . unE) g))|]
    _ -> runQ [|error "shim2"|]

doPVNodesOfField :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Name -> TGVSimple -> TGV -> Name -> m Exp
doPVNodesOfField x v f n =
  do let wtyp = view (vsimple . etype . unE) f
     w <- expandType wtyp >>= typeVertex
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wtyp = bestType w
         wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
         ptname = mkName ("Path_" ++ nameBase tname)
         pcname = mkName (nameBase n)
         pvtname = mkName ("PV_" ++ nameBase tname)
         pvcname = mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase wname)
         pwname = mkName ("PV_" ++ nameBase wname)
         matchpath = [|\x -> case x of
                               $(conP pcname [wildP]) -> True
                               _ -> False|]
     p <- runQ $ newName "p"
     q <- runQ $ newName "q"
     pvlift <- shim w v pvcname (conE pcname)
     runQ [| case filter $matchpath (pathsOf $(varE x) (undefined :: Proxy $(pure wtyp))) :: [$(conT ptname) $(conT wname)] of
               $(listP [asP p (conP pcname [varP q])] :: PatQ) ->
                   let [y] = toListOf (toLens $(varE p)) $(varE x) :: [$(pure wtyp)] in
                   Node ($(conE pvcname) $(varE p) y) (forestMap $(pure pvlift) (pvNodes y :: Forest $(conT pwname)))
               [] -> error $(litE (stringL ("No " ++ show pcname ++ " field found")))
               ps -> error $ $(litE (stringL ("Multiple " ++ show pcname ++ " fields found: "))) ++ show ps |]

doPVNodesOfView :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> m [ClauseQ]
doPVNodesOfView v wtyp =
    let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v) in
    doPVNodeOf v wtyp (mkName ("Path_" ++ nameBase tname ++ "_View"))

doPVNodesOfOrder :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> m [ClauseQ]
doPVNodesOfOrder v wtyp =
  do w <- expandType wtyp >>= typeVertex
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wtyp = bestType w
         wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
         ptname = mkName ("Path_" ++ nameBase tname)
         pcname = 'Path_At
         pvtname = mkName ("PV_" ++ nameBase tname)
         pvcname = mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase wname)
         pwname = mkName ("PV_" ++ nameBase wname)
     p <- runQ $ newName "p"
     q <- runQ $ newName "q"
     k <- runQ $ newName "k"
     pvlift <- shim w v pvcname [|$(conE pcname) $(varE k)|]
     runQ [d| _f x = (let paths = pathsOf x (undefined :: Proxy $(pure wtyp)) :: [$(conT ptname) $(conT wname)] in
                      List.map
                          (\path ->
                                 case path of
                                   $(asP p (conP pcname [varP k, wildP]) :: PatQ) ->
                                       let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                                       Node ($(conE pvcname) $(varE p) y) (forestMap $(pure pvlift) (pvNodes y :: Forest $(conT pwname)))
                                   _ -> error ("doPVNodesOfOrder: " ++ show path)) paths) :: [Tree (PVType $(pure (view (etype . unE) v)))] |] >>= return . clauses
{-
     runQ [d| _f x = (case filter $matchpath (pathsOf x (undefined :: Proxy $(pure wtyp))) :: [$(conT ptname) $(conT wname)] of
                        $(listP [asP p (conP pcname [varP k, varP q])] :: PatQ) ->
                            let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                            [Node ($(conE pvcname) $(varE p) y) (forestMap $(pure pvlift) (pvNodes y :: Forest $(conT pwname)))]
                        ps -> error ("doPVNodesOfOrder: " ++ show ps)) :: [Tree (PVType $(pure (view (etype . unE) v)))] |] >>= return . clauses
-}

doPVNodesOfMap :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> m [ClauseQ]
doPVNodesOfMap v wtyp =
  do w <- expandType wtyp >>= typeVertex
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wtyp = bestType w
         wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
         ptname = mkName ("Path_" ++ nameBase tname)
         pcname = 'Path_Look
         pvtname = mkName ("PV_" ++ nameBase tname)
         pvcname = mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase wname)
         pwname = mkName ("PV_" ++ nameBase wname)
     p <- runQ $ newName "p"
     q <- runQ $ newName "q"
     k <- runQ $ newName "k"
     pvlift <- shim w v pvcname [|$(conE pcname) $(varE k)|]
     runQ [d| _f x = (let paths = pathsOf x (undefined :: Proxy $(pure wtyp)) :: [$(conT ptname) $(conT wname)] in
                      List.map
                          (\path ->
                                 case path of
                                   $(asP p (conP pcname [varP k, wildP]) :: PatQ) ->
                                       let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                                       Node ($(conE pvcname) $(varE p) y) (forestMap $(pure pvlift) (pvNodes y :: Forest $(conT pwname)))
                                   _ -> error ("doPVNodesOfMap: " ++ show path)) paths) :: [Tree (PVType $(pure (view (etype . unE) v)))] |] >>= return . clauses
{-
     runQ [d| _f x = (case pathsOf x (undefined :: Proxy $(pure wtyp)) :: [$(conT ptname) $(conT wname)] of
                        $(listP [asP p (conP pcname [varP k, varP q])] :: PatQ) ->
                            let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                            [Node ($(conE pvcname) $(varE p) y) (forestMap $(pure pvlift) (pvNodes y :: Forest $(conT pwname)))]
                        _ -> []) :: [Tree (PVType $(pure (view (etype . unE) v)))] |] >>= return . clauses
-}

doPVNodeOf :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> Name -> m [ClauseQ]
doPVNodeOf v wtyp pcname =
  do w <- expandType wtyp >>= typeVertex :: m TGVSimple
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wtyp = bestType w
         wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
         ptname = mkName ("Path_" ++ nameBase tname)
         pvtname = mkName ("PV_" ++ nameBase tname)
         pvcname = mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase wname)
         pwname = mkName ("PV_" ++ nameBase wname)
         -- We need to filter these because of types like Either String String
         matchpath = [|\x -> case x of
                               $(conP pcname [wildP]) -> True
                               _ -> False|]
     -- sf <- pvNodeClauses w
     p <- runQ $ newName "p"
     q <- runQ $ newName "q"
     pvlift <- shim w v pvcname (conE pcname)
     runQ [d| _f x = (case filter $matchpath (pathsOf x (undefined :: Proxy $(conT wname))) :: [$(conT ptname) $(conT wname)] of
                        $(listP [asP p (conP pcname [varP q])] :: PatQ) ->
                            let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                            [Node ($(conE pvcname) $(varE p) y) (forestMap $(pure pvlift) (pvNodes y :: Forest $(conT pwname)))]
                        [] -> [])  :: [Tree (PVType $(pure (view (etype . unE) v)))] |] >>= return . clauses

