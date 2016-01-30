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
import Data.Tree (Tree(Node))
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IsPathType(idPath), IsPathNode(PVType, pvNodes), IsPath(..), Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (bestPathTypeName, bestTypeName, clauses, fieldLensNameOld, pathConNameOfField, pvName)
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
                                 [] -> [clause [wildP] (normalB [|error "no pvNode clauses"|]) []]
                                 _ -> pnc)]) >>= tell . (: [])
      Nothing -> return ()

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
             do w <- expandType (fromJust viewType) >>= typeVertex :: {-StateT (Set Name)-} m TGVSimple
                let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
                    wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
                    ptname = mkName ("Path_" ++ nameBase tname)
                    pcname = mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase wname)
                sf <- pvNodeClauses w
                runQ [d|f x = (case pathsOf (x :: $(conT tname)) (undefined :: Proxy $(conT wname)) :: [$(conT ptname) $(conT wname)] of
                                 [p] -> let [y] = toListOf (toLens p) x in
                                        [($(conE pcname) p y)]
                                 [] -> []
                                 _ -> error "More than one path returned for view"
                              ) :: [PVType $(pure (view (etype . unE) v))] |] >>= return . clauses
       ConT tname -> doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               do w <- expandType vtyp >>= typeVertex
                  runQ [d|f x = List.map (\p -> let [y] = toListOf (toLens p) x in
                                                $(conE (pvName v w)) p y) (pathsOf x (undefined :: Proxy $(pure vtyp))) |] >>= return . clauses
               -- return [clause [wildP] (normalB [|error "order"|]) []]
       AppT ListT _etyp -> error "list" {- return [clause [wildP] (normalB [|error "list"|]) []]-}
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do w <- expandType vtyp >>= typeVertex
                  runQ [d|f x = List.map (\p -> let [y] = toListOf (toLens p) x in
                                                $(conE (pvName v w)) p y) (pathsOf x (undefined :: Proxy $(pure vtyp))) |] >>= return . clauses
       AppT (AppT (TupleT 2) ftyp) styp ->
           do x <- runQ $ newName "x"
              f <- doSingleton x v ftyp
              s <- doSingleton x v styp
              runQ [d| _f $(varP x) = [$(pure f), $(pure s)] |] >>= return . clauses
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do e <- expandType etyp >>= typeVertex
                  let typ = view (etype . unE) v
                  runQ [d|_f x = List.map
                                   (\p -> $(conE (pvName v e)) p (head (toListOf (toLens p) x)))
                                   (pathsOf x (undefined :: Proxy $(pure etyp))) |] >>= return . clauses
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do l <- expandType ltyp >>= typeVertex
                  r <- expandType rtyp >>= typeVertex
                  let typ = view (etype . unE) v
                  runQ [d|_f x = [let p = (head (pathsOf x (undefined :: Proxy $(pure ltyp)))) in $(conE (pvName v l)) p (head (toListOf (toLens p) x)),
                                  let p = (head (pathsOf x (undefined :: Proxy $(pure rtyp)))) in $(conE (pvName v r)) p (head (toListOf (toLens p) x))] |] >>= return . clauses
       _ -> return []
    where
      doSingleton :: Name -> TGVSimple -> Type -> m Exp
      doSingleton x v etyp = do
        e <- expandType etyp >>= typeVertex
        runQ [|let p = (head (pathsOf $(varE x) (undefined :: Proxy $(pure etyp)))) in
               $(conE (pvName v e)) p (head (toListOf (toLens p) $(varE x))) |]

      doName :: Name -> m [ClauseQ]
      doName tname = qReify tname >>= doInfo
      doInfo :: Info -> m [ClauseQ]
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return []
      doDec :: Dec -> m [ClauseQ]
      doDec (NewtypeD cx tname binds con supers) = doCons tname [con]
      doDec (DataD cx tname binds cons supers) = doCons tname cons
      -- concat <$> mapM doCon cons
      doCons :: Name -> [Con] -> m [ClauseQ]
      doCons tname [] = error "No constructors"
      doCons tname [ForallC _ _ con] = doCons tname [con]
      doCons tname [RecC cname vsts] = do
        x <- runQ $ newName "x"
        flds <- mapM (doField tname x) vsts
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [NormalC cname sts] = do
        x <- runQ $ newName "x"
        flds <- mapM (doField' tname x) sts
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [InfixC lhs cname rhs] = do
        x <- runQ $ newName "x"
        flds <- mapM (doField' tname x) [lhs, rhs]
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname cons = concat <$> mapM (doCon tname) cons
      -- If we have multiple constructors, only generate values for
      -- the one that matches
      doCon :: Name -> Con -> m [ClauseQ]
      doCon tname (ForallC _ _ con) = doCon tname con
      -- doCon ''Markup _ con@(Markdown {}) = runQ [d|_f (Markdown {}) = doFields ''Markup con
      --                                              _f (Html {}) = doFields ''Markup con
      --                                              _f _ = [] |]
      -- doCon ''Markup _ (Html {htmlText :: Text}) = ...
      doCon tname (RecC cname vsts) = doMatchingFields tname cname vsts
      doCon tname (NormalC cname sts) = doMatchingFields' tname cname sts
      doCon tname (InfixC lhs cname rhs) = doMatchingFields' tname cname [lhs, rhs]

      doMatchingFields :: Name -> Name -> [(Name, Strict, Type)] -> m [ClauseQ]
      doMatchingFields tname cname vsts = do
        x <- runQ $ newName "x"
        flds <- mapM (doField tname x) vsts
        return [clause [asP x (recP cname [])] (normalB (listE (List.map pure flds))) []]
      doMatchingFields' :: Name -> Name -> [(Strict, Type)] -> m [ClauseQ]
      doMatchingFields' tname cname sts = do
        x <- runQ $ newName "x"
        flds <- mapM (doField' tname x) sts
        return [clause [asP x (recP cname [])] (normalB (listE (List.map pure flds))) []]

      doField :: Name -> Name -> (Name, Strict, Type) -> m Exp
      doField tname x (fname, _, ftype) = do
        f <- expandType ftype >>= typeVertex
        f' <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
        let p = pathConNameOfField f'
        case (bestName f, p) of
          (Nothing, _) -> runQ [|error $(litE (stringL ("doField " ++ pprint fname)))|]
          (Just _, Just n) -> runQ [|$(conE (pvName v f)) ($(conE n) idPath) ($(varE fname) $(varE x)) |]
          -- runQ [|conE (mkName $(litE (stringL ("Path_" ++ nameBase tname ++ "_" ++ nameBase fname)))) idPath|]
      doField' :: Name -> Name -> (Strict, Type) -> m Exp
      doField' tname x (_, ftype) = do
        f <- expandType ftype >>= typeVertex
        -- Anonymous fields are not supported.
        runQ [|error $(litE (stringL ("doField' " ++ pprint ftype)))|]
{-
      doCon tname binds (InfixC st1 cname st2) =
          do l <- runQ $ newName "l"
             r <- runQ $ newName "r"
             clause [infixP (varP l) cname (varP r)] (normalB [|map (\p -> ) (pathsOf x (undefined :: Proxy $(pure
          recP
          -- If constructor of x matches return PVTypes for its fields (what if it has no fields?)

          runQ [d|_f x = map (\con -> case x of
          map (\con -> runQ
      doCon (InfixC lhs cname rhs) = return []
-}
