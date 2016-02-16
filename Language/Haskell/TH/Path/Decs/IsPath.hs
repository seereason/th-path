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
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.IsPath (peekDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Map as Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (IsPathStart(Peek, peek, Hop), IsPath(..), ToLens(toLens), SelfPath, SinkType,
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Decs.Common (asConQ, asName, asType, asTypeQ, bestPathTypeName, bestTypeName,
                                             makeFieldCon, makePathCon, makePathType, makePeekCon, makeHopCon,
                                             ModelType(ModelType), PathCon, PathCon(PathCon))
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Expand (expandType, unE)
import Language.Haskell.TH.TypeGraph.TypeGraph (lensKeys, pathKeys, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, tgv, TGV, TGVSimple, vsimple)

peekDecs :: forall m. (MonadWriter [Dec] m, ContextM m, MonadReaders TypeInfo m, MonadReaders TypeGraph m) =>
            TGVSimple -> m ()
peekDecs v =
    case bestTypeName v of
      Just _tname -> do
        (pnc :: [ClauseQ]) <- peekClauses v
        (cons :: [ConQ]) <- peekCons
        (hcons :: [ConQ]) <- hopCons
        runQ (instanceD (cxt []) (appT (conT ''IsPathStart) (asTypeQ v))
                [dataInstD (cxt []) ''Peek [asTypeQ v] cons [''Eq, ''Show],
                 funD 'peek (case pnc of
                               [] -> [clause [wildP] (normalB [| [] |]) []]
                               _ -> pnc),
                 dataInstD (cxt []) ''Hop [asTypeQ v] (case hcons of
                                                         [] -> [normalC (asName (makeHopCon (ModelType (asName v)) (tgv v))) []]
                                                         _ -> hcons) [''Eq, ''Show]
                ]) >>= tell . (: [])
      Nothing -> return ()
    where
      peekCons :: m [ConQ]
      peekCons = (concat . List.map doPair . toList) <$> (pathKeys v)
      doPair :: TGVSimple -> [ConQ]
      doPair g =
          let Just (vp, _) = bestPathTypeName v in
          case (bestName v, bestName g) of
            (Just vn, Just gn) ->
                [normalC (asName (makePeekCon (ModelType vn) (ModelType gn)))
                         [(,) <$> notStrict <*> [t|$(asTypeQ vp) $(pure (view (etype . unE) g))|],
                          (,) <$> notStrict <*> pure (view (etype . unE) g)]]
            _ -> []
      hopCons :: m [ConQ]
      hopCons = (concat . List.map doHopPair . toList) <$> (lensKeys v)
      doHopPair :: TGV -> [ConQ]
      doHopPair g =
          let Just (vp, _) = bestPathTypeName v in
          case (bestName v, bestName g) of
            (Just vn, Just gn) ->
                [normalC (asName (makeHopCon (ModelType vn) g))
                         [(,) <$> notStrict <*> [t|$(asTypeQ vp) $(pure (view (vsimple . etype . unE) g))|]]]
            _ -> []

peekClauses :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
               TGVSimple -> m [ClauseQ]
peekClauses v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewType <- viewInstanceType (view etype v)
     case asType v of
       _ | selfPath -> return []
         | simplePath -> return []
         | isJust viewType ->
             let wtyp = fromJust viewType in
             let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v) in
             doPeekNodeOf v wtyp (makePathCon (makePathType tname) "View")
       ConT tname -> doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               doPeekNodesOfOrder v vtyp (PathCon 'Path_At)
       AppT ListT _etyp -> error "list" {- return [clause [wildP] (normalB [|error "list"|]) []]-}
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               doPeekNodesOfMap v vtyp (PathCon 'Path_Look)
       AppT (AppT (TupleT 2) ftyp) styp ->
           mappend <$> doPeekNodeOf v ftyp (PathCon 'Path_First) <*> doPeekNodeOf v styp (PathCon 'Path_Second)
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               doPeekNodeOf v etyp (PathCon 'Path_Just)
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               mappend <$> doPeekNodeOf v ltyp (PathCon 'Path_Left) <*> doPeekNodeOf v rtyp (PathCon 'Path_Right)
       _ -> return []
    where
      doName :: Name -> m [ClauseQ]
      doName tname = qReify tname >>= doInfo
      doInfo :: Info -> m [ClauseQ]
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return []
      doDec :: Dec -> m [ClauseQ]
      doDec (NewtypeD _cx tname _binds con _supers) = runQ (newName "x") >>= doCons tname [con]
      doDec (DataD _cx tname _binds cons _supers) = runQ (newName "x") >>= doCons tname cons
      doDec dec = error $ "Unexpected dec: " ++ pprint dec

      doCons :: Name -> [Con] -> Name -> m [ClauseQ]
      doCons _tname [] _x = error "No constructors"
      doCons tname [ForallC _ _ con] x = doCons tname [con] x
      doCons tname [RecC _cname vsts] x = do
        flds <- mapM (doField tname x) vsts
        return [clause [varP x] (normalB [|concat $(listE (List.map pure flds))|]) []]
      doCons tname [NormalC _cname sts] x = do
        flds <- mapM (doFieldAnon tname x) sts
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [InfixC lhs _cname rhs] x = do
        flds <- mapM (doFieldAnon tname x) [lhs, rhs]
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname cons x = concat <$> mapM (doCon tname x) cons
      -- If we have multiple constructors, only generate values for
      -- the one that matches
      doCon :: Name -> Name -> Con -> m [ClauseQ]
      doCon tname x (ForallC _ _ con) = doCon tname x con
      doCon tname x (RecC cname vsts) = doMatchingFields tname x cname vsts
      doCon tname x (NormalC cname sts) = doMatchingFieldsAnon tname x cname sts
      doCon tname x (InfixC lhs cname rhs) = doMatchingFieldsAnon tname x cname [lhs, rhs]

      doMatchingFields :: Name -> Name -> Name -> [(Name, Strict, Type)] -> m [ClauseQ]
      doMatchingFields tname x cname vsts = do
        flds <- mapM (doField tname x) vsts
        return [clause [asP x (recP cname [])] (normalB [|concat $(listE (List.map pure flds))|]) []]
      doField :: Name -> Name -> (Name, Strict, Type) -> m Exp
      doField tname x (fname, _, ftype) = do
        f <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
        let Just p = makeFieldCon f
        maybe (runQ [|error $(litE (stringL ("doField " ++ show (asName p))))|])
              (doPeekNodesOfField x v f)
              (makeFieldCon f)

      -- Anonymous fields are not supported.
      doMatchingFieldsAnon :: Name -> Name -> Name -> [(Strict, Type)] -> m [ClauseQ]
      doMatchingFieldsAnon tname x cname sts = do
        flds <- mapM (doFieldAnon tname x) sts
        return [clause [recP cname []] (normalB [|concat $(listE (List.map pure flds))|]) []]
      doFieldAnon :: Name -> Name -> (Strict, Type) -> m Exp
      doFieldAnon _tname _x (_, ftype) =
          runQ [|error $(litE (stringL ("doFieldAnon " ++ pprint ftype)))|]

doPeekNodesOfField :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Name -> TGVSimple -> TGV -> PathCon -> m Exp
doPeekNodesOfField x v field pcname =
  do let wname = fromMaybe (error $ "No name for " ++ pprint field ++ ", view of " ++ pprint v) (bestTypeName field)
         matchpath = [|\p -> case p of
                               $(conP (asName pcname) [wildP]) -> True
                               _ -> False|]
     p <- runQ $ newName "p"
     peeklift <- shim (view vsimple field) v (asConQ pcname)
     runQ [| concatMap
                  (\path ->
                       case path of
                         $(asP p (conP (asName pcname) [wildP]) :: PatQ) ->
                             let [y] = toListOf (toLens $(varE p)) $(varE x) :: [$(pure (asType field))] in
                             [Node ($(asConQ (makePeekCon (ModelType (asName v)) wname)) $(varE p) y)
                                   (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ field))))]
                         _ -> [])
                  (filter $matchpath (pathsOf $(varE x) (undefined :: Proxy $(pure (asType field)))) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ field)]) |]

doPeekNodesOfOrder :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfOrder v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex
     let wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
     p <- runQ $ newName "p"
     k <- runQ $ newName "k"
     peeklift <- shim w v [|$(asConQ pcname) $(varE k)|]
     return [clause
              [varP x]
              (normalB [| concatMap
                            (\path ->
                                 case path of
                                   $(asP p (conP (asName pcname) [varP k, wildP]) :: PatQ) ->
                                       let [y] = toListOf (toLens $(varE p)) $(varE x) :: [$(pure wtyp)] in
                                       [Node ($(asConQ (makePeekCon (ModelType (asName v)) wname)) $(varE p) y)
                                             (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ wtyp))))]
                                   _ -> error ("doPeekNodesOfOrder: " ++ show path))
                            (pathsOf $(varE x) (undefined :: Proxy $(pure wtyp)) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ wtyp)]) :: Forest (Peek $(asTypeQ v)) |])
              []]

doPeekNodesOfMap :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfMap v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex
     let wname = fromMaybe (error $ "No name for " ++ pprint wtyp ++ ", view of " ++ pprint v) (bestTypeName w)
     p <- runQ $ newName "p"
     k <- runQ $ newName "k"
     peeklift <- shim w v [|$(asConQ pcname) $(varE k)|]
     return [clause
              [varP x]
              (normalB [| concatMap
                            (\path ->
                                 case path of
                                   $(asP p (conP (asName pcname) [varP k, wildP]) :: PatQ) ->
                                       let [y] = toListOf (toLens $(varE p)) $(varE x) :: [$(pure wtyp)] in
                                       [Node ($(asConQ (makePeekCon (ModelType (asName v)) wname)) $(varE p) y)
                                            (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ wtyp))))]
                                   _ -> [])
                            (pathsOf $(varE x) (undefined :: Proxy $(pure wtyp)) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ wtyp)]) :: Forest (Peek $(asTypeQ  v)) |])
              []]

doPeekNodeOf :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodeOf v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     p <- runQ $ newName "p"
     peeklift <- shim w v (asConQ pcname)
     return [clause
              [varP x]
              (normalB [| concatMap
                               (\path ->
                                    case path of
                                      $(asP p (conP (asName pcname) [wildP])) ->
                                          let [y] = toListOf (toLens $(varE p)) $(varE x) :: [$(pure wtyp)] in
                                          [Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) y)
                                                (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ wtyp))))]
                                      _ -> [])
                               (pathsOf $(varE x) (undefined :: Proxy $(asTypeQ wtyp)) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ wtyp)])
                               |])
              []]

-- | Build a value of type such as Peek_AbbrevPair -> Peek_AbbrevPairs
shim :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> TGVSimple -> ExpQ -> m Exp
shim w v pcname =
    do z <- runQ $ newName "z"
       gs <- pathKeys w
       matches <- concat <$> (mapM (doPair z) (Foldable.toList gs))
       runQ [| \v' -> $(caseE [|v'|] (List.map pure matches)) |]
    where
      doPair :: Name -> TGVSimple -> m [Match]
      doPair z g = do
        case (bestName v, bestName w, bestName g) of
          (Just vn, Just wn, Just gn) ->
              do q <- runQ $ newName "q"
                 sequence
                   [runQ $ match (conP (asName (makePeekCon (ModelType wn) (ModelType gn))) [varP q, varP z])
                             (normalB [|$(asConQ (makePeekCon (ModelType vn) (ModelType gn)))
                                         (($pcname :: Path $(asTypeQ (ModelType wn)) $(asTypeQ g) ->
                                                      Path $(asTypeQ (ModelType vn)) $(asTypeQ g)) $(varE q)) $(varE z)|])
                             []]
          _ -> pure []
