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
import Language.Haskell.TH.Path.Decs.Common (asConQ, asName, asType, asTypeQ, bestPathTypeName, bestTypeName, clauses,
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
             doPeekNodeOf v wtyp [makePathCon (makePathType tname) "View"]
       ConT tname -> doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               doPeekNodesOfOrder v vtyp (PathCon 'Path_At)
       AppT ListT _etyp -> error "list" {- return [clause [wildP] (normalB [|error "list"|]) []]-}
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               doPeekNodesOfMap v vtyp (PathCon 'Path_Look)
       AppT (AppT (TupleT 2) ftyp) styp ->
           doPeekNodeOf v ftyp [PathCon 'Path_First, PathCon 'Path_Second]
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               doPeekNodeOf v etyp [PathCon 'Path_Just]
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               doPeekNodeOf v ltyp [PathCon 'Path_Left, PathCon 'Path_Right]
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
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [NormalC _cname sts] x = do
        flds <- mapM (doField' tname x) sts
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname [InfixC lhs _cname rhs] x = do
        flds <- mapM (doField' tname x) [lhs, rhs]
        return [clause [varP x] (normalB (listE (List.map pure flds))) []]
      doCons tname cons x = concat <$> mapM (doCon tname x) cons
      -- If we have multiple constructors, only generate values for
      -- the one that matches
      doCon :: Name -> Name -> Con -> m [ClauseQ]
      doCon tname x (ForallC _ _ con) = doCon tname x con
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
        return [clause [recP cname []] (normalB (listE (List.map pure flds))) []]

      doField :: Name -> Name -> (Name, Strict, Type) -> m Exp
      doField tname x (fname, _, ftype) = do
        f <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
        let Just p = makeFieldCon f
        maybe (runQ [|error $(litE (stringL ("doField " ++ show (asName p))))|])
              (doPeekNodesOfField x v f)
              (makeFieldCon f)
      -- Anonymous fields are not supported.
      doField' :: Name -> Name -> (Strict, Type) -> m Exp
      doField' _tname _x (_, ftype) =
          runQ [|error $(litE (stringL ("doField' " ++ pprint ftype)))|]

doPeekNodesOfField :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Name -> TGVSimple -> TGV -> PathCon -> m Exp
doPeekNodesOfField x v f pcname =
  do let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wname = fromMaybe (error $ "No name for " ++ pprint f ++ ", view of " ++ pprint v) (bestTypeName f)
         matchpath = [|\p -> case p of
                               $(conP (asName pcname) [wildP]) -> True
                               _ -> False|]
     p <- runQ $ newName "p"
     peeklift <- peekShim (view vsimple f) v (asConQ pcname)
     runQ [| case filter $matchpath (pathsOf $(varE x) (undefined :: Proxy $(pure (asType f)))) :: [$(asTypeQ (makePathType tname)) $(asTypeQ f)] of
               $(listP [asP p (conP (asName pcname) [wildP])] :: PatQ) ->
                   let [y] = toListOf (toLens $(varE p)) $(varE x) :: [$(pure (asType f))] in
                   Node ($(asConQ (makePeekCon tname wname)) $(varE p) y) (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ f))))
               [] -> error $(litE (stringL ("No " ++ show (asName pcname) ++ " field found")))
               ps -> error $ $(litE (stringL ("Multiple " ++ show (asName pcname) ++ " fields found: "))) ++ show ps |]

doPeekNodesOfOrder :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfOrder v wtyp pcname =
  do w <- expandType wtyp >>= typeVertex
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
     p <- runQ $ newName "p"
     k <- runQ $ newName "k"
     peeklift <- peekShim w v [|$(asConQ pcname) $(varE k)|]
     runQ [d| _f x = (let paths = pathsOf x (undefined :: Proxy $(pure wtyp)) :: [$(asTypeQ (makePathType tname)) $(asTypeQ wtyp)] in
                      List.map
                          (\path ->
                                 case path of
                                   $(asP p (conP (asName pcname) [varP k, wildP]) :: PatQ) ->
                                       let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                                       Node ($(asConQ (makePeekCon tname wname)) $(varE p) y) (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ wtyp))))
                                   _ -> error ("doPeekNodesOfOrder: " ++ show path)) paths) :: Forest (Peek $(asTypeQ v)) |] >>= return . clauses

doPeekNodesOfMap :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfMap v wtyp pcname =
  do w <- expandType wtyp >>= typeVertex
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wname = fromMaybe (error $ "No name for " ++ pprint wtyp ++ ", view of " ++ pprint v) (bestTypeName w)
     p <- runQ $ newName "p"
     k <- runQ $ newName "k"
     peeklift <- peekShim w v [|$(asConQ pcname) $(varE k)|]
     runQ [d| _f x = (let paths = pathsOf x (undefined :: Proxy $(pure wtyp)) :: [$(asTypeQ (makePathType tname)) $(asTypeQ wtyp)] in
                      List.map
                          (\path ->
                                 case path of
                                   $(asP p (conP (asName pcname) [varP k, wildP]) :: PatQ) ->
                                       let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                                       Node ($(asConQ (makePeekCon tname wname)) $(varE p) y) (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ wtyp))))
                                   _ -> error ("doPeekNodesOfMap: " ++ show path)) paths) :: Forest (Peek $(asTypeQ  v)) |] >>= return . clauses

doPeekNodeOf :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> [PathCon] -> m [ClauseQ]
doPeekNodeOf v wtyp pcnames =
  do w <- expandType wtyp >>= typeVertex :: m TGVSimple
     let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
         wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
         -- We need to filter these because of types like Either String String
         matchpath = [| \p -> $(caseE [|p|] (map (\pcname -> match (conP (asName pcname) [wildP]) (normalB [|True|]) []) pcnames ++
                                             [match wildP (normalB [|False|]) []] )) |]
{-
         matchpath = [|\p -> case p of
                               $(conP (asName pcname) [wildP]) -> True
                               _ -> False|]
-}
     p <- runQ $ newName "p"
     peeklifts <- mapM (peekShim w v . asConQ) pcnames
     runQ [d| _f x = let paths = filter $matchpath (pathsOf x (undefined :: Proxy $(asTypeQ wtyp))) :: [$(asTypeQ (makePathType tname)) $(asTypeQ wtyp)] in
                     List.map
                         (\path ->
                              $(caseE [|path|]
                                 (map (\(pcname, peeklift) ->
                                           match (asP p (conP (asName pcname) [wildP]))
                                                 (normalB [|let [y] = toListOf (toLens $(varE p)) x :: [$(pure wtyp)] in
                                                            Node ($(asConQ (makePeekCon tname wname)) $(varE p) y)
                                                                 (forestMap $(pure peeklift) (peek y :: Forest (Peek $(asTypeQ wtyp))))|])
                                                 []) (zip pcnames peeklifts) ++
                                  [match wildP (normalB [|error ("doPeekNodesOf: " ++ show path)|]) []]))) paths |] >>= return . clauses

-- | Build a value of type such as Peek_AbbrevPair -> Peek_AbbrevPairs
peekShim :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> TGVSimple -> ExpQ -> m Exp
peekShim w v pcname =
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
