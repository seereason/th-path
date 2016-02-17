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
module Language.Haskell.TH.Path.Decs.IsPath (peekDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Map as Map (Map)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Set (Set)
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (IsPathStart(Peek, peek, Hop), IsPath(..), ToLens(toLens), SelfPath, SinkType,
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Decs.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ), bestPathTypeName, bestTypeName,
                                             makeFieldCon, makePathCon, makePathType, makeHopCon,
                                             ModelType(ModelType), PathCon, PathCon(PathCon))
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Expand (expandType, unE)
import Language.Haskell.TH.TypeGraph.TypeGraph (lensKeys, pathKeys, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, tgv, TGV, TGVSimple, vsimple)

newtype PeekType = PeekType {unPeekType :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs
newtype PeekCon = PeekCon {unPeekCon :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs_Markup

instance HasName PeekType where asName = unPeekType
instance HasType PeekType where asType = ConT . unPeekType
instance HasTypeQ PeekType where asTypeQ = conT . unPeekType
instance HasName PeekCon where asName = unPeekCon
instance HasCon PeekCon where asCon = ConE . unPeekCon
instance HasConQ PeekCon where asConQ = conE . unPeekCon

makePeekType :: ModelType -> PeekType
makePeekType (ModelType s) = PeekType (mkName ("Peek_" ++ nameBase s))

makePeekCon :: ModelType -> ModelType -> PeekCon
makePeekCon (ModelType s) (ModelType g) = PeekCon (mkName ("Peek_" ++ nameBase s ++ "_" ++ nameBase g))

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
             do let wtyp = fromJust viewType
                let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
                w <- expandType wtyp >>= typeVertex
                doPeekNodeOf v w (makePathCon (makePathType tname) "View")
       ConT tname -> doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               doPeekNodesOfOrder v vtyp (PathCon 'Path_At)
       AppT ListT _etyp -> error "list" {- return [clause [wildP] (normalB [|error "list"|]) []]-}
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               doPeekNodesOfMap v vtyp (PathCon 'Path_Look)
       AppT (AppT (TupleT 2) ftyp) styp ->
           do f <- expandType ftyp >>= typeVertex
              s <- expandType styp >>= typeVertex
              mappend <$> doPeekNodeOf v f (PathCon 'Path_First) <*> doPeekNodeOf v s (PathCon 'Path_Second)
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do e <- expandType etyp >>= typeVertex
                  doPeekNodeOf v e (PathCon 'Path_Just)
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do l <- expandType ltyp >>= typeVertex
                  r <- expandType rtyp >>= typeVertex
                  mappend <$> doPeekNodeOf v l (PathCon 'Path_Left) <*> doPeekNodeOf v r (PathCon 'Path_Right)
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
      doCons tname cons x = concat <$> mapM (doCon tname x) cons

      -- If we have multiple constructors, only generate values for
      -- the one that matches
      doCon :: Name -> Name -> Con -> m [ClauseQ]
      doCon tname x (ForallC _ _ con) = doCon tname x con

      doCon tname x (RecC cname vsts) = do
        flds <- mapM (doField tname x) (zip vsts [1..])
        return [clause [asP x (recP cname [])] (normalB [|concat $(listE (List.map pure flds))|]) []]
      doCon tname x (NormalC cname sts) = do
        flds <- mapM (doFieldAnon tname x) (zip sts [1..])
        return [clause [asP x (recP cname [])] (normalB (listE (List.map pure flds))) []]
      doCon tname x (InfixC lhs cname rhs) = do
        flds <- mapM (doFieldAnon tname x) (zip [lhs, rhs] [1..])
        return [clause [asP x (infixP wildP cname wildP)] (normalB (listE (List.map pure flds))) []]

      doField :: Name -> Name -> ((Name, Strict, Type), Int) -> m Exp
      doField tname x ((fname, _, ftype), _fpos) = do
        f <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
        let w = view vsimple f
        gs <- pathKeys w
        maybe (runQ [|error $(litE (stringL ("doField " ++ show f)))|])
              (\pcname -> runQ $ doPeekNodesOfField x v w gs (conP (asName pcname) [wildP]) (asConQ pcname))
              (makeFieldCon f)

#if 1
        -- Anonymous fields are not supported.
      doFieldAnon :: Name -> Name -> ((Strict, Type), Int) -> m Exp
      doFieldAnon _tname _x ((_, ftype), _fpos) = do
        runQ [|error $(litE (stringL ("doFieldAnon " ++ pprint ftype)))|]
#else
      doFieldAnon :: Name -> Name -> ((Strict, Type), Int) -> m Exp
      doFieldAnon tname x ((_, ftype), fpos) = do
        -- Would like to support anonymous fields, but haven't handled the naming yet
        f <- expandType ftype >>= fieldVertex (tname, undefined, Left fpos)
        let Just p = makeFieldCon f
        let w = view vsimple f
        gs <- pathKeys w
        maybe (runQ [|error $(litE (stringL ("doField " ++ show f)))|])
              (\pcname -> runQ $ doPeekNodesOfField x v w gs (conP (asName pcname) [wildP]) (asConQ pcname))
              (makeFieldCon f)
#endif

doPeekNodesOfOrder :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfOrder v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     gs <- pathKeys w
     k <- runQ $ newName "k"
     return [clause [varP x] (normalB (doPeekNodesOfField x v w gs (conP (asName pcname) [varP k, wildP]) [|$(asConQ pcname) $(varE k)|])) []]

doPeekNodesOfMap :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfMap v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     gs <- pathKeys w
     k <- runQ $ newName "k"
     return [clause [varP x] (normalB (doPeekNodesOfField x v w gs (conP (asName pcname) [varP k, wildP]) [|$(asConQ pcname) $(varE k)|])) []]

doPeekNodeOf :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> TGVSimple -> PathCon -> m [ClauseQ]
doPeekNodeOf v w pcname =
  do x <- runQ $ newName "x"
     gs <- pathKeys w
     return [clause [varP x] (normalB (doPeekNodesOfField x v w gs (conP (asName pcname) [wildP]) (asConQ pcname))) []]

doPeekNodesOfField :: Name -> TGVSimple -> TGVSimple -> Set TGVSimple -> PatQ -> ExpQ -> ExpQ
doPeekNodesOfField x v w gs ppat pcon =
  do p <- newName "p"
     [| concatMap (\path ->
                       case path of
                         $(asP p ppat :: PatQ) ->
                             map (\y -> Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) y)
                                             -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                                             -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                                             (forestMap (\v' -> $(caseE [|v'|] (concatMap doGoal (Foldable.toList gs)))) (peek y :: Forest (Peek $(asTypeQ w)))))
                                 (toListOf (toLens $(varE p)) $(varE x) :: [$(asTypeQ w)])
                         _ -> [])
                  (pathsOf $(varE x) (undefined :: Proxy $(asTypeQ w)) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]) :: Forest (Peek $(asTypeQ v)) |]
    where
      doGoal :: TGVSimple -> [MatchQ]
      doGoal g = do
        case (bestName v, bestName w, bestName g) of
          (Just vn, Just wn, Just gn) ->
              [newName "z" >>= \z ->
               newName "q" >>= \q ->
               match (conP (asName (makePeekCon (ModelType wn) (ModelType gn))) [varP q, varP z])
                     (normalB [|$(asConQ (makePeekCon (ModelType vn) (ModelType gn)))
                                 (($pcon :: Path $(asTypeQ w) $(asTypeQ g) ->
                                            Path $(asTypeQ v) $(asTypeQ g)) $(varE q)) $(varE z)|])
                     []]
          _ -> []
