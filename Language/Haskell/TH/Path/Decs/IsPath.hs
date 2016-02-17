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
import Data.List as List (map, nub)
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

-- makePeekType :: ModelType -> PeekType
-- makePeekType (ModelType s) = PeekType (mkName ("Peek_" ++ nameBase s))

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
     x <- runQ $ newName "x"
     case asType v of
       _ | selfPath -> return []
         | simplePath -> return []
         | isJust viewType ->
             do let wtyp = fromJust viewType
                let tname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
                w <- expandType wtyp >>= typeVertex
                let PathCon pcon = makePathCon (makePathType tname) "View"
                doPeekNodeOf (varE x) v [(varP x, [(w, conP pcon [wildP], conE pcon)])]
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
              mappend <$> doPeekNodeOf (varE x) v [(varP x, [(f, conP 'Path_First [wildP], [|Path_First|])])]
                      <*> doPeekNodeOf (varE x) v [(varP x, [(s, conP 'Path_Second [wildP], [|Path_Second|])])]
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do e <- expandType etyp >>= typeVertex
                  doPeekNodeOf (varE x) v [(varP x, [(e, conP 'Path_Just [wildP], [|Path_Just|])])]
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do l <- expandType ltyp >>= typeVertex
                  r <- expandType rtyp >>= typeVertex
                  mappend <$> doPeekNodeOf (varE x) v [(asP x (conP 'Left [wildP]), [(l, conP 'Path_Left [wildP], [|Path_Left|])])]
                          <*> doPeekNodeOf (varE x) v [(asP x (conP 'Right [wildP]), [(r, conP 'Path_Right [wildP], [|Path_Right|])])]
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

      doCon :: Name -> Name -> Con -> m [ClauseQ]
      doCon tname x (ForallC _ _ con) = doCon tname x con

      -- Each constructor turns into a clause of the peek function
      doCon tname x (RecC cname vsts) = do
        flds <- mapM (doField tname x) (zip vsts [1..])
        return [clause [asP x (recP cname [])] (normalB [|concat $(listE flds)|]) []]
      doCon tname x (NormalC cname sts) = do
        flds <- mapM (doFieldAnon tname x) (zip sts [1..])
        return [clause [asP x (recP cname [])] (normalB (listE flds)) []]
      doCon tname x (InfixC lhs cname rhs) = do
        flds <- mapM (doFieldAnon tname x) (zip [lhs, rhs] [1..])
        return [clause [asP x (infixP wildP cname wildP)] (normalB (listE flds)) []]

      -- Each field turns into an expression involving x and return a value of type Forest (Peek t)
      doField :: Name -> Name -> ((Name, Strict, Type), Int) -> m ExpQ
      doField tname x ((fname, _, ftype), _fpos) = do
        f <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
        let w = view vsimple f
        gs <- pathKeys w
        maybe (pure [|error $(litE (stringL ("doField " ++ show f)))|])
              (\pcname -> forestOf (varE x) v [(w, conP (asName pcname) [wildP], asConQ pcname)])
              (makeFieldCon f)

#if 1
        -- Anonymous fields are not supported.
      doFieldAnon :: Name -> Name -> ((Strict, Type), Int) -> m ExpQ
      doFieldAnon _tname _x ((_, ftype), _fpos) = do
        pure [|error $(litE (stringL ("doFieldAnon " ++ pprint ftype)))|]
#else
      doFieldAnon :: Name -> Name -> ((Strict, Type), Int) -> m Exp
      doFieldAnon tname x ((_, ftype), fpos) = do
        -- Would like to support anonymous fields, but haven't handled the naming yet
        f <- expandType ftype >>= fieldVertex (tname, undefined, Left fpos)
        let Just p = makeFieldCon f
        let w = view vsimple f
        maybe (runQ [|error $(litE (stringL ("doField " ++ show f)))|])
              (\pcname -> runQ $ forestOf (varE x) v [(w, conP (asName pcname) [wildP], asConQ pcname)])
              (makeFieldCon f)
#endif

doPeekNodesOfOrder :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfOrder v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     k <- runQ $ newName "k"
     forest <- forestOf (varE x) v [(w, conP (asName pcname) [varP k, wildP], [|$(asConQ pcname) $(varE k)|])]
     return [clause [varP x] (normalB forest) []]

doPeekNodesOfMap :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> Type -> PathCon -> m [ClauseQ]
doPeekNodesOfMap v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     k <- runQ $ newName "k"
     forest <- forestOf (varE x) v [(w, conP (asName pcname) [varP k, wildP], [|$(asConQ pcname) $(varE k)|])]
     return [clause [varP x] (normalB forest) []]

doPeekNodeOf :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => ExpQ -> TGVSimple -> [(PatQ, [(TGVSimple, PatQ, ExpQ)])] -> m [ClauseQ]
doPeekNodeOf x v alts@[(xpat, [(w, ppat, pcon)])] =
    mapM (\(xpat, concs) -> do
            forest <- forestOf x v concs
            return $ clause [xpat] (normalB forest) []) alts

-- | Given a value @x@ of type @v@, return the corresponding expression of
-- type @Forest (Peek v)@.
forestOf :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => ExpQ -> TGVSimple -> [(TGVSimple, {-Set TGVSimple,-} PatQ, ExpQ)] -> m ExpQ
forestOf x v alts =
  do ms <- mapM forestOfType alts
     return [| concatMap (\path -> $(caseE [|path|] (ms ++ [match wildP (normalB [| [] |]) []])))
                         $paths :: Forest (Peek $(asTypeQ v)) |]
    where
      forestOfType (w, ppat, pcon) =
          do gs <- pathKeys w
             p <- runQ $ newName "p"
             return $ match (asP p ppat :: PatQ)
                            (normalB
                             [| map (\y -> Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) y)
                                                -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                                                -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                                                (forestMap (\v' -> $(caseE [|v'|] (concatMap (doGoal w pcon) (Foldable.toList gs)))) (peek y :: Forest (Peek $(asTypeQ w)))))
                                    (toListOf (toLens $(varE p)) $x :: [$(asTypeQ w)]) |])
                            []
      paths = [| concat $(listE (map (\w -> [| (pathsOf $x (undefined :: Proxy $(asTypeQ w)) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]) |])
                                     (nub (map (view _1) alts)))) |]

      doGoal :: TGVSimple -> ExpQ -> TGVSimple -> [MatchQ]
      doGoal w pcon g = do
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
