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
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (IsPathStart(Peek, peek, Hop), IsPath(..), ToLens(toLens), SelfPath, SinkType,
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Decs.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                             bestPathTypeName, bestTypeName,
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

makePeekCon :: (HasName s, HasName g) => ModelType s -> ModelType g -> PeekCon
makePeekCon (ModelType s) (ModelType g) = PeekCon (mkName ("Peek_" ++ nameBase (asName s) ++ "_" ++ nameBase (asName g)))

peekDecs :: forall m. (MonadWriter [Dec] m, ContextM m, MonadReaders TypeInfo m, MonadReaders TypeGraph m) =>
            TGVSimple -> m ()
peekDecs v =
    do (pnc :: [ClauseQ]) <- peekClauses v
       (cons :: [ConQ]) <- peekCons
       (hcons :: [ConQ]) <- hopCons
       runQ (instanceD (cxt []) (appT (conT ''IsPathStart) (asTypeQ v))
               [dataInstD (cxt []) ''Peek [asTypeQ v] cons [''Eq, ''Show],
                funD 'peek (case pnc of
                              [] -> [clause [wildP] (normalB [| [] |]) []]
                              _ -> pnc),
                dataInstD (cxt []) ''Hop [asTypeQ v] (case hcons of
                                                        [] -> [normalC (asName (makeHopCon v (tgv v))) []]
                                                        _ -> hcons) [''Eq, ''Show]
               ]) >>= tell . (: [])
    where
      peekCons :: m [ConQ]
      peekCons = (concat . List.map doPair . toList) <$> (pathKeys v)
      doPair :: TGVSimple -> [ConQ]
      doPair g =
          let Just vp = bestPathTypeName v in
          case (bestName v, bestName g) of
            (Just vn, Just gn) ->
                [normalC (asName (makePeekCon (ModelType vn) (ModelType gn)))
                         [(,) <$> notStrict <*> [t|Path $(asTypeQ v) $(pure (view (etype . unE) g))|],
                          (,) <$> notStrict <*> [t|Maybe $(pure (view (etype . unE) g))|] ]]
            _ -> []
      hopCons :: m [ConQ]
      hopCons = (concat . List.map doHopPair . toList) <$> (lensKeys v)
      doHopPair :: TGV -> [ConQ]
      doHopPair g =
          let Just vp = bestPathTypeName v in
          case (bestName v, bestName g) of
            (Just vn, Just gn) ->
                [normalC (asName (makeHopCon v g))
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
                (: []) <$> forestOfAlt (varE x) v (varP x, [(w, conP pcon [wildP], conE pcon)])
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
              (: []) <$> forestOfAlt (varE x) v (varP x, [(f, conP 'Path_First [wildP], [|Path_First|]), (s, conP 'Path_Second [wildP], [|Path_Second|])])
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do e <- expandType etyp >>= typeVertex
                  (: []) <$> forestOfAlt (varE x) v (varP x, [(e, conP 'Path_Just [wildP], [|Path_Just|])])
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do l <- expandType ltyp >>= typeVertex
                  r <- expandType rtyp >>= typeVertex
                  mapM (forestOfAlt (varE x) v) [(asP x (conP 'Left [wildP]), [(l, conP 'Path_Left [wildP], [|Path_Left|])]),
                                                 (asP x (conP 'Right [wildP]), [(r, conP 'Path_Right [wildP], [|Path_Right|])])]
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
        flds <- mapM (doNamedField tname) (zip vsts [1..])
        c <- forestOfAlt (varE x) v (asP x (recP cname []), flds)
        return [c]
      doCon tname x (NormalC cname sts) = do
#if 1
        pure []
#else
        flds <- mapM (doAnonField tname) (zip sts [1..])
        c <- forestOfAlt (varE x) v (asP x (recP cname []), flds)
        return [c]
#endif
      doCon tname x (InfixC lhs cname rhs) = do
#if 1
        pure []
#else
        flds <- mapM (doAnonField tname) (zip [lhs, rhs] [1..])
        c <- forestOfAlt (varE x) v (asP x (infixP wildP cname wildP), flds)
        return [c]
#endif

      doNamedField :: Name -> ((Name, Strict, Type), Int) -> m (TGVSimple, PatQ, ExpQ)
      doNamedField tname ((fname, _, ftype), _fpos) =
          do f <- expandType ftype >>= fieldVertex (tname, undefined, Right fname)
             let w = view vsimple f
             let Just pcname = makeFieldCon f
             return (w, conP (asName pcname) [wildP], asConQ pcname)

      doAnonField :: Name -> ((Strict, Type), Int) -> m (TGVSimple, PatQ, ExpQ)
      doAnonField tname ((_, ftype), fpos) =
          do f <- expandType ftype >>= fieldVertex (tname, undefined, Left fpos)
             let w = view vsimple f
             let Just pcname = makeFieldCon f
             return (w, conP (asName pcname) [wildP], asConQ pcname)

doPeekNodesOfOrder :: forall m a. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, HasName a) =>
                      TGVSimple -> Type -> PathCon a -> m [ClauseQ]
doPeekNodesOfOrder v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     k <- runQ $ newName "k"
     (: []) <$> forestOfAlt (varE x) v (varP x, [(w, conP (asName pcname) [varP k, wildP], [|$(asConQ pcname) $(varE k)|])])

doPeekNodesOfMap :: forall m a. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, HasName a) =>
                    TGVSimple -> Type -> PathCon a -> m [ClauseQ]
doPeekNodesOfMap v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- expandType wtyp >>= typeVertex :: m TGVSimple
     k <- runQ $ newName "k"
     (: []) <$> forestOfAlt (varE x) v (varP x, [(w, conP (asName pcname) [varP k, wildP], [|$(asConQ pcname) $(varE k)|])])

forestOfAlt :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>  ExpQ -> TGVSimple -> (PatQ, [(TGVSimple, PatQ, ExpQ)]) -> m ClauseQ
forestOfAlt x v (xpat, concs) =
    do fs <- mapM (forestOfConc x v) concs
       return $ clause [xpat] (normalB [| concat $(listE fs) |]) []

forestOfConc :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>  ExpQ -> TGVSimple -> (TGVSimple, PatQ, ExpQ) -> m ExpQ
forestOfConc x v (w, ppat, pcon) =
    do gs <- pathKeys w
       p <- runQ $ newName "p"
       let vn = maybe (error "forestOfConc") asName (bestTypeName v)
           wn = maybe (error "forestOfConc") asName (bestTypeName w)
       return [| concatMap
                   (\path -> case path of
                               $(asP p ppat) ->
                                   map (\y -> let vs = (peek y :: Forest (Peek $(asTypeQ w))) in
                                              Node ($(asConQ (makePeekCon (ModelType vn) (ModelType wn))) $(varE p) (if null vs then Just y else Nothing))
                                                   -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                                                   -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                                                   (forestMap (\v' -> $(caseE [|v'|] (concatMap (doGoal v w pcon) (Foldable.toList gs)))) vs))
                                       (toListOf (toLens $(varE p)) $x :: [$(asTypeQ w)])
                               _ -> [])
                   (pathsOf $x (undefined :: Proxy $(asTypeQ w)) :: [$(asTypeQ (makePathType (ModelType vn))) $(asTypeQ w)]) :: Forest (Peek $(asTypeQ v)) |]

doGoal :: TGVSimple -> TGVSimple -> ExpQ -> TGVSimple -> [MatchQ]
doGoal v w pcon g = do
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
