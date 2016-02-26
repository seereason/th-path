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
module Language.Haskell.TH.Path.Decs.IsPathStart (peekDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Char (isUpper, toUpper)
import Data.Foldable as Foldable
import Data.Generics (everywhere, mkT)
import Data.List as List (groupBy, map)
import Data.Map as Map (fromList, lookup, Map)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        bestTypeName, makeFieldCon, makePathCon, makePathType,
                                        ModelType(ModelType), PathCon, PathCon(PathCon))
import Language.Haskell.TH.Path.Core (IsPathStart(Peek, peek, hop, describe'), HasPaths(..), ToLens(toLens),
                                      SelfPath, SinkType, Describe(describe),
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, pathKeys', tgv, tgvSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo)
import Language.Haskell.TH.TypeGraph.Vertex (TGV, field, TGVSimple)

newtype PeekType = PeekType {unPeekType :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs
newtype PeekCon = PeekCon {unPeekCon :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs_Markup

instance HasName PeekType where asName = unPeekType
instance HasType PeekType where asType = ConT . unPeekType
instance HasTypeQ PeekType where asTypeQ = conT . unPeekType
instance HasName PeekCon where asName = unPeekCon
instance HasCon PeekCon where asCon = ConE . unPeekCon
instance HasConQ PeekCon where asConQ = conE . unPeekCon

data ClauseType
    = PeekClause ClauseQ
    | HopClause ClauseQ
    | DescClause ClauseQ

partitionClauses :: [ClauseType] -> ([ClauseQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], []) xs
        where
          f (PeekClause c) (pcs, hcs, dcs) = (c : pcs, hcs, dcs)
          f (HopClause c) (pcs, hcs, dcs) = (pcs, c : hcs, dcs)
          f (DescClause c) (pcs, hcs, dcs) = (pcs, hcs, c : dcs)

makePeekCon :: (HasName s, HasName g) => ModelType s -> ModelType g -> PeekCon
makePeekCon (ModelType s) (ModelType g) = PeekCon (mkName ("Peek_" ++ nameBase (asName s) ++ "_" ++ nameBase (asName g)))

peekDecs :: forall m. (MonadWriter [Dec] m, ContextM m, MonadReaders TypeInfo m, MonadReaders TypeGraph m) =>
            TGVSimple -> m ()
peekDecs v =
    do (clauses :: [ClauseType]) <- execWriterT (peekClauses v)
       let (pcs, hcs, dcs) = partitionClauses clauses
       (cons :: [ConQ]) <- peekCons
       runQ (instanceD (cxt []) (appT (conT ''IsPathStart) (asTypeQ v))
               [dataInstD (cxt []) ''Peek [asTypeQ v] cons [''Eq, ''Show],
                funD 'peek (case pcs of
                              [] -> [clause [wildP] (normalB [| [] |]) []]
                              _ -> pcs),
                funD 'hop (case hcs of
                              [] -> [clause [wildP] (normalB [| [] |]) []]
                              _ -> hcs),
                funD 'describe' (case dcs of
                                   [] -> [clause [wildP] (normalB [| [] |]) []]
                                   _ -> dcs)
               ]) >>= tell . (: [])
    where
      peekCons :: m [ConQ]
      peekCons = (concat . List.map doPair . toList) <$> (pathKeys v)
      doPair :: TGVSimple -> [ConQ]
      doPair g = [normalC (asName (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                          [(,) <$> notStrict <*> [t|Path $(asTypeQ v) $(asTypeQ g)|],
                           (,) <$> notStrict <*> [t|Maybe $(asTypeQ g)|] ]]

peekClauses :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseType] m) =>
               TGVSimple -> m ()
peekClauses v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewType <- viewInstanceType (asType v)
     x <- runQ $ newName "x"
     case asType v of
       _ | selfPath -> return ()
         | simplePath -> return ()
         | isJust viewType ->
             do let wtyp = fromJust viewType
                let tname = bestTypeName v
                w <- tgvSimple wtyp >>= tgv Nothing
                let PathCon pcon = makePathCon (makePathType tname) "View"
                forestOfAlt (varE x) v (varP x, [(w, conP pcon [wildP], conE pcon)])
       typ -> doType typ []
    where
      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType (ConT tname) [_ityp, vtyp]
          | tname == ''Order =
              doPeekNodesOfOrder v vtyp (PathCon 'Path_At)
      doType ListT [_etyp] = error "list" {- return [clause [wildP] (normalB [|error "list"|]) []]-}
      doType (ConT tname) [_ktyp, vtyp]
          | tname == ''Map =
              doPeekNodesOfMap v vtyp (PathCon 'Path_Look)
      doType (TupleT 2) [ftyp, styp] =
          do x <- runQ $ newName "x"
             f <- tgvSimple ftyp >>= tgv Nothing -- (Just (''(,), '(,), Left 1))
             s <- tgvSimple styp >>= tgv Nothing -- (Just (''(,), '(,), Left 2))
             forestOfAlt (varE x) v (varP x, [(f, conP 'Path_First [wildP], [|Path_First|]), (s, conP 'Path_Second [wildP], [|Path_Second|])])
      doType (ConT tname) [etyp]
          | tname == ''Maybe =
              do x <- runQ $ newName "x"
                 e <- tgvSimple etyp >>= tgv Nothing -- (Just (''Maybe, 'Just, Left 1))
                 forestOfAlt (varE x) v (varP x, [(e, conP 'Path_Just [wildP], [|Path_Just|])])
      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do x <- runQ $ newName "x"
                 l <- tgvSimple ltyp >>= tgv Nothing -- (Just (''Either, 'Left, Left 1))
                 r <- tgvSimple rtyp >>= tgv Nothing -- (Just (''Either, 'Right, Left 1))
                 mapM_ (forestOfAlt (varE x) v) [(asP x (conP 'Left [wildP]), [(l, conP 'Path_Left [wildP], [|Path_Left|])]),
                                                 (asP x (conP 'Right [wildP]), [(r, conP 'Path_Right [wildP], [|Path_Right|])])]
      doType (ConT tname) tps = doName tps tname
      doType _ _ = return ()

      doName :: [Type] -> Name -> m ()
      doName tps tname = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m ()
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo _ _ = return ()
      doDec :: [Type] -> Dec -> m ()
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx tname binds cons _supers) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
        runQ (newName "x") >>= doCons bindings tname cons
      doDec _tps dec = error $ "Unexpected dec: " ++ pprint dec

      doCons :: Map Name Type -> Name -> [Con] -> Name -> m ()
      doCons _bindings _tname [] _x = error "No constructors"
      doCons bindings tname cons x = mapM_ (doCon bindings tname x) cons

      doCon :: Map Name Type -> Name -> Name -> Con -> m ()
      doCon bindings tname x (ForallC _ _ con) = doCon bindings tname x con

      -- Each constructor turns into a clause of the peek function
      doCon bindings tname x (RecC cname vsts) = do
        flds <- mapM (doNamedField bindings tname cname) (zip vsts [1..])
        forestOfAlt (varE x) v (asP x (recP cname []), flds)
      doCon bindings tname x (NormalC cname sts) = do
#if 1
        return ()
#else
        flds <- mapM (doAnonField bindings tname cname) (zip sts [1..])
        forestOfAlt (varE x) v (asP x (recP cname []), flds)
#endif
      doCon bindings tname x (InfixC lhs cname rhs) = do
#if 1
        return ()
#else
        flds <- mapM (doAnonField bindings tname cname) (zip [lhs, rhs] [1..])
        c <- forestOfAlt (varE x) v (asP x (infixP wildP cname wildP), flds)
        return [c]
#endif

      doNamedField :: Map Name Type -> Name -> Name -> ((Name, Strict, Type), Int) -> m (TGV, PatQ, ExpQ)
      doNamedField bindings tname cname ((fname, _, ftype), _fpos) =
          do let subst t@(VarT name) = maybe t id (Map.lookup name bindings)
                 subst t = t
             let ftype' = (everywhere (mkT subst) ftype)
             f <- tgvSimple ftype' >>= tgv (Just (tname, cname, Right fname))
             let pcname = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
             return (f, conP (asName pcname) [wildP], asConQ pcname)

      doAnonField :: Map Name Type -> Name -> Name -> ((Strict, Type), Int) -> m (TGV, PatQ, ExpQ)
      doAnonField bindings tname cname ((_, ftype), fpos) =
          do let subst t@(VarT name) = maybe t id (Map.lookup name bindings)
                 subst t = t
             let ftype' = (everywhere (mkT subst) ftype)
             f <- tgvSimple ftype' >>= tgv (Just (tname, cname, Left fpos))
             let pcname = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
             return (f, conP (asName pcname) [wildP], asConQ pcname)

doPeekNodesOfOrder :: forall m a. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, HasName a, MonadWriter [ClauseType] m) =>
                      TGVSimple -> Type -> PathCon a -> m ()
doPeekNodesOfOrder v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- tgvSimple wtyp >>= tgv Nothing
     k <- runQ $ newName "k"
     forestOfAlt (varE x) v (varP x, [(w, conP (asName pcname) [varP k, wildP], [|$(asConQ pcname) $(varE k)|])])

doPeekNodesOfMap :: forall m a. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, HasName a, MonadWriter [ClauseType] m) =>
                    TGVSimple -> Type -> PathCon a -> m ()
doPeekNodesOfMap v wtyp pcname =
  do x <- runQ $ newName "x"
     w <- tgvSimple wtyp >>= tgv Nothing
     k <- runQ $ newName "k"
     forestOfAlt (varE x) v (varP x, [(w, conP (asName pcname) [varP k, wildP], [|$(asConQ pcname) $(varE k)|])])

forestOfAlt :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseType] m) =>
               ExpQ -> TGVSimple -> (PatQ, [(TGV, PatQ, ExpQ)]) -> m ()
forestOfAlt x v (xpat, concs) =
    do hfs <- mapM (forestOfConc True x v) concs
       pfs <- mapM (forestOfConc False x v) concs
       tell [PeekClause $ clause [xpat] (normalB [| concat $(listE pfs) |]) [],
             HopClause $ clause [xpat] (normalB [| concat $(listE hfs) |]) []]
       mapM_ (describeConc x v xpat) concs

forestOfConc :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
                Bool -> ExpQ -> TGVSimple -> (TGV, PatQ, ExpQ) -> m ExpQ
forestOfConc hop x v (w, ppat, pcon) =
    do gs <- pathKeys' w
       p <- runQ $ newName "p"
       return [| concatMap
                   (\path -> case path of
                               $(asP p ppat) ->
                                  map (\w' ->
                                         $(if hop
                                           then [| Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w))))
                                                         $(varE p)
                                                         (Just w')) [] |]
                                           else [| let wf = $(if hop then [| [] |] else [| (peek w' :: Forest (Peek $(asTypeQ w))) |]) in
                                                   Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w))))
                                                         $(varE p)
                                                         (if null wf then Just w' else Nothing))
                                                        -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                                                        -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                                                        (forestMap (\wp -> $(caseE [|wp|] (concatMap (doGoal v w pcon) (Foldable.toList gs)))) wf) |]))
                                       (toListOf (toLens $(varE p)) $x :: [$(asTypeQ w)])
                               _ -> [])
                   (pathsOf $x (undefined :: Proxy $(asTypeQ w)) :: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]) :: Forest (Peek $(asTypeQ v)) |]

describeConc :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseType] m) =>
                ExpQ -> TGVSimple -> PatQ -> (TGV, PatQ, ExpQ) -> m ()
describeConc x v xpat (w, ppat, pcon) =
    do gs <- pathKeys' w
       p <- runQ $ newName "p"
       x <- runQ $ newName "x"
       ppat' <- runQ ppat
       pcon' <- runQ pcon
       hasDescribeInstance <- (not . null) <$> reifyInstancesWithContext ''Describe [asType w]
       let PeekCon n = makePeekCon (ModelType (asName v)) (ModelType (asName w))
       tell [DescClause $ clause [conP n [asP p ppat, varP x]] (normalB (case hasDescribeInstance of
                                                                           True -> [|fromMaybe $(lift (toDescription w))
                                                                                               (describe (Proxy :: Proxy $(asTypeQ w)) $(lift (view (_2 . field) w)))|]
                                                                           False -> lift (toDescription w))) []]

doGoal :: TGVSimple -> TGV -> ExpQ -> TGVSimple -> [Q Match]
doGoal v w pcon g =
    [newName "z" >>= \z ->
     newName "q" >>= \q ->
     match (conP (asName (makePeekCon (ModelType (asName w)) (ModelType (asName g)))) [varP q, varP z])
           (normalB [|$(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                       (($pcon :: Path $(asTypeQ w) $(asTypeQ g) ->
                                  Path $(asTypeQ v) $(asTypeQ g)) $(varE q)) $(varE z)|])
           []]

toDescription :: TGV -> String
toDescription w =
    camelWords $
    case view (_2 . field) w of
      Just (_, _, Right fname) -> nameBase fname
      Just (_, _, Left fpos) -> nameBase (asName w) ++ "[" ++ show fpos ++ "]"
      Nothing -> nameBase (asName w)

-- | Convert a camel case string (no whitespace) into a natural
-- language looking phrase:
--   camelWords3 "aCamelCaseFOObar123" -> "A Camel Case FOObar123"
camelWords :: String -> String
camelWords s =
    case groupBy (\ a b -> isUpper a == isUpper b) (dropWhile (== '_') s) of -- "aCamelCaseFOObar123"
      (x : xs) -> concat $ capitalize x : map (\ (c : cs) -> if isUpper c then ' ' : c : cs else c : cs) xs
      [] -> ""

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c) : cs
