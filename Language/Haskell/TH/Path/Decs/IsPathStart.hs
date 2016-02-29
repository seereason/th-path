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
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Char (isUpper, toUpper)
import Data.Foldable as Foldable
import Data.Generics (Data, everywhere, mkT)
import Data.List as List (groupBy, map)
import Data.Map as Map (fromList, lookup, Map)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        bestTypeName, makeFieldCon, makePathCon, makePathType,
                                        ModelType(ModelType))
import Language.Haskell.TH.Path.Core (IsPathStart(Peek, peek, hop, describe'), HasPaths(..), ToLens(toLens),
                                      SelfPath, SinkType, Describe(describe),
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
-- import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, pathKeys', tgv, tgvSimple)
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

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
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

peekClauses :: forall m conc alt. (TypeGraphM m, MonadWriter [ClauseType] m, conc ~ (TGV, PatQ, ExpQ), alt ~ (PatQ, [conc])) =>
               TGVSimple -> m ()
peekClauses v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case asType v of
       _ | selfPath -> pure ()
         | simplePath -> pure ()
         | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                let tname = bestTypeName v
                w <- tgvSimple viewtyp >>= tgv Nothing
                let pcname = makePathCon (makePathType tname) "View"
                doAlt (wildP, [(w, conP (asName pcname) [wildP], conE (asName pcname))])
       typ -> doType typ []
    where
      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType (ConT tname) [_ityp, vtyp]
          | tname == ''Order =
              do w <- tgvSimple vtyp >>= tgv Nothing
                 k <- runQ $ newName "k"
                 doAlt (wildP, [(w, conP 'Path_At [varP k, wildP], [|Path_At $(varE k)|])])
      doType (ConT tname) [_ktyp, vtyp]
          | tname == ''Map =
              do w <- tgvSimple vtyp >>= tgv Nothing
                 k <- runQ $ newName "k"
                 doAlt (wildP, [(w, conP 'Path_Look [varP k, wildP], [|Path_Look $(varE k)|])])
      doType (TupleT 2) [ftyp, styp] =
          do f <- tgvSimple ftyp >>= tgv Nothing -- (Just (''(,), '(,), Left 1))
             s <- tgvSimple styp >>= tgv Nothing -- (Just (''(,), '(,), Left 2))
             doAlt (wildP, [(f, conP 'Path_First [wildP], [|Path_First|]), (s, conP 'Path_Second [wildP], [|Path_Second|])])
      doType (ConT tname) [etyp]
          | tname == ''Maybe =
              do e <- tgvSimple etyp >>= tgv Nothing -- (Just (''Maybe, 'Just, Left 1))
                 doAlt (wildP, [(e, conP 'Path_Just [wildP], [|Path_Just|])])
      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do l <- tgvSimple ltyp >>= tgv Nothing -- (Just (''Either, 'Left, Left 1))
                 r <- tgvSimple rtyp >>= tgv Nothing -- (Just (''Either, 'Right, Left 1))
                 mapM_ doAlt [(conP 'Left [wildP], [(l, conP 'Path_Left [wildP], [|Path_Left|])]),
                                   (conP 'Right [wildP], [(r, conP 'Path_Right [wildP], [|Path_Right|])])]
      doType (ConT tname) tps = doName tps tname
      doType ListT [_etyp] = error "list" {- tell [clause [wildP] (normalB [|error "list"|]) []]-}
      doType _ _ = return ()

      doName :: [Type] -> Name -> m ()
      doName tps tname = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m ()
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo _ _ = pure ()
      doDec :: [Type] -> Dec -> m ()
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx tname binds cons _supers) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
            subst = substG bindings
        doCons subst tname cons
      doDec _tps dec = error $ "Unexpected dec: " ++ pprint dec

      doCons :: (Type -> Type) -> Name -> [Con] -> m ()
      doCons _subst _tname [] = error "No constructors"
      doCons subst tname cons = mapM_ (doCon subst tname) cons

      doCon :: (Type -> Type) -> Name -> Con -> m ()
      doCon subst tname (ForallC _ _ con) = doCon subst tname con
      doCon subst tname (RecC cname vsts) = do
        flds <- mapM (doNamedField subst tname cname) (zip vsts [1..])
        doAlt (recP cname [], flds)
      doCon _subst _tname (NormalC _cname _sts) = do
#if 1
        pure ()
#else
        flds <- mapM (doAnonField bindings tname cname) (zip sts [1..])
        doAlt (recP cname [], flds)
#endif
      doCon _bindings _tname (InfixC _lhs _cname _rhs) = do
#if 1
        pure ()
#else
        flds <- mapM (doAnonField bindings tname cname) (zip [lhs, rhs] [1..])
        c <- doAlt (infixP wildP cname wildP, flds)
        return [c]
#endif

      doNamedField :: (Type -> Type) -> Name -> Name -> ((Name, Strict, Type), Int) -> m conc
      doNamedField subst tname cname ((fname, _, ftype), _fpos) =
          do let ftype' = subst ftype
             f <- tgvSimple ftype' >>= tgv (Just (tname, cname, Right fname))
             let pcname = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
             return (f, conP (asName pcname) [wildP], asConQ pcname)

      doAnonField :: (Type -> Type) -> Name -> Name -> ((Strict, Type), Int) -> m conc
      doAnonField subst tname cname ((_, ftype), fpos) =
          do let ftype' = subst ftype
             f <- tgvSimple ftype' >>= tgv (Just (tname, cname, Left fpos))
             let pcname = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
             return (f, conP (asName pcname) [wildP], asConQ pcname)

      doAlt :: alt -> m ()
      doAlt (xpat, concs) = do
        s <- runQ $ newName "s"
        mapM (doConc s) concs >>= finishAlt s xpat
          where
            finishAlt :: Name -> PatQ -> [(ExpQ, ExpQ)] -> m ()
            finishAlt s xpat rs = do
                let (hfs, pfs) = unzip rs
                tell [PeekClause $ clause [asP s xpat] (normalB [| $(concatMapQ pfs) :: Forest (Peek $(asTypeQ v)) |]) [],
                      HopClause $ clause [asP s xpat] (normalB [| $(concatMapQ hfs) :: Forest (Peek $(asTypeQ v)) |]) []]

      -- Build an expression that returns a Forest (Peek s) for paths
      -- that have a hop from v to w.
      doConc :: Name -> conc -> m (ExpQ, ExpQ)
      doConc s conc@(w, ppat, pcon) =
          do describeConc v conc
             p <- runQ $ newName "p"
             hf <- doHop v w p pcon >>= liftPeek p
             pf <- doPeek v w p pcon >>= liftPeek p
             return (hf, pf)
          where
            liftPeek p node =
                return [| concatMap
                         (\path -> case path of
                                     $(asP p ppat) ->
                                        map $node (toListOf (toLens $(varE p)) $(varE s) :: [$(asTypeQ w)])
                                     _ -> [])
                         (pathsOf $(varE s) (undefined :: Proxy $(asTypeQ w)) {-:: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]-}) |]

concatMapQ :: [ExpQ] -> ExpQ
concatMapQ [] = [|mempty|]
concatMapQ [x] = x
concatMapQ xs = [|mconcat $(listE xs)|]

doHop :: forall m. (TypeGraphM m) => TGVSimple -> TGV -> Name -> ExpQ -> m ExpQ
doHop v w p _pcon =
    pure [| \a -> Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) (Just a)) [] |]

doPeek :: forall m. (TypeGraphM m) => TGVSimple -> TGV -> Name -> ExpQ -> m ExpQ
doPeek v w p pcon = do
  gs <- pathKeys' w
  let liftPeek = mkName "liftPeek"
  pure [| \a -> let f = peek a {-:: Forest (Peek $(asTypeQ w))-} in
                $(letE [funD liftPeek (map (doGoal v w pcon) (Foldable.toList gs))]
                       [|Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) (if null f then Just a else Nothing))
                              -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                              -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                              (forestMap $(varE liftPeek) f) |]) |]

doGoal :: TGVSimple -> TGV -> ExpQ -> TGVSimple -> ClauseQ
doGoal v w pcon g =
    do z <- newName "z"
       q <- newName "q"
       clause [conP (asName (makePeekCon (ModelType (asName w)) (ModelType (asName g)))) [varP q, varP z]]
              (normalB [|$(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                         (($pcon {- :: Path $(asTypeQ w) $(asTypeQ g) ->
                                     Path $(asTypeQ v) $(asTypeQ g) -}) $(varE q)) $(varE z)|])
              []

describeConc :: forall m. (TypeGraphM m, MonadWriter [ClauseType] m) =>
                TGVSimple -> (TGV, PatQ, ExpQ) -> m ()
describeConc v (w, ppat, _pcon) =
    do p <- runQ $ newName "p"
       x <- runQ $ newName "x"
       hasDescribeInstance <- (not . null) <$> reifyInstancesWithContext ''Describe [asType w]
       let PeekCon n = makePeekCon (ModelType (asName v)) (ModelType (asName w))
       tell [DescClause $ clause [conP n [asP p ppat, varP x]] (normalB (case hasDescribeInstance of
                                                                           True -> [|fromMaybe $(lift (toDescription w))
                                                                                               (describe (Proxy :: Proxy $(asTypeQ w)) $(lift (view (_2 . field) w)))|]
                                                                           False -> lift (toDescription w))) []]

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

substG :: Data a => Map Name Type -> a -> a
substG bindings typ = everywhere (mkT (subst1 bindings)) typ

subst1 :: Map Name Type -> Type -> Type
subst1 bindings t@(VarT name) = maybe t id (Map.lookup name bindings)
subst1 _ t = t

mconcatQ :: [ExpQ] -> ExpQ
mconcatQ [] = [| mempty |]
mconcatQ [x] = x
mconcatQ xs = [|mconcat $(listE xs)|]
