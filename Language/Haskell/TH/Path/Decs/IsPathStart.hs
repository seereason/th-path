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
import Data.List as List (groupBy, map)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeFieldCon, makePathCon, makePathType,
                                        ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (IsPathStart(Peek, peek, hop, describe'), HasPaths(..), ToLens(toLens),
                                      Describe(describe), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doType, finishEither, finishPair)
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
       tells [instanceD (cxt []) (appT (conT ''IsPathStart) (asTypeQ v))
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
               ]]
    where
      peekCons :: m [ConQ]
      peekCons = (concat . List.map doPair . toList) <$> (pathKeys v)
      doPair :: TGVSimple -> [ConQ]
      doPair g = [normalC (asName (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                          [(,) <$> notStrict <*> [t|Path $(asTypeQ v) $(asTypeQ g)|],
                           (,) <$> notStrict <*> [t|Maybe $(asTypeQ g)|] ]]

isPathControl :: forall m. (TypeGraphM m, MonadWriter [ClauseType] m) => TGVSimple -> Name -> Control m (TGV, PatQ, ExpQ) () ()
isPathControl v x =
    let control :: Control m (TGV, PatQ, ExpQ) () ()
        control = isPathControl v x in
    Control { _doSimple = pure ()
            , _doSelf = pure ()
            , _doView =
                \w ->
                    do let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                       alt <- _doConcs control wildP [(w, conP (asName pcname) [wildP], conE (asName pcname))]
                       _doAlts control [alt]
            , _doOrder =
                \_i w ->
                    do k <- runQ $ newName "k"
                       pure (w, conP 'Path_At [varP k, wildP], [|Path_At $(varE k)|])
            , _doMap =
                \_i w ->
                    do k <- runQ $ newName "k"
                       pure (w, conP 'Path_Look [varP k, wildP], [|Path_Look $(varE k)|])
            , _doPair =
                \f s ->
                    finishPair control
                               (f, conP 'Path_First [wildP], [|Path_First|])
                               (s, conP 'Path_Second [wildP], [|Path_Second|])
            , _doMaybe =
                \w ->
                    pure (w, conP 'Path_Just [wildP], [|Path_Just|])
            , _doEither =
                \ltyp rtyp ->
                    do l <- tgvSimple ltyp >>= tgv Nothing
                       r <- tgvSimple rtyp >>= tgv Nothing
                       let lconc = (l, conP 'Path_Left [wildP], [|Path_Left|])
                           rconc = (r, conP 'Path_Right [wildP], [|Path_Right|])
                       finishEither control lconc rconc
            , _doField =
                \f ->
                    do let pcname = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
                       pure (f, conP (asName pcname) [wildP], asConQ pcname)
            , _doConcs =
                \xpat concs -> do
                  rs <- mapM (\conc@(w, ppat, pcon) ->
                                  do let liftPeek p node =
                                             pure [| concatMap
                                                         (\path -> case path of
                                                                     $(asP p ppat) ->
                                                                         map $node (toListOf (toLens $(varE p)) $(varE x) :: [$(asTypeQ w)])
                                                                     _ -> [])
                                                         (pathsOf $(varE x) (undefined :: Proxy $(asTypeQ w))
                                                             {-:: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]-}) |]
                                     describeConc v conc
                                     p <- runQ $ newName "p"
                                     hf <- doHop v w p pcon >>= liftPeek p
                                     pf <- doPeek v w p pcon >>= liftPeek p
                                     return (hf, pf))
                             concs
                  let (hfs, pfs) = unzip rs
                  tell [PeekClause $ clause [asP' x xpat] (normalB [| $(concatMapQ pfs) :: Forest (Peek $(asTypeQ v)) |]) [],
                        HopClause $ clause [asP' x xpat] (normalB [| $(concatMapQ hfs) :: Forest (Peek $(asTypeQ v)) |]) []]
            , _doSyn =
                \_tname _typ -> pure ()
            , _doAlts = \_ -> pure ()
            }

peekClauses :: forall m conc alt. (TypeGraphM m, MonadWriter [ClauseType] m, conc ~ (TGV, PatQ, ExpQ), alt ~ (PatQ, [conc])) =>
               TGVSimple -> m ()
peekClauses v = do
  x <- runQ (newName "s")
  doType (isPathControl v x) (asType v)

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
