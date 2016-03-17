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
module Language.Haskell.TH.Path.Decs.PathStart (peekDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Char (isUpper, toUpper)
import Data.Foldable as Foldable
import Data.List as List (groupBy, map)
import Data.Proxy
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeFieldCon, makePathCon, makePathType,
                                        ModelType(ModelType))
import Language.Haskell.TH.Path.Core (PathStart(Peek, peek, hop), Paths(..), ToLens(toLens),
                                      Describe(describe), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doType, finishConc, finishEither, finishPair)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys', simplify, tgv, tgvSimple')
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

instanceD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> TypeQ -> m [DecQ] -> m ()
instanceD' cxt' typ decs =
    instanceD cxt' typ <$> decs >>= runQ >>= tell . (: [])

dataInstD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> Name -> [TypeQ] -> m [ConQ] -> [Name] -> m DecQ
dataInstD' cxt' name params cons supers =
    dataInstD cxt' name params <$> cons <*> pure supers

funD' :: (TypeGraphM m, MonadWriter [Dec] m) => Name -> m [ClauseQ] -> m DecQ
funD' name clauses = funD name <$> clauses

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGV -> m ()
peekDecs v =
    do (clauses :: [ClauseType]) <- execWriterT (peekClauses v)
       let (pcs, hcs, dcs) = partitionClauses clauses
       instanceD' (cxt []) (appT (conT ''PathStart) (asTypeQ v))
         (sequence
          [dataInstD' (cxt []) ''Peek [asTypeQ v]
                      ((concat .
                        List.map (\g -> [normalC (asName (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                                                 [(,) <$> notStrict <*> [t|FromTo $(asTypeQ v) $(asTypeQ g)|],
                                                  (,) <$> notStrict <*> [t|Maybe $(asTypeQ g)|] ]]) .
                        toList) <$> (pathKeys' v)) [''Eq, ''Show],
           funD' 'peek (case pcs of
                          [] -> pure [clause [wildP] (normalB [| [] |]) []]
                          _ -> pure pcs),
           funD' 'hop (case hcs of
                         [] -> pure [clause [wildP] (normalB [| [] |]) []]
                         _ -> pure hcs)])
       instanceD' (cxt []) (appT (conT ''Describe) [t|Peek $(asTypeQ v)|])
                  (pure [funD 'describe (case dcs of
                                           [] -> [clause [wildP, wildP] (normalB [|Nothing|]) []]
                                           _ -> dcs)])


isPathControl :: forall m. (TypeGraphM m, MonadWriter [ClauseType] m) => TGV -> Name -> Name -> Control m (TGV, PatQ, ExpQ) () ()
isPathControl v x wPathVar =
    let control :: Control m (TGV, PatQ, ExpQ) () ()
        control = isPathControl v x wPathVar in
    Control { _doSimple = pure ()
            , _doSelf = pure ()
            , _doView =
                \typ ->
                    do w <- tgv Nothing typ
                       let vPathConName = makePathCon (makePathType (ModelType (asName v))) "View"
                       alt <- _doConcs control wildP [(w, conP (asName vPathConName) [varP wPathVar], conE (asName vPathConName))]
                       _doAlts control [alt]
            , _doOrder =
                \_i typ ->
                    do w <- tgv Nothing typ
                       k <- runQ $ newName "_k"
                       finishConc control (w, conP 'Path_At [varP k, varP wPathVar], [|Path_At $(varE k)|])
            , _doMap =
                \_i typ ->
                    do w <- tgv Nothing typ
                       k <- runQ $ newName "_k"
                       finishConc control (w, conP 'Path_Look [varP k, varP wPathVar], [|Path_Look $(varE k)|])
            , _doList =
                \_e -> pure ()
            , _doPair =
                \ftyp styp ->
                    do f <- tgv Nothing ftyp
                       s <- tgv Nothing styp
                       finishPair control
                               (f, conP 'Path_First [varP wPathVar], [|Path_First|])
                               (s, conP 'Path_Second [varP wPathVar], [|Path_Second|])
            , _doMaybe =
                \typ ->
                    do w <- tgv Nothing typ
                       finishConc control (w, conP 'Path_Just [varP wPathVar], [|Path_Just|])
            , _doEither =
                \ltyp rtyp ->
                    do l <- tgv Nothing ltyp
                       r <- tgv Nothing rtyp
                       let lconc = (l, conP 'Path_Left [varP wPathVar], [|Path_Left|])
                           rconc = (r, conP 'Path_Right [varP wPathVar], [|Path_Right|])
                       finishEither control lconc rconc
            , _doField =
                \fld typ ->
                    do f <- tgvSimple' 12 v typ >>= tgv (Just fld)
                       let fieldPathConName = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
                       pure (f, conP (asName fieldPathConName) [varP wPathVar], asConQ fieldPathConName)
            , _doConcs =
                \xpat concs -> do
                  rs <- mapM (\conc@(w, ppat, pcon) ->
                                  do let liftPeek p node =
                                             pure [| concatMap
                                                         (\pth -> case pth of
                                                                     $(asP p ppat) ->
                                                                         map $node (toListOf (toLens $(varE p)) $(varE x) :: [$(asTypeQ w)])
                                                                     _ -> [])
                                                         (paths $(varE x) (undefined :: Proxy $(asTypeQ w))
                                                             {-:: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]-}) |]
                                     describeConc v wPathVar conc
                                     p <- runQ $ newName "_pp"
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
               TGV -> m ()
peekClauses v = do
  x <- runQ $ newName "_s"
  wPathVar <- runQ $ newName "_wp"
  doType (isPathControl v x wPathVar) v

concatMapQ :: [ExpQ] -> ExpQ
concatMapQ [] = [|mempty|]
concatMapQ [x] = x
concatMapQ xs = [|mconcat $(listE xs)|]

doHop :: forall m. (TypeGraphM m) => TGV -> TGV -> Name -> ExpQ -> m ExpQ
doHop v w p _pcon =
    pure [| \a -> Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) (Just a)) [] |]

doPeek :: forall m. (TypeGraphM m) => TGV -> TGV -> Name -> ExpQ -> m ExpQ
doPeek v w p pcon = do
  gs <- pathKeys' w
  let liftPeek = mkName "liftPeek"
  pure [| \a -> let f = peek a {-:: Forest (Peek $(asTypeQ w))-} in
                $(letE [funD liftPeek (map (doGoal v w pcon) (Foldable.toList gs))]
                       [|Node ($(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName w)))) $(varE p) (if null f then Just a else Nothing))
                              -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                              -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                              (forestMap $(varE liftPeek) f) |]) |]

doGoal :: TGV -> TGV -> ExpQ -> TGVSimple -> ClauseQ
doGoal v w pcon g =
    do z <- newName "z"
       q <- newName "q"
       clause [conP (asName (makePeekCon (ModelType (asName w)) (ModelType (asName g)))) [varP q, varP z]]
              (normalB [|$(asConQ (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                         (($pcon {- :: FromTo $(asTypeQ w) $(asTypeQ g) ->
                                       FromTo $(asTypeQ v) $(asTypeQ g) -}) $(varE q)) $(varE z)|])
              []

describeConc :: forall m. (TypeGraphM m, MonadWriter [ClauseType] m) =>
                TGV -> Name -> (TGV, PatQ, ExpQ) -> m ()
describeConc v wPathVar (w, ppat, _pcon) =
    do p <- runQ $ newName "_p"
       x <- runQ $ newName "_x"
       hasDescribeInstance <- (not . null) <$> reifyInstancesWithContext ''Describe [AppT (ConT ''Proxy) (asType w)]
       pathKeys' w >>= mapM_ (doGoal' p x hasDescribeInstance)
    where
      doGoal' :: Name -> Name -> Bool -> TGVSimple -> m ()
      doGoal' p x hasDescribeInstance g = do
        w' <- simplify w
        let PeekCon vn = makePeekCon (ModelType (asName v)) (ModelType (asName g))
            PeekCon wn = makePeekCon (ModelType (asName w)) (ModelType (asName g))
            -- Describe the next hop on the path.
            next =
                if w' == g
                then [|Nothing|]
                else [|describe $(lift (view (_2 . field) w)) ($(conE wn) $(varE wPathVar) undefined)|]
            custom =
                if hasDescribeInstance
                then [|describe $(lift (view (_2 . field) w)) (Proxy :: Proxy $(asTypeQ w))|]
                else [|Nothing|]
        tell [DescClause $
                clause
                  [wildP, conP vn [asP p ppat, varP x]]
                  (normalB [| maybe (Just $(lift (toDescription v))) Just (maybe $custom Just $next) |])
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
