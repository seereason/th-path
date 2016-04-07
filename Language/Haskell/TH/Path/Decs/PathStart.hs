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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.PathStart (peekDecs, makePeekCon, makeUPeekCon) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Map as Map (toList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Proxy
import Data.Set as Set (Set, toList)
import Data.Tree (Tree(Node), Forest)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeFieldCon, makeUFieldCon, makePathCon, makePathType, makeUPathType, ModelType(ModelType))
import Language.Haskell.TH.Path.Core (camelWords, fieldStrings, IdPath(idPath), PathStart(..), Paths(..), ToLens(toLens),
                                      Describe(describe'), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap, U(u, unU'))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode, finishConcs)
import Language.Haskell.TH.Syntax (liftString)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, pathKeys', tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (TGV, field, TGVSimple)

newtype PeekType = PeekType {unPeekType :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs
newtype PeekCon = PeekCon {unPeekCon :: Name} deriving (Eq, Ord, Show) -- e.g. Peek_AbbrevPairs_Markup

instance HasName PeekType where asName = unPeekType
instance HasType PeekType where asType = ConT . unPeekType
instance HasTypeQ PeekType where asTypeQ = conT . unPeekType
instance HasName PeekCon where asName = unPeekCon
instance HasCon PeekCon where asCon = ConE . unPeekCon
instance HasConQ PeekCon where asConQ = conE . unPeekCon

data PathConc
    = PathConc
      { _concTGV :: TGV
      , _concPat :: PatQ
      , _concUPat :: PatQ
      , _concExp :: ExpQ
      , _concUCon :: ExpQ -> ExpQ
      , _concLift :: ExpQ
      }

data WriterType
    = TreeClause ClauseQ
    | RowClause ClauseQ
    | DescClause ClauseQ
    | UPathField StrictTypeQ
    | UPathClause ClauseQ
    | UPathRowClause ClauseQ
    | UPathTreeClause ClauseQ

partitionClauses :: [WriterType] -> ([ClauseQ], [ClauseQ], [ClauseQ], [StrictTypeQ], [ClauseQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], [], [], [], [], []) xs
        where
          f (TreeClause x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (x : tcs, rcs, dcs, upfs, upcs, uprcs, uptcs)
          f (RowClause x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (tcs, x : rcs, dcs, upfs, upcs, uprcs, uptcs)
          f (DescClause x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (tcs, rcs, x : dcs, upfs, upcs, uprcs, uptcs)
          f (UPathField x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (tcs, rcs, dcs, x : upfs, upcs, uprcs, uptcs)
          f (UPathClause x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (tcs, rcs, dcs, upfs, x : upcs, uprcs, uptcs)
          f (UPathRowClause x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (tcs, rcs, dcs, upfs, upcs, x : uprcs, uptcs)
          f (UPathTreeClause x) (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = (tcs, rcs, dcs, upfs, upcs, uprcs, x : uptcs)

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
peekDecs utype v =
    do (clauses :: [WriterType]) <- execWriterT (peekClauses utype v)
       let (tcs, rcs, dcs, upfs, upcs, uprcs, uptcs) = partitionClauses clauses
       instanceD' (cxt []) [t|PathStart $utype $(asTypeQ v)|]
         (sequence
          [dataInstD' (cxt []) ''Peek [utype, asTypeQ v]
                      ((concat .
                        List.map (\g -> [normalC (asName (makePeekCon (ModelType (asName v)) (ModelType (asName g))))
                                                 [(,) <$> notStrict <*> [t|Path $utype $(asTypeQ v) $(asTypeQ g)|],
                                                  (,) <$> notStrict <*> [t|Maybe $(asTypeQ g)|] ]]) .
                        Foldable.toList) <$> (pathKeys v)) [''Eq, ''Show],
           pure (dataInstD (cxt []) ''UPeek [utype, asTypeQ v]
                           [normalC (asName (makeUPeekCon (ModelType (asName v))))
                                    [strictType notStrict [t|UPath $utype $(asTypeQ v)|],
                                     strictType notStrict [t|Maybe $utype|]]]
                           [''Eq, ''Show]),
           pure (funD 'upeekCons [clause [] (normalB (conE (asName (makeUPeekCon (ModelType (asName v)))))) []]),
           pure (funD 'upeekPath [newName "p" >>= \p -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [varP p, wildP]] (normalB (varE p)) []]),
           pure (funD 'upeekValue [newName "x" >>= \x -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [wildP, varP x]] (normalB (varE x)) []]),
           funD' 'peekTree (case tcs of
                              [] -> pure [clause [wildP, wildP] (normalB [| [] |]) []]
                              _ -> pure tcs),
           funD' 'peekRow (case rcs of
                             [] -> pure [clause [wildP, wildP] (normalB [| [] |]) []]
                             _ -> pure rcs),
           pure (tySynInstD ''UPath (tySynEqn [utype, asTypeQ v] (conT (mkName ("UPath_" ++ nameBase (asName v)))))),
           funD' 'upaths (case upcs of
                            [] -> pure [newName "r" >>= \r -> clause [wildP, wildP, varP r, wildP] (normalB (varE r)) []]
                            _ -> pure upcs),
           funD' 'upathRow (case uprcs of
                              [] -> pure [newName "r" >>= \r -> clause [wildP, wildP] (normalB [| [] |]) []]
                              _ -> pure uprcs),
           funD' 'upathTree (case uptcs of
                               [] -> pure [newName "r" >>= \r -> clause [wildP, wildP] (normalB [| Node idPath [] |]) []]
                               _ -> pure uptcs)])
       instanceD' (cxt []) [t|Describe (Peek $utype $(asTypeQ v))|]
                  (pure [funD 'describe' (case dcs of
                                           [] -> [clause [wildP, wildP] (normalB [|Nothing|]) []]
                                           _ -> dcs ++ [newName "_f" >>= \f -> clause [varP f, wildP] (normalB [|describe' $(varE f) (Proxy :: Proxy $(asTypeQ v))|]) []])])
       proxyV <- runQ $ [t|Proxy $(asTypeQ v)|]
       hasCustomInstance <- (not . null) <$> reifyInstancesWithContext ''Describe [proxyV]
       when (not hasCustomInstance)
            (instanceD' (cxt []) [t|Describe (Proxy $(asTypeQ v))|]
               (pure [newName "_f" >>= \f ->
                      funD 'describe'
                        [clause [varP f, wildP]
                           (normalB [| case $(varE f) of
                                         Nothing -> Just $(liftString (camelWords (nameBase (asName v))))
                                         Just (_tname, _cname, Right fname) -> Just (camelWords fname)
                                         Just (_tname, cname, Left fpos) -> Just (camelWords $ cname ++ "[" ++ show fpos ++ "]")
                                     |]) []]]))

makePeekCon :: (HasName s, HasName g) => ModelType s -> ModelType g -> PeekCon
makePeekCon (ModelType s) (ModelType g) = PeekCon (mkName ("Peek_" ++ nameBase (asName s) ++ "_" ++ nameBase (asName g)))

makeUPeekCon :: (HasName s) => ModelType s -> PeekCon
makeUPeekCon (ModelType s) = PeekCon (mkName ("UPeek_" ++ nameBase (asName s)))

instanceD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> TypeQ -> m [DecQ] -> m ()
instanceD' cxt' typ decs =
    instanceD cxt' typ <$> decs >>= runQ >>= tell . (: [])

dataInstD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> Name -> [TypeQ] -> m [ConQ] -> [Name] -> m DecQ
dataInstD' cxt' name params cons supers =
    dataInstD cxt' name params <$> cons <*> pure supers

funD' :: (TypeGraphM m, MonadWriter [Dec] m) => Name -> m [ClauseQ] -> m DecQ
funD' name clauses = funD name <$> clauses

peekClauses :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) =>
               TypeQ -> TGVSimple -> m ()
peekClauses utype v = do
  x <- runQ $ newName "_s"
  wPathVar <- runQ $ newName "_wp"
  doNode (pathControl utype v x wPathVar) v

pathControl :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) => TypeQ -> TGVSimple -> Name -> Name -> Control m PathConc () ()
pathControl utype v _x wPathVar = do
  let control :: Control m PathConc () ()
      control = pathControl utype v _x wPathVar in
    Control
    { _doSimple = pure ()
    , _doSelf = pure ()
    , _doView =
        \typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               let vPathConName = makePathCon (makePathType (ModelType (asName v))) "View"
               let vUPathConName = makePathCon (makeUPathType (ModelType (asName v))) "View"
               finishConcs control [(varP x, [PathConc w (conP (asName vPathConName) [varP wPathVar]) (conP (asName vUPathConName) [varP wPathVar]) (conE (asName vUPathConName)) (\p -> [| [$(conE (asName vUPathConName)) $p] |]) [| [$(conE (asName vUPathConName))] |]])]
    , _doOrder =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               k <- runQ $ newName "_k"
               finishConcs control [(varP x, [PathConc w (conP 'Path_At [varP k, varP wPathVar]) (conP 'Path_At [varP k, varP wPathVar]) [|Path_At $(varE k)|]
                                                        (\p -> [|map (\(k, _v) -> Path_At k $p) (toPairs $(varE x))|])
                                                        [|map (\(k, _v) -> Path_At k) (toPairs $(varE x))|] ])]
    , _doMap =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               k <- runQ $ newName "_k"
               finishConcs control [(varP x, [PathConc w (conP 'Path_Look [varP k, varP wPathVar]) (conP 'Path_Look [varP k, varP wPathVar]) [|Path_Look $(varE k)|]
                                                       (\p -> [|map (\(k, _v) -> Path_Look k $p) (Map.toList $(varE x))|])
                                                       [|map (\(k, _v) -> Path_Look k) (Map.toList $(varE x))|]
                                             ])]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \ftyp styp ->
            do f <- tgv Nothing ftyp
               s <- tgv Nothing styp
               finishConcs control
                       [(wildP, [PathConc f (conP 'Path_First [varP wPathVar]) (conP 'Path_First [varP wPathVar]) [|Path_First|] (\p -> [|[Path_First $p]|]) [| [Path_First] |],
                                 PathConc s (conP 'Path_Second [varP wPathVar]) (conP 'Path_Second [varP wPathVar]) [|Path_Second|] (\p -> [|[Path_Second $p]|]) [| [Path_Second] |] ])]
    , _doMaybe =
        \typ ->
            do w <- tgv Nothing typ
               finishConcs control [(wildP, [PathConc w (conP 'Path_Just [varP wPathVar]) (conP 'Path_Just [varP wPathVar]) [|Path_Just|] (\p -> [|[Path_Just $p]|]) [|[Path_Just]|] ])]
    , _doEither =
        \ltyp rtyp ->
            do l <- tgv Nothing ltyp
               r <- tgv Nothing rtyp
               finishConcs control [(conP 'Left [wildP], [PathConc l (conP 'Path_Left [varP wPathVar]) (conP 'Path_Left [varP wPathVar]) [|Path_Left|] (\p -> [|[Path_Left $p]|]) [|[Path_Left]|]]),
                                    (conP 'Right [wildP], [PathConc r (conP 'Path_Right [varP wPathVar]) (conP 'Path_Right [varP wPathVar]) [|Path_Right|] (\p -> [|[Path_Right $p]|]) [|[Path_Right]|] ])]
    , _doField =
        \fld typ ->
            do f <- tgvSimple' typ >>= tgv (Just fld)
               let fieldPathConName = maybe (error $ "Not a field: " ++ show f) id (makeFieldCon f)
               let fieldUPathConName = maybe (error $ "Not a field: " ++ show f) id (makeUFieldCon f)
               pure (PathConc f (conP (asName fieldPathConName) [varP wPathVar]) (conP (asName fieldUPathConName) [varP wPathVar]) (asConQ fieldUPathConName) (\p -> [|[$(asConQ fieldUPathConName) $p]|]) [|[$(asConQ fieldUPathConName)]|])
    , _doConcs =
        \xpat concs -> do
          x <- runQ $ newName "_xconc"
          rs <- mapM (\conc@(PathConc w ppat upat ucon ucons ulift) ->
                          do describeConc utype v wPathVar conc
                             p <- runQ $ newName "p"
                             let hf = peekList' utype (varE x) p v w upat
                                        [| \a -> upeekCons $(varE p) (Just a :: Maybe $utype) |]
                             gs <- pathKeys' w
                             let pf = peekList' utype (varE x) p v w upat
                                        [| \a -> -- Get the peek forest for the w value
                                                 let wtree = peekTree Proxy (fromJust (unU' a) :: $(asTypeQ w)) :: Forest (UPeek $utype $(asTypeQ w)) in
                                                 Node (upeekCons $(varE p) (if null wtree then Just a else Nothing))
                                                      -- Build a function with type such as Peek_AbbrevPair -> Peek_AbbrevPairs, so we
                                                      -- can lift the forest of type AbbrevPair to be a forest of type AbbrevPairs.
                                                      (forestMap ($(liftPeekE utype v w ucon gs)) wtree) |]
                             return (hf, pf))
                     concs
          let (hfs, pfs) = unzip rs
          tell [TreeClause $ clause [conP 'Proxy [], asP' x xpat] (normalB [| $(concatMapQ pfs) :: Forest (UPeek $utype $(asTypeQ v)) |]) [],
                RowClause $ clause [conP 'Proxy [], asP' x xpat] (normalB [| $(concatMapQ hfs) :: [UPeek $utype $(asTypeQ v)] |]) [],
                UPathClause $
                  do f <- newName "_f"
                     r0 <- newName "r0"
                     let upathss = map (\(PathConc _ _ _ _ x _) -> x [|idPath|]) concs
                     let f' :: ExpQ -> ExpQ -> ExpQ
                         f' upaths r = [|foldr $(varE f) $r $upaths|]
                     clause [wildP, varP f, varP r0, asP x xpat]
                            (normalB (foldr f' (varE r0) upathss)) [],
                            -- (normalB [|foldr $(varE f) $(varE r0) $upaths|])
                            -- (normalB (foldr (\upath r -> (appE (appE (varE f) upath) (varE r0))) (varE r0) upaths)) []
                UPathRowClause $ do
                  do let upathss = map (\(PathConc _ _ _ _ x _) -> x) concs
                     clause [wildP, xpat] (normalB [|concat $(listE (map (\p -> p [|idPath|]) upathss))|]) [],
                UPathTreeClause $ do
                  do u <- newName "_unv"
                     let pairs = map (\(PathConc w _ _ _ _ fs) -> (w, fs)) concs
                         fn :: ExpQ -> (TGV, ExpQ) -> ExpQ -> ExpQ
                         fn x (w, fs) r = [| concatMap (\f -> forestMap f (map (upathTree $(varE u))
                                                                               (mapMaybe unU' (toListOf (toLens (f idPath)) $x) :: [$(asTypeQ w)]))) $fs ++ $r |] -- [| (map (\f -> map unU' (toListOf (toLens (f idPath)) x) :: [Maybe $(asTypeQ w)]) $fs) : $r |]
                     clause [varP u, asP' x xpat] (normalB [|Node idPath $(foldr (fn (varE x)) [| [] |] pairs)|]) []
               ]
    , _doSyn =
        \_tname _typ -> pure ()
    , _doAlts = \_ -> pure ()
    , _doSyns = \() _ -> pure ()
    }

concatMapQ :: [ExpQ] -> ExpQ
concatMapQ [] = [|mempty|]
concatMapQ [x] = x
concatMapQ xs = [|mconcat $(listE xs)|]

-- | Get the list of paths to subvalues of x *of type w only*.
-- This needs to be changed to get all subvalues in proper order.
-- This further means we need a version of paths that isn't tied
-- to the goal type, which means we need a new @Path u s@ type.
{-
peekList :: TypeQ -> Name -> Name -> TGVSimple -> TGV -> PatQ -> ExpQ -> ExpQ
peekList utype x p v w ppat node =
    [| let dopath pth = case pth of
                          $(asP p ppat) ->
                              map $node (toListOf (toLens $(varE p)) $(varE x) :: [$(asTypeQ w)])
                          _ -> [] in
       paths (Proxy :: Proxy $utype) $(varE x) (Proxy :: Proxy $(asTypeQ w)) (\pth r -> dopath pth ++ r) []
                                              {-:: [$(asTypeQ (makePathType (ModelType (asName v)))) $(asTypeQ w)]-} |]
-}

peekList' :: TypeQ -> ExpQ -> Name -> TGVSimple -> TGV -> PatQ -> ExpQ -> ExpQ
peekList' utype x p v w upat node =
    [| let dopath pth = case pth of
                          $(asP p upat) ->
                              map ($node) (toListOf (toLens $(varE p)) $x :: [$utype])
                          _ -> [] in
       upaths (Proxy :: Proxy $utype) (\pth r -> dopath pth ++ r) [] $x
                                              {-:: [$(asTypeQ (makeUPathType (ModelType (asName v)))) $(asTypeQ w)]-} |]

liftPeekE :: TypeQ -> TGVSimple -> TGV -> ExpQ -> Set TGVSimple -> ExpQ
liftPeekE utype v w pcon gs = do
  pk <- newName "pk"
  [| $(lamE [varP pk]
            (caseE [| $(varE pk) :: UPeek $utype $(asTypeQ w) |]
                   [match (conP (asName (makeUPeekCon (ModelType (asName w)))) [wildP, wildP])
                          (normalB [|upeekCons (($pcon {-:: Path $utype $(asTypeQ w) $(asTypeQ g) ->
                                                            Path $utype $(asTypeQ v) $(asTypeQ g)-})
                                                (upeekPath $(varE pk)
                                                    {-:: Path $utype $(asTypeQ w) $(asTypeQ g)-}))
                                               (upeekValue $(varE pk))|])
                          []]
            )) {-:: Peek $utype $(asTypeQ v)-} |]

-- Insert a string into an expression by applying an id function
-- tag :: String -> ExpQ -> ExpQ
-- tag s e = [|bool (undefined $(lift s)) $e True|]

-- | Generate clauses of the 'Describe' instance for v.  Because the
-- description is based entirely on the types, we can generate a
-- string literal here.  Example:
--    v = TGV {tgvSimple = ReportView, field = _reportLetterOfTransmittal :: Markup}
--    w = Markup
--    ppat = Path_ReportView__reportLetterOfTransmittal _wp
describeConc :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) =>
                TypeQ -> TGVSimple -> Name -> PathConc -> m ()
describeConc utype v wPathVar (PathConc w ppat _upat _ucon _ucons _ulift) =
    do p <- runQ $ newName "_p"
       x <- runQ $ newName "_x"
       f <- runQ $ newName "_f"
       pathKeys' w >>= mapM_ (doGoal' p x f)
       -- Is there a custom describe instance for Proxy (asType v)?
       -- if so it overrides the default label we build using camelWords.
    where
      doGoal' :: Name -> Name -> Name -> TGVSimple -> m ()
      doGoal' p x f g = do
        -- w' <- simplify w
        let PeekCon vn = makePeekCon (ModelType (asName v)) (ModelType (asName g))
            -- PeekCon wn = makePeekCon (ModelType (asName w)) (ModelType (asName g))
            -- Describe the next hop on the path:
            --   1. If the next hop in the path returns anything, use that
            --   2. Otherwise construct a description from asType v and its context f
        tell [DescClause $
              -- f contains the context in which v appears, while we can tell
              -- the context in which w appears from the path constructor.
              clause [varP f, conP vn [asP p ppat, varP x]]
                     (normalB ([| let -- The context in which the w value appears
                                      wfld :: Maybe (String, String, Either Int String)
                                      wfld = ($(maybe [|Nothing|] (\y -> [|Just $(fieldStrings y)|]) (view (_2 . field) w)))
                                      -- The label for the next hop along the path
                                      next = describe' wfld (peekCons $(varE wPathVar) (undefined :: Maybe $(asTypeQ g)) :: Peek $utype $(asTypeQ w))
                                      -- The label for the current node.  This will call the custom
                                      -- instance if there is one, otherwise one will have been generated.
                                      top = describe' $(varE f) (Proxy :: Proxy $(asTypeQ v)) in
                                  maybe top Just next |]))
                     []]
