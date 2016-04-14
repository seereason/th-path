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
module Language.Haskell.TH.Path.Decs.PathStart (peekDecs, makeUPeekCon) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Map as Map (toList)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Tree (Tree(Node))
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeUFieldCon, makePathCon, makeUPathType, ModelType(ModelType))
import Language.Haskell.TH.Path.Core (camelWords, IsPath(idPath), PathStart(..), ToLens(toLens),
                                      Describe(describe'), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), forestMap, U(u), ulens')
import Language.Haskell.TH.Path.Decs.PathType (upathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode, finishConcs)
import Language.Haskell.TH.Syntax (liftString)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
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
      , _concUPat :: PatQ
      , _concLift :: ExpQ
      }

data WriterType
    = UDescClause ClauseQ
    | UPathClause ClauseQ
    | UPeekRowClause ClauseQ
    | UPeekTreeClause ClauseQ

partitionClauses :: [WriterType] -> ([ClauseQ], [ClauseQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], [], []) xs
        where
          f (UDescClause x)     (udcs, upcs, uprcs, uptcs) = (x : udcs,         upcs,     uprcs,     uptcs)
          f (UPathClause x)     (udcs, upcs, uprcs, uptcs) = (    udcs,     x : upcs,     uprcs,     uptcs)
          f (UPeekRowClause x)  (udcs, upcs, uprcs, uptcs) = (    udcs,         upcs, x : uprcs,     uptcs)
          f (UPeekTreeClause x) (udcs, upcs, uprcs, uptcs) = (    udcs,         upcs,     uprcs, x : uptcs)

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
peekDecs utype v =
    do uptype <- upathType v
       (clauses :: [WriterType]) <- execWriterT (peekClauses utype v)
       let (udcs, upcs, uprcs, uptcs) = partitionClauses clauses
       instanceD' (cxt []) [t|PathStart $utype $(asTypeQ v)|]
         (sequence
          [pure (dataInstD (cxt []) ''UPeek [utype, asTypeQ v]
                           [normalC (asName (makeUPeekCon (ModelType (asName v))))
                                    [strictType notStrict [t|UPath $utype $(asTypeQ v)|],
                                     strictType notStrict [t|Maybe $utype|]]]
                           [''Eq, ''Show]),
           pure (funD 'upeekCons [clause [] (normalB (conE (asName (makeUPeekCon (ModelType (asName v)))))) []]),
           pure (funD 'upeekPath [newName "p" >>= \p -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [varP p, wildP]] (normalB (varE p)) []]),
           pure (funD 'upeekValue [newName "x" >>= \x -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [wildP, varP x]] (normalB (varE x)) []]),
           pure (tySynInstD ''UPath (tySynEqn [utype, asTypeQ v] (pure uptype))),
           funD' 'upaths (case upcs of
                            [] -> pure [newName "r" >>= \r -> clause [wildP, wildP, varP r, wildP] (normalB (varE r)) []]
                            _ -> pure upcs),
           funD' 'upeekRow (case uprcs of
                              [] -> pure [clause [wildP, wildP] (normalB [| Node (upeekCons idPath Nothing) [] |]) []]
                              _ -> pure uprcs),
           funD' 'upeekTree (case uptcs of
                               [] -> pure [newName "x" >>= \x -> clause [wildP, varP x] (normalB [| Node (upeekCons idPath (Just (u $(varE x)))) [] |]) []]
                               _ -> pure uptcs)])
       instanceD' (cxt []) [t|Describe $(pure uptype)|]
                  (pure [do f <- newName "f"
                            p <- newName "p"
                            funD 'describe' (udcs ++
                                             [clause [varP f, varP p] (guardedB [(normalGE [|$(varE p) == idPath|]
                                                                                  [|describe' $(varE f) (Proxy :: Proxy $(asTypeQ v))|])]) [],
                                              clause [wildP, varP p] (normalB [|error ("Unexpected " ++ $(lift (pprint1 (asType v))) ++ " path: " ++ show $(varE p))|]) []]
                                            )])
       proxyV <- runQ $ [t|Proxy $(asTypeQ v)|]
       hasCustomInstance <- (not . null) <$> reifyInstancesWithContext ''Describe [proxyV]
       when (not hasCustomInstance)
            (instanceD' (cxt []) [t|Describe (Proxy $(asTypeQ v))|]
               (pure [newName "_f" >>= \f ->
                      funD 'describe'
                        [clause [varP f, wildP]
                           (normalB [| Just (fromMaybe $(liftString (camelWords (nameBase (asName v)))) $(varE f)) |]) []]]))

makeUPeekCon :: (HasName s) => ModelType s -> PeekCon
makeUPeekCon (ModelType s) = PeekCon (mkName ("UPeek_" ++ nameBase (asName s)))

instanceD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> TypeQ -> m [DecQ] -> m ()
instanceD' cxt' typ decs =
    instanceD cxt' typ <$> decs >>= runQ >>= tell . (: [])

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
               let vUPathConName = makePathCon (makeUPathType (ModelType (asName v))) "View"
               let conc = PathConc w (conP (asName vUPathConName) [varP wPathVar])
                                     [| [$(conE (asName vUPathConName))] |]
               describeConc v wPathVar conc
               finishConcs control [(varP x, [conc])]
    , _doOrder =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               let conc = PathConc w (conP 'Path_At [varP i, varP wPathVar])
                                     [|map (\(k, _v) -> Path_At k) (toPairs $(varE x))|]
               describeConc v wPathVar conc
               finishConcs control [(varP x, [conc])]
    , _doMap =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               let conc = PathConc w (conP 'Path_Look [varP i, varP wPathVar])
                                     [|map (\(k, _v) -> Path_Look k) (Map.toList $(varE x))|]
               describeConc v wPathVar conc
               finishConcs control [(varP x, [conc])]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \ftyp styp ->
            do f <- tgv Nothing ftyp
               s <- tgv Nothing styp
               let fconc = PathConc f (conP 'Path_First [varP wPathVar]) [| [Path_First] |]
                   sconc = PathConc s (conP 'Path_Second [varP wPathVar]) [| [Path_Second] |]
               describeConc v wPathVar fconc
               describeConc v wPathVar sconc
               finishConcs control [(wildP, [fconc, sconc])]
    , _doMaybe =
        \typ ->
            do w <- tgv Nothing typ
               let conc = PathConc w (conP 'Path_Just [varP wPathVar]) [|[Path_Just]|]
               describeConc v wPathVar conc
               finishConcs control [(wildP, [conc])]
    , _doEither =
        \ltyp rtyp ->
            do l <- tgv Nothing ltyp
               r <- tgv Nothing rtyp
               let lconc = PathConc l (conP 'Path_Left [varP wPathVar]) [|[Path_Left]|]
                   rconc = PathConc r (conP 'Path_Right [varP wPathVar]) [|[Path_Right]|]
               describeConc v wPathVar lconc
               describeConc v wPathVar rconc
               finishConcs control [(conP 'Left [wildP], [lconc]), (conP 'Right [wildP], [rconc])]
    , _doField =
        \fld typ ->
            do f <- tgvSimple' typ >>= tgv (Just fld)
               let fieldUPathConName = maybe (error $ "Not a field: " ++ show f) id (makeUFieldCon f)
               let conc = PathConc f (conP (asName fieldUPathConName) [varP wPathVar]) [|[$(asConQ fieldUPathConName)]|]
               describeConc v wPathVar conc
               pure conc
    , _doConcs =
        \xpat concs -> do
          x <- runQ $ newName "_xconc"
          tell [UPathClause $
                  do f <- newName "_f"
                     r0 <- newName "r0"
                     let upathss = map (\(PathConc _ _ fs) -> [|map (\pf -> pf idPath) $fs|]) concs
                     clause [wildP, varP f, varP r0, asP x xpat]
                            (normalB [|foldr $(varE f) $(varE r0) (concat $(listE upathss))|]) [],
                UPeekRowClause $
                  do unv <- newName "_unv"
                     let pairs = map (\(PathConc w _ fs) -> (w, fs)) concs
                         fn :: ExpQ -> (TGV, ExpQ) -> ExpQ -> ExpQ
                         fn ex (w, fs) r =
                             [| concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk))
                                                           (map (\x' -> Node ((upeekCons (idPath) (Just (u x' :: $utype))) :: UPeek $utype $(asTypeQ w)) [])
                                                                (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy $utype)) $ex :: [$(asTypeQ w)]))) $fs ++ $r |]
                     clause [varP unv, asP' x xpat] (normalB [|Node (upeekCons idPath Nothing) $(foldr (fn (varE x)) [| [] |] pairs)|]) [],
                UPeekTreeClause $
                  do unv <- newName "_unv"
                     let pairs = map (\(PathConc w _ fs) -> (w, fs)) concs
                         fn :: ExpQ -> (TGV, ExpQ) -> ExpQ -> ExpQ
                         fn ex (w, fs) r =
                             [| concatMap (\f -> forestMap (\pk -> upeekCons (f (upeekPath pk)) (upeekValue pk))
                                                           (map (upeekTree $(varE unv))
                                                                (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy $utype)) $ex :: [$(asTypeQ w)]))) $fs ++ $r |]
                     clause [varP unv, asP' x xpat] (normalB [|Node (upeekCons idPath Nothing) $(foldr (fn (varE x)) [| [] |] pairs)|]) []
               ]
    , _doSyn =
        \_tname _typ -> pure ()
    , _doAlts = \_ -> pure ()
    , _doSyns = \() _ -> pure ()
    }

-- | Generate clauses of the 'Describe' instance for v.  Because the
-- description is based entirely on the types, we can generate a
-- string literal here.  Example:
--    v = TGV {tgvSimple = ReportView, field = _reportLetterOfTransmittal :: Markup}
--    w = Markup
--    ppat = Path_ReportView__reportLetterOfTransmittal _wp
describeConc :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) =>
                TGVSimple -> Name -> PathConc -> m ()
describeConc v wPathVar (PathConc w upat _ulift) =
    do p <- runQ $ newName "_p"
       f <- runQ $ newName "_f"
       -- Is there a custom describe instance for Proxy (asType v)?
       -- if so it overrides the default label we build using camelWords.
       tell [UDescClause $
             -- f contains the context in which v appears, while we can tell
             -- the context in which w appears from the path constructor.
             clause [varP f, asP p upat]
                    (normalB ([| maybe
                                   -- The label for the current node.  This will call the custom
                                   -- instance if there is one, otherwise one will have been generated.
                                   (describe' $(varE f) (Proxy :: Proxy $(asTypeQ v)))
                                   Just
                                   -- The label for the next hop along the path
                                   (describe'
                                      -- The context in which the w value appears
                                      ($(maybe [|Nothing|] (\y -> [|Just $(fieldString y)|]) (view (_2 . field) w)))
                                      $(varE wPathVar)) |]))
                    []]

-- | Convert a 'Language.Haskell.TH.TypeGraph.Shape.Field' into the argument used by describe'.
fieldString :: (Name, Name, Either Int Name) -> ExpQ
fieldString (_tname, cname, Left fpos) = liftString (camelWords (nameBase cname) ++ "[" ++ show fpos ++ "]")
fieldString (_tname, _cname, Right fname) = liftString (camelWords (nameBase fname))
