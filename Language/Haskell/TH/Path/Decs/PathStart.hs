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
import Data.Data (Data, Typeable)
import Data.Map as Map (toList)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Tree (Tree(Node))
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeUFieldCon, makeUPathType, ModelType(ModelType), telld, tells)
import Language.Haskell.TH.Path.Core (camelWords, forestMap, IsPath(..), liftPeek, PathStart(..), ToLens(toLens), Describe(describe'),
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), Path_View(..), U(u), ulens')
import Language.Haskell.TH.Path.Decs.PathType (upathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode)
import Language.Haskell.TH.Syntax (liftString)
import Language.Haskell.TH.TypeGraph.Shape (Field)
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
    | UPathCon ConQ
    | UPeekRowClause ClauseQ
    | UPeekTreeClause ClauseQ

partitionClauses :: [WriterType] -> ([ClauseQ], [ConQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], [], []) xs
        where
          f (UDescClause x)     (udcs, upcs, uprcs, uptcs) = (x : udcs,         upcs,     uprcs,     uptcs)
          f (UPathCon x)         (udcs, upcs, uprcs, uptcs) = (    udcs,     x : upcs,     uprcs,     uptcs)
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
           funD' 'upeekRow (case uprcs of
                              [] -> pure [clause [wildP, wildP] (normalB [| Node (upeekCons idPath Nothing) [] |]) []]
                              _ -> pure uprcs),
           funD' 'upeekTree (case uptcs of
                               [] -> pure [newName "x" >>= \x -> clause [wildP, varP x] (normalB [| Node (upeekCons idPath (Just (u $(varE x)))) [] |]) []]
                               _ -> pure uptcs)])
       when (not (null udcs)) (describeInst v udcs)
       proxyV <- runQ $ [t|Proxy $(asTypeQ v)|]
       hasCustomInstance <- (not . null) <$> reifyInstancesWithContext ''Describe [proxyV]
       when (not hasCustomInstance)
            (instanceD' (cxt []) [t|Describe (Proxy $(asTypeQ v))|]
               (pure [newName "_f" >>= \f ->
                      funD 'describe'
                        [clause [varP f, wildP]
                           (normalB [| Just (fromMaybe $(liftString (camelWords (nameBase (asName v)))) $(varE f)) |]) []]]))
       when (not (null upcs))
            (do let pname = makeUPathType (ModelType (asName v))
                tells [dataD (cxt []) (asName pname) [] (upcs {-++ [normalC (asName pname) []]-}) supers]
                telld [d|instance IsPath $(asTypeQ pname) where
                            type UType $(asTypeQ pname) = $utype
                            type SType $(asTypeQ pname) = $(asTypeQ v)
                            idPath = $(asConQ pname)|])
describeInst :: (MonadWriter [Dec] f, TypeGraphM f) => TGVSimple -> [ClauseQ] -> f ()
describeInst v udcs =
    upathType v >>= \uptype ->
    instanceD' (cxt []) [t|Describe $(pure uptype)|]
               (pure [do f <- newName "f"
                         p <- newName "p"
                         funD 'describe' (udcs ++
                                          [ clause [varP f, varP p] (guardedB [(normalGE [|$(varE p) == idPath|]
                                                                                [|describe' $(varE f) (Proxy :: Proxy $(asTypeQ v))|])]) []
                                          -- , clause [wildP, varP p] (normalB [|error ("Unexpected " ++ $(lift (pprint1 (asType v))) ++ " path: " ++ show $(varE p))|]) []
                                          ])])

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
    { _doSimple =
          let pname = makeUPathType (ModelType (asName v)) in
          tell [UDescClause $
                newName "f" >>= \f ->
                clause [varP f, wildP]
                       (normalB [| describe' $(varE f) (Proxy :: Proxy $(asTypeQ v)) |])
                       [],
                UPathCon (normalC (asName pname) [])]
    , _doSelf = pure ()
    , _doView =
        \typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               _doConcs control (varP x) [PathConc w (conP 'Path_To [wildP, varP wPathVar]) [| [Path_To Proxy] |]]
    , _doOrder =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               _doConcs control (varP x) [PathConc w (conP 'Path_At [varP i, varP wPathVar])
                                                   [|map (\(k, _v) -> Path_At k) (toPairs $(varE x))|]]
    , _doMap =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               _doConcs control (varP x) [PathConc w (conP 'Path_Look [varP i, varP wPathVar])
                                                   [|map (\(k, _v) -> Path_Look k) (Map.toList $(varE x))|]]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \ftyp styp ->
            do f <- tgv Nothing ftyp
               s <- tgv Nothing styp
               _doConcs control wildP [PathConc f (conP 'Path_First [varP wPathVar]) [| [Path_First] |],
                                       PathConc s (conP 'Path_Second [varP wPathVar]) [| [Path_Second] |]]
    , _doMaybe =
        \typ ->
            do w <- tgv Nothing typ
               _doConcs control wildP [PathConc w (conP 'Path_Just [varP wPathVar]) [|[Path_Just]|]]
    , _doEither =
        \ltyp rtyp ->
            do l <- tgv Nothing ltyp
               r <- tgv Nothing rtyp
               _doConcs control (conP 'Left [wildP]) [PathConc l (conP 'Path_Left [varP wPathVar]) [|[Path_Left]|]]
               _doConcs control (conP 'Right [wildP]) [PathConc r (conP 'Path_Right [varP wPathVar]) [|[Path_Right]|]]
    , _doField =
        \fld typ ->
            do w <- tgvSimple' typ >>= tgv (Just fld)
               let fieldUPathConName = makeUFieldCon fld
               let upat = conP (asName fieldUPathConName) [varP wPathVar]
                   conc = PathConc w upat [|[$(asConQ fieldUPathConName)]|]
               p <- runQ $ newName "_p"
               f <- runQ $ newName "_f"
               -- Generate clauses of the 'Describe' instance for v.  Because the
               -- description is based entirely on the types, we can generate a
               -- string literal here.  Example:
               --    v = TGV {tgvSimple = ReportView, field = _reportLetterOfTransmittal :: Markup}
               --    w = Markup
               --    ppat = Path_ReportView__reportLetterOfTransmittal _wp
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
               case fld of
                 (_tname, _cname, Right _fname) ->
                     do let pcname = fieldUPathName fld
                        ptype <- fieldUPathType typ
                        tell [UPathCon (normalC (asName pcname) [strictType notStrict (return ptype)])]
                 (_tname, _cname, Left _fname) -> pure ()
               pure conc
    , _doConcs =
        \xpat concs -> do
          x <- runQ $ newName "_xconc"
          tell [UPeekRowClause $
                  do unv <- newName "_unv"
                     let pairs = map (\(PathConc w _ fs) -> (w, fs)) concs
                         fn :: ExpQ -> (TGV, ExpQ) -> ExpQ -> ExpQ
                         fn ex (w, fs) r =
                             [| concatMap (\f -> forestMap (liftPeek f)
                                                           (map (\x' -> Node ((upeekCons (idPath) (Just (u x' :: $utype))) :: UPeek $utype $(asTypeQ w)) [])
                                                                (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy $utype)) $ex :: [$(asTypeQ w)]))) $fs ++ $r |]
                     clause [varP unv, asP' x xpat] (normalB [|Node (upeekCons idPath Nothing) $(foldr (fn (varE x)) [| [] |] pairs)|]) [],
                UPeekTreeClause $
                  do unv <- newName "_unv"
                     let pairs = map (\(PathConc w _ fs) -> (w, fs)) concs
                         fn :: ExpQ -> (TGV, ExpQ) -> ExpQ -> ExpQ
                         fn ex (w, fs) r =
                             [| concatMap (\f -> forestMap (liftPeek f)
                                                           (map (upeekTree $(varE unv))
                                                                (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy $utype)) $ex :: [$(asTypeQ w)]))) $fs ++ $r |]
                     clause [varP unv, asP' x xpat] (normalB [|Node (upeekCons idPath Nothing) $(foldr (fn (varE x)) [| [] |] pairs)|]) []
               ]
    , _doSyn =
        \_tname _typ -> pure ()
    , _doAlts = \_ -> let pname = makeUPathType (ModelType (asName v)) in
                      tell [UPathCon (normalC (asName pname) [])]
    , _doSyns = \() _ -> pure ()
    }

-- | Convert a 'Language.Haskell.TH.TypeGraph.Shape.Field' into the argument used by describe'.
fieldString :: (Name, Name, Either Int Name) -> ExpQ
fieldString (_tname, cname, Left fpos) = liftString (camelWords (nameBase cname) ++ "[" ++ show fpos ++ "]")
fieldString (_tname, _cname, Right fname) = liftString (camelWords (nameBase fname))

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
{-
bestUPathTypeName :: HasName v => v -> PathType Name
bestUPathTypeName = makeUPathType . ModelType . asName
-}

fieldUPathName :: Field -> Name
fieldUPathName fld = asName (makeUFieldCon fld)

fieldUPathType :: TypeGraphM m => Type -> m Type
fieldUPathType typ = tgvSimple' typ >>= upathType
