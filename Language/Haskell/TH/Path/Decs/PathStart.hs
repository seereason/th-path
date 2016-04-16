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
import Language.Haskell.TH.Path.Core (camelWords, forestMap, IsPath(..), liftPeek, PathStart(..), ToLens(toLens), Describe(describe'), mat,
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), Path_View(..), U(u), ulens')
import Language.Haskell.TH.Path.Decs.PathType (upathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode)
import Language.Haskell.TH.Path.View (viewLens)
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
      , _concUPat :: PatQ -> PatQ
      , _concLift :: ExpQ
      , _concLens :: ExpQ
      }

data WriterType
    = UDescClause ClauseQ
    | UPathCon ConQ
    | UPeekRowClause ClauseQ
    | UPeekTreeClause ClauseQ
    | ToLensClause ClauseQ

partitionClauses :: [WriterType] -> ([ClauseQ], [ConQ], [ClauseQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], [], [], []) xs
        where
          f (UDescClause x)     (udcs, upcs, uprcs, uptcs, tlcs) = (x : udcs,     upcs,     uprcs,     uptcs,     tlcs)
          f (UPathCon x)        (udcs, upcs, uprcs, uptcs, tlcs) = (    udcs, x : upcs,     uprcs,     uptcs,     tlcs)
          f (UPeekRowClause x)  (udcs, upcs, uprcs, uptcs, tlcs) = (    udcs,     upcs, x : uprcs,     uptcs,     tlcs)
          f (UPeekTreeClause x) (udcs, upcs, uprcs, uptcs, tlcs) = (    udcs,     upcs,     uprcs, x : uptcs,     tlcs)
          f (ToLensClause x)    (udcs, upcs, uprcs, uptcs, tlcs) = (    udcs,     upcs,     uprcs,     uptcs, x : tlcs)

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
peekDecs utype v =
    do uptype <- upathType v
       (clauses :: [WriterType]) <- execWriterT (peekClauses utype v)
       let (udcs, upcs, uprcs, uptcs, tlcs) = partitionClauses clauses
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
       when (not (null udcs)) (instanceD' (cxt []) [t|Describe $(pure uptype)|] (pure [do funD 'describe' udcs]))
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
  doNode (pathControl utype v x) v

pathControl :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) => TypeQ -> TGVSimple -> Name -> Control m PathConc () ()
pathControl utype v _x = do
  let control :: Control m PathConc () ()
      control = pathControl utype v _x in
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
            do w <- tgv Nothing typ
               _doConcs control wildP [PathConc w (\p -> conP 'Path_To [wildP, p]) [| [Path_To Proxy] |] [|viewLens|]]
    , _doOrder =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               _doConcs control (varP x) [PathConc w (\p -> conP 'Path_At [varP i, p])
                                                     [|map (\($(varP i), _) -> Path_At $(varE i)) (toPairs $(varE x))|]
                                                     [|lens_omat $(varE i)|] ]

    , _doMap =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               _doConcs control (varP x) [PathConc w (\p -> conP 'Path_Look [varP i, p])
                                                   [|map (\($(varP i), _) -> Path_Look $(varE i)) (Map.toList $(varE x))|]
                                                   [|mat $(varE i)|] ]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \ftyp styp ->
            do f <- tgv Nothing ftyp
               s <- tgv Nothing styp
               _doConcs control wildP [PathConc f (\p -> conP 'Path_First [p]) [| [Path_First] |] [|_1|],
                                       PathConc s (\p -> conP 'Path_Second [p]) [| [Path_Second] |] [|_2|]]
    , _doMaybe =
        \typ ->
            do w <- tgv Nothing typ
               _doConcs control wildP [PathConc w (\p -> conP 'Path_Just [p]) [|[Path_Just]|] [|_Just|]]
    , _doEither =
        \ltyp rtyp ->
            do l <- tgv Nothing ltyp
               r <- tgv Nothing rtyp
               _doConcs control (conP 'Left [wildP]) [PathConc l (\p -> conP 'Path_Left [p]) [|[Path_Left]|] [|_Left|]]
               _doConcs control (conP 'Right [wildP]) [PathConc r (\p -> conP 'Path_Right [p]) [|[Path_Right]|] [|_Right|]]
    , _doField =
        \fld@(tname, cname, Right fname) typ ->
            do w <- tgvSimple' typ >>= tgv (Just fld)
               let upat p = conP (asName (makeUFieldCon fld)) [p]
                   conc = PathConc w upat [|[$(asConQ (makeUFieldCon fld))]|]
                                   (varE (fieldLensNameOld (asName v) fname))
               f <- runQ $ newName "_f"
               -- Generate clauses of the 'Describe' instance for v.  Because the
               -- description is based entirely on the types, we can generate a
               -- string literal here.  Example:
               --    v = TGV {tgvSimple = ReportView, field = _reportLetterOfTransmittal :: Markup}
               --    w = Markup
               --    ppat = Path_ReportView__reportLetterOfTransmittal _wp
               tell [UDescClause $
                     newName "q" >>= \q ->
                     -- f contains the context in which v appears, while we can tell
                     -- the context in which w appears from the path constructor.
                     clause [varP f, upat (varP q)]
                            (normalB ([| maybe
                                           -- The label for the current node.  This will call the custom
                                           -- instance if there is one, otherwise one will have been generated.
                                           (describe' $(varE f) (Proxy :: Proxy $(asTypeQ v)))
                                           Just
                                           -- The label for the next hop along the path
                                           (describe'
                                              -- The context in which the w value appears
                                              ($(maybe [|Nothing|] (\y -> [|Just $(fieldString y)|]) (view (_2 . field) w)))
                                              $(varE q)) |]))
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
                     let pairs = map (\(PathConc w _ fs _) -> (w, fs)) concs
                         fn :: ExpQ -> (TGV, ExpQ) -> ExpQ -> ExpQ
                         fn ex (w, fs) r =
                             [| concatMap (\f -> forestMap (liftPeek f)
                                                           (map (\x' -> Node ((upeekCons (idPath) (Just (u x' :: $utype))) :: UPeek $utype $(asTypeQ w)) [])
                                                                (toListOf (toLens (f idPath) . ulens' (Proxy :: Proxy $utype)) $ex :: [$(asTypeQ w)]))) $fs ++ $r |]
                     clause [varP unv, asP' x xpat] (normalB [|Node (upeekCons idPath Nothing) $(foldr (fn (varE x)) [| [] |] pairs)|]) [],
                UPeekTreeClause $
                  do unv <- newName "_unv"
                     let pairs = map (\(PathConc w _ fs _) -> (w, fs)) concs
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
                      tell [UPathCon (normalC (asName pname) []),
                            UDescClause $ do f <- newName "f"
                                             clause [varP f, conP (asName pname) []]
                                                    (normalB [|describe' $(varE f) (Proxy :: Proxy $(asTypeQ v))|])
                                                    []]
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

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)
