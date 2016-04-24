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
module Language.Haskell.TH.Path.Decs.PathStart (peekDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Map as Map (toList)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Tree (Tree(Node))
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeUFieldCon, makeUPathType, ModelType(ModelType), PathType, telld, tells)
import Language.Haskell.TH.Path.Core (camelWords, IsPath(..), makeRow, makeTrees, makeCol, makePeek,
                                      PathStart(..), ToLens(toLens), Describe(describe'), mat,
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), Path_List, Path_View(..), U(u, unU'))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.Syntax (liftString)
import Language.Haskell.TH.TypeGraph.Shape (constructorName, Field)
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

data Hop
    = Hop
      { upat :: PatQ -> PatQ
      , xpaths :: ExpQ
      , pnext :: ExpQ  -- Turn Path_At i q into q
      , pprev :: ExpQ  -- Turn Path_At i q into Path_At i
      , lns :: ExpQ
      , wtyp :: TGV
      }

data WriterType
    = UDescClause ClauseQ
    | UPathCon ConQ
    | UPeekRowClause ClauseQ
    | UPeekTreeClause ClauseQ
    | UPeekColClause ClauseQ
    | ToLensClause ClauseQ

partitionClauses :: [WriterType] -> ([ClauseQ], [ConQ], [ClauseQ], [ClauseQ], [ClauseQ], [ClauseQ])
partitionClauses xs =
    foldr f ([], [], [], [], [], []) xs
        where
          f (UDescClause x)     (udcs, upcs, uprcs, uptcs, upccs, tlcs) = (x : udcs,     upcs,     uprcs,     uptcs,     upccs,     tlcs)
          f (UPathCon x)        (udcs, upcs, uprcs, uptcs, upccs, tlcs) = (    udcs, x : upcs,     uprcs,     uptcs,     upccs,     tlcs)
          f (UPeekRowClause x)  (udcs, upcs, uprcs, uptcs, upccs, tlcs) = (    udcs,     upcs, x : uprcs,     uptcs,     upccs,     tlcs)
          f (UPeekTreeClause x) (udcs, upcs, uprcs, uptcs, upccs, tlcs) = (    udcs,     upcs,     uprcs, x : uptcs,     upccs,     tlcs)
          f (UPeekColClause x)  (udcs, upcs, uprcs, uptcs, upccs, tlcs) = (    udcs,     upcs,     uprcs,     uptcs, x : upccs,     tlcs)
          f (ToLensClause x)    (udcs, upcs, uprcs, uptcs, upccs, tlcs) = (    udcs,     upcs,     uprcs,     uptcs,     upccs, x : tlcs)

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
peekDecs utype v =
    do uptype <- upathType v
       (clauses :: [WriterType]) <- execWriterT (doNode (pathControl v) v)
       let (udcs, upcs, uprcs, uptcs, upccs, tlcs) = partitionClauses clauses
       instanceD' (cxt []) [t|PathStart $utype $(asTypeQ v)|]
         (sequence
          [pure (dataInstD (cxt []) ''UPeek [utype, asTypeQ v]
                           [normalC (asName (makeUPeekCon (ModelType (asName v))))
                                    [strictType notStrict [t|UPath $utype $(asTypeQ v)|],
                                     strictType notStrict [t|Maybe $utype|]]]
                           [''Eq, ''Show, ''Generic, ''FromJSON, ''ToJSON]),
           pure (funD 'upeekCons [clause [] (normalB (conE (asName (makeUPeekCon (ModelType (asName v)))))) []]),
           pure (funD 'upeekPath [newName "p" >>= \p -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [varP p, wildP]] (normalB (varE p)) []]),
           pure (funD 'upeekValue [newName "x" >>= \x -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [wildP, varP x]] (normalB (varE x)) []]),
           pure (tySynInstD ''UPath (tySynEqn [utype, asTypeQ v] (pure uptype))),
           funD' 'upeekRow (case uprcs of
                              [] -> pure [clause [wildP, wildP] (normalB [| Node (upeekCons idPath Nothing) [] |]) []]
                              _ -> pure uprcs),
           funD' 'upeekTree (case uptcs of
                               [] -> pure [do x <- newName "x"
                                              clause [wildP, wildP, varP x] (normalB [| Node (upeekCons idPath (Just (u $(varE x)))) [] |]) []]
                               _ -> pure uptcs),
           funD' 'upeekCol (case upccs of
                              [] -> pure [do x <- newName "x"
                                             clause [wildP, wildP, varP x] (normalB [|Node (upeekCons idPath (Just (u $(varE x)))) []|]) []]
                              _ -> pure upccs)
          ])
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
                tells [dataD (cxt []) (asName pname) [] upcs supers]
                telld [d|instance IsPath $(asTypeQ pname) where
                            type UType $(asTypeQ pname) = $utype
                            type SType $(asTypeQ pname) = $(asTypeQ v)
                            idPath = $(asConQ pname)|])
       when (not (null tlcs))
            (tells [ instanceD (pure []) [t|ToLens $utype $(asTypeQ v)|]
                     [ funD 'toLens tlcs
                     ] ])

pathControl :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) => TGVSimple -> Control m Hop () ()
pathControl v =
    Control
    { _doSimple =
          do let pname = makeUPathType (ModelType (asName v))
             f <- runQ $ newName "f"
             tell [UDescClause $ clause [varP f, wildP] (normalB [| describe' $(varE f) (Proxy :: Proxy $(asTypeQ v)) |]) [],
                   ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) []),
                   UPathCon (normalC (asName pname) [])]
    , _doSelf = pure ()
    , _doView =
        \typ ->
            do w <- tgv Nothing typ
               doHops wildP
                      [Hop (\p -> conP 'Path_To [wildP, p]) [| [Path_To Proxy] |] [|\(Path_To Proxy q) -> q|] [|Path_To Proxy|] [|viewLens|] w]
               tell [-- ToLensClause (clause [upat conc (varP p)] (normalB [|viewLens . toLens $(varE p)|]) []),
                     ToLensClause (clause [[p|Path_Self|]] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doOrder =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               doHops (varP x)
                      [Hop (\p -> [p|Path_At $(varP i) $p|])
                           [|map (\($(varP i), _) -> Path_At $(varE i)) (toPairs $(varE x))|] -- a list of functions corresponding to elements of the Order
                           [|\(Path_At _ p) -> p|]
                           [|Path_At $(varE i)|]
                           [|lens_omat $(varE i)|] w]
               tell [-- ToLensClause (clause [upat conc (varP p)] (normalB [|$(lns conc) . toLens $(varE p)|]) []),
                     ToLensClause (clause [[p|Path_OMap|]] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]

    , _doMap =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               w <- tgv Nothing typ
               i <- runQ $ newName "_k"
               doHops (varP x)
                      [Hop (\p -> [p|Path_Look $(varP i) $p|])
                           [|map (\($(varP i), _) -> Path_Look $(varE i)) (Map.toList $(varE x))|]
                           [|\(Path_Look _ p) -> p|]
                           [|Path_Look $(varE i)|]
                           [|mat $(varE i)|] w]
               tell [-- ToLensClause (clause [upat conc (varP p)] (normalB [|$(lns conc) . toLens $(varE p)|]) []),
                     ToLensClause (clause [[p|Path_Map|]] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \ftyp styp ->
            do f <- tgv Nothing ftyp
               s <- tgv Nothing styp
               doHops wildP
                      [Hop (\p -> conP 'Path_First [p]) [| [Path_First] |] [|\(Path_First p) -> p|] [|Path_First|] [|_1|] f,
                       Hop (\p -> conP 'Path_Second [p]) [| [Path_Second] |] [|\(Path_Second p) -> p|] [|Path_Second|] [|_2|] s]
               tell [-- ToLensClause (clause [upat fconc (varP p)] (normalB [|$(lns fconc) . toLens $(varE p)|]) []),
                     -- ToLensClause (clause [upat sconc (varP p)] (normalB [|$(lns sconc) . toLens $(varE p)|]) []),
                     ToLensClause (clause [[p|Path_Pair|]] (normalB [|lens u (\s' a -> maybe s' id (unU' a))|]) [])]
    , _doMaybe =
        \typ ->
            do w <- tgv Nothing typ
               doHops wildP
                      [Hop (\p -> conP 'Path_Just [p]) [|[Path_Just]|] [|\(Path_Just p) -> p|] [|\q -> Path_Just q|] [|_Just|] w]
               tell [-- ToLensClause (clause [upat conc (varP p)] (normalB [|$(lns conc) . toLens $(varE p)|]) []),
                     ToLensClause (clause [[p|Path_Maybe|]] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doEither =
        \ltyp rtyp ->
            do l <- tgv Nothing ltyp
               r <- tgv Nothing rtyp
               let upati = const [p|Path_Either|]
               doHops (conP 'Left [wildP])
                      [Hop (\p -> conP 'Path_Left [p]) [|[Path_Left]|] [|\(Path_Left p) -> p|] [|Path_Left|] [|_Left|] l]
               doHops (conP 'Right [wildP])
                      [Hop (\p -> conP 'Path_Right [p]) [|[Path_Right]|] [|\(Path_Right p) -> p|] [|Path_Right|] [|_Right|] r]
               tell [-- ToLensClause (clause [upat lconc (varP p)] (normalB [|$(lns lconc) . toLens $(varE p)|]) []),
                     -- ToLensClause (clause [upat rconc (varP p)] (normalB [|$(lns rconc) . toLens $(varE p)|]) []),
                     ToLensClause (clause [upati wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doField =
        \fld@(_tname, _con, Right fname) typ ->
            do w <- tgvSimple' typ >>= tgv (Just fld)
               p <- runQ $ newName "p"
               let conc = Hop (\p -> conP (asName (makeUFieldCon fld)) [p])
                              [|[$(asConQ (makeUFieldCon fld))]|]
                              (lamE [conP (asName (makeUFieldCon fld)) [varP p]] (varE p))
                              (conE (asName (makeUFieldCon fld)))
                              (varE (fieldLensNameOld (asName v) fname)) w
               f <- runQ $ newName "_f"
               -- Generate clauses of the 'Describe' instance for v.  Because the
               -- description is based entirely on the types, we can generate a
               -- string literal here.  Example:
               --    v = TGV {tgvSimple = ReportView, field = _reportLetterOfTransmittal :: Markup}
               --    w = Markup
               --    ppat = Path_ReportView__reportLetterOfTransmittal _wp
               tell [UDescClause $ do
                       q <- newName "_q"
                       -- f contains the context in which v appears, while we can tell
                       -- the context in which w appears from the path constructor.
                       clause [varP f, upat conc (varP q)]
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
                 (_tname, _con, Right _fname) ->
                     do let pcname = fieldUPathName fld
                        ptype <- fieldUPathType typ
                        tell [UPathCon (normalC (asName pcname) [strictType notStrict (return ptype)])]
                 (_tname, _con, Left _fname) -> pure ()
               pure conc
    , _doConcs = doHops
    , _doSyn =
        \_tname _typ -> pure ()
    , _doAlts = \_ -> let pname = makeUPathType (ModelType (asName v)) in
                      tell [UPathCon (normalC (asName pname) []),
                            UDescClause $ do f <- newName "f"
                                             clause [varP f, conP (asName pname) []]
                                                    (normalB [|describe' $(varE f) (Proxy :: Proxy $(asTypeQ v))|])
                                                    [],
                            ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doSyns = \() _ -> pure ()
    }

-- | Do several different path hops from one value.
doHops :: (TypeGraphM m, MonadWriter [WriterType] m) => PatQ -> [Hop] -> m ()
doHops xpat hops = do
  x <- runQ $ newName "x"
  tell [UPeekRowClause $
          do clause [wildP, asP' x xpat]
                    (normalB [|Node (upeekCons idPath Nothing)
                                    (concat $(listE (map (\(Hop _ xpaths _ _ _ _) -> [|concatMap (makeRow $(varE x)) $xpaths|]) hops)))|])
                    [],
        UPeekTreeClause $
          do d <- newName "d"
             case null hops of
               -- There are no hops from here, so no subnodes.
               True -> clause [wildP, wildP, asP' x xpat]
                              (normalB [|Node (upeekCons idPath (Just (u $(varE x)))) []|])
                              []
               False ->  clause [wildP, varP d, asP' x xpat]
                                (normalB [|case $(varE d) of
                                             -- We reached the desired depth
                                             Just 0 -> Node (upeekCons idPath (Just (u $(varE x)))) []
                                             _ -> Node (upeekCons idPath Nothing)
                                                       (concat $(listE (map (\(Hop _ xpaths _ _ _ _) -> [|concatMap (makeTrees $(varE x)) $xpaths|]) hops)))|])
                                []]
  tell (map (\Hop{..} ->
                 UPeekColClause $ do
                   p <- newName "_p"
                   q <- newName "_q"
                   clause [wildP, asP p (upat (varP q)), asP' x xpat]
                              (normalB [|Node (upeekCons idPath Nothing)
                                              (makeCol $(varE x)
                                                       $pprev -- Turn q into Path_At i q
                                                       $pnext -- Turn Path_At i q into Path_At i
                                                       $(varE p)
                                              )|])
                              []) hops ++
        [UPeekColClause $ do
           p <- newName "_p"
           clause [wildP, varP p, asP' x xpat]
                  (normalB [|Node (upeekCons idPath (Just (u $(varE x)))) []|])
                  []])
  tell (map (\Hop{..} -> ToLensClause $ do
                           p <- newName "_p"
                           clause [upat (varP p)] (normalB [|$(lns) . toLens $(varE p)|]) []) hops)

-- | Given a type, compute the corresponding path type.
upathType :: forall m. TypeGraphM m =>
             TGVSimple -- ^ The type to convert to a path type
          -> m Type
upathType v = doNode (upathTypeControl v) v

upathTypeControl :: (TypeGraphM m) => TGVSimple -> Control m () () Type
upathTypeControl v =
    Control
    { _doSelf = pure $ asType v
    , _doSimple = runQ (asTypeQ (bestUPathTypeName v))
    , _doView =
        \w -> do ptype <- upathType w
                 runQ [t|Path_View $(asTypeQ v) $(asTypeQ ptype)|]
    , _doOrder =
        \ityp etyp ->
            do epath <- upathType etyp
               runQ [t|Path_OMap $(pure ityp) $(pure epath)|]
    , _doMap =
        \ktyp vtyp ->
            do vpath <- upathType vtyp
               runQ [t| Path_Map $(pure ktyp) $(pure vpath)|]
    , _doList =
        \etyp ->
            do epath <- upathType etyp
               runQ [t|Path_List $(return epath)|]
    , _doPair =
        \ftyp styp ->
            do fpath <- upathType ftyp
               spath <- upathType styp
               runQ [t| Path_Pair $(return fpath) $(return spath) |]
    , _doMaybe =
        \typ ->
            do epath <- upathType typ
               runQ [t|Path_Maybe $(pure epath)|]
    , _doEither =
        \ltyp rtyp ->
            do lpath <- upathType ltyp
               rpath <- upathType rtyp
               runQ [t| Path_Either $(return lpath) $(return rpath) |]
    , _doField = \_ _ -> pure ()
    , _doConcs = \_ _ -> pure ()
    , _doSyn =
        \tname _typ ->
            runQ $ (asTypeQ (makeUPathType (ModelType tname)))
    , _doAlts =
        \_ -> runQ $ (asTypeQ (makeUPathType (ModelType (asName v))))
    , _doSyns = \r0 _rs -> pure r0
    }

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.

bestUPathTypeName :: HasName v => v -> PathType Name
bestUPathTypeName = makeUPathType . ModelType . asName

-- | Convert a 'Language.Haskell.TH.TypeGraph.Shape.Field' into the argument used by describe'.
fieldString :: Field -> ExpQ
fieldString (_tname, con, Left fpos) = liftString (camelWords (nameBase (constructorName con)) ++ "[" ++ show fpos ++ "]")
fieldString (_tname, _con, Right fname) = liftString (camelWords (nameBase fname))

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data, ''Generic, ''FromJSON, ''ToJSON]

fieldUPathName :: Field -> Name
fieldUPathName fld = asName (makeUFieldCon fld)

fieldUPathType :: TypeGraphM m => Type -> m Type
fieldUPathType typ = tgvSimple' typ >>= upathType

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)

makeUPeekCon :: (HasName s) => ModelType s -> PeekCon
makeUPeekCon (ModelType s) = PeekCon (mkName ("UPeek_" ++ nameBase (asName s)))

instanceD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> TypeQ -> m [DecQ] -> m ()
instanceD' cxt' typ decs =
    instanceD cxt' typ <$> decs >>= runQ >>= tell . (: [])

funD' :: (TypeGraphM m, MonadWriter [Dec] m) => Name -> m [ClauseQ] -> m DecQ
funD' name clauses = funD name <$> clauses
