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
import Language.Haskell.TH.Path.Core (camelWords, IsPath(..), makeRow, makeTrees, makeCol,
                                      PathStart(..), Describe(describe'), mat,
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), Path_List, Path_View(..), U(u, unU'))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.Syntax (liftString)
import Language.Haskell.TH.TypeGraph.Shape (constructorName, Field)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (field, TGVSimple)

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
      { upat :: PatQ -> PatQ -- Turn p into Path_At i p
      , xpaths :: ExpQ -- Turn s into [Path_At i a]
      , pnext :: ExpQ  -- Turn Path_At i q into q
      , pprev :: ExpQ  -- Turn q into Path_At i q
      , lns :: ExpQ    -- turn UPath s a into Traversal' s u
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
           pure (funD 'toLens tlcs),
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
            do doHops wildP
                      [Hop { upat = (\p -> conP 'Path_To [wildP, p])
                           , xpaths = [| [Path_To Proxy] |]
                           , pnext = [|\(Path_To Proxy q) -> q|]
                           , pprev = [|Path_To Proxy|]
                           , lns = [|viewLens|]}]
               tell [ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doOrder =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               i <- runQ $ newName "_k"
               doHops (varP x)
                      [Hop { upat = (\p -> [p|Path_At $(varP i) $p|])
                           , xpaths = [|map (\($(varP i), _) -> Path_At $(varE i)) (toPairs $(varE x))|] -- a list of functions corresponding to elements of the Order
                           , pnext = [|\(Path_At _ p) -> p|]
                           , pprev = [|Path_At $(varE i)|]
                           , lns = [|lens_omat $(varE i)|]}]
               tell [ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]

    , _doMap =
        \_i typ ->
            do x <- runQ $ newName "_xyz"
               i <- runQ $ newName "_k"
               doHops (varP x)
                      [Hop { upat = (\p -> [p|Path_Look $(varP i) $p|])
                           , xpaths = [|map (\($(varP i), _) -> Path_Look $(varE i)) (Map.toList $(varE x))|]
                           , pnext = [|\(Path_Look _ p) -> p|]
                           , pprev = [|Path_Look $(varE i)|]
                           , lns = [|mat $(varE i)|]}]
               tell [ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \ftyp styp ->
            do doHops wildP
                      [ Hop { upat =  (\p -> conP 'Path_First [p])
                            , xpaths =  [| [Path_First] |]
                            , pnext =  [|\(Path_First p) -> p|]
                            , pprev =  [|Path_First|]
                            , lns =  [|_1|] }
                      , Hop { upat = (\p -> conP 'Path_Second [p])
                            , xpaths = [| [Path_Second] |]
                            , pnext = [|\(Path_Second p) -> p|]
                            , pprev = [|Path_Second|]
                            , lns = [|_2|] } ]
               tell [ToLensClause (clause [wildP] (normalB [|lens u (\s' a -> maybe s' id (unU' a))|]) [])]
    , _doMaybe =
        \typ ->
            do doHops wildP
                      [Hop { upat = (\p -> conP 'Path_Just [p])
                           , xpaths = [|[Path_Just]|]
                           , pnext = [|\(Path_Just p) -> p|]
                           , pprev = [|\q -> Path_Just q|]
                           , lns = [|_Just|] }]
               tell [ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doEither =
        \ltyp rtyp ->
            do doHops (conP 'Left [wildP])
                      [Hop { upat = (\p -> conP 'Path_Left [p])
                           , xpaths = [|[Path_Left]|]
                           , pnext = [|\(Path_Left p) -> p|]
                           , pprev = [|Path_Left|]
                           , lns = [|_Left|] }]
               doHops (conP 'Right [wildP])
                      [Hop { upat = (\p -> conP 'Path_Right [p])
                           , xpaths = [|[Path_Right]|]
                           , pnext = [|\(Path_Right p) -> p|]
                           , pprev = [|Path_Right|]
                           , lns = [|_Right|] }]
               tell [ToLensClause (clause [wildP] (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doField =
        \fld@(_tname, _con, Right fname) typ ->
            do w <- tgvSimple' typ >>= tgv (Just fld)
               p <- runQ $ newName "p"
               let hop = Hop { upat = conP (asName (makeUFieldCon fld)) . (: [])
                             , xpaths = [|[$(asConQ (makeUFieldCon fld))]|]
                             , pnext = (lamE [conP (asName (makeUFieldCon fld)) [varP p]] (varE p))
                             , pprev = (conE (asName (makeUFieldCon fld)))
                             , lns = varE (fieldLensNameOld (asName v) fname) }
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
                       clause [varP f, upat hop (varP q)]
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
               pure hop
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
                                    (concat $(listE (map (\hop -> [|concatMap (makeRow $(varE x)) $(xpaths hop)|]) hops)))|])
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
                                                       (concat $(listE (map (\hop -> [|concatMap (makeTrees $(varE x)) $(xpaths hop)|]) hops)))|])
                                []]
  tell (map (\hop ->
                 UPeekColClause $ do
                   p <- newName "_p"
                   q <- newName "_q"
                   clause [wildP, asP p ((upat hop) (varP q)), asP' x xpat]
                              (normalB [|Node (upeekCons idPath Nothing)
                                              (makeCol $(varE x)
                                                       $(pprev hop)
                                                       $(pnext hop)
                                                       $(varE p)
                                              )|])
                              []) hops ++
        [UPeekColClause $ do
           p <- newName "_p"
           clause [wildP, varP p, asP' x xpat]
                  (normalB [|Node (upeekCons idPath (Just (u $(varE x)))) []|])
                  []])
  tell (map (\hop -> ToLensClause $ do
                       p <- newName "_p"
                       clause [(upat hop) (varP p)] (normalB [|$(lns hop) . toLens $(varE p)|]) []) hops)

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
