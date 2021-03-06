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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Peek (peekDecs) where

import Debug.Trace
import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Order (Path_OMap(..))
import Data.Proxy
import Data.Tree (Tree(Node))
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasConQ(asConQ), HasCon(asCon), HasName(asName), HasType(asType), HasTypeQ(asTypeQ),
                                        makeUFieldCon, makeUPathType, ModelType(ModelType), PathType, telld, tells)
import Language.Haskell.TH.Path.Core (camelWords, Describe(describe'), IsPath(..), makeRow, makeTrees, makeCol,
                                      PathStart(..), Peek(..),
                                      Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), Path_List, Path_View(..), U(u, unU'))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Traverse (asP', Control(..), doNode)
import Language.Haskell.TH.Syntax (liftString, qReify)
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
      }

data WriterType
    = UDescClause ClauseQ
    | UPathCon ConQ
    | UPeekRowClause ClauseQ
    | UPeekTreeClause ClauseQ
    | UPeekColClause ClauseQ
    | ToLensMatch MatchQ

partitionClauses :: [WriterType] -> ([ClauseQ], [ConQ], [ClauseQ], [ClauseQ], [ClauseQ], [MatchQ])
partitionClauses xs =
    foldr f ([], [], [], [], [], []) xs
        where
          f (UDescClause x)     (udcs, upcs, uprcs, uptcs, upccs, tlms) = (x : udcs,     upcs,     uprcs,     uptcs,     upccs,     tlms)
          f (UPathCon x)        (udcs, upcs, uprcs, uptcs, upccs, tlms) = (    udcs, x : upcs,     uprcs,     uptcs,     upccs,     tlms)
          f (UPeekRowClause x)  (udcs, upcs, uprcs, uptcs, upccs, tlms) = (    udcs,     upcs, x : uprcs,     uptcs,     upccs,     tlms)
          f (UPeekTreeClause x) (udcs, upcs, uprcs, uptcs, upccs, tlms) = (    udcs,     upcs,     uprcs, x : uptcs,     upccs,     tlms)
          f (UPeekColClause x)  (udcs, upcs, uprcs, uptcs, upccs, tlms) = (    udcs,     upcs,     uprcs,     uptcs, x : upccs,     tlms)
          f (ToLensMatch x)    (udcs, upcs, uprcs, uptcs, upccs, tlms) = (    udcs,     upcs,     uprcs,     uptcs,     upccs, x : tlms)

peekDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
peekDecs utype v =
    do uptype <- upathType v
       (clauses :: [WriterType]) <- execWriterT (doNode (pathControl v) v)
       let (udcs, upcs, uprcs, uptcs, upccs, tlms) = partitionClauses clauses
       hasInst <- doType (asType v)
       when (not hasInst)
            (instanceD' (cxt []) [t|PathStart $utype $(asTypeQ v)|]
               (sequence
                [{-pure (dataInstD (cxt []) ''UPeek [utype, asTypeQ v]
                                 [normalC (asName (makeUPeekCon (ModelType (asName v))))
                                          [strictType notStrict [t|UPath $utype $(asTypeQ v)|],
                                           strictType notStrict [t|Maybe $utype|]]]
                                 [''Eq, ''Ord, ''Read, ''Show, ''Generic, ''FromJSON, ''ToJSON]),
                 pure (funD 'upeekCons [clause [] (normalB (conE (asName (makeUPeekCon (ModelType (asName v)))))) []]),
                 pure (funD 'upeekPath [newName "p" >>= \p -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [varP p, wildP]] (normalB (varE p)) []]),
                 pure (funD 'upeekValue [newName "x" >>= \x -> clause [conP (asName (makeUPeekCon (ModelType (asName v)))) [wildP, varP x]] (normalB (varE x)) []]),-}
                 pure (tySynInstD ''UPath (tySynEqn [utype, asTypeQ v] (pure uptype))),
                 funD' 'upeekRow (case uprcs of
                                    [] -> pure [clause [wildP, wildP] (normalB [| Node (Peek idPath Nothing) [] |]) []]
                                    _ -> pure uprcs),
                 funD' 'upeekTree (case uptcs of
                                     [] -> pure [do x <- newName "x"
                                                    clause [wildP, wildP, varP x] (normalB [| Node (Peek idPath (Just (u $(varE x)))) [] |]) []]
                                     _ -> pure uptcs),
                 funD' 'upeekCol (case upccs of
                                    [] -> pure [do x <- newName "x"
                                                   clause [wildP, wildP, varP x] (normalB [|Node (Peek idPath (Just (u $(varE x)))) []|]) []]
                                    _ -> pure upccs)
                ]))
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
#if MIN_VERSION_template_haskell(2,11,0)
                tells [dataD (cxt []) (asName pname) [] Nothing upcs (sequence (map conT supers))]
#else
                tells [dataD (cxt []) (asName pname) [] upcs supers]
#endif
                telld [d|instance IsPath $(asTypeQ pname) where
                            type UType $(asTypeQ pname) = $utype
                            type SType $(asTypeQ pname) = $(asTypeQ v)
                            idPath = $(asConQ pname)
                            toLens p = $(caseE [|p|] tlms)|])
    where
      doType (ConT f) | nameBase f == "AbbrevPairs" = pure True
      doType (ConT f) | nameBase f == "AbbrevPair" = pure True
      doType (ConT f) | nameBase f == "Authors" = pure True
      doType (ConT f) | nameBase f == "EUI" = pure True
      doType (ConT f) | nameBase f == "MaybeImageFile" = pure True
      doType (ConT f) | nameBase f == "MEUI" = pure True
      doType (ConT f) | nameBase f == "ReportImages" = pure True
      doType (ConT f) | nameBase f == "MIM" = pure True
      doType (ConT f) | nameBase f == "MRR" = pure True
      doType (ConT f) | nameBase f == "MarkupPairs" = pure True
      doType (ConT f) | nameBase f == "MarkupPair" = pure True
      doType (ConT f) | nameBase f == "Markups" = pure True
      doType (ConT f) | nameBase f == "ReportElems" = pure True
      doType typ@(ConT name) = qReify name >>= doInfo typ
      doType (AppT (AppT (ConT name) _a) _b) | name == ''Map = pure True
      doType (AppT (AppT (TupleT 2) _a) _b ) = pure True
      doType typ = trace ("type: " ++ show typ) (pure False)
      doInfo typ (TyConI dec) = doDec typ dec
      doInfo typ (FamilyI dec _) = doDec typ dec
      doInfo typ _ = trace ("type: " ++ show typ) (pure False)
      doDec _typ (TySynD _ _ typ') = doType typ'
      doDec typ _ = trace ("type: " ++ show typ) (pure False)

pathControl :: forall m. (TypeGraphM m, MonadWriter [WriterType] m) => TGVSimple -> Control m Hop () ()
pathControl v =
    Control
    { _doSelf = pure ()
    , _doOrder = \_i _typ -> pure ()
    , _doMap = \_i _typ -> pure ()
    , _doList = \_e -> pure ()
    , _doPair = \_ftyp _styp -> pure ()
    , _doMaybe = \_typ -> pure ()
    , _doEither = \_ltyp _rtyp -> pure ()
    , _doSimple =
          do let pname = makeUPathType (ModelType (asName v))
             f <- runQ $ newName "f"
             tell [UDescClause $ clause [varP f, wildP] (normalB [| describe' $(varE f) (Proxy :: Proxy $(asTypeQ v)) |]) [],
                   ToLensMatch (match wildP (normalB [|lens u (\s a -> maybe s id (unU' a))|]) []),
                   UPathCon (normalC (asName pname) [])]
    , _doView =
        \_typ ->
            do doHops wildP
                      [Hop { upat = (\p -> conP 'Path_To [wildP, p])
                           , xpaths = [| [Path_To Proxy] |]
                           , pnext = [|\(Path_To Proxy q) -> q|]
                           , pprev = [|Path_To Proxy|]}]
    , _doField =
        \fld@(_tname, _con, Right fname) typ ->
            do w <- tgvSimple' typ >>= tgv (Just fld)

               let hop = Hop { upat = conP (asName (makeUFieldCon fld)) . (: [])
                             , xpaths = [|[$(asConQ (makeUFieldCon fld))]|]
                             , pnext = (runQ (newName "p") >>= \p -> lamE [conP (asName (makeUFieldCon fld)) [varP p]] (varE p))
                             , pprev = (conE (asName (makeUFieldCon fld))) }
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
               tell [ToLensMatch $ do p <- newName "_p"
                                      match ((upat hop) (varP p)) (normalB [|$(varE (fieldLensNameOld (asName v) fname)) . toLens $(varE p)|]) []]
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
                            ToLensMatch (match wildP (normalB [|lens u (\s a -> maybe s id (unU' a))|]) [])]
    , _doSyns = \() _ -> pure ()
    }

-- | Do several different path hops from one value.
doHops :: (TypeGraphM m, MonadWriter [WriterType] m) => PatQ -> [Hop] -> m ()
doHops xpat hops = do
  x <- runQ $ newName "x"
  tell [UPeekRowClause $
          do clause [wildP, asP' x xpat]
                    (normalB [|Node (Peek idPath Nothing)
                                    (concat $(listE (map (\hop -> [|concatMap (makeRow $(varE x)) $(xpaths hop)|]) hops)))|])
                    [],
        UPeekTreeClause $
          do d <- newName "d"
             case null hops of
               -- There are no hops from here, so no subnodes.
               True -> clause [wildP, wildP, asP' x xpat]
                              (normalB [|Node (Peek idPath (Just (u $(varE x)))) []|])
                              []
               False ->  clause [wildP, varP d, asP' x xpat]
                                (normalB [|case $(varE d) of
                                             -- We reached the desired depth
                                             Just 0 -> Node (Peek idPath (Just (u $(varE x)))) []
                                             _ -> Node (Peek idPath Nothing)
                                                       (concat $(listE (map (\hop -> [|concatMap (makeTrees $(varE d) $(varE x)) $(xpaths hop)|]) hops)))|])
                                []]
  tell (map (\hop ->
                 UPeekColClause $ do
                   p <- newName "_p"
                   q <- newName "_q"
                   clause [wildP, asP p ((upat hop) (varP q)), asP' x xpat]
                              (normalB [|Node (Peek idPath Nothing)
                                              (makeCol $(varE x)
                                                       $(pprev hop)
                                                       $(pnext hop)
                                                       $(varE p)
                                              )|])
                              []) hops ++
        [UPeekColClause $ do
           p <- newName "_p"
           clause [wildP, varP p, asP' x xpat]
                  (normalB [|Node (Peek idPath (Just (u $(varE x)))) []|])
                  []])

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

-- makeUPeekCon :: (HasName s) => ModelType s -> PeekCon
-- makeUPeekCon (ModelType s) = PeekCon (mkName ("UPeek_" ++ nameBase (asName s)))

instanceD' :: (TypeGraphM m, MonadWriter [Dec] m) => CxtQ -> TypeQ -> m [DecQ] -> m ()
instanceD' cxt' typ decs =
    instanceD cxt' typ <$> decs >>= runQ >>= tell . (: [])

funD' :: (TypeGraphM m, MonadWriter [Dec] m) => Name -> m [ClauseQ] -> m DecQ
funD' name clauses = funD name <$> clauses
