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
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs
    ( pathDecs
    ) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad as List ( mapM )
import Control.Monad.Reader (runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Control.Monad.States (MonadStates(getPoly, putPoly), modifyPoly)
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (MonadWriter, execWriterT, tell, WriterT)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable (mapM_)
import Data.Foldable as Foldable
import Data.List as List (concatMap, intercalate, map)
import Data.Map as Map (Map, toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set as Set (delete, minView)
import Data.Set.Extra as Set (insert, map, member, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Data.Tree (Tree(Node))
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IsPathType(idPath), IsPathNode(PVType, pvTree), IsPath(..), Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType, viewLens)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify), VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), unE, ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, allPathStarts, goalReachableSimple, reachableFromSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, field, TGV, TGVSimple, syns, TypeGraphVertex(bestType), typeNames, vsimple)

pathDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
pathDecs = execWriterT $ allPathStarts >>= \ps -> Foldable.mapM_ doNode ps >> doPVType ps

doPVType :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => Set TGVSimple -> m ()
doPVType vs = do
  mapM_ doVert (Foldable.toList vs)
    where
      doVert :: TGVSimple -> m ()
      doVert v = do
        gs <- pathKeys v
        let cons = concat (List.map (doPair v) (Foldable.toList gs))
        case bestName v of
          Just vn -> do
                   let leName = mkName ("PV_" ++ nameBase vn)
                   runQ (dataD (return []) leName [] cons [''Eq, ''Show]) >>= tell . (: [])
          _ -> return ()
      doPair :: TGVSimple -> TGVSimple -> [ConQ]
      doPair v g =
          let Just (vp, _) = bestPathTypeName v in
          case (bestName v, bestName g) of
            (Just vn, Just gn) ->
                [normalC (mkName ("PV_" ++ nameBase vn ++ "_" ++ nameBase gn))
                         [(,) <$> notStrict <*> [t|$(conT vp) $(pure (view (etype . unE) g))|],
                          (,) <$> notStrict <*> pure (view (etype . unE) g)]]
            _ -> []

doNode :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  -- generate the path type declarations
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
  viewType <- viewInstanceType (view etype v)
  case () of
    _ | selfPath -> return ()
      | simplePath -> maybe (error $ "pathTypeDecs: simple path type has no name: " ++ pprint' v) doSimplePath (bestNames v)
      | isJust viewType ->
          do let b = fromJust viewType
             viewPath b
    _ -> doNames
  -- generate the lens declarations
  case simplePath of
    False -> mapM makePathLens (Foldable.toList (typeNames v)) >>= {- t1 >>= -} tell . concat
    _ -> return ()
  -- generate the path instance declarations
  pathKeys v >>= Set.mapM_ (pathInstanceDecs v)
    where
      doNames = mapM_ (\tname -> runQ (reify tname) >>= doInfo) (typeNames v)

      doSimplePath :: (Name, Name, Set Name) -> m ()
      doSimplePath (tname, pname, syns') = do
        a <- runQ $ newName "a"
        runQ (dataD (return []) pname [PlainTV a] [normalC pname []] supers) >>= tell . (: [])
        doIsPathType pname a
        doIsPathNode v
        mapM_ (\psyn -> runQ (newName "a" >>= \a -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) >>= tell . (: [])) (Foldable.toList syns')

      -- viewPath [t|Text|] = data Path_Branding a = Path_Branding (Path_Text a)
      viewPath :: Type -> m ()
      viewPath styp = do
        let Just tname = bestTypeName v
            Just (pname, syns') = bestPathTypeName v
            -- gname = mkName ("Goal_" ++ nameBase pname)
        skey <- expandType styp >>= typeVertex
        a <- runQ $ newName "a"
        ptype <- pathType (varT a) skey
        -- A view type may have a type variable, which
        -- we need to replace with the goal type a.
        let ptype' = substitute (VarT a) ptype
        runQ (sequence (dataD (return []) pname [PlainTV a]
                              [ normalC (mkName (nameBase pname ++ "_View")) [strictType notStrict (pure ptype')]
                              , normalC (mkName (nameBase pname)) []
                              ] supers
                         : List.map (\psyn -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) (Foldable.toList syns'))) >>= tell
        doIsPathType pname a
        doIsPathNode v

      substitute :: Type -> Type -> Type
      substitute gtype (AppT x (VarT _)) = (AppT x gtype)
      substitute gtype (AppT a b) = AppT (substitute gtype a) (substitute gtype b)
      substitute gtype (VarT _) = gtype
      substitute _ x = x

      doInfo (TyConI dec) =
          -- tell [d| z = $(litE (stringL ("doDec " ++ pprint' dec))) |] >>
          doDec dec
      doInfo (FamilyI dec _insts) = doDec dec
      doInfo info = error $ "pathTypeDecs - unexpected Info: " ++ pprint' info ++ "\n  " ++ show info
      doDec :: Dec -> m ()
      -- If we have a type synonym, we can create a path type synonym
      doDec (TySynD _ _ typ') =
          do a <- runQ $ newName "a"
             key' <- expandType typ' >>= typeVertex
             ptype <- pathType (varT a) key'
             mapM_ (\pname -> runQ (tySynD pname [PlainTV a] (return ptype)) >>= tell . (: [])) (pathTypeNames' v)
      doDec (NewtypeD _ tname _ con _) = doDataD tname [con]
      doDec (DataD _ tname _ cons _) = doDataD tname cons
      doDec (FamilyD _flavour _name _tvbs _mkind) = return ()
      doDec dec = error $ "doName - unexpected Dec: " ++ pprint dec ++ "\n  " ++ show dec

      doDataD :: Name -> [Con] -> m ()
      doDataD tname cons =
          do a <- runQ $ newName "a"
             mapM (doCon a tname) cons >>= makeDecs a

      makeDecs :: Name -> [[Con]] -> m ()
      makeDecs a pconss =
          case filter (/= []) pconss of
            [pcons] -> mapM_ (\pname -> do let Just tname = bestName v
                                           runQ (dataD (cxt []) pname [PlainTV a] (List.map return (pcons ++ [NormalC pname []])) supers) >>= tell . (: [])
                                           doIsPathType pname a
                                           doIsPathNode v
                             ) (pathTypeNames' v)
            [] | length pconss > 1 -> return () -- enum
            [] -> return ()
                  -- FIXME - if there are paths from several different
                  -- constructors we want an extra path type to
                  -- represent the values for each constructor.
            pconss' ->
                makeDecs a [concat pconss']

      doCon :: (DsMonad m, MonadReaders TypeGraph m) => Name -> Name -> Con -> m [Con]
      doCon a tname (ForallC _ _ con) = doCon a tname con
      doCon _ _ (NormalC _ _) = return []
      doCon _ _ (InfixC _ _ _) = return []
      doCon a tname (RecC cname ts) = concat <$> mapM (doField a tname cname) ts

      -- Each field of the original type turns into zero or more (Con, Clause)
      -- pairs, each of which may or may not have a field representing the path type
      -- of some piece of the field value.  FIXME: This exact code is in PathTypes.hs
      doField :: (DsMonad m, MonadReaders TypeGraph m) => Name -> Name -> Name -> VarStrictType -> m [Con]
      doField a tname cname (fname', _, ftype) =
          do key' <- expandType ftype >>= fieldVertex (tname, cname, Right fname')
             let Just pcname = pathConNameOfField key'
             ptype <- case ftype of
                        ConT ftname -> runQ $ appT (conT (pathTypeNameFromTypeName ftname)) (varT a)
                        -- It would be nice to use pathTypeCall (varT a) key' here, but
                        -- it can't infer the superclasses for (PathType Foo a) - Ord,
                        -- Read, Data, etc.
                        _ -> pathType (varT a) (view vsimple key') -- runQ (appT (appT (conT ''PathType) (pure (view (vsimple . etype . unE) key'))) (varT a))
             case ptype of
               TupleT 0 -> return []
               -- Given the list of clauses for a field's path type, create new
               -- constructor for the field in the parent record and alter the
               -- clauses to match expressions wrapped in this new constructor.
               _ -> (: []) <$> runQ (normalC pcname [strictType notStrict (return ptype)])

      supers :: [Name]
      supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]

      -- Given a vertex in the type graph, return the names of the
      -- corresponding path type and its synonyms.
      pathTypeNames' :: TypeGraphVertex v => v -> Set Name
      pathTypeNames' = Set.map pathTypeNameFromTypeName . typeNames

doIsPathType pname a = runQ [d|instance IsPathType ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell

doIsPathNode :: forall m. (MonadWriter [Dec] m, ContextM m, MonadReaders TypeInfo m, MonadReaders TypeGraph m) =>
                TGVSimple -> m ()
doIsPathNode v =
    let typ = view (etype . unE) v in
    case bestTypeName v of
      Just tname -> do
        (pvc :: [ClauseQ]) <- evalStateT (pvTreeClauses v) mempty
        runQ (instanceD (cxt []) (appT (conT ''IsPathNode) (pure typ))
                [tySynInstD ''PVType (tySynEqn [pure typ] (conT (mkName ("PV_" ++ nameBase tname)))),
                 funD 'pvTree pvc]) >>= tell . (: [])
      Nothing -> return ()

-- | Given a type, compute the corresponding path type.
pathType :: (MonadReaders TypeGraph m, MonadReaders TypeInfo m, ContextM m) =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = do
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype key in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ]
  viewType <- viewInstanceType (view etype key)
  case view (etype . unE) key of
    _ | selfPath -> return $ view (etype . unE) key
      | simplePath -> let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
      | isJust viewType ->
          let Just (pname, _syns) = bestPathTypeName key in
          runQ [t|$(conT pname) $gtyp|]
    ConT tname ->
        runQ $ [t|$(conT (pathTypeNameFromTypeName tname)) $gtyp|]
    AppT (AppT mtyp ityp) etyp
        | mtyp == ConT ''Order ->
            do ipath <- vert ityp >>= pathType gtyp
               epath <- vert etyp >>= pathType gtyp
               runQ [t|Path_OMap $(return ipath) $(return epath)|]
    AppT ListT etyp ->
        do epath <- vert etyp >>= pathType gtyp
           runQ [t|Path_List $(return epath)|]
    AppT (AppT t3 ktyp) vtyp
        | t3 == ConT ''Map ->
            do kpath <- vert ktyp >>= pathType gtyp
               vpath <- vert vtyp >>= pathType gtyp
               runQ [t| Path_Map $(return kpath) $(return vpath)|]
    AppT (AppT (TupleT 2) ftyp) styp ->
        do fpath <- vert ftyp >>= pathType gtyp
           spath <- vert styp >>= pathType gtyp
           runQ [t| Path_Pair $(return fpath) $(return spath) |]
    AppT t1 vtyp
        | t1 == ConT ''Maybe ->
            do epath <- vert vtyp >>= pathType gtyp
               runQ [t|Path_Maybe $(return epath)|]
    AppT (AppT t3 ltyp) rtyp
        | t3 == ConT ''Either ->
            do lpath <- vert ltyp >>= pathType gtyp
               rpath <- vert rtyp >>= pathType gtyp
               runQ [t| Path_Either $(return lpath) $(return rpath)|]
    _ -> do ks <- reachableFromSimple key
            error $ "pathType otherf: " ++ pprint' key ++ "\n" ++
                    intercalate "\n  " ("reachable from:" : List.map pprint' (Foldable.toList ks))

    where
      vert typ = askPoly >>= \(ti :: TypeInfo) -> runReaderT (typeVertex (E typ)) ti

-- Naming conventions

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TGV -> Maybe Name
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ mkName $ "Path_" ++ nameBase tname ++ "_" ++ nameBase fname') (key ^. field)

bestNames :: TypeGraphVertex v => v -> Maybe (Name, Name, Set Name)
bestNames v =
    case (bestTypeName v, bestPathTypeName v) of
      (Just tname, Just (pname, pnames)) -> Just (tname, pname, pnames)
      _ -> Nothing

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: TypeGraphVertex v => v -> Maybe (Name, Set Name)
bestPathTypeName v =
    case (bestType v, typeNames v) of
      (ConT tname, names) -> Just (pathTypeNameFromTypeName tname, Set.map pathTypeNameFromTypeName (Set.delete tname names))
      (_t, s) | null s -> Nothing
      (_t, _s) -> error "bestPathTypeName - unexpected name"

bestTypeName :: TypeGraphVertex v => v -> Maybe Name
bestTypeName v =
    case bestType v of
      ConT tname -> Just tname
      _ -> maybe Nothing (Just . fst) (minView (typeNames v))

pathTypeNameFromTypeName :: Name -> Name
pathTypeNameFromTypeName tname = mkName $ "Path_" ++ nameBase tname

-- | Make lenses for a type with the names described by fieldLensNamePair, which is a little
-- different from the namer used in th-typegraph (for historical reasons I guess.)
makePathLens :: Quasi m => Name -> m [Dec]
makePathLens tname =
    -- runQ (runIO (putStrLn ("makePathLens " ++ nameBase tname))) >>
    qReify tname >>= execWriterT . doInfo
    where
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec (NewtypeD {}) = lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec (DataD {}) =    lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec _ = return ()

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)

-- | Version of fieldLensName suitable for use as argument to
-- findNames below.
fieldLensNamePair :: Name -> Name -> Name -> (String, String)
fieldLensNamePair tname _cname fname = (nameBase fname, nameBase (fieldLensNameOld tname fname))

uncap :: String -> String
uncap (n : ame) = toLower n : ame
uncap "" = ""

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathInstanceDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
                     TGVSimple -> TGVSimple -> m ()
pathInstanceDecs key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  tlc <- execWriterT $ evalStateT (toLensClauses key gkey) mempty
  poc <- execWriterT $ evalStateT (pathsOfClauses key gkey) mempty
  -- clauses' <- runQ $ sequence clauses
  -- exp <- thePathExp gkey key ptyp clauses'
  when (not (null tlc)) $
       (runQ $ sequence
             [instanceD (pure []) [t|IsPath $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'toLens tlc
                , funD 'pathsOf poc
                ]]) >>= tell
    where
      -- Send a single dec to our funky writer monad
      -- tell :: (DsMonad m, MonadWriter [Dec] m) => [DecQ] -> m ()
      -- tell dec = runQ (sequence dec) >>= tell

instance (Monad m, MonadStates InstMap m) => MonadStates InstMap (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates ExpandMap m) => MonadStates ExpandMap (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates String m) => MonadStates String (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates InstMap m) => MonadStates InstMap (StateT (Set TGVSimple) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates ExpandMap m) => MonadStates ExpandMap (StateT (Set TGVSimple) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates String m) => MonadStates String (StateT (Set TGVSimple) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance ContextM m => ContextM (StateT (Set Name) m)
instance ContextM m => ContextM (StateT (Set TGVSimple) m)

toLensClauses :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m) =>
                       TGVSimple -- ^ the type whose clauses we are generating
                    -> TGVSimple -- ^ the goal type key
                    -> StateT (Set Name) m ()
toLensClauses key gkey
    | view etype key == view etype gkey =
        tell [clause [wildP] (normalB [|iso id id|]) []]
toLensClauses key gkey =
  -- Use this to raise errors when the path patterns aren't exhaustive.
  -- That is supposed to be impossible, so this is debugging code.
  -- toLensClauses key gkey ptyp = do
  --   x <- runQ (newName "x")
  --   r <- foldPath control key
  --   return $ r ++ [clause [varP x] (normalB [|error ("toLens (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
  do ptyp <- pathType (pure (bestType gkey)) key
     let v = key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
     viewType <- viewInstanceType (view etype v)
     case view (etype . unE) v of
       _ | selfPath -> return ()
         | simplePath -> return () -- Simple paths only work if we are at the goal type, and that case is handled above.
       typ
         | isJust viewType -> do
             let ltyp = fromJust viewType
             lns <- runQ [|viewLens :: Lens' $(return typ) $(return ltyp)|]
             -- Ok, we have a type key, and a lens that goes between key and
             -- lkey, and we need to create a toLens function for key's path type.
             -- The tricky bit is to extract the path value for lkey from the path
             -- value we have.
             let (AppT (ConT pname) _gtyp) = ptyp
             lkey <- expandType ltyp >>= typeVertex
             doClause gkey ltyp (\p -> conP (mkName (nameBase pname ++ "_View")) [if lkey == gkey then wildP else p]) (pure lns)
             Monad.lift final
       ConT tname ->
           getPoly >>= \s -> if Set.member tname s
                             then return ()
                             else modifyPoly (Set.insert tname) >>
                                  namedTypeClause tname gkey ptyp >>
                                  Monad.lift final
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               do k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
                  Monad.lift final
       AppT ListT _etyp -> return ()
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
                  Monad.lift final
       AppT (AppT (TupleT 2) ftyp) styp ->
           do doClause gkey ftyp (\p -> [p|Path_First $p|]) [|_1|]
              doClause gkey styp (\p -> [p|Path_Second $p|]) [|_2|]
              Monad.lift final
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do doClause gkey etyp (\p -> [p|Path_Just $p|]) [|_Just|]
                  Monad.lift final
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do doClause gkey ltyp (\p -> [p|Path_Left $p|]) [|_Left|]
                  doClause gkey rtyp (\p -> [p|Path_Right $p|]) [|_Right|]
                  Monad.lift final
       _ -> tell [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (view etype key))))) :: Traversal' $(pure (view (etype . unE) key)) $(pure (bestType gkey))|]) [] ]
    where
      -- Add a clause to the toLens function handling unexpected values.
      final :: m ()
      final = tell [ {- newName "u" >>= \u ->
                    clause [varP u] (normalB [|(error $ $(litE (stringL ("Unexpected goal " ++ pprint' gkey ++ " for " ++ pprint' key ++ ": "))) ++
                                                show $(varE u))
                                              |]) [] -} ]

-- | Given a function pfunc that modifies a pattern, add a
-- 'Language.Haskell.TH.Clause' (a function with a typically incomplete
-- pattern) to the toLens method we are building to handle the new
-- pattern.
doClause :: forall m. (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m, MonadStates InstMap m, MonadStates ExpandMap m) =>
            TGVSimple -> Type -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause gkey typ pfunc lns = do
  v <- runQ (newName "v")
  key <- expandType typ >>= typeVertex
  ok <- goalReachableSimple gkey key
  let pat = bool wildP (varP v) (key /= gkey)
      lns' = bool lns [|$lns . toLens $(varE v)|] (key /= gkey)
  when ok $ tell [clause [pfunc pat] (normalB lns') []]

namedTypeClause :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m) =>
                   Name -> TGVSimple -> Type -> StateT (Set Name) m ()
namedTypeClause tname gkey ptyp =
    -- If encounter a named type and the stack is empty we
    -- need to build the clauses for its declaration.
    do nameInfo <- runQ $ reify tname
       case nameInfo of
         TyConI dec -> doDec dec
         _ -> error "doNameClauses"
    where
            doDec :: Dec -> StateT (Set Name) m ()
            doDec (TySynD _ _ typ') =
                do -- If we have a type synonym we can use the corresponding
                   -- path type synonym instead of the path type of the
                   -- alias type.
                  key' <- expandType typ' >>= typeVertex
                  ok <- goalReachableSimple gkey key'
                  case ok of
                    False -> return ()
                    True -> toLensClauses key' gkey
            doDec (NewtypeD _ _ _ con _) = doCons [con]
            doDec (DataD _ _ _ cons _) = doCons cons
            doDec dec = error $ "doName - unexpected Dec: " ++ show dec

            doCons :: [Con] -> StateT (Set Name) m ()
            doCons cons = ((concatMap snd . concat) <$> List.mapM doCon cons) >>= tell
                -- clauses <- (concatMap snd . concat) <$> mapM doCon cons
                -- tell $ clauses ++ [newName "u" >>= \u -> clause [varP u] (normalB [|error $ "Goal " ++ $(lift (pprint' gkey)) ++ " unexpected for " ++ $(lift (show tname)) ++ ": " ++ show $(varE u)|]) []]

            -- For each constructor of the original type, we create a list of pairs, a
            -- path type constructor and the clause which recognizes it.
            doCon :: Con -> StateT (Set Name) m [(Con, [ClauseQ])]
            doCon (ForallC _ _ con) = doCon con
            doCon (NormalC _ _) = return []
            doCon (InfixC _ _ _) = return []
            doCon (RecC cname ts) = concat <$> List.mapM (doField cname) ts

            -- Each field of the original type turns into zero or more (Con, Clause)
            -- pairs, each of which may or may not have a field representing the path type
            -- of some piece of the field value.
            doField :: Name -> VarStrictType -> StateT (Set Name) m [(Con, [ClauseQ])]
            doField cname (fn, _, ft) = do
                    fkey <- expandType ft >>= fieldVertex (tname, cname, Right fn)
                    ok <- goalReachableSimple gkey (view vsimple fkey)  -- is the goal type reachable from here?
                    case ok of
                      False -> return []  -- Goal type isn't reachable, return empty clause list
                      True ->
                          do -- Build a type expression for the path type, inserting any
                             -- necessary declarations into the state.  Also, build an
                             -- expression for the lens that turns this field value into the
                             -- goal type.
                             clauses <- runQ (newName "_x") >>= \x -> return [clause [varP x] (normalB [|toLens $(varE x)|]) []]
                             let Just pcname = pathConNameOfField fkey
                             ptype' <- pathType (pure (bestType gkey)) (view vsimple fkey)
                             -- This is the new constructor for this field
                             con <- runQ $ normalC pcname [strictType notStrict (return ptype')]
                             -- These are the field's clauses.  Each pattern gets wrapped with the field path constructor,
                             -- and each field lens gets composed with the lens produced for the field's type.
                             let goal = view (vsimple . etype) fkey == view etype gkey
                             clauses' <- List.mapM (Monad.lift .
                                                    mapClause (\ pat -> conP pcname [pat])
                                                              (\ lns -> if goal
                                                                        then varE (fieldLensNameOld tname fn)
                                                                        else [|$(varE (fieldLensNameOld tname fn)) . $lns|])) clauses
                             return [(con, clauses')]


            -- Apply arity 1 functions to the clause pattern and expression
            mapClause :: (DsMonad m, MonadReaders TypeGraph m) => (PatQ -> PatQ) -> (ExpQ -> ExpQ) -> ClauseQ -> m ClauseQ
            mapClause patf lnsf clauseq =
                runQ clauseq >>= \(Clause [pat] (NormalB lns) xs) -> return $ clause [patf (pure pat)] (normalB (lnsf (pure lns))) (List.map pure xs)

pathsOfClauses :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m) =>
                       TGVSimple -- ^ the type whose clauses we are generating
                    -> TGVSimple -- ^ the goal type key
                    -> StateT (Set TGVSimple) m ()
pathsOfClauses key gkey
    | view etype key == view etype gkey = tell [clause [wildP, wildP] (normalB [|[idPath] |]) []]
pathsOfClauses key gkey =
  do -- the corresponding path type - first type parameter of ToLens
     ptyp <- pathType (pure (bestType gkey)) key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [view (etype . unE) key]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [view (etype . unE) key]
     viewType <- viewInstanceType (view etype key)
     case view (etype . unE) key of
       _ | selfPath -> return ()
         | simplePath -> return ()
       typ
         | isJust viewType ->
             do let Just vtyp = viewType
                vIsPath <- testIsPath vtyp gkey
                vkey <- expandType vtyp >>= typeVertex
                let Just tname = bestTypeName key
                    Just vname = bestTypeName vkey
                let ptname = mkName ("Path_" ++ nameBase tname)
                let pcname = mkName ("Path_" ++ nameBase tname ++ "_View")
                runQ [d| f x a =
                           $(case vIsPath of
                               True -> [| -- Get the value as transformed by the view lens
                                          let p = $(conE pcname) idPath :: PathType $(pure (view (etype . unE) key)) $(pure vtyp)
                                              [x'] = toListOf (toLens p) x :: [$(pure vtyp)] in
                                          List.map $(conE pcname) (pathsOf x' a {- :: [PathType $(pure vtyp) $(pure (view (etype . unE) gkey))] -}) |]
                               False -> [| [] |]) |] >>= tell . clauses
       ConT tname ->
           doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               -- Return a path for each element of an order, assuming
               -- there is a path from the element type to the goal.
               do vIsPath <- testIsPath vtyp gkey
                  runQ [d| f o a =
                             $(case vIsPath of
                                 True -> [| List.concatMap (\(k, v) -> List.map (Path_At k) (pathsOf (v :: $(pure vtyp)) a {-:: [PathType $(pure vtyp) (pure (view (etype . unE) gkey))]-})) (toPairs o) |]
                                 False -> [| [] |]) |] >>= tell . clauses
       AppT ListT _etyp -> return ()
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do vIsPath <- testIsPath vtyp gkey
                  runQ [d| f mp a =
                             $(case vIsPath of
                                 True -> [| List.concatMap (\(k, v) -> List.map (Path_Look k) (pathsOf (v :: $(pure vtyp)) a {-:: [PathType $(pure vtyp) (pure (view (etype . unE) gkey))]-})) (Map.toList mp) |]
                                 False -> [| [] |]) |] >>= tell . clauses
       AppT (AppT (TupleT 2) ftyp) styp ->
           do fIsPath <- testIsPath ftyp gkey
              sIsPath <- testIsPath styp gkey
              -- trace ("testIsPath " ++ pprint styp ++ " " ++ pprint gkey ++ " -> " ++ show sIsPath) (return ())
              runQ [d| f (x, _) a =
                         $(case fIsPath of
                             True -> [| List.map Path_First (pathsOf (x :: $(pure ftyp)) a {- :: [PathType $(pure ftyp) $(pure (view (etype . unE) gkey))] -}) |]
                             False -> [| [] |]) |] >>= tell . clauses
              runQ [d| f (_, x) a =
                         $(case sIsPath of
                             True -> [| List.map Path_Second (pathsOf (x :: $(pure styp)) a {- :: [PathType $(pure styp) $(pure (view (etype . unE) gkey))] -}) |]
                             False -> [| [] |]) |] >>= tell . clauses
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do eIsPath <- testIsPath etyp gkey
                  runQ [d| f (Just x) a =
                             $(case eIsPath of
                                 True -> [| List.map Path_Just (pathsOf (x :: $(pure etyp)) a {- :: [PathType $(pure etyp) $(pure (view (etype . unE) gkey))] -}) |]
                                 False -> [| [] |]) |] >>= tell . clauses
                  runQ [d| f Nothing a = [] |] >>= tell . clauses
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do -- Are there paths from the left type to a?  This is
                  -- the test we use in pathInstanceDecs, but using it
                  -- here is kind of a hack.
                  lIsPath <- testIsPath ltyp gkey
                  rIsPath <- testIsPath rtyp gkey
                  runQ [d| f (Left x) a =
                             $(case lIsPath of
                                 True -> [| List.map Path_Left (pathsOf (x :: $(pure ltyp)) a {- :: [PathType $(pure ltyp) $(pure (view (etype . unE) gkey))] -}) |]
                                 False -> [| [] |]) |] >>= tell . clauses
                  runQ [d| f (Right x) a =
                             $(case rIsPath of
                                 True -> [| List.map Path_Right (pathsOf (x :: $(pure rtyp)) a {- :: [PathType $(pure rtyp) $(pure (view (etype . unE) gkey))] -}) |]
                                 False -> [| [] |]) |] >>= tell . clauses
       _ -> tell [clause [wildP, wildP] (normalB [|error $ "pathsOfClauses - unexpected type: " ++ pprint key|]) []]
    where
      doName :: Name -> StateT (Set TGVSimple) m ()
      doName tname = do
        ns <- get
        case Set.member key ns of
          True -> return ()
          False -> modify (Set.insert key) >> qReify tname >>= lift . doInfo
      doInfo :: Info -> m ()
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec :: Dec -> m ()
      doDec (NewtypeD cx tname binds con supers) = doDec (DataD cx tname binds [con] supers)
      doDec (DataD cx tname binds cons supers) = mapM_ (doCon cons) cons
      doCon :: [Con] -> Con -> m ()
      doCon cons (InfixC lhs cname rhs) = doCon cons (NormalC cname [lhs, rhs])
      doCon cons (ForallC binds cx con) = doCon cons con -- Should probably do something here
      doCon cons con@(InfixC lhs cname rhs) = do
        [b@(btype, bname, bpath),
         c@(ctype, cname, cpath)] <- mapM (\((_, ftype), n) -> do
                                             fname <- runQ (newName ("a" ++ show n))
                                             fpath <- testIsPath ftype gkey
                                             return (ftype, fname, fpath)) (zip [lhs, rhs] [1..]) :: m [(Type, Name, Bool)]
        runQ [d| f $(infixP (varP bname) cname (varP cname)) a =
                   concat [$(case bpath of
                               True -> [|List.map (error "doCon InfixC") (pathsOf ($(varE bname) :: $(pure btype)) a)|]
                               False -> [| [] |]),
                           $(case cpath of
                               True -> [|List.map (error "doCon InfixC") (pathsOf ($(varE cname) :: $(pure ctype)) a)|]
                               False -> [| [] |])] |] >>= tell . clauses
      doCon cons con@(NormalC cname binds) = do
        tns <- mapM (\((_, ftype), n) -> do
                       fname <- runQ (newName ("a" ++ show n))
                       fpath <- testIsPath ftype gkey
                       return (ftype, fname, fpath)) (zip binds [1..]) :: m [(Type, Name, Bool)]
        runQ [d| f $(conP cname (List.map (varP . view _2) tns)) a =
                   concat $(listE (List.map (\(ftype, fname, fpath) ->
                                                 case fpath of
                                                   True -> [|List.map (error "doCon NormalC") (pathsOf ($(varE fname) :: $(pure ftype)) a)|]
                                                   False -> [| [] |]) tns)) |] >>= tell . clauses
      doCon cons con@(RecC cname vbinds) = do
        tns <- mapM (\((fname, _, ftype), n) -> do
                       fparm <- runQ (newName ("a" ++ show n))
                       fpath <- testIsPath ftype gkey
                       return (ftype, fparm, fpath, fname)) (zip vbinds [1..]) :: m [(Type, Name, Bool, Name)]
        runQ [d| f $(conP cname (List.map (varP . view _2) tns)) a =
                   concat $(listE (List.map (\(ftype, fparm, fpath, fname) ->
                                                 let Just tname = bestName key in
                                                 let pcon = conE (mkName ("Path_" ++ nameBase tname ++ "_" ++ nameBase fname)) in
                                                 case fpath of
                                                   True -> [|List.map $pcon (pathsOf ($(varE fparm) :: $(pure ftype)) a)|]
                                                   False -> [| [] |]) tns)) |] >>= tell . clauses
{-
          -- Each constructor will generate a clause that matches
          do runQ [d| f x a =
                        ca
          mapM_ (doNamedField cons con vbinds) vbinds
      doCon cons con@(NormalC cname binds) = mapM_ (doField cons con binds) binds
      doField cons con flds (_, ftype) =
          do fIsPath <- testIsPath ftype gkey
          run [d| f x a =
                    undefined
      doNamedField cons con flds (fname, _, ftype) =
          undefined
-}

      testIsPath typ gkey = do
        expandType typ >>= typeVertex >>= \key -> execWriterT (evalStateT (toLensClauses key gkey) mempty) >>= return . not . null

class ToPat x where
    toPat :: x -> PatQ

instance ToPat (Strict, Type) where
    toPat _ = wildP

instance ToPat (Name, Strict, Type) where
    toPat _ = wildP

-- | Extract the template haskell Clauses
class Clauses x where
    clauses :: x -> [ClauseQ]

instance Clauses Dec where
    clauses (FunD _ xs) = List.map pure xs
    clauses _ = error "No clauses"

instance Clauses a => Clauses [a] where
    clauses = concatMap clauses

pvTreeClauses :: forall m v. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
                 TGVSimple -> StateT (Set Name) m [ClauseQ]
pvTreeClauses v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
     viewType <- viewInstanceType (view etype v)
     case view (etype . unE) v of
       _ | selfPath -> return [clause [wildP] (normalB [|error "self"|]) []]
         | simplePath -> return [clause [wildP] (normalB [|error "simple"|]) []]
       typ
         | isJust viewType ->
             do x <- runQ $ newName "x"
                w <- expandType (fromJust viewType) >>= typeVertex :: StateT (Set Name) m TGVSimple
                let vname = fromMaybe (error $ "No name for " ++ pprint v) (bestTypeName v)
                    wname = fromMaybe (error $ "No name for " ++ pprint w ++ ", view of " ++ pprint v) (bestTypeName w)
                    ptname = mkName ("Path_" ++ nameBase vname)
                    -- pcname = mkName ("Path_" ++ nameBase vname ++ "_" ++ nameBase wname)
                    pvname = mkName ("PV_" ++ nameBase vname ++ "_" ++ nameBase wname)
                -- mname <- runQ $ lookupValueName s
                -- let pvname = fromMaybe (error $ "Not declared: " ++ show s) mname
                sf <- pvTreeClauses w
                -- Node (PV_Report_ReportView p $(varE x)) [pvTree w]
                return [clause [varP x] (normalB [| let p = (error "view") :: $(conT ptname) $(conT wname) in
                                                    Node ($(conE pvname) p (let [r] = toListOf (toLens p) $(varE x) in r))
                                                         [error ("subtype nodes for " ++ show wname)] :: Tree (PVType $(pure (view (etype . unE) v))) |]) []]
                -- PV_Report_ReportView (Path_Report_View undefined)
                -- return [clause [wildP] (normalB [|error "view"|]) []]
             -- [|Node (PV_Report_ReportView p x) <$> pvTreeClauses (doReportViewFields x
             -- tell [clause [wildP] (normalB [|error "view"|]) []]
       ConT tname -> return [clause [wildP] (normalB [|error "named"|]) []]
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order -> return [clause [wildP] (normalB [|error "order"|]) []]
       AppT ListT _etyp -> return [clause [wildP] (normalB [|error "list"|]) []]
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map -> return [clause [wildP] (normalB [|error "map"|]) []]
       AppT (AppT (TupleT 2) ftyp) styp -> return [clause [wildP] (normalB [|error "pair"|]) []]
       AppT t1 etyp
           | t1 == ConT ''Maybe -> return [clause [wildP] (normalB [|error "maybe"|]) []]
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either -> return [clause [wildP] (normalB [|error "either"|]) []]
       _ -> return [clause [wildP] (normalB [|error "other"|]) []]
{-
pvTreeClauses key gkey _ptyp
    | view etype key == view etype gkey =
        tell [clause [wildP] (normalB [|undefined|]) []]
pvTreeClauses key gkey ptyp =
    tell [clause [wildP] (normalB [|undefined|]) []]
-}
