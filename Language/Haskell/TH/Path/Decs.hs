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
{-# OPTIONS_GHC -ddump-minimal-imports #-}
module Language.Haskell.TH.Path.Decs
    ( pathDecs
    ) where

import Control.Lens hiding (cons)
import Control.Monad (when)
import Control.Monad as List ( mapM )
import Control.Monad.Reader (runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Control.Monad.State (evalStateT, StateT)
import Control.Monad.States (MonadStates(getPoly, putPoly), modifyPoly)
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable (Foldable(toList), mapM_)
import Data.Foldable
import Data.List as List (intercalate, map)
import Data.Map as Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Set as Set (delete)
import Data.Set.Extra as Set (insert, map, member, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Context (InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IsPathType(idPath), IsPath(..), Path_List, Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.Order (lens_omat, Order)
import Language.Haskell.TH.Path.View (viewInstanceType, viewLens)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify), VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), unE, ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, allPathStarts, goalReachableSimple, reachableFromSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (etype, field, TGV, TGVSimple, TypeGraphVertex(bestType), typeNames, vsimple)

pathDecs :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
pathDecs = allPathStarts >>= execWriterT . Foldable.mapM_ doNode

doNode :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  -- generate the path type declarations
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
  viewType <- viewInstanceType (let (E typ) = view etype v in typ)
  case view (etype . unE) v of
    _ | selfPath -> return ()
      | simplePath -> maybe (error $ "pathTypeDecs: simple path type has no name: " ++ pprint' v) (uncurry doSimplePath) (bestPathTypeName v)
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

      doSimplePath :: Name -> Set Name -> m ()
      doSimplePath pname syns = do
        runQ (newName "a" >>= \a -> dataD (return []) pname [PlainTV a] [normalC pname []] supers) >>= tell . (: [])
        runQ [d|instance IsPathType ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell
        mapM_ (\psyn -> runQ (newName "a" >>= \a -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) >>= tell . (: [])) (toList syns)

      -- viewPath [t|Text|] = data Path_Branding a = Path_Branding (Path_Text a)
      viewPath :: Type -> m ()
      viewPath styp = do
        let Just (pname, syns) = bestPathTypeName v
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
                         : List.map (\psyn -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) (toList syns))) >>= tell
        runQ [d|instance IsPathType ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell

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
            [pcons] -> mapM_ (\pname -> do runQ (dataD (cxt []) pname [PlainTV a] (List.map return (pcons ++ [NormalC pname []])) supers) >>= tell . (: [])
                                           runQ [d|instance IsPathType ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell
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

      pathTypeNames' :: TypeGraphVertex v => v -> Set Name
      pathTypeNames' = Set.map pathTypeNameFromTypeName . typeNames

-- | Given a type, compute the corresponding path type.
pathType :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates ExpandMap m, MonadStates InstMap m) =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = do
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype key in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ]
  viewType <- viewInstanceType (let (E typ) = view etype key in typ)
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
                    intercalate "\n  " ("reachable from:" : List.map pprint' (toList ks))

    where
      vert typ = askPoly >>= \(ti :: TypeInfo) -> runReaderT (typeVertex (E typ)) ti

-- Naming conventions

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TGV -> Maybe Name
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ mkName $ "Path_" ++ nameBase tname ++ "_" ++ nameBase fname') (key ^. field)

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: TypeGraphVertex v => v -> Maybe (Name, Set Name)
bestPathTypeName v =
    case (bestType v, typeNames v) of
      (ConT tname, names) -> Just (pathTypeNameFromTypeName tname, Set.map pathTypeNameFromTypeName (Set.delete tname names))
      (_t, s) | null s -> Nothing
      (_t, _s) -> error "bestPathTypeName - unexpected name"

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
pathInstanceDecs :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
                     TGVSimple -> TGVSimple -> m ()
pathInstanceDecs key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  clauses <- execWriterT $ evalStateT (pathInstanceClauses key gkey ptyp) mempty
  -- clauses' <- runQ $ sequence clauses
  -- exp <- thePathExp gkey key ptyp clauses'
  when (not (null clauses)) $
       tell1 (instanceD (pure []) [t|IsPath $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'toLens clauses
                ])
    where
      -- Send a single dec to our funky writer monad
      tell1 :: (DsMonad m, MonadWriter [Dec] m) => DecQ -> m ()
      tell1 dec = runQ (sequence ([dec])) >>= tell

instance (Monad m, MonadStates InstMap m) => MonadStates InstMap (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

instance (Monad m, MonadStates ExpandMap m) => MonadStates ExpandMap (StateT (Set Name) m) where
    getPoly = Monad.lift getPoly
    putPoly = Monad.lift . putPoly

pathInstanceClauses :: forall m. (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m, MonadStates InstMap m, MonadStates ExpandMap m) =>
                       TGVSimple -- ^ the type whose clauses we are generating
                    -> TGVSimple -- ^ the goal type key
                    -> Type -- ^ the corresponding path type - first type parameter of ToLens
                    -> StateT (Set Name) m ()
pathInstanceClauses key gkey _ptyp
    | view etype key == view etype gkey =
        tell [clause [wildP] (normalB [|iso id id|]) []]
pathInstanceClauses key gkey ptyp =
  -- Use this to raise errors when the path patterns aren't exhaustive.
  -- That is supposed to be impossible, so this is debugging code.
  -- pathInstanceClauses key gkey ptyp = do
  --   x <- runQ (newName "x")
  --   r <- foldPath control key
  --   return $ r ++ [clause [varP x] (normalB [|error ("toLens (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
  do let v = key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
     viewType <- viewInstanceType (let (E typ) = view etype v in typ)
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
      final = tell [newName "u" >>= \u ->
                    clause [varP u] (normalB [|(error $ $(litE (stringL ("Unexpected goal " ++ pprint' gkey ++ " for " ++ pprint' key ++ ": "))) ++
                                                show $(varE u))
                                              |]) []]

-- | Given a function pfunc that modifies a pattern, add a
-- 'Language.Haskell.TH.Clause' (a function with a typically incomplete
-- pattern) to the toLens instance we are building to handle the new
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

namedTypeClause :: forall m. (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m, MonadStates InstMap m, MonadStates ExpandMap m) =>
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
                    True -> pathInstanceClauses key' gkey ptyp
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
