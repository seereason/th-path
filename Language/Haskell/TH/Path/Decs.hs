{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable (Foldable(toList), mapM_)
import Data.Foldable
import Data.Function (on)
import Data.List as List (intercalate)
import Data.List as List (map, sortBy)
import Data.Map as Map (keys)
import qualified Data.Map as Map (toList)
import Data.Set as Set (delete)
import Data.Set.Extra as Set (insert, map, member, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Context (InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IdPath(idPath), Path(..), Path_List, Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (foldPath, FoldPathControl(..), SinkType)
import Language.Haskell.TH.Path.Order (lens_omat)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify), Lift(lift), VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E, unE), ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames, pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (allLensKeys, allPathKeys, allPathNodes, goalReachableSimple, reachableFromSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (etype, field, TGV, TGVSimple, TypeGraphVertex(bestType), typeNames, vsimple)

-- | Given a type, compute the corresponding path type.
pathType :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates ExpandMap m, MonadStates InstMap m) =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key =
  foldPath control key
    where
          -- Nest the definition of control so it can see this binding of hints,
          -- and call pathType' with a modified hint list.
      control =
              FoldPathControl
                { simplef = let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
                , substf = \_lns _styp ->
                    -- This is safe because hint types are now required to have a name
                    let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
                , pathyf = return $ unE $ view etype key
                , namedf = \tname -> runQ $ [t|$(conT (pathTypeNameFromTypeName tname)) $gtyp|]
                , maybef = \etyp -> do
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_Maybe $(return epath)|]
                , listf = \etyp -> do
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_List $(return epath)|]
                , orderf = \ityp etyp -> do
                    ipath <- vert ityp >>= pathType gtyp
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_OMap $(return ipath) $(return epath)|]
                , mapf = \ktyp vtyp -> do
                    kpath <- vert ktyp >>= pathType gtyp
                    vpath <- vert vtyp >>= pathType gtyp
                    runQ [t| Path_Map $(return kpath) $(return vpath)|]
                , pairf = \ftyp styp -> do
                    fpath <- vert ftyp >>= pathType gtyp
                    spath <- vert styp >>= pathType gtyp
                    runQ [t| Path_Pair $(return fpath) $(return spath) |]
                , eitherf = \ltyp rtyp -> do
                    lpath <- vert ltyp >>= pathType gtyp
                    rpath <- vert rtyp >>= pathType gtyp
                    runQ [t| Path_Either $(return lpath) $(return rpath)|]
                , otherf = do
                    ks <- reachableFromSimple key
                    error $ "pathType otherf: " ++ pprint' key ++ "\n" ++
                            intercalate "\n  " ("reachable from:" : List.map pprint' (toList ks))
                }

      vert typ = askPoly >>= \(ti :: TypeInfo) -> runReaderT (typeVertex (E typ)) ti

{-
-- | pathType for the simplified vertex
pathType' :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates ExpandMap m, MonadStates InstMap m) => TypeQ -> TGV -> m Type
pathType' gtyp key = pathType gtyp (view vsimple key)

-- | Call the type function PathType.
pathTypeCall :: (DsMonad m, MonadReaders TypeGraph m) =>
                TypeQ           -- ^ The goal type - possibly a type variable
             -> TGV -- ^ The type to convert to a path type
             -> m Type
pathTypeCall gtyp key = runQ [t|PathType $(let (E typ) = view (vsimple . etype) key in return typ) $gtyp|]
-}

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

pathDecs :: (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
pathDecs = do
  types <-     execWriterT (allPathNodes >>=  Foldable.mapM_ pathTypeDecs . toList . Set.map (view vsimple))                         >>= return . sortBy (compare `on` show) . List.map friendlyNames
  lenses <-    execWriterT (allLensKeys >>=   Foldable.mapM_ pathLensDecs . Map.keys)                                                >>= return . sortBy (compare `on` show) . List.map friendlyNames
  instances <- execWriterT (allPathKeys >>=   Foldable.mapM_ (\(key, gkeys) -> Set.mapM_ (pathInstanceDecs key) gkeys) . Map.toList) >>= return . sortBy (compare `on` show) . List.map friendlyNames
  -- To do - subpath instances, convert one path into another that stops earlier.
  return (types ++ lenses ++ instances)

pathLensDecs :: (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m, MonadReaders TypeGraph m, MonadWriter [Dec] m) =>
                TGVSimple -> m ()
pathLensDecs key = do
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ]
  case simplePath of
    False -> mapM makePathLens (Foldable.toList (typeNames key)) >>= {- t1 >>= -} tell . concat
    _ -> return ()
    -- where t1 x = trace (pprint' x) (return x)

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
       tell1 (instanceD (pure []) [t|Path $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'toLens clauses
                --, valD (varP 'thePath) (normalB exp) []
                ])

-- | Send a single dec to our funky writer monad
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
  foldPath control key
  --   return $ r ++ [clause [varP x] (normalB [|error ("toLens (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
    where
          control :: FoldPathControl (StateT (Set Name) m) ()
          control =
            FoldPathControl
              { simplef = return () -- Simple paths only work if we are at the goal type, and that case is handled above.
              , substf = \lns ltyp -> do
                  -- Ok, we have a type key, and a lens that goes between key and
                  -- lkey, and we need to create a toLens function for key's path type.
                  -- The tricky bit is to extract the path value for lkey from the path
                  -- value we have.
                  let (AppT (ConT pname) _gtyp) = ptyp
                  lkey <- expandType ltyp >>= typeVertex
                  doClause gkey ltyp (\p -> conP (mkName (nameBase pname ++ "_View")) [if lkey == gkey then wildP else p]) (pure lns)
                  Monad.lift final
              , pathyf = return ()
              , namedf = \tname ->
                         getPoly >>= \s -> if Set.member tname s
                                       then return ()
                                       else modifyPoly (Set.insert tname) >>
                                            namedTypeClause tname gkey ptyp >>
                                            Monad.lift final
              , maybef = \etyp -> do
                  doClause gkey etyp (\p -> [p|Path_Just $p|]) [|_Just|]
                  Monad.lift final
              , listf = \_etyp -> return ()
              , orderf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
                  Monad.lift final
              , mapf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
                  Monad.lift final
              , pairf = \ftyp styp -> do
                  doClause gkey ftyp (\p -> [p|Path_First $p|]) [|_1|]
                  doClause gkey styp (\p -> [p|Path_Second $p|]) [|_2|]
                  Monad.lift final
              , eitherf = \ltyp rtyp -> do
                  doClause gkey ltyp (\p -> [p|Path_Left $p|]) [|_Left|]
                  doClause gkey rtyp (\p -> [p|Path_Right $p|]) [|_Right|]
                  Monad.lift final
              , otherf = tell [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (view etype key))))) :: Traversal' $(pure (unE (view etype key))) $(pure (bestType gkey))|]) [] ]
              }
          final :: m ()
          final = tell [newName "u" >>= \u ->
                            clause [varP u] (normalB [|(error $ $(TH.lift ("Unexpected goal " ++ pprint' gkey ++ " for " ++ pprint' key ++ ": ")) ++
                                                        show $(varE u))
                                                      |]) []]


doClause :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m, MonadStates InstMap m, MonadStates ExpandMap m) =>
            TGVSimple -> Type -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause gkey typ pfunc lns = do
  v <- runQ (newName "v")
  key <- expandType typ >>= typeVertex
  case key == gkey of
    True -> testClause gkey typ (clause [ pfunc wildP ] (normalB lns) [])
    False -> do
      testClause gkey typ (clause [ pfunc (varP v) ] (normalB [|$lns . toLens $(varE v)|]) [])
      when (key == gkey) $ testClause gkey typ (clause [ wildP ] (normalB [|iso id id|]) [])

testClause :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m, MonadStates InstMap m, MonadStates ExpandMap m) =>
              TGVSimple -> Type -> ClauseQ -> m ()
testClause gkey typ cl = do
  ok <- testPath gkey typ
  when ok $ tell [cl]

testPath :: (DsMonad m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadStates InstMap m, MonadStates ExpandMap m) => TGVSimple -> Type -> m Bool
testPath gkey typ = do
  key <- expandType typ >>= typeVertex
  goalReachableSimple gkey key

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
                             clauses' <- List.mapM (mapClause (\ pat -> conP pcname [pat])
                                                              (\ lns -> if goal
                                                                        then varE (fieldLensNameOld tname fn)
                                                                        else [|$(varE (fieldLensNameOld tname fn)) . $lns|])) clauses
                             return [(con, clauses')]


-- | Apply arity 1 functions to the clause pattern and expression
mapClause :: (DsMonad m, MonadReaders TypeGraph m) => (PatQ -> PatQ) -> (ExpQ -> ExpQ) -> ClauseQ -> m ClauseQ
mapClause patf lnsf clauseq =
    runQ clauseq >>= \(Clause [pat] (NormalB lns) xs) -> return $ clause [patf (pure pat)] (normalB (lnsf (pure lns))) (List.map pure xs)

-- | Given a type, generate the corresponding path type declarations
pathTypeDecs :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathTypeDecs key =
  pathTypeDecs'
    where
      pathTypeDecs' = foldPath control key
        where
          control =
            FoldPathControl
              { simplef = maybe (error $ "pathTypeDecs: simple path type has no name: " ++ pprint' key) (uncurry simplePath) (bestPathTypeName key)
              , substf = \_lns styp -> viewPath styp -- maybe (return ()) (uncurry simplePath) (bestPathTypeName key)
              , pathyf = return ()
              , namedf = \_tname -> doNames
              , maybef = \_etyp -> doNames
              , listf = \_etyp -> doNames
              , orderf = \_ityp _etyp -> doNames
              , mapf = \_ktyp _vtyp -> doNames
              , pairf = \_ftyp _styp -> doNames
              , eitherf = \_ltyp _rtyp -> doNames
              , otherf = doNames
              }

      doNames = mapM_ (\tname -> runQ (reify tname) >>= doInfo) (typeNames key)

      simplePath :: Name -> Set Name -> m ()
      simplePath pname syns = do
        runQ (newName "a" >>= \a -> dataD (return []) pname [PlainTV a] [normalC pname []] supers) >>= tell . (: [])
        runQ [d|instance IdPath ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell
        mapM_ (\psyn -> runQ (newName "a" >>= \a -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) >>= tell . (: [])) (toList syns)

      -- viewPath [t|Text|] = data Path_Branding a = Path_Branding (Path_Text a)
      viewPath :: Type -> m ()
      viewPath styp = do
        let Just (pname, syns) = bestPathTypeName key
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
        runQ [d|instance IdPath ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell

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
             mapM_ (\pname -> runQ (tySynD pname [PlainTV a] (return ptype)) >>= tell . (: [])) (pathTypeNames' key)
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
                                           runQ [d|instance IdPath ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell
                             ) (pathTypeNames' key)
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
                        _ -> pathType (varT a) (view vsimple key')
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
