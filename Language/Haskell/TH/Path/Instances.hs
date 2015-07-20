{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Instances
    ( pathInstances
    ) where

import Control.Applicative
import Control.Lens hiding (cons) -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad as List (mapM)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.RWS (evalRWST)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Map as Map (toList)
import Data.Set.Extra as Set (empty, insert, mapM_, member, Set, singleton)
-- import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, Path(..), Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (foldPath, FoldPathControl(..), typeGraphEdges', SinkType)
import Language.Haskell.TH.Path.Lens (fieldLensName, makePathLens)
import Language.Haskell.TH.Path.PathType (pathType, pathConNameOfField)
import Language.Haskell.TH.Path.Order (lens_omat)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (lift, VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Graph (allLensKeys, allPathKeys, goalReachableSimple, makeTypeGraph, TypeGraph, typeInfo)
import Language.Haskell.TH.TypeGraph.Info (fieldVertex, makeTypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.Vertex (bestType, etype, TGV, TGVSimple, typeNames, vsimple)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

-- | Construct the 'Path' instances for all types reachable from the
-- argument types.  Each edge in the type graph corresponds to a Path instance.
pathInstances :: Q [Type] -> Q [Dec]
pathInstances st = do
  r <- st >>= makeTypeInfo (\t -> maybe mempty singleton <$> runQ (viewInstanceType t)) >>= \ti -> runReaderT (typeGraphEdges' >>= makeTypeGraph) ti
  -- runIO $ putStr ("\nLanguage.Haskell.TH.Path.Types.pathInstances - type graph " ++ pprint (view edges r))
  (_, decs) <- evalRWST (do lmp <- allLensKeys
                            pmp <- allPathKeys
                            Foldable.mapM_ (uncurry pathLensDecs) (Map.toList lmp)
                            Foldable.mapM_ (uncurry pathInstanceDecs) (Map.toList pmp)) r Set.empty
  _ <- runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs" $ decs
  runIO $ putStr ("\nPathInstances finished\n")
  return decs

pathLensDecs :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [Dec] m) => TGVSimple -> Set TGV -> m ()
pathLensDecs key gkeys = do
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ])
  case simplePath of
    False -> mapM makePathLens (Foldable.toList (typeNames key)) >>= {- t1 >>= -} tell . concat
    _ -> return ()
    -- where t1 x = trace (pprint' x) (return x)

pathInstanceDecs :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadWriter [Dec] m) => TGVSimple -> Set TGVSimple -> m ()
pathInstanceDecs key gkeys = Set.mapM_ (pathInstanceDecs' key) gkeys

-- | For a given TypeGraphVertex, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathInstanceDecs' :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadWriter [Dec] m) => TGVSimple -> TGVSimple -> m ()
pathInstanceDecs' key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  clauses <- execWriterT $ evalStateT (pathInstanceClauses key gkey ptyp) mempty
  let final = [newName "u" >>= \u ->
               clause [varP u] (normalB [|(error $ $(lift ("Unexpected goal " ++ pprint' gkey ++ " for " ++ pprint' key ++ ": ")) ++
                                                   show $(varE u))
                                             -- :: Lens' $(let E typ = view etype key in pure typ) $(let E typ = view etype gkey in pure typ)
                                         |]) []]
  -- clauses' <- runQ $ sequence clauses
  -- exp <- thePathExp gkey key ptyp clauses'
  when (not (null clauses)) $
       tell1 (instanceD (pure []) [t|Path $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'toLens (clauses ++ final)
                --, valD (varP 'thePath) (normalB exp) []
                ])

-- | Send a single dec to our funky writer monad
tell1 :: (DsMonad m, MonadWriter [Dec] m) => DecQ -> m ()
tell1 dec = runQ (sequence ([dec])) >>= tell

pathInstanceClauses :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) =>
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
                  lkey <- view typeInfo >>= runReaderT (expandType ltyp >>= typeVertex)
                  doClause gkey ltyp (\p -> conP (mkName (nameBase pname ++ "_View")) [if lkey == gkey then wildP else p]) (pure lns)
              , pathyf = return ()
              , namedf = \tname ->
                         get >>= \s -> if Set.member tname s
                                       then return ()
                                       else modify (Set.insert tname) >>
                                            namedTypeClause tname gkey ptyp
              , maybef = \etyp -> do
                   doClause gkey etyp (\p -> [p|Path_Just $p|]) [|_Just|]
              , listf = \_etyp -> return ()
              , orderf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
              , mapf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
              , pairf = \ftyp styp -> do
                  doClause gkey ftyp (\p -> [p|Path_First $p|]) [|_1|]
                  doClause gkey styp (\p -> [p|Path_Second $p|]) [|_2|]
              , eitherf = \ltyp rtyp -> do
                  doClause gkey ltyp (\p -> [p|Path_Left $p|]) [|_Left|]
                  doClause gkey rtyp (\p -> [p|Path_Right $p|]) [|_Right|]
              , otherf = tell [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (view etype key))))) :: Traversal' $(pure (runExpanded (view etype key))) $(pure (bestType gkey))|]) [] ]
              }

doClause :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) => TGVSimple -> Type -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause gkey typ pfunc lns = do
  v <- runQ (newName "v")
  key <- view typeInfo >>= runReaderT (expandType typ >>= typeVertex)
  case key == gkey of
    True -> testClause gkey typ (clause [ pfunc wildP ] (normalB lns) [])
    False -> do
      testClause gkey typ (clause [ pfunc (varP v) ] (normalB [|$lns . toLens $(varE v)|]) [])
      when (key == gkey) $ testClause gkey typ (clause [ wildP ] (normalB [|iso id id|]) [])

testClause :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) => TGVSimple -> Type -> ClauseQ -> m ()
testClause gkey typ cl = do
  ok <- testPath gkey typ
  when ok $ tell [cl]

testPath :: (DsMonad m, MonadReader TypeGraph m) => TGVSimple -> Type -> m Bool
testPath gkey typ = do
  key <- view typeInfo >>= runReaderT (expandType typ >>= typeVertex)
  goalReachableSimple gkey key

namedTypeClause :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) => Name -> TGVSimple -> Type -> StateT (Set Name) m ()
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
                  key' <- view typeInfo >>= runReaderT (expandType typ' >>= typeVertex)
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
                    fkey <- view typeInfo >>= runReaderT (expandType ft >>= fieldVertex (tname, cname, Right fn))
                    ok <- goalReachableSimple gkey (view vsimple fkey)  -- is the goal type reachable from here?
                    case ok of
                      False -> return []  -- Goal type isn't reachable, return empty clause list
                      True ->
                          do -- Build a type expression for the path type, inserting any
                             -- necessary declarations into the state.  Also, build an
                             -- expression for the lens that turns this field value into the
                             -- goal type.
                             clauses <- runQ (newName "x") >>= \x -> return [clause [varP x] (normalB [|toLens $(varE x)|]) []]
                             let Just pcname = pathConNameOfField fkey
                             ptype' <- pathType (pure (bestType gkey)) (view vsimple fkey)
                             -- This is the new constructor for this field
                             con <- runQ $ normalC pcname [strictType notStrict (return ptype')]
                             -- These are the field's clauses.  Each pattern gets wrapped with the field path constructor,
                             -- and each field lens gets composed with the lens produced for the field's type.
                             let goal = view (vsimple . etype) fkey == view etype gkey
                             clauses' <- List.mapM (mapClause (\ pat -> conP pcname [pat])
                                                              (\ lns -> if goal
                                                                        then varE (fieldLensName tname fn)
                                                                        else [|$(varE (fieldLensName tname fn)) . $lns|])) clauses
                             return [(con, clauses')]


-- | Apply arity 1 functions to the clause pattern and expression
mapClause :: (DsMonad m, MonadReader TypeGraph m) => (PatQ -> PatQ) -> (ExpQ -> ExpQ) -> ClauseQ -> m ClauseQ
mapClause patf lnsf clauseq =
    runQ clauseq >>= \(Clause [pat] (NormalB lns) xs) -> return $ clause [patf (pure pat)] (normalB (lnsf (pure lns))) (List.map pure xs)

#if 0
-- | Apply arity 2 functions to the clause pattern and expression
mapClause2 :: (DsMonad m, MonadReader TypeGraph m) => (PatQ -> PatQ -> PatQ) -> (ExpQ -> ExpQ -> ExpQ) -> Clause -> Clause -> m Clause
mapClause2 patf lnsf (Clause [pat1] (NormalB lns1) xs) (Clause [pat2] (NormalB lns2) ys) =
    runQ $ clause [patf (pure pat1) (pure pat2)] (normalB (lnsf (pure lns1) (pure lns2))) (List.map pure (xs ++ ys))
mapClause2 _ _ x1 x2 = error $ "mapClause - unexpected Clause: " ++ show x1 ++ ", " ++ show x2
#endif

#if 0
pathInstanceDecs' :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadState (Set TypeGraphVertex) m, MonadWriter [[Dec]] m) => m ()
pathInstanceDecs' = do
  unsimplifiedEdges <- view edges
  let unsimplifiedKeys <- Map.keys unsimplifiedEdges
      simplifiedKeys = Set.map (view vsimple) unsimplifiedKeys
      keyMap :: Map TypeGraphVertex (Set TypeGraphVertex)
      keyMap = foldr Set.union mempty (Map.fromList (map (\a -> (view vsimple a, singleton a)) unsimplifiedKeys))
  mapM (\ (a, (hint, s)) -> mapM (\b -> pathType (pure (bestType b)) a >>= \ptyp ->
                                        doClauses keyMap (view vsimple a) b >>= \clauses ->
                                        case clauses of
                                          [] -> return []
                                          _ -> runQ [d|instance Path $(pure (bestType a)) $(pure (bestType b)) where
                                                          type PathType $(pure (bestType a)) $(pure (bestType b)) =
                                                              $(pure ptyp) |] >>= mapM (insertClauses clauses)) (Set.toList s)) (Map.toList es) >>= tell . concat
    where
      -- There is a clause of the toLens function for every pair of
      -- types a b where according to the typegraph we can reach b
      -- from a.
      insertClauses cl (InstanceD c t ds) = return $ InstanceD c t (ds ++ cl)
      -- Clauses corresponding to all the edges from simplified a to unsimplified b
      doClauses keyMap a b =
          case Map.lookup a keyMap of
            Just bs@(_ : _) -> map (doClause a) bs
            _ -> return []
      doClause a b = newName "u" >>= \u ->
                     clause [varP u] (normalB [|error $ $(lift "clause (" ++ pprint' a ++ ") (" ++ pprint' b ++ ")")|]) []
#endif
