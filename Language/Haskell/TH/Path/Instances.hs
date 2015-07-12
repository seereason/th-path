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
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.RWS (evalRWST)
import Control.Monad.State (MonadState, get, modify)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Foldable
import Data.List as List (map)
import Data.Set as Set (empty, insert, member, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (Path(..), fieldLensName, pathConNameOfField, Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (foldPath, FoldPathControl(..), makePathLenses, makeTypeGraphEdges)
import Language.Haskell.TH.Path.PathType (pathType)
import Language.Haskell.TH.Path.Lens (mat)
import Language.Haskell.TH.Path.Order (lens_omat)
import Language.Haskell.TH.Syntax as TH (lift, VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Graph (allLensKeys, goalReachableSimple, makeTypeGraph, TypeGraph, typeInfo)
import Language.Haskell.TH.TypeGraph.Info (makeTypeInfo, vertex)
import Language.Haskell.TH.TypeGraph.Shape (pprint')
import Language.Haskell.TH.TypeGraph.Vertex (bestType, etype, TypeGraphVertex)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

#if ! MIN_VERSION_base(4,8,0)
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

-- | Construct the 'Path' instances for all types reachable from the
-- types in the argument.
pathInstances :: Q [Type] -> Q [Dec]
pathInstances st = do
  r <- st >>= makeTypeInfo >>= makeTypeGraph makeTypeGraphEdges
  (_, decss) <- evalRWST (allLensKeys >>= mapM (uncurry pathInstanceDecs) . toList) r Set.empty
  runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs" $ concat decss

-- | For a given TypeGraphVertex, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathInstanceDecs :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadState (Set TypeGraphVertex) m, MonadWriter [[Dec]] m) => TypeGraphVertex -> TypeGraphVertex -> m ()
pathInstanceDecs gkey key = do
  done <- Set.member key <$> get
  when (not done) $ do
    modify (Set.insert key)
    makePathLenses key
  ptyp <- pathType (pure (bestType gkey)) key
  clauses <- execWriterT $ pathInstanceClauses key gkey ptyp
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
    where
      -- Send a single dec to our funky writer monad
      tell1 :: DecQ -> m ()
      tell1 dec = runQ (sequence (map sequence [[dec]])) >>= tell

pathInstanceClauses :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) =>
                       TypeGraphVertex -- ^ the type whose clauses we are generating
                    -> TypeGraphVertex -- ^ the goal type key
                    -> Type -- ^ the corresponding path type - first type parameter of ToLens
                    -> m ()
pathInstanceClauses key gkey _ptyp | view etype key == view etype gkey = tell [clause [wildP] (normalB [|iso id id|]) []]
pathInstanceClauses key gkey ptyp =
  -- Use this to raise errors when the path patterns aren't exhaustive.
  -- That is supposed to be impossible, so this is debugging code.
  -- pathInstanceClauses key gkey ptyp = do
  --   x <- runQ (newName "x")
  --   r <- foldPath control key
  foldPath control key
  --   return $ r ++ [clause [varP x] (normalB [|error ("toLens (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
    where
          control :: FoldPathControl m ()
          control =
            FoldPathControl
              { simplef = return () -- Simple paths only work if we are at the goal type, and that case is handled above.
              , substf = \lns ltyp -> do
                  -- Ok, we have a type key, and a lens that goes between key and
                  -- lkey, and we need to create a toLens function for key's path type.
                  -- The tricky bit is to extract the path value for lkey from the path
                  -- value we have.
                  let (AppT (ConT pname) _gtyp) = ptyp
                  lkey <- view typeInfo >>= runReaderT (expandType ltyp >>= vertex Nothing)
                  doClause gkey ltyp (\p -> conP (mkName (nameBase pname ++ "_View")) [if lkey == gkey then wildP else p]) (pure lns)
              , pathyf = return ()
              , namedf = \tname -> namedTypeClause tname gkey ptyp
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

doClause :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) => TypeGraphVertex -> Type -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause gkey typ pfunc lns = do
  v <- runQ (newName "v")
  key <- view typeInfo >>= runReaderT (expandType typ >>= vertex Nothing)
  case key == gkey of
    True -> testClause gkey typ (clause [ pfunc wildP ] (normalB lns) [])
    False -> do
      testClause gkey typ (clause [ pfunc (varP v) ] (normalB [|$lns . toLens $(varE v)|]) [])
      when (key == gkey) $ testClause gkey typ (clause [ wildP ] (normalB [|iso id id|]) [])

testClause :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) => TypeGraphVertex -> Type -> ClauseQ -> m ()
testClause gkey typ cl = do
  ok <- testPath gkey typ
  when ok $ tell [cl]

testPath :: (DsMonad m, MonadReader TypeGraph m) => TypeGraphVertex -> Type -> m Bool
testPath gkey typ = do
  key <- view typeInfo >>= runReaderT (expandType typ >>= vertex Nothing)
  goalReachableSimple gkey key

namedTypeClause :: forall m. (DsMonad m, MonadReader TypeGraph m, MonadWriter [ClauseQ] m) => Name -> TypeGraphVertex -> Type -> m ()
namedTypeClause tname gkey ptyp =
    -- If encounter a named type and the stack is empty we
    -- need to build the clauses for its declaration.
    do nameInfo <- runQ $ reify tname
       case nameInfo of
         TyConI dec -> doDec dec
         _ -> error "doNameClauses"
    where
            doDec :: Dec -> m ()
            doDec (TySynD _ _ typ') =
                do -- If we have a type synonym we can use the corresponding
                   -- path type synonym instead of the path type of the
                   -- alias type.
                  key' <- view typeInfo >>= runReaderT (expandType typ' >>= vertex Nothing)
                  ok <- goalReachableSimple gkey key'
                  case ok of
                    False -> return ()
                    True -> pathInstanceClauses key' gkey ptyp
            doDec (NewtypeD _ _ _ con _) = doCons [con]
            doDec (DataD _ _ _ cons _) = doCons cons
            doDec dec = error $ "doName - unexpected Dec: " ++ show dec

            doCons :: [Con] -> m ()
            doCons cons = ((concatMap snd . concat) <$> mapM doCon cons) >>= tell
                -- clauses <- (concatMap snd . concat) <$> mapM doCon cons
                -- tell $ clauses ++ [newName "u" >>= \u -> clause [varP u] (normalB [|error $ "Goal " ++ $(lift (pprint' gkey)) ++ " unexpected for " ++ $(lift (show tname)) ++ ": " ++ show $(varE u)|]) []]

            -- For each constructor of the original type, we create a list of pairs, a
            -- path type constructor and the clause which recognizes it.
            doCon :: Con -> m [(Con, [ClauseQ])]
            doCon (ForallC _ _ con) = doCon con
            doCon (NormalC _ _) = return []
            doCon (InfixC _ _ _) = return []
            doCon (RecC cname ts) = concat <$> mapM (doField cname) ts

            -- Each field of the original type turns into zero or more (Con, Clause)
            -- pairs, each of which may or may not have a field representing the path type
            -- of some piece of the field value.
            doField :: Name -> VarStrictType -> m [(Con, [ClauseQ])]
            doField cname (fn, _, ft) = do
                    fkey <- view typeInfo >>= runReaderT (expandType ft >>= vertex (Just (tname, cname, Right fn)))
                    ok <- goalReachableSimple gkey fkey  -- is the goal type reachable from here?
                    case ok of
                      False -> return []  -- Goal type isn't reachable, return empty clause list
                      True ->
                          do -- Build a type expression for the path type, inserting any
                             -- necessary declarations into the state.  Also, build an
                             -- expression for the lens that turns this field value into the
                             -- goal type.
                             clauses <- runQ (newName "x") >>= \x -> return [clause [varP x] (normalB [|toLens $(varE x)|]) []]
                             let Just pcname = pathConNameOfField fkey
                             ptype' <- pathType (pure (bestType gkey)) fkey
                             -- This is the new constructor for this field
                             con <- runQ $ normalC pcname [strictType notStrict (return ptype')]
                             -- These are the field's clauses.  Each pattern gets wrapped with the field path constructor,
                             -- and each field lens gets composed with the lens produced for the field's type.
                             let goal = view etype fkey == view etype gkey
                             clauses' <- mapM (mapClause (\ pat -> conP pcname [pat])
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
