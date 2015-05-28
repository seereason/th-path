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
module Language.Haskell.TH.Path.Decs
    ( pathInstances
    ) where

import Control.Applicative ((<$>), Applicative(pure))
import Control.Lens hiding (cons) -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.RWS (evalRWST)
import Control.Monad.State (MonadState, get, modify)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable
import Data.List as List (map)
import Data.Set as Set (empty, insert, member, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (Field, Path(..), LensHint(..), bestPathTypeName, fieldLensName, pathConNameOfField, Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..))
import Language.Haskell.TH.Path.Monad (allLensKeys, foldPath, FoldPathControl(..), goalReachableSimple, makePathLenses, makeTypeGraph, pathHints, R, typeInfo)
import Language.Haskell.TH.Path.PathType (pathType)
import Language.Haskell.TH.Path.PathTypeDecs (pathTypeDecs)
import Language.Haskell.TH.Path.Lens (idLens, mat)
import Language.Haskell.TH.Path.Order (lens_omat)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.Core (pprint')
import Language.Haskell.TH.TypeGraph.Expand (expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Monad (vertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestType, TypeGraphVertex(..), etype)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

#if ! MIN_VERSION_base(4,8,0)
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

-- | Create the lenses, path types, and path to lens functions for a
-- named type and all of its subtypes.  Each lens extracts or updates
-- a portion of a value.  Each path type describes the correspondence
-- between a value and the portions of that value available via lens.
-- Each path to lens function turns a path type value into a lens.
pathInstances :: Q [Type] -> [(Maybe Field, Name, Q LensHint)] -> Q [Dec]
pathInstances st hs = do
  r <- makeTypeGraph st hs
  (_, decss) <- evalRWST (allLensKeys >>= mapM (uncurry pathInstanceDecs) . toList) r Set.empty
  runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs" $ concat decss

-- | For a given TypeGraphVertex, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathInstanceDecs :: forall m. (DsMonad m, MonadReader R m, MonadState (Set TypeGraphVertex) m, MonadWriter [[Dec]] m) => TypeGraphVertex -> TypeGraphVertex -> m ()
pathInstanceDecs gkey key = do
  done <- Set.member key <$> get
  when (not done) $ do
    modify (Set.insert key)
    makePathLenses key
    pathTypeDecs key
  ptyp <- pathType (pure (bestType gkey)) key
  (clauses :: [ClauseQ]) <- pathInstanceClauses key gkey ptyp
  when (not (null clauses)) $
       tell1 (instanceD (pure []) [t|Path $(pure (bestType key)) $(pure (bestType gkey))|]
                [tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp)),
                 funD 'toLens clauses])
    where
      -- Send a single dec to our funky writer monad
      tell1 :: DecQ -> m ()
      tell1 dec = runQ (sequence (map sequence [[dec]])) >>= tell

pathInstanceClauses :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) =>
                       TypeGraphVertex -- ^ the type whose clauses we are generating
                    -> TypeGraphVertex -- ^ the goal type key
                    -> Type -- ^ the corresponding path type - first type parameter of ToLens
                    -> m [ClauseQ]
pathInstanceClauses key gkey ptyp = do
  pathHints key >>= pathInstanceClauses'
    where
      pathInstanceClauses' :: [(TypeGraphVertex, LensHint)] -> m [ClauseQ]
      pathInstanceClauses' _hints | view etype key == view etype gkey = return [clause [wildP] (normalB [|idLens|]) []]
      pathInstanceClauses' hints = foldPath control key hints
      -- Use this to raise errors when the path patterns aren't exhaustive.
      -- That is supposed to be impossible, so this is debugging code.
      -- pathInstanceClauses' hints = do
      --   x <- runQ (newName "x")
      --   r <- foldPath control key hints
      --   return $ r ++ [clause [varP x] (normalB [|error ("toLens (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
        where
          control =
            FoldPathControl
              { simplef = return [] -- Simple paths only work if we are at the goal type, and that case is handled above.
              , substf = \lns ltyp -> do
                  -- let ((_, Substitute lns ltyp) : _) = [x | x@(_, Substitute _ _) <- hints]
                  lkey <- view typeInfo >>= runReaderT (expandType ltyp >>= vertex Nothing)
                  lval <- pathValue lkey
                  let lns' = if lkey == gkey then pure lns else [|$(pure lns) . toLens $(pure lval)|]
                  testClause gkey ltyp (clause [wildP] (normalB lns') [])
              , pathyf = return []
              , namedf = \tname -> namedTypeClause tname gkey ptyp
              , maybef = \etyp -> do
                  e <- runQ (newName "e")
                  ekey <- view typeInfo >>= runReaderT (expandType etyp >>= vertex Nothing)
                  let lns = [|_Just|]
                      lns' = if ekey == gkey then lns else [|$lns . toLens $(varE e)|]
                  testClause gkey etyp (clause [ [p|Path_Maybe $(varP e)|] ] (normalB lns') [])
              , listf = \_etyp -> return []
              , orderf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  v <- runQ (newName "v")
                  vkey <- view typeInfo >>= runReaderT (expandType vtyp >>= vertex Nothing)
                  let lns = [|lens_omat $(varE k)|]
                      lns' = if vkey == gkey then lns else [|$lns . toLens $(varE v)|]
                  testClause gkey vtyp (clause [ [p|Path_At $(varP k) $(varP v)|] ] (normalB lns') [])
              , mapf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  v <- runQ (newName "v")
                  vkey <- view typeInfo >>= runReaderT (expandType vtyp >>= vertex Nothing)
                  let lns = [|mat $(varE k)|]
                      lns' = if vkey == gkey then lns else [|$lns . toLens $(varE v)|]
                  testClause gkey vtyp (clause [ [p|Path_Map $(varP k) $(varP v)|] ] (normalB lns') [])
              , pairf = \ftyp styp -> do
                  f <- runQ (newName "f")
                  s <- runQ (newName "s")
                  fkey <- view typeInfo >>= runReaderT (expandType ftyp >>= vertex Nothing)
                  skey <- view typeInfo >>= runReaderT (expandType styp >>= vertex Nothing)
                  let flns = [|_1|]
                      slns = [|_2|]
                      flns' = if fkey == gkey then flns else [|$flns . toLens $(varE f)|]
                      slns' = if skey == gkey then slns else [|$slns . toLens $(varE s)|]
                  fclause <- testClause gkey ftyp (clause [ [p|Path_First $(varP f)|] ] (normalB flns') [])
                  sclause <- testClause gkey styp (clause [ [p|Path_Second $(varP s)|] ] (normalB slns') [])
                  return $ concat $ [fclause, sclause]
              , eitherf = \ltyp rtyp -> do
                  l <- runQ (newName "l")
                  r <- runQ (newName "r")
                  lkey <- view typeInfo >>= runReaderT (expandType ltyp >>= vertex Nothing)
                  rkey <- view typeInfo >>= runReaderT (expandType rtyp >>= vertex Nothing)
                  let llns = [|_Left|]
                      rlns = [|_Right|]
                      llns' = if lkey == gkey then llns else [|$llns . toLens $(varE l)|]
                      rlns' = if rkey == gkey then rlns else [|$rlns . toLens $(varE r)|]
                  lclause <- testClause gkey ltyp (clause [ [p|Left $(varP l)|] ] (normalB llns') [])
                  rclause <- testClause gkey rtyp (clause [ [p|Right $(varP r)|] ] (normalB rlns') [])
                  return $ concat [lclause, rclause]
              , otherf = return [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (view etype key))))) :: Traversal' $(pure (runExpanded (view etype key))) $(pure (bestType gkey))|]) [] ]
              }

testClause :: (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> Type -> ClauseQ -> m [ClauseQ]
testClause gkey typ cl = do
  ok <- testPath gkey typ
  return $ if ok then [cl] else []

testPath :: (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> Type -> m Bool
testPath gkey typ = do
  key <- view typeInfo >>= runReaderT (expandType typ >>= vertex Nothing)
  goalReachableSimple gkey key

-- | Return an expression whose type is the path type of the vertex.
pathValue :: (DsMonad m, MonadReader R m) => TypeGraphVertex -> m Exp
pathValue key = maybe (error $ "pathValue - no type name: " ++ pprint' key) (runQ . conE . fst) (bestPathTypeName key)

namedTypeClause :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => Name -> TypeGraphVertex -> Type -> m [ClauseQ]
namedTypeClause tname gkey ptyp =
    -- If encounter a named type and the stack is empty we
    -- need to build the clauses for its declaration.
    do nameInfo <- runQ $ reify tname
       case nameInfo of
         TyConI dec -> doDec dec
         _ -> error "doNameClauses"
    where
            doDec :: Dec -> m [ClauseQ]
            doDec (TySynD _ _ typ') =
                do -- If we have a type synonym we can use the corresponding
                   -- path type synonym instead of the path type of the
                   -- alias type.
                  key' <- view typeInfo >>= runReaderT (expandType typ' >>= vertex Nothing)
                  ok <- goalReachableSimple gkey key'
                  case ok of
                    False -> return []
                    True -> pathInstanceClauses key' gkey ptyp
            doDec (NewtypeD _ _ _ con _) = doCons [con]
            doDec (DataD _ _ _ cons _) = doCons cons
            doDec dec = error $ "doName - unexpected Dec: " ++ show dec

            doCons :: [Con] -> m [ClauseQ]
            doCons cons = (concatMap snd . concat) <$> mapM doCon cons

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
                             clauses' <- mapM (mapClause (\ pat -> conP pcname [pat]) (\ lns -> if view etype fkey == view etype gkey
                                                                                                then varE (fieldLensName tname fn)
                                                                                                else [|$(varE (fieldLensName tname fn)) . $lns|])) clauses
                             return [(con, clauses')]


-- | Apply arity 1 functions to the clause pattern and expression
mapClause :: (DsMonad m, MonadReader R m) => (PatQ -> PatQ) -> (ExpQ -> ExpQ) -> ClauseQ -> m ClauseQ
mapClause patf lnsf clauseq =
    runQ clauseq >>= \(Clause [pat] (NormalB lns) xs) -> return $ clause [patf (pure pat)] (normalB (lnsf (pure lns))) (List.map pure xs)

-- | Apply arity 2 functions to the clause pattern and expression
mapClause2 :: (DsMonad m, MonadReader R m) => (PatQ -> PatQ -> PatQ) -> (ExpQ -> ExpQ -> ExpQ) -> Clause -> Clause -> m Clause
mapClause2 patf lnsf (Clause [pat1] (NormalB lns1) xs) (Clause [pat2] (NormalB lns2) ys) =
    runQ $ clause [patf (pure pat1) (pure pat2)] (normalB (lnsf (pure lns1) (pure lns2))) (List.map pure (xs ++ ys))
mapClause2 _ _ x1 x2 = error $ "mapClause - unexpected Clause: " ++ show x1 ++ ", " ++ show x2
