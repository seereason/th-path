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
module Language.Haskell.TH.Path.PathInstanceDecs
    ( pathInstanceDecs
    ) where

import Control.Applicative ((<$>), Applicative(pure))
import Control.Lens hiding (cons) -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable
import Data.List as List (map)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (Path(..), LensHint(..), bestPathTypeName, fieldLensName, pathConNameOfField, Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..))
import Language.Haskell.TH.Path.Monad (R, typeInfo, goalReachable, pathHints, foldPath, FoldPathControl(..))
import Language.Haskell.TH.Path.PathType (pathType)
import Language.Haskell.TH.Path.Lens (idLens, mat)
import Language.Haskell.TH.Path.Order (lens_omat)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.Core (pprint')
import Language.Haskell.TH.TypeGraph.Expand (expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Monad (vertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestType, TypeGraphVertex(..), etype)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)

#if ! MIN_VERSION_base(4,8,0)
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

-- | For a given TypeGraphVertex, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathInstanceDecs :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> TypeGraphVertex -> m ()
pathInstanceDecs gkey key = do
  let gtyp = bestType gkey
  ptyp <- pathType (pure gtyp) key
  (clauses :: [ClauseQ]) <- pathInstanceClauses (pure gtyp) key gkey ptyp
  when (not (null clauses)) $
       tell1 (instanceD (pure []) [t|Path $(pure (bestType key)) $(pure (bestType gkey))|]
                [tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp)),
                 funD 'toLens clauses])
    where
      -- Send a single dec to our funky writer monad
      tell1 :: DecQ -> m ()
      tell1 dec = runQ (sequence (map sequence [[dec]])) >>= tell

pathInstanceClauses :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) =>
                       TypeQ -- ^ the goal type
                    -> TypeGraphVertex -- ^ the type whose clauses we are generating
                    -> TypeGraphVertex -- ^ the goal type key
                    -> Type -- ^ the corresponding path type - first type parameter of ToLens
                    -> m [ClauseQ]
pathInstanceClauses gtyp key gkey ptyp =
  pathHints key >>= pathInstanceClauses'
    where
      pathInstanceClauses' :: [(TypeGraphVertex, LensHint)] -> m [ClauseQ]
      pathInstanceClauses' _hints | view etype key == view etype gkey = return [clause [wildP] (normalB [|idLens|]) []]
      pathInstanceClauses' hints = foldPath control key hints
        where
          control =
            FoldPathControl
              { simplef = return [] -- Simple paths only work if we are at the goal type, and that case is handled above.
              , substf = \lns ltyp -> do
                  -- let ((_, Substitute lns ltyp) : _) = [x | x@(_, Substitute _ _) <- hints]
                  lkey <- view typeInfo >>= runReaderT (expandType ltyp >>= vertex Nothing)
                  lval <- pathValue lkey
                  testClause gkey ltyp (clause [wildP] (normalB [|$(pure lns) . toLens $(pure lval)|]) [])
              , pathyf = return []
              , namedf = \tname -> namedTypeClause gtyp tname gkey ptyp
              , maybef = \etyp -> do
                  e <- runQ (newName "e")
                  testClause gkey etyp (clause [ [p|Path_Maybe $(varP e)|] ] (normalB [|_Just . toLens $(varE e)|]) [])
              , listf = \_etyp -> return []
              , orderf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  v <- runQ (newName "v")
                  testClause gkey vtyp (clause [ [p|Path_At $(varP k) $(varP v)|] ] (normalB [|lens_omat $(varE k) . toLens $(varE v)|]) [])
              , mapf = \_ktyp vtyp -> do
                  k <- runQ (newName "k")
                  v <- runQ (newName "v")
                  testClause gkey vtyp (clause [ [p|Path_Map $(varP k) $(varP v)|] ] (normalB [|mat $(varE k) . toLens $(varE v)|]) [])
              , pairf = \ftyp styp -> do
                  f <- runQ (newName "f")
                  s <- runQ (newName "s")
                  fclause <- testClause gkey ftyp (clause [ [p|Path_First $(varP f)|] ] (normalB [|_1 . toLens $(varE f)|]) [])
                  sclause <- testClause gkey styp (clause [ [p|Path_Second $(varP s)|] ] (normalB [|_2 . toLens $(varE s)|]) [])
                  return $ concat $ [fclause, sclause]
              , eitherf = \ltyp rtyp -> do
                  l <- runQ (newName "l")
                  r <- runQ (newName "r")
                  lclause <- testClause gkey ltyp (clause [ [p|Left $(varP l)|] ] (normalB [|_Left . toLens $(varE l)|]) [])
                  rclause <- testClause gkey rtyp (clause [ [p|Right $(varP r)|] ] (normalB [|_Right . toLens $(varE r)|]) [])
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
  goalReachable gkey key

-- | Return an expression whose type is the path type of the vertex.
pathValue :: (DsMonad m, MonadReader R m) => TypeGraphVertex -> m Exp
pathValue key = maybe (error $ "pathValue - no type name: " ++ pprint' key) (runQ . conE . fst) (bestPathTypeName key)

namedTypeClause :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeQ -> Name -> TypeGraphVertex -> Type -> m [ClauseQ]
namedTypeClause gtyp tname gkey ptyp =
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
                  ok <- goalReachable gkey key'
                  case ok of
                    False -> return []
                    True -> pathInstanceClauses gtyp key' gkey ptyp
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
                    key' <- view typeInfo >>= runReaderT (expandType ft >>= vertex (Just (tname, cname, Right fn)))
                    ok <- goalReachable gkey key'  -- is the goal type reachable from here?
                    case ok of
                      False -> return []  -- Goal type isn't reachable, return empty clause list
                      True ->
                          do -- Build a type expression for the path type, inserting any
                             -- necessary declarations into the state.  Also, build an
                             -- expression for the lens that turns this field value into the
                             -- goal type.
                             clauses <- runQ (newName "x") >>= \x -> return [clause [varP x] (normalB [|toLens $(varE x)|]) []]
                             let Just pcname = pathConNameOfField key'
                             ptype' <- pathType gtyp key'
                             -- This is the new constructor for this field
                             con <- runQ $ normalC pcname [strictType notStrict (return ptype')]
                             -- These are the field's clauses.  Each pattern gets wrapped with the field path constructor,
                             -- and each field lens gets composed with the lens produced for the field's type.
                             clauses' <- mapM (mapClause (\ pat -> conP pcname [pat]) (\ lns -> [|$(varE (fieldLensName tname fn)) . $lns|])) clauses
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
