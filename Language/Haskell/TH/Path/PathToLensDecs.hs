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
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.PathToLensDecs
    ( ToLens(toLens)
    , Path
    , pathToLensDecs
    ) where

import Control.Applicative ((<$>), Applicative(pure))
import Control.Lens hiding (cons) -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.List as List (map)
import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (R, typeInfo, LensHint(..), bestPathTypeName, fieldLensName, goalReachable, pathConNameOfField, pathHints, Path_OMap(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), foldPath, FoldPathControl(..))
import Language.Haskell.TH.Path.PathType (pathType)
import Language.Haskell.TH.Path.Lens (idLens, mat)
import Language.Haskell.TH.Path.Order (lens_omat)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.Core (pprint')
import Language.Haskell.TH.TypeGraph.Expand (expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Monad (vertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestType, TypeGraphVertex(..), etype)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)

import Data.Foldable
#if ! MIN_VERSION_base(4,8,0)
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

-- | The functional dependency here says that for any given start and
-- end types, there is only one path type that can describe the path
-- from start to end.  The toLens function will typically have several
-- clauses describing different paths for a given (start, goal) pair.
class ToLens s a where
    type Path s a
    toLens :: Path s a -> Traversal' s a

-- instance OrderKey k => ToLens (Path_OMap k a) (Order k a) a where
--     toLens (Path_At k a) = lens_omat k . toLens a

-- | For a given TypeGraphVertex, compute the declaration of the
-- corresponding path type.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathToLensDecs :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> TypeGraphVertex -> m ()
pathToLensDecs gkey key = do
  let gtyp = bestType gkey -- runExpanded (view etype gkey)
  ptyp <- pathType (pure gtyp) key
  ok <- goalReachable gkey key
  when ok $ do
    (clauses :: [ClauseQ]) <- pathToLensClauses (pure gtyp) key gkey ptyp
    -- a <- runQ $ newName "a"
    when (not (null clauses)) $
         tell1 (instanceD (pure []) [t|ToLens $(pure (bestType key)) $(pure (bestType gkey))|]
                  [tySynInstD ''Path (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp)),
                   funD 'toLens clauses])
    where
      -- Send a single dec to our funky writer monad
      tell1 :: DecQ -> m ()
      tell1 dec = runQ (sequence (map sequence [[dec]])) >>= tell

pathToLensClauses :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) =>
                     TypeQ -- ^ the goal type
                  -> TypeGraphVertex -- ^ they type whose clauses we are generating
                  -> TypeGraphVertex -- ^ the goal type key
                  -> Type -- ^ the corresponding path type - first type parameter of ToLens
                  -> m [ClauseQ]
pathToLensClauses gtyp key gkey ptyp =
  pathHints key >>= pathToLensClauses'
    where
      pathToLensClauses' :: [(TypeGraphVertex, LensHint)] -> m [ClauseQ]
      pathToLensClauses' _hints | view etype key == view etype gkey = return [clause [wildP] (normalB [|idLens|]) []]
      pathToLensClauses' hints = foldPath control key hints
        where
          control =
            FoldPathControl
              { simplef = return [] -- Simple paths only work if we are at the goal type, and that case is handled above.
              , substf = \_lns _styp -> do
                  let ((_, Substitute lns ltyp) : _) = [x | x@(_, Substitute _ _) <- hints]
                  lkey <- view typeInfo >>= runReaderT (expandType ltyp >>= vertex Nothing)
                  pval <- pathValue lkey
                  return [clause [wildP] (normalB [|$(pure lns) . toLens $(pure pval)|]) []]
              , pathyf = return []
              , namedf = \tname -> namedTypeClause gtyp tname gkey ptyp
              , maybef = \_etyp -> do
                  e <- runQ (newName "e")
                  return [ clause [ [p|Path_Maybe $(varP e)|] ] (normalB [|_Just . toLens $(varE e)|]) [] ]
              , listf = \_etyp -> return []
              , orderf = \_ityp _etyp -> do
                  k <- runQ (newName "k")
                  v <- runQ (newName "v")
                  return [ clause [ [p|Path_At $(varP k) $(varP v)|] ] (normalB [|lens_omat $(varE k) . toLens $(varE v)|]) [] ]
              , mapf = \_ktyp _vtyp -> do
                  k <- runQ (newName "k")
                  v <- runQ (newName "v")
                  return [ clause [ [p|Path_Map $(varP k) $(varP v)|] ] (normalB [|mat $(varE k) . toLens $(varE v)|]) [] ]
              , pairf = \ftyp styp -> do
                  f <- runQ (newName "f")
                  s <- runQ (newName "s")
                  fclause <- testClause gkey ftyp (clause [ [p|Path_First $(varP f)|] ] (normalB [|_1 . toLens $(varE f)|]) [])
                  sclause <- testClause gkey styp (clause [ [p|Path_Second $(varP s)|] ] (normalB [|_2 . toLens $(varE s)|]) [])
                  return $ catMaybes $ [fclause, sclause]
              , eitherf = \ltyp rtyp -> do
                  l <- runQ (newName "l")
                  r <- runQ (newName "r")
                  lclause <- testClause gkey ltyp (clause [ [p|Left $(varP l)|] ] (normalB [|_Left . toLens $(varE l)|]) [])
                  rclause <- testClause gkey rtyp (clause [ [p|Right $(varP r)|] ] (normalB [|_Right . toLens $(varE r)|]) [])
                  return $ catMaybes [lclause, rclause]
              , otherf = return [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (view etype key))))) :: Traversal' $(pure (runExpanded (view etype key))) $(pure (bestType gkey))|]) [] ]
              }

testClause :: (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> Type -> ClauseQ -> m (Maybe ClauseQ)
testClause gkey typ cl = do
  key <- view typeInfo >>= runReaderT (expandType typ >>= vertex Nothing)
  ok <- goalReachable gkey key
  return $ if ok then Just cl else Nothing

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
                    True -> pathToLensClauses gtyp key' gkey ptyp
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
