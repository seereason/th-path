-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

{-# OPTIONS -Wall -fno-warn-unused-imports #-}
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
module Language.Haskell.TH.Path.Decs.ToLens where

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
import Data.List as List (concatMap, intercalate, isPrefixOf, map)
import Data.Map as Map (Map, toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Set as Set (delete, minView)
import Data.Set.Extra as Set (insert, map, member, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Data.Tree (Tree(Node))
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IsPathType(idPath), IsPathNode(PVType, pvTree), IsPath(..), Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (bestPathTypeName, fieldLensNameOld, pathConNameOfField)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
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
       ConT tname ->
           getPoly >>= \s -> if Set.member tname s
                             then return ()
                             else modifyPoly (Set.insert tname) >>
                                  doName tname gkey ptyp
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               do k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
       AppT ListT _etyp -> return ()
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
       AppT (AppT (TupleT 2) ftyp) styp ->
           do doClause gkey ftyp (\p -> [p|Path_First $p|]) [|_1|]
              doClause gkey styp (\p -> [p|Path_Second $p|]) [|_2|]
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               doClause gkey etyp (\p -> [p|Path_Just $p|]) [|_Just|]
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do doClause gkey ltyp (\p -> [p|Path_Left $p|]) [|_Left|]
                  doClause gkey rtyp (\p -> [p|Path_Right $p|]) [|_Right|]
       _ -> tell [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (view etype key))))) :: Traversal' $(pure (view (etype . unE) key)) $(pure (bestType gkey))|]) [] ]

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

doName :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m) =>
                   Name -> TGVSimple -> Type -> StateT (Set Name) m ()
doName tname gkey ptyp =
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