-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.ToLens (toLensDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad as List ( mapM )
import Control.Monad.State (evalStateT, StateT)
import Control.Monad.States (MonadStates(getPoly), modifyPoly)
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Bool (bool)
import Data.List as List (map)
import Data.Map as Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Set.Extra as Set (insert, mapM_, member, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasName(asName), HasType(asType), HasTypeQ(asTypeQ), makeFieldCon)
import Language.Haskell.TH.Path.Core (mat, S, A, ToLens(toLens), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType, viewLens)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.TypeGraph (goalReachableSimple, pathKeys, simplify, tgv, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGV, TGVSimple, TypeGraphVertex(bestType))

toLensDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
toLensDecs v =
    pathKeys v >>= Set.mapM_ (toLensDecs' v)

toLensDecs' :: forall m s. (TypeGraphM m, MonadWriter [Dec] m, s ~TGVSimple) => s -> s -> m ()
toLensDecs' key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  tlc <- execWriterT $ evalStateT (toLensClauses key gkey) mempty
  when (not (null tlc)) $
       (runQ $ sequence
            [ instanceD (pure []) [t|ToLens $(pure ptyp)|]
                [ tySynInstD ''S (tySynEqn [(pure ptyp)] (pure (bestType key)))
                , tySynInstD ''A (tySynEqn [(pure ptyp)] (pure (bestType gkey)))
                , funD 'toLens tlc -- [clause [wildP] (normalB (if key == gkey then [|id|] else [|undefined|])) []]
                ] ]) >>= tell


toLensClauses :: forall m s. (TypeGraphM m, MonadWriter [ClauseQ] m, s ~ TGVSimple) =>
                       s -- ^ the type whose clauses we are generating
                    -> s -- ^ the goal type key
                    -> StateT (Set Name) m ()
toLensClauses key gkey
    | key == gkey =
        tell [clause [wildP] (normalB [|id|]) []]
toLensClauses key gkey =
  -- Use this to raise errors when the path patterns aren't exhaustive.
  -- That is supposed to be impossible, so this is debugging code.
  -- toLensClauses key gkey ptyp = do
  --   x <- runQ (newName "x")
  --   r <- foldPath control key
  --   return $ r ++ [clause [varP x] (normalB [|error ("toLens' (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
  do ptyp <- pathType (pure (bestType gkey)) key
     let v = key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewType <- viewInstanceType (asType v)
     case asType v of
       _ | selfPath -> return ()
         | simplePath -> return () -- Simple paths only work if we are at the goal type, and that case is handled above.
       typ
         | isJust viewType -> do
             let ltyp = fromJust viewType
             lns <- runQ [|viewLens :: Lens' $(return typ) $(return ltyp)|]
             -- Ok, we have a type key, and a lens that goes between key and
             -- lkey, and we need to create a toLens' function for key's path type.
             -- The tricky bit is to extract the path value for lkey from the path
             -- value we have.
             let (AppT (ConT pname) _gtyp) = ptyp
             lkey <- tgvSimple ltyp
             doClause gkey ltyp (\p -> conP (mkName (nameBase pname ++ "_View")) [if lkey == gkey then wildP else p]) (pure lns)
       ConT tname ->
           getPoly >>= \s -> if Set.member tname s
                             then return ()
                             else modifyPoly (Set.insert tname) >>
                                  doName tname gkey
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
       _ -> tell [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (asType key))))) :: Traversal' $(asTypeQ key) $(pure (bestType gkey))|]) [] ]

-- | Given a function pfunc that modifies a pattern, add a
-- 'Language.Haskell.TH.Clause' (a function with a typically incomplete
-- pattern) to the toLens' method we are building to handle the new
-- pattern.
doClause :: forall m s. (TypeGraphM m, MonadWriter [ClauseQ] m, s ~ TGVSimple) =>
            s-> Type -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause gkey typ pfunc lns = do
  v <- runQ (newName "v")
  key <- tgvSimple typ
  ok <- goalReachableSimple gkey key
  let pat = bool wildP (varP v) (key /= gkey)
      lns' = bool lns [|$lns . toLens $(varE v)|] (key /= gkey)
  when ok $ tell [clause [pfunc pat] (normalB lns') []]

doName :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
          Name -> TGVSimple -> StateT (Set Name) m ()
doName tname gkey =
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
                  key' <- tgvSimple typ'
                  ok <- goalReachableSimple gkey key'
                  case ok of
                    False -> return ()
                    True -> toLensClauses key' gkey
            doDec (NewtypeD _ _ _ con _) = doCons [con]
            doDec (DataD _ _ _ cons _) = doCons cons
            doDec dec = error $ "doName - unexpected Dec: " ++ show dec

            doCons :: [Con] -> StateT (Set Name) m ()
            doCons cons = ((concatMap snd . concat) <$> List.mapM doCon cons) >>= tell

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
                    fkey <- (tgvSimple ft :: StateT (Set Name) m TGVSimple) >>= tgv (Just (tname, cname, Right fn)) :: StateT (Set Name) m TGV
                    skey <- simplify fkey
                    ok <- goalReachableSimple gkey skey  -- is the goal type reachable from here?
                    case ok of
                      False -> return []  -- Goal type isn't reachable, return empty clause list
                      True ->
                          do -- Build a type expression for the path type, inserting any
                             -- necessary declarations into the state.  Also, build an
                             -- expression for the lens that turns this field value into the
                             -- goal type.
                             clauses <- runQ (newName "_x") >>= \x -> return [clause [varP x] (normalB [|toLens $(varE x)|]) []]
                             let Just pcname = makeFieldCon fkey
                             ptype' <- pathType (pure (bestType gkey)) skey
                             -- This is the new constructor for this field
                             con <- runQ $ normalC (asName pcname) [strictType notStrict (return ptype')]
                             -- These are the field's clauses.  Each pattern gets wrapped with the field path constructor,
                             -- and each field lens gets composed with the lens produced for the field's type.
                             let goal = skey == gkey
                             clauses' <- List.mapM (Monad.lift .
                                                    mapClause (\ pat -> conP (asName pcname) [pat])
                                                              (\ lns ->
                                                                   let hop = [|\f x -> fmap (\y -> $(recUpdE [|x|] [fieldExp fn [|y|]])) (f $(appE (varE fn) [|x|]))|] in
                                                                   -- let hop = varE (fieldLensNameOld tname fn) in
                                                                   if goal then hop else [|$hop . $lns|])) clauses
                             return [(con, clauses')]


            -- Apply arity 1 functions to the clause pattern and expression
            mapClause :: (PatQ -> PatQ) -> (ExpQ -> ExpQ) -> ClauseQ -> m ClauseQ
            mapClause patf lnsf clauseq =
                runQ clauseq >>= \(Clause [pat] (NormalB lns) xs) -> return $ clause [patf (pure pat)] (normalB (lnsf (pure lns))) (List.map pure xs)
