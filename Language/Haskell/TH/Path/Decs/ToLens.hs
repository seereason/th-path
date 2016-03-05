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
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Bool (bool)
import Data.List as List (map)
import Data.Map as Map (fromList, Map)
import Data.Maybe (fromJust, isJust)
import Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (fieldLensNameOld, HasName(asName), HasType(asType), HasTypeQ(asTypeQ), makeFieldCon)
import Language.Haskell.TH.Path.Core (mat, S, A, ToLens(toLens), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), substG)
import Language.Haskell.TH.Path.View (viewInstanceType, viewLens)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.TypeGraph (goalReachableSimple, pathKeys, simplify, tgv, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (field, TGV, TGVSimple, TypeGraphVertex(bestType))

toLensControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TGVSimple -> TGVSimple -> Control m () ()
toLensControl key gkey =
    Control
    { _doView = undefined
    , _doOrder = undefined
    , _doMap = undefined
    , _doPair = undefined
    , _doMaybe = undefined
    , _doEither = undefined
    , _doField =
        \x fkey -> do
          skey <- simplify fkey
          ok <- goalReachableSimple gkey skey
          case (ok, view (_2 . field) fkey) of
            (True, Just (tname, cname, Right fname)) ->
                do -- Build a type expression for the path type, inserting any
                   -- necessary declarations into the state.  Also, build an
                   -- expression for the lens that turns this field value into the
                   -- goal type.
                   clauses <- {-runQ (newName "_x") >>= \x ->-} return [clause [varP x] (normalB [|toLens $(varE x)|]) []]
                   let Just pcname = makeFieldCon fkey
                   ptype' <- pathType (pure (bestType gkey)) skey
                   -- This is the new constructor for this field
                   con <- runQ $ normalC (asName pcname) [strictType notStrict (return ptype')]
                   -- These are the field's clauses.  Each pattern gets wrapped with the field path constructor,
                   -- and each field lens gets composed with the lens produced for the field's type.
                   let goal = skey == gkey
                   clauses' <- List.mapM (mapClause (\ pat -> conP (asName pcname) [pat])
                                                    (\ lns ->
                                                         -- let hop = [|\f x -> fmap (\y -> $(recUpdE [|x|] [fieldExp fn [|y|]])) (f $(appE (varE fn) [|x|]))|] in
                                                         let hop = varE (fieldLensNameOld (asName key) fname) in
                                                         if goal then hop else [|$hop . $lns|])) clauses
                   tell clauses'
            (False, _) -> pure ()
    , _doConc = undefined
    , _doAlt = undefined
    }

toLensDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
toLensDecs v =
    pathKeys v >>= Set.mapM_ (toLensDecs' v)

toLensDecs' :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> TGVSimple -> m ()
toLensDecs' key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  tlc <- execWriterT $ toLensClauses key gkey
  when (not (null tlc)) $
       (runQ $ sequence
            [ instanceD (pure []) [t|ToLens $(pure ptyp)|]
                [ tySynInstD ''S (tySynEqn [(pure ptyp)] (pure (bestType key)))
                , tySynInstD ''A (tySynEqn [(pure ptyp)] (pure (bestType gkey)))
                , funD 'toLens tlc -- [clause [wildP] (normalB (if key == gkey then [|id|] else [|undefined|])) []]
                ] ]) >>= tell


toLensClauses :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
                 TGVSimple -- ^ the type whose clauses we are generating
              -> TGVSimple -- ^ the goal type key
              -> m ()
toLensClauses key gkey
    | key == gkey =
        tell [clause [wildP] (normalB [|id|]) []]
toLensClauses key gkey =
  -- Use this to raise errors when the path patterns aren't exhaustive.
  -- That is supposed to be impossible, so this is debugging code.
  -- toLensClauses key gkey ptyp = do
  --   r <- foldPath control key
  --   return $ r ++ [clause [varP x] (normalB [|error ("toLens' (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
  do let control = toLensControl key gkey :: Control m () ()
     x <- runQ (newName "_x")
     toLensClauses' control x key gkey

toLensClauses' :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
                  Control m () ()
               -> Name
               -> TGVSimple -- ^ the type whose clauses we are generating
               -> TGVSimple -- ^ the goal type key
               -> m ()
toLensClauses' control x key gkey =
  do ptyp <- pathType (pure (bestType gkey)) key
     let v = key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case asType v of
       _ | selfPath -> return ()
         | simplePath -> return () -- Simple paths only work if we are at the goal type, and that case is handled above.
         | isJust viewTypeMaybe -> do
             let viewtyp = fromJust viewTypeMaybe
             lns <- runQ [|viewLens :: Lens' $(return (asType v)) $(return viewtyp)|]
             -- Ok, we have a type key, and a lens that goes between key and
             -- lkey, and we need to create a toLens' function for key's path type.
             -- The tricky bit is to extract the path value for lkey from the path
             -- value we have.
             let (AppT (ConT pname) _gtyp) = ptyp
             lkey <- tgvSimple viewtyp
             doClause gkey viewtyp (\p -> conP (mkName (nameBase pname ++ "_View")) [if lkey == gkey then wildP else p]) (pure lns)
       typ -> doType typ []
    where
      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType (ConT tname) [_ityp, vtyp]
           | tname == ''Order =
               do k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
      doType (ConT tname) [_ktyp, vtyp]
           | tname == ''Map =
               do k <- runQ (newName "k")
                  doClause gkey vtyp (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
      doType (TupleT 2) [ftyp, styp] =
           do doClause gkey ftyp (\p -> [p|Path_First $p|]) [|_1|]
              doClause gkey styp (\p -> [p|Path_Second $p|]) [|_2|]
      doType (ConT tname) [etyp]
           | tname == ''Maybe =
               doClause gkey etyp (\p -> [p|Path_Just $p|]) [|_Just|]
      doType (ConT tname) [ltyp, rtyp]
           | tname == ''Either =
               do doClause gkey ltyp (\p -> [p|Path_Left $p|]) [|_Left|]
                  doClause gkey rtyp (\p -> [p|Path_Right $p|]) [|_Right|]

      doType (ConT tname) tps = doName tps tname
      doType ListT [_etyp] = return ()
      doType _ _ = tell [ clause [wildP] (normalB [|(error $ $(litE (stringL ("Need to find lens for field type: " ++ pprint (asType key))))) :: Traversal' $(asTypeQ key) $(pure (bestType gkey))|]) [] ]

      doName :: [Type] -> Name -> m ()
      doName tps tname =
          -- If encounter a named type and the stack is empty we
          -- need to build the clauses for its declaration.
          do nameInfo <- runQ $ reify tname
             case nameInfo of
               TyConI dec -> doDec tps dec
               _ -> error "doNameClauses"

      doDec :: [Type] -> Dec -> m ()
      doDec tps (TySynD _ _ typ') =
          do -- If we have a type synonym we can use the corresponding
             -- path type synonym instead of the path type of the
             -- alias type.
             key' <- tgvSimple typ'
             ok <- goalReachableSimple gkey key'
             case ok of
               False -> return ()
               True -> toLensClauses key' gkey
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _ _tname binds _cons _)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _ tname binds cons _) =
          let bindings = Map.fromList (zip (map asName binds) tps)
              subst = substG bindings in
          doCons subst tname cons
      doDec _tps dec = error $ "doName - unexpected Dec: " ++ show dec

      doCons :: (Type -> Type) -> Name -> [Con] -> m ()
      doCons subst tname cons = Prelude.mapM_ (doCon subst tname) cons

      -- For each constructor of the original type, we create a list of pairs, a
      -- path type constructor and the clause which recognizes it.
      doCon :: (Type -> Type) -> Name -> Con -> m ()
      doCon subst tname (ForallC _ _ con) = doCon subst tname con
      doCon subst tname (NormalC _ _) = pure ()
      doCon subst tname (InfixC _ _ _) = pure ()
      doCon subst tname (RecC cname ts) =
          Prelude.mapM_ (\(fname, _, ftype') -> do
                           let ftype = subst ftype'
                           fkey <- tgvSimple ftype >>= tgv (Just (tname, cname, Right fname))
                           _doField control x fkey) ts

-- Apply arity 1 functions to the clause pattern and expression
mapClause :: TypeGraphM m => (PatQ -> PatQ) -> (ExpQ -> ExpQ) -> ClauseQ -> m ClauseQ
mapClause patf lnsf clauseq =
    runQ clauseq >>= \(Clause [pat] (NormalB lns) xs) -> return $ clause [patf (pure pat)] (normalB (lnsf (pure lns))) (List.map pure xs)

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
