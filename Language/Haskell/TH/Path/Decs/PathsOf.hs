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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.PathsOf (pathDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.List as List (concatMap, map)
import Data.Map as Map (lookup, Map, toList)
import Data.Maybe (isJust)
import Data.Set.Extra as Set (insert, member, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (IsPathType(idPath), IsPath(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (asConQ, bestTypeName, clauses, makePathCon, makePathType, ModelType(ModelType))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Decs.ToLens (toLensClauses)
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Expand (unE, expandType)
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, TGVSimple, TypeGraphVertex(bestType))

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
            TGVSimple -> TGVSimple -> m ()
pathDecs key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  tlc <- execWriterT $ evalStateT (toLensClauses key gkey) mempty
  poc <- execWriterT $ evalStateT (pathsOfClauses key gkey) mempty
  -- clauses' <- runQ $ sequence clauses
  -- exp <- thePathExp gkey key ptyp clauses'
  when (not (null tlc)) $
       (runQ $ sequence
             [instanceD (pure []) [t|IsPath $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'toLens tlc
                , funD 'pathsOf poc
                ]]) >>= tell
    where
      -- Send a single dec to our funky writer monad
      -- tell :: (DsMonad m, MonadWriter [Dec] m) => [DecQ] -> m ()
      -- tell dec = runQ (sequence dec) >>= tell

-- | Clauses of the pathsOf function.  Each clause matches a value of
-- type s and returns a path from s to some subtype a.  These clauses
-- are of the form
--
--    f x = exp :: [PathType s (Proxy a)]
pathsOfClauses :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m) =>
                       TGVSimple -- ^ the type whose clauses we are generating
                    -> TGVSimple -- ^ the goal type key
                    -> StateT (Set TGVSimple) m ()
pathsOfClauses key gkey
    | view etype key == view etype gkey = tell [clause [wildP, wildP] (normalB [|[idPath] |]) []]
pathsOfClauses key gkey =
  do -- the corresponding path type - first type parameter of ToLens
     -- ptyp <- pathType (pure (bestType gkey)) key
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [view (etype . unE) key]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [view (etype . unE) key]
     viewType <- viewInstanceType (view etype key)
     case view (etype . unE) key of
       _ | selfPath -> return ()
         | simplePath -> return ()
         | isJust viewType ->
             do let Just vtyp = viewType
                vIsPath <- testIsPath vtyp gkey
                let Just tname = bestTypeName key
                let pcname = makePathCon (makePathType tname) "View"
                -- Get the value as transformed by the view lens
                when vIsPath (runQ [d| _f x a =
                                         let p = $(asConQ pcname) idPath :: Path $(pure (view (etype . unE) key)) $(pure vtyp)
                                             [x'] = toListOf (toLens p) x :: [$(pure vtyp)] in
                                         List.map $(asConQ pcname) (pathsOf x' a {- :: [Path $(pure vtyp) $(pure (view (etype . unE) gkey))] -}) |] >>= tell . clauses)
       ConT tname ->
           doName tname
       AppT (AppT mtyp _ityp) vtyp
           | mtyp == ConT ''Order ->
               -- Return a path for each element of an order, assuming
               -- there is a path from the element type to the goal.
               do vIsPath <- testIsPath vtyp gkey
                  when vIsPath (runQ [d| _f o a = List.concatMap (\(k, v) -> List.map (Path_At k) (pathsOf (v :: $(pure vtyp)) a {-:: [Path $(pure vtyp) (pure (view (etype . unE) gkey))]-})) (toPairs o) |] >>= tell . clauses)
       AppT ListT _etyp -> return ()
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do vIsPath <- testIsPath vtyp gkey
                  when vIsPath (runQ [d| _f mp a = List.concatMap (\(k, v) -> List.map (Path_Look k) (pathsOf (v :: $(pure vtyp)) a {-:: [Path $(pure vtyp) (pure (view (etype . unE) gkey))]-})) (Map.toList mp) |] >>= tell . clauses)
       AppT (AppT (TupleT 2) ftyp) styp ->
           do fIsPath <- testIsPath ftyp gkey
              sIsPath <- testIsPath styp gkey
              -- trace ("testIsPath " ++ pprint styp ++ " " ++ pprint gkey ++ " -> " ++ show sIsPath) (return ())
              when fIsPath (runQ [d| _f x a = List.map Path_First (pathsOf (fst x :: $(pure ftyp)) a {- :: [Path $(pure ftyp) $(pure (view (etype . unE) gkey))] -}) |] >>= tell . clauses)
              when sIsPath (runQ [d| _f x a = List.map Path_Second (pathsOf (snd x :: $(pure styp)) a {- :: [Path $(pure styp) $(pure (view (etype . unE) gkey))] -}) |] >>= tell . clauses)
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do eIsPath <- testIsPath etyp gkey
                  when eIsPath (runQ [d| _f x a = maybe [] (\y -> List.map Path_Just (pathsOf (y :: $(pure etyp)) a {- :: [Path $(pure etyp) $(pure (view (etype . unE) gkey))] -})) x |] >>= tell . clauses)
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do -- Are there paths from the left type to a?  This is
                  -- the test we use in pathInstanceDecs, but using it
                  -- here is kind of a hack.
                  lIsPath <- testIsPath ltyp gkey
                  rIsPath <- testIsPath rtyp gkey
                  when lIsPath (runQ [d| _f x a = either (\y -> List.map Path_Left (pathsOf (y :: $(pure ltyp)) a {- :: [Path $(pure ltyp) $(pure (view (etype . unE) gkey))] -})) (\_ -> []) x |] >>= tell . clauses)
                  when rIsPath (runQ [d| _f x a = either (\_ -> []) (\y -> List.map Path_Right (pathsOf (y :: $(pure rtyp)) a {- :: [Path $(pure ltyp) $(pure (view (etype . unE) gkey))] -})) x |] >>= tell . clauses)
       _ -> tell [clause [wildP, wildP] (normalB [|error $ "pathsOfClauses - unexpected type: " ++ pprint key|]) []]
    where
      doName :: Name -> StateT (Set TGVSimple) m ()
      doName tname = do
        ns <- get
        case Set.member key ns of
          True -> return ()
          False -> modify (Set.insert key) >> qReify tname >>= lift . doInfo
      doInfo :: Info -> m ()
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec :: Dec -> m ()
      doDec (NewtypeD cx tname binds con supers) = doDec (DataD cx tname binds [con] supers)
      doDec (DataD _cx _tname _binds cons _supers) = mapM_ doCon cons
      doDec dec = error $ "Unexpected Dec: " ++ pprint dec
      doCon :: Con -> m ()
      doCon (ForallC _binds _cx con) = doCon con -- Should probably do something here
      doCon (InfixC lhs _cname rhs) = do
        [(btype, bname, bpath),
         (ctype, cname, cpath)] <- mapM (\((_, ftype), n) -> do
                                             fname <- runQ (newName ("_a" ++ show n))
                                             fpath <- testIsPath ftype gkey
                                             return (ftype, fname, fpath)) (zip [lhs, rhs] ([1..] :: [Int])) :: m [(Type, Name, Bool)]
        runQ [d| _f $(infixP (varP bname) cname (varP cname)) a =
                   concat [$(case bpath of
                               True -> [|List.map (error "doCon InfixC") (pathsOf ($(varE bname) :: $(pure btype)) a)|]
                               False -> [| [] |]),
                           $(case cpath of
                               True -> [|List.map (error "doCon InfixC") (pathsOf ($(varE cname) :: $(pure ctype)) a)|]
                               False -> [| [] |])] |] >>= tell . clauses
      doCon (NormalC cname binds) = do
        tns <- mapM (\((_, ftype), n) -> do
                       fname <- runQ (newName ("_a" ++ show n))
                       fpath <- testIsPath ftype gkey
                       return (ftype, fname, fpath)) (zip binds ([1..] :: [Int])) :: m [(Type, Name, Bool)]
        runQ [d| _f $(conP cname (List.map (varP . view _2) tns)) a =
                   concat $(listE (List.map (\(ftype, fname, fpath) ->
                                                 case fpath of
                                                   True -> [|List.map (error "doCon NormalC") (pathsOf ($(varE fname) :: $(pure ftype)) a)|]
                                                   False -> [| [] |]) tns)) |] >>= tell . clauses
      doCon (RecC cname vbinds) = do
        tns <- mapM (\((fname, _, ftype), n) -> do
                       fparm <- runQ (newName ("_a" ++ show n))
                       fpath <- testIsPath ftype gkey
                       return (ftype, fparm, fpath, fname)) (zip vbinds ([1..] :: [Int])) :: m [(Type, Name, Bool, Name)]
        runQ [d| _f $(conP cname (List.map (varP . view _2) tns)) a =
                   concat $(listE (List.map (\(ftype, fparm, fpath, fname) ->
                                                 let Just tname = bestName key in
                                                 let pcon = makePathCon (makePathType (ModelType tname)) (nameBase fname) in
                                                 case fpath of
                                                   True -> [|List.map $(asConQ pcon) (pathsOf ($(varE fparm) :: $(pure ftype)) a)|]
                                                   False -> [| [] |]) tns)) |] >>= tell . clauses

testIsPath :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Type -> TGVSimple -> m Bool
testIsPath typ gkey = do
  key <- expandType typ >>= typeVertex
  (maybe False (Set.member gkey) . Map.lookup key) <$> allPathKeys
