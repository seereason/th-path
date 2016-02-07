-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

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
import Language.Haskell.TH.Path.Core (IsPathType(idPath), IsPath(..), ToLens(..), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (asConQ, asTypeQ, bestTypeName, makePathCon, makePathType, ModelType(ModelType))
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
            [ instanceD (pure []) [t|ToLens $(pure ptyp)|]
                [ tySynInstD ''S (tySynEqn [(pure ptyp)] (pure (bestType key)))
                , tySynInstD ''A (tySynEqn [(pure ptyp)] (pure (bestType gkey)))
                , funD 'toLens tlc -- [clause [wildP] (normalB (if key == gkey then [|id|] else [|undefined|])) []]
                ]
            , instanceD (pure []) [t|IsPath $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
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
                x <- runQ $ newName "x"
                a <- runQ $ newName "a"
                when vIsPath (tell [clause [varP x, varP a]
                                           (normalB [| let p = $(asConQ pcname) idPath :: Path $(asTypeQ key) $(pure vtyp)
                                                           [x'] = toListOf (toLens p) $(varE x) :: [$(pure vtyp)] in
                                                       List.map $(asConQ pcname) (pathsOf x' $(varE a) :: [Path $(pure vtyp) $(pure (view (etype . unE) gkey))]) |])
                                           []])
       ConT tname ->
           doName tname
       AppT (AppT mtyp ityp) vtyp
           | mtyp == ConT ''Order ->
               -- Return a path for each element of an order, assuming
               -- there is a path from the element type to the goal.
               do vIsPath <- testIsPath vtyp gkey
                  when vIsPath (doClause vtyp gkey varP
                                         [|Path_At . fst|]
                                         [|snd|]
                                         [|toPairs :: Order $(pure ityp) $(pure vtyp) -> [($(pure ityp), $(pure vtyp))]|])
       AppT ListT _etyp -> return ()
       AppT (AppT t3 _ktyp) vtyp
           | t3 == ConT ''Map ->
               do vIsPath <- testIsPath vtyp gkey
                  when vIsPath (doClause vtyp gkey varP
                                         [|Path_Look . fst|]
                                         [|snd|]
                                         [|Map.toList|])
       AppT (AppT (TupleT 2) ftyp) styp ->
           do fIsPath <- testIsPath ftyp gkey
              sIsPath <- testIsPath styp gkey
              when fIsPath (doClause ftyp gkey varP [|\_ -> Path_First|] [|fst|] [|(: [])|])
              when sIsPath (doClause styp gkey varP [|\_ -> Path_Second|] [|snd|] [|(: [])|])
       AppT t1 etyp
           | t1 == ConT ''Maybe ->
               do eIsPath <- testIsPath etyp gkey
                  when eIsPath (doClause etyp gkey varP [|\_ -> Path_Just|] [|id|] [|maybe [] (: [])|])
       AppT (AppT t3 ltyp) rtyp
           | t3 == ConT ''Either ->
               do -- Are there paths from the left type to a?  This is
                  -- the test we use in pathInstanceDecs, but using it
                  -- here is kind of a hack.
                  lIsPath <- testIsPath ltyp gkey
                  rIsPath <- testIsPath rtyp gkey
                  when lIsPath (doClause ltyp gkey varP [|\_ -> Path_Left|] [|id|] [|either (: []) (const [])|])
                  when rIsPath (doClause rtyp gkey varP [|\_ -> Path_Right|] [|id|] [|either (const []) (: [])|])
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
      doDec (NewtypeD _cx _tname _binds con _supers) = doCon con
      doDec (DataD _cx _tname _binds cons _supers) = mapM_ doCon cons
      doDec dec = error $ "Unexpected Dec: " ++ pprint dec
      doCon :: Con -> m ()
      doCon (ForallC _binds _cx con) = doCon con -- Should probably do something here
      doCon (InfixC lhs _cname rhs) = return () -- Implement
      doCon (NormalC cname binds) = do
        fIsPath <- mapM (\(_, ftype) -> testIsPath ftype gkey) binds
        -- x <- runQ $ newName "x"
        a <- runQ $ newName "a"
        let ns = map snd (zip binds ([1..] :: [Int]))
        ps <- runQ $ mapM (\n -> newName ("p" ++ show n)) ns
        let Just tname = bestName key
        let doField (_, False, _, _) = []
            doField (_, True, n, p) =
                [ [| map $(asConQ (makePathCon (makePathType (ModelType tname)) (show n)))
                         (pathsOf $(varE p) $(varE a)) |] ]
        tell [clause [conP cname (map varP ps), varP a]
                     (normalB [|concat $(listE (concatMap doField (zip4 binds fIsPath ns ps)))|])
                     []]
      doCon (RecC cname vbinds) = do
        fIsPath <- mapM (\(_, _, ftype) -> testIsPath ftype gkey) vbinds
        x <- runQ $ newName "x"
        a <- runQ $ newName "a"
        let ns = map snd (zip vbinds ([1..] :: [Int]))
        let Just tname = bestName key
        let doField (_, False, _) = []
            doField ((fname, _, _), _, _) =
                [ [| map $(asConQ (makePathCon (makePathType (ModelType tname)) (nameBase fname)))
                         (pathsOf ($(varE fname) $(varE x)) $(varE a)) |] ]
        tell [clause [asP x (recP cname []), varP a]
                     (normalB [|concat $(listE (concatMap doField (zip3 vbinds fIsPath ns)))|])
                     []]

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 l1 l2 l3 l4 = map (\((a, b), (c, d)) -> (a, b, c, d)) (zip (zip l1 l2) (zip l3 l4))

doClause :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [ClauseQ] m) =>
            Type
         -> TGVSimple
         -> (Name -> PatQ)
         -> ExpQ -- ^ Build a path
         -> ExpQ -- ^ Build a value
         -> ExpQ -- ^ The list of values we will turn into paths
         -> m ()
doClause vtyp gkey xpat toPath toVal asList = do
  x <- runQ $ newName "x"
  a <- runQ $ newName "a"
  tell [clause [xpat x, varP a]
               (normalB [| List.concatMap (\pv -> List.map ($toPath pv) (pathsOf ($toVal pv) $(varE a) :: [Path $(pure vtyp) $(asTypeQ gkey)])) ($asList $(varE x)) |])
               []]

testIsPath :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Type -> TGVSimple -> m Bool
testIsPath typ gkey = do
  key <- expandType typ >>= typeVertex
  (maybe False (Set.member gkey) . Map.lookup key) <$> allPathKeys
