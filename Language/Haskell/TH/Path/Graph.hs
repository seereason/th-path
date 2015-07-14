-- | The reader monad for the type graph info, and some monadic functions.

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
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Graph
    ( makePathLenses
    , makeTypeGraphEdges
    , FoldPathControl(..)
    , foldPath
    , SinkType
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#else
import Control.Applicative
#endif
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (execStateT, get, modify, StateT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Default (Default(def))
import Data.Foldable as Foldable (toList)
import Data.Foldable.Compat
import Data.Graph hiding (edges)
import Data.List as List (intercalate, map)
import Data.Map as Map (alter, fromList, keys, lookup, Map, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set as Set (difference, empty, fromList, map, member, minView, Set, singleton, toList)
import Debug.Trace (trace)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Core (fieldLensName, SelfPath)
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.View (View(viewLens), viewInstanceType)
import Language.Haskell.TH.TypeGraph.Edges (cut, cutM, dissolveM, GraphEdges, isolate, linkM, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (graphFromMap, TypeGraph(..), typeInfo)
import Language.Haskell.TH.TypeGraph.Info (startTypes, synonyms, TypeInfo, vertex)
import Language.Haskell.TH.TypeGraph.Shape (pprint', unlifted)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex, etype, field, typeNames)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, MonadReader TypeInfo m) =>
                      m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges =
  typeGraphEdges                   >>= \e1 -> tr "initial" mempty e1 >>=
  cutM isUnlifted                  >>= \e2 -> tr "unlifted" e1 e2 >>=
  dissolveM higherOrder            >>= \e3 -> tr "higherOrder" e2 e3 >>=
  -- viewEdges must not be applied until we have removed higher order types - otherwise
  -- we get a compiler error: "Expecting one more argument to..."
  linkM viewEdges                  >>= \e3a -> tr "view edges" e3 e3a >>=
  pruneTypeGraph                   >>= \e4 -> tr "prune" e3a e4 >>=
  dissolveM hasFreeVars            >>= \e5 -> tr "freeVars" e4 e5 >>=
  dissolveM isUnlifted             >>= \e6 -> tr "unlifted2" e5 e6 >>= -- looks redundant
  -- return . cutEdges isAnonymous >>= \e7 -> tr "anonymous" e6 e7 >>=
  isolateUnreachable               >>= \e8 -> t2 e6 e8
    where
      viewEdges :: TypeGraphVertex -> m (Maybe (Set TypeGraphVertex))
      viewEdges v = viewInstanceType (runExpanded (view etype v)) >>= maybe (return Nothing) (\t -> expandType t >>= vertex Nothing >>= return . Just . singleton)

      higherOrder :: TypeGraphVertex -> m Bool
      higherOrder v = (/= Right StarT) <$> runQ (inferKind (runExpanded (view etype v)))
      hasFreeVars :: TypeGraphVertex -> m Bool
      hasFreeVars v = (/= Set.empty) <$> runQ (freeTypeVars (runExpanded (view etype v)))
      -- Primitive (unlifted) types can not be used as parameters to a
      -- type class, which makes them unusable in this system.
      isUnlifted :: TypeGraphVertex -> m Bool
      isUnlifted v = {- t2 v <$> -} unlifted (runExpanded (view etype v))
      -- We don't make lenses for fields that have no name, except
      -- for special cases like Either, Maybe, Map, Order.
      isAnonymous :: TypeGraphVertex -> TypeGraphVertex -> Bool
      isAnonymous a b =
          case view etype a of
            E (AppT ListT _etyp) -> False
            E (AppT (AppT (TupleT 2) _) _) -> False
            E (AppT (AppT (AppT (TupleT 3) _) _) _)-> False
            E (AppT (AppT (AppT (AppT (TupleT 4) _) _) _) _)-> False
            E (AppT (AppT (ConT mtyp) ityp) etyp)
              -- Special case - the second field of the Map and Order types is not anonymous,
              -- we recognize these and generate code for them specifically
              | elem mtyp [''Either] -> False
              | elem mtyp [''Map, ''Order] && view etype b == E ityp -> True
              | elem mtyp [''Map, ''Order] && view etype b == E etyp -> False
            _ -> maybe True (\(_, _, fld) -> either (const True) (const False) fld) (view field b)

      isolateUnreachable :: GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
      isolateUnreachable es = do
        let (g, vf, kf) = graphFromMap es
        (kernel :: [Vertex]) <- view startTypes >>= mapM expandType >>= mapM (vertex Nothing) >>= return . mapMaybe kf
        let keep :: Set TypeGraphVertex
            keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
            victims = {- t3 $ -} Set.difference (Set.fromList (Map.keys es)) keep
        return $ isolate (flip member victims) es

      tr :: String -> GraphEdges hint TypeGraphVertex -> GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
      tr s old new =
          runQ (runIO (putStr ("\n\f\nLanguage.Haskell.TH.Path.Graph.makeTypeGraphEdges " ++ s ++
                               " - added " ++ indent "+" (pprint (diff new old)) ++
                               "\nremoved " ++  indent "-" (pprint (diff old new))))) >> return new

      indent s t = unlines . List.map (s ++) . lines $ t

      t2 :: Monad m => GraphEdges hint TypeGraphVertex -> GraphEdges hint TypeGraphVertex -> m (GraphEdges hint TypeGraphVertex)
      t2 old new = trace ("\n\f\nLanguage.Haskell.TH.Path.Graph.makeTypeGraphEdges " ++ pprint old ++ "\nunreachable " ++ pprint (diff new old)) (return new)

      t3 :: Set TypeGraphVertex -> Set TypeGraphVertex
      t3 s = trace (intercalate "\n  " ("unreachable:" : Set.toList (Set.map pprint' s))) s

      -- Exact difference between two maps
      diff m1 m2 = Map.fromList $ Set.toList $ Set.difference (Set.fromList (Map.toList m1))
                                                              (Set.fromList (Map.toList m2))

makePathLenses :: (DsMonad m, MonadReader TypeGraph m, MonadWriter [[Dec]] m) => TypeGraphVertex -> m ()
makePathLenses key = do
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ])
  case simplePath of
    False -> mapM make (Foldable.toList (typeNames key)) >>= tell
    _ -> return ()
    where
      make tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))

data FoldPathControl m r
    = FoldPathControl
      { simplef :: m r
      , pathyf :: m r
      , substf :: Exp -> Type -> m r
      , namedf :: Name -> m r
      , maybef :: Type -> m r
      , listf :: Type -> m r
      , orderf :: Type -> Type -> m r
      , mapf :: Type -> Type -> m r
      , pairf :: Type -> Type -> m r
      , eitherf :: Type -> Type -> m r
      , otherf :: m r
      }

foldPath :: (DsMonad m, MonadReader TypeGraph m) => FoldPathControl m r -> TypeGraphVertex -> m r
foldPath (FoldPathControl{..}) v = do
  selfPath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ])
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ])
  viewType <- evalContext (viewInstanceType (let (E typ) = view etype v in typ))
  syns <- Map.lookup (view etype v) <$> view (typeInfo . synonyms)
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | simplePath -> simplef
    typ
      | isJust viewType -> do
          let b = fromJust viewType
          expr <- runQ [|viewLens :: Lens' $(return typ) $(return b)|]
          substf expr b
    ConT tname -> namedf tname
    _ | maybe False (not . null) syns -> namedf (fst (fromJust (Set.minView (fromJust syns)))) -- yes, I'm sure
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf

-- | 'Path' instances can be customized by declaring types to be
-- instances of this class and the ones that follow.  If a type is an
-- instance of 'SinkType', no paths that lead to the internal stucture
-- of the value will be created - the value is considered atomic.
class SinkType a

-- | Like SinkType, but no paths out or into the type will be created.
class HideType a

-- | Remove any vertices that are labelled with primitive types, and then
-- apply the hints obtained from the
-- a new graph which incorporates the information from the hints.
pruneTypeGraph :: forall m label. (DsMonad m, Default label, Eq label, MonadReader TypeInfo m) =>
                  (GraphEdges label TypeGraphVertex) -> m (GraphEdges label TypeGraphVertex)
pruneTypeGraph edges =
  doSink edges >>=
  execStateT (get >>= mapM_ doHide . Map.keys) >>=
  execStateT (get >>= mapM_ doView . Map.keys)
    where
      doSink :: (GraphEdges label TypeGraphVertex) -> m (GraphEdges label TypeGraphVertex)
      doSink es =
          execStateT (mapM_ (\v -> do
                               let (E typ) = view etype v
                               sinkHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
                               when sinkHint (modify (Map.alter (alterFn (const Set.empty)) v))) (Map.keys es)) es

{-
      sinkP :: TypeGraphVertex -> m Bool
      sinkP v = do
        let (E typ) = view etype v
        (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
-}
      doHide :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doHide v = do
        let (E typ) = view etype v
        hideHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
        when hideHint (modify (cut (== v)))

{-
      hideP :: TypeGraphVertex -> m Bool
      hideP v = do
        let (E typ) = view etype v
        (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
-}
      doView :: TypeGraphVertex -> StateT (GraphEdges label TypeGraphVertex) m ()
      doView v = let (E a) = view etype v in evalContext (viewInstanceType a) >>= maybe (return ()) (doDivert v)

      -- Replace all of v's out edges with a single edge to typ'
      doDivert v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        modify (Map.alter (alterFn (const (singleton v'))) v)
#if 0
      -- Add an extra out edge from v to typ' (unused)
      doExtra v typ' = do
        v' <- expandType typ' >>= vertex Nothing
        modify (Map.alter (alterFn (Set.insert v')) v)
#endif

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: Default label => (Set TypeGraphVertex -> Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex) -> Maybe (label, Set TypeGraphVertex)
alterFn setf (Just (label, s)) = Just (label, setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (def, setf Set.empty)
