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
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Language.Haskell.TH.Path.Graph
    ( typeGraphEdges'
    , FoldPathControl(..)
    , foldPath
    -- * Hint classes
    , SinkType
    , SelfPath
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
import Data.Foldable.Compat
import Data.Graph as Graph (reachable)
import Data.List as List (filter, map)
import Data.Map as Map (alter, keys, Map)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set as Set (empty, fromList, map, member, Set, singleton)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.View (View(viewLens), viewInstanceType)
import Language.Haskell.TH.TypeGraph.Edges (cut, cutM, cutEdges, cutEdgesM, dissolveM, GraphEdges, isolate, linkM, simpleEdges, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (graphFromMap, TypeGraph)
import Language.Haskell.TH.TypeGraph.Info (startTypes, TypeInfo, fieldVertex, typeVertex, typeVertex')
import Language.Haskell.TH.TypeGraph.Prelude (constructorName, unlifted)
import Language.Haskell.TH.TypeGraph.Shape (constructorFieldTypes, FieldType(..))
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGV(TGV, _vsimple), vsimple, TGVSimple(TGVSimple, _etype))
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

#if 0
import Data.Map as Map (fromList, lookup, toList)
import Data.Set as Set (difference, toList)
import Debug.Trace (trace)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.Graph (typeInfo)
import Language.Haskell.TH.TypeGraph.Info (synonyms)
#endif

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
typeGraphEdges' :: forall m. (DsMonad m, MonadReader TypeInfo m) => m (GraphEdges TGV)
typeGraphEdges' = do
  e1 <- typeGraphEdges                      -- ; _tr "initial" mempty e1
  e1a <- return (cutEdges isMapKey e1)
  e2 <- cutM isUnlifted e1a                 -- ; _tr "unlifted" e1 e2
  e3 <- dissolveM higherOrder e2            -- ; _tr "higherOrder" e2 e3
  -- viewEdges must not be applied until we have removed higher order types - otherwise
  -- we get a compiler error: "Expecting one more argument to..."
  e3a <- linkM viewEdges e3                 -- ; _tr "view edges" e3 e3a
  e4 <- pruneTypeGraph e3a                  -- ; _tr "prune" e3a e4
  e5 <- dissolveM hasFreeVars e4            -- ; _tr "freeVars" e4 e5
  e6 <- dissolveM isUnlifted e5             -- ; _tr "unlifted2" e5 e6   -- looks redundant
  e7 <- cutEdgesM anonymous e6              -- ; _tr "anonymous" e6 e7
  e8 <- isolateUnreachable e7               -- ; _tr "unreachable" e7 e8
  runQ (runIO (putStr ("typeGraphEdges' final - " ++ pprint e8)))
  return e8
    where
      viewEdges :: TGV -> m (Maybe (Set TGV))
      viewEdges v = viewInstanceType (runExpanded (view (vsimple . etype) v)) >>= maybe (return Nothing) (\t -> expandType t >>= typeVertex' >>= return . Just . singleton)

      higherOrder :: TGV -> m Bool
      higherOrder v = (/= Right StarT) <$> runQ (inferKind (runExpanded (view (vsimple . etype) v)))
      hasFreeVars :: TGV -> m Bool
      hasFreeVars v = (/= Set.empty) <$> runQ (freeTypeVars (runExpanded (view (vsimple . etype) v)))
      -- Primitive (unlifted) types can not be used as parameters to a
      -- type class, which makes them unusable in this system.
      isUnlifted :: TGV -> m Bool
      isUnlifted v = unlifted (runExpanded (view (vsimple . etype) v))

      isMapKey :: TGV -> TGV -> Bool
      isMapKey (TGV {_vsimple = TGVSimple {_etype = E (AppT a@(AppT (ConT name) _) _b)}}) a' |
          (name == ''Order || name == ''Map) && a == runExpanded (view (vsimple . etype) a') = True
      isMapKey _ _ = False

      -- Ignore unnamed fields of records, and the key types of Map or Order types.
      anonymous :: TGV -> TGV -> m Bool
      anonymous (TGV {_vsimple = TGVSimple {_etype = E (ConT aname)}}) b =
          runQ (reify aname) >>= doInfo >>= \r -> case r of
                                                    [True] -> return True
                                                    [False] -> return False
                                                    [] -> return False
                                                    _ -> error $ "Unexpected result in makeTypeGraphEdges/notAnonymous: " ++ show r
          where
            doInfo :: Info -> m [Bool]
            doInfo (TyConI dec) = doDec dec
            doInfo _ = return []
            doDec :: Dec -> m [Bool]
            doDec (DataD _ _ _ cs _) = concat <$> mapM doCon cs
            doDec _ = return []
            doCon :: Con -> m [Bool]
            doCon (ForallC _ _ con) = doCon con
            doCon con = concat <$> mapM doField (List.map (con,) (constructorFieldTypes con))
            -- Find the vertex for b and test it
            doField :: (Con, FieldType) -> m [Bool]
            doField (con, (Named (fname, _, ftype))) = do
              etyp <- expandType ftype
              b' <- fieldVertex (aname, constructorName con, Right fname) etyp
              return $ if b == b' then [False] else []
            doField (con, (Positional i (_, ftype))) = do
              etyp <- expandType ftype
              b' <- fieldVertex (aname, constructorName con, Left i) etyp
              return $ if b == b' then [True] else []
      anonymous _ _ = return False

      isolateUnreachable :: GraphEdges TGV -> m (GraphEdges TGV)
      isolateUnreachable es = do
        st <- view startTypes >>= mapM expandType >>= mapM typeVertex
        let (g, vf, kf) = graphFromMap (simpleEdges es)
        let keep :: Set TGVSimple
            keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) (mapMaybe kf st)
            -- Discard any nodes whose simplified version is not in keep
            victims = List.filter (\ v -> not (Set.member (view vsimple v) keep)) (Map.keys es)
            -- victims = {- t3 $ -} Set.difference (Set.fromList (Map.keys es)) keep
        -- trace ("isolateUnreachable - " ++ pprint es ++ "\n" ++
        --        intercalate "\n  " ("startTypes:" : List.map pprint' st) ++ "\n" ++
        --        intercalate "\n  " ("Unreachable:" : List.map pprint' victims)) (return ())
        return $ isolate (flip member (Set.fromList victims)) es
#if 0
      tr :: String -> GraphEdges TGV -> GraphEdges TGV -> m (GraphEdges TGV)
      tr s old new =
          runQ (runIO (putStr ("\n\f\nLanguage.Haskell.TH.Path.Graph.makeTypeGraphEdges " ++ s ++
                               " - added " ++ indent "+" (pprint (diff new old)) ++
                               "\nremoved " ++  indent "-" (pprint (diff old new))))) >> return new

      indent s t = unlines . List.map (s ++) . lines $ t

      -- Exact difference between two maps
      diff m1 m2 = Map.fromList $ Set.toList $ Set.difference (Set.fromList (Map.toList m1))
                                                              (Set.fromList (Map.toList m2))
#endif

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

foldPath :: (DsMonad m, MonadReader TypeGraph m) => FoldPathControl m r -> TGVSimple -> m r
foldPath (FoldPathControl{..}) v = do
  selfPath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ])
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ])
  viewType <- evalContext (viewInstanceType (let (E typ) = view etype v in typ))
#if 0
  syns <- Map.lookup (view etype v) <$> view (typeInfo . synonyms)
#endif
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | simplePath -> simplef
    typ
      | isJust viewType -> do
          let b = fromJust viewType
          expr <- runQ [|viewLens :: Lens' $(return typ) $(return b)|]
          substf expr b
    ConT tname -> namedf tname
#if 0
    -- This seemed like a good idea at one point, but now it either
    -- makes no difference (in PathType.hs and Types.hs) or makes
    -- things worse (in Instances.hs.)
    _ | maybe False (not . null) syns -> namedf (fst (fromJust (Set.minView (fromJust syns))))
#endif
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

-- | Types for which
-- a 'SelfPath' instance is declared will be used as their own Path
-- Type.  For example, a UUID or some enumerated type contained in a
-- record could be used directly to reference the object that contains
-- it.
class SelfPath a

-- | Remove any vertices that are labelled with primitive types, and then
-- apply the hints obtained from the
-- a new graph which incorporates the information from the hints.
pruneTypeGraph :: forall m. (DsMonad m, MonadReader TypeInfo m) =>
                  (GraphEdges TGV) -> m (GraphEdges TGV)
pruneTypeGraph edges =
  doSink edges >>=
  execStateT (get >>= mapM_ doHide . Map.keys) >>=
  execStateT (get >>= mapM_ doView . Map.keys)
    where
      doSink :: (GraphEdges TGV) -> m (GraphEdges TGV)
      doSink es =
          execStateT (mapM_ (\v -> do
                               let (E typ) = view (vsimple . etype) v
                               sinkHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
                               when sinkHint (modify (Map.alter (alterFn (const Set.empty)) v))) (Map.keys es)) es

{-
      sinkP :: TGV -> m Bool
      sinkP v = do
        let (E typ) = view (vsimple . etype) v
        (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [typ])
-}
      doHide :: TGV -> StateT (GraphEdges TGV) m ()
      doHide v = do
        let (E typ) = view (vsimple . etype) v
        hideHint <- (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
        when hideHint (modify (cut (== v)))

{-
      hideP :: TGV -> m Bool
      hideP v = do
        let (E typ) = view (vsimple . etype) v
        (not . null) <$> evalContext (reifyInstancesWithContext ''HideType [typ])
-}
      doView :: TGV -> StateT (GraphEdges TGV) m ()
      doView v = let (E a) = view (vsimple . etype) v in evalContext (viewInstanceType a) >>= maybe (return ()) (doDivert v)

      -- Replace all of v's out edges with a single edge to typ'
      doDivert v typ' = do
        v' <- expandType typ' >>= typeVertex'
        modify (Map.alter (alterFn (const (singleton v'))) v)
#if 0
      -- Add an extra out edge from v to typ' (unused)
      doExtra v typ' = do
        v' <- expandType typ' >>= typeVertex'
        modify (Map.alter (alterFn (Set.insert v')) v)
#endif

-- | build the function argument of Map.alter for the GraphEdges map.
alterFn :: (Set TGV -> Set TGV) -> Maybe (Set TGV) -> Maybe (Set TGV)
alterFn setf (Just s) = Just (setf s)
alterFn setf Nothing | null (setf Set.empty) = Nothing
alterFn setf Nothing = Just (setf Set.empty)
