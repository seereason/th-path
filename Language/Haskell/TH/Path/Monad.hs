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
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Monad
    ( R(..), startTypes, typeInfo, edges

    , makeTypeGraph

    , FoldPathControl(..)
    , foldPath
    , allLensKeys
    , allPathKeys
    , makePathLenses
    , reachableFrom
    , goalReachableFull
    , goalReachableSimple
    ) where

import Control.Applicative ((<$>))
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (filterM)
import Control.Monad.Reader (ask, local, MonadReader, ReaderT, runReaderT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable.Compat
import Data.Graph (Graph, reachable, transposeG, Vertex)
import Data.List as List (intercalate, map)
import Data.Map as Map (keys, Map)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set as Set (empty, fromList, map, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (fieldLensName, SelfPath)
import Language.Haskell.TH.Path.Graph (makeTypeGraphEdges)
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.Prune (SinkType)
import Language.Haskell.TH.Path.Stack (HasStack(withStack), push, StackElement(StackElement))
import Language.Haskell.TH.Path.View (View(viewLens), viewInstanceType)
import Language.Haskell.TH.TypeGraph (pprint', E(E), expandType, runExpanded,
                                      GraphEdges, graphFromMap, TypeGraphInfo, typeGraphInfo,
                                      simpleEdges, simpleVertex, vertex,
                                      TypeGraphVertex, etype, typeNames)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

data R
    = R
      { _startTypes :: [Type]
      , _typeInfo :: TypeGraphInfo
      , _edges :: GraphEdges () TypeGraphVertex
      , _graph :: (Graph, Vertex -> ((), TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex)
      , _gsimple :: (Graph, Vertex -> ((), TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex)
      , _stack :: [StackElement]
      }

$(makeLenses ''R)

instance Monad m => HasStack (ReaderT R m) where
    withStack f = ask >>= f . view stack
    push fld con dec action = local (stack %~ (\s -> StackElement fld con dec : s)) action

-- | A lens key is a pair of vertexes corresponding to a Path instance.
allLensKeys :: (DsMonad m, MonadReader R m) => m (Set (TypeGraphVertex, TypeGraphVertex))
allLensKeys = do
  pathKeys <- allPathKeys
  Set.fromList <$> filterM (uncurry goalReachableSimple) [ (g, k) | g <- toList (Set.map simpleVertex pathKeys), k <- toList (Set.map simpleVertex pathKeys) ]

allPathKeys :: forall m. (DsMonad m, MonadReader R m) => m (Set TypeGraphVertex)
allPathKeys = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view graph
  kernel <- view startTypes >>= \st -> view typeInfo >>= runReaderT (mapM expandType st >>= mapM (vertex Nothing))
  let kernel' = mapMaybe kf kernel
  let keep = Set.fromList $ concatMap (reachable g) kernel'
      keep' = Set.map (\(_, key, _) -> key) . Set.map vf $ keep
  return keep'

makePathLenses :: (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> m ()
makePathLenses key = do
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ])
  case simplePath of
    False -> mapM make (toList (typeNames key)) >>= tell
    _ -> return ()
    where
      make tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))

reachableFrom :: forall m. (DsMonad m, MonadReader R m) => TypeGraphVertex -> m (Set TypeGraphVertex)
reachableFrom v = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view graph
  case kf v of
    Nothing -> return Set.empty
    Just v' -> return $ Set.map (\(_, key, _) -> key) . Set.map vf $ Set.fromList $ reachable (transposeG g) v'

isReachable :: (Functor m, DsMonad m, MonadReader R m) =>
               TypeGraphVertex -> TypeGraphVertex -> (Graph, Vertex -> ((), TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex) -> m Bool
isReachable gkey key0 (g, _vf, kf) = do
  es <- view edges
  case kf key0 of
    Nothing -> error ("isReachable - unknown key: " ++ pprint' key0)
    Just key -> do
      let gvert = fromMaybe (error $ "Unknown goal type: " ++ pprint' gkey ++ "\n" ++ intercalate "\n  " ("known:" : List.map pprint' (Map.keys es))) (kf gkey)
      -- Can we reach any node whose type matches (ConT gname)?  Fields don't matter.
      return $ elem gvert (reachable g key)

-- | Can we reach the goal type from the start type in this key?
goalReachableFull :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
goalReachableFull gkey key0 = view graph >>= isReachable gkey key0

goalReachableSimple :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
goalReachableSimple gkey key0 = view gsimple >>= isReachable (simpleVertex gkey) (simpleVertex key0)

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

foldPath :: (DsMonad m, MonadReader R m) => FoldPathControl m r -> TypeGraphVertex -> m r
foldPath (FoldPathControl{..}) v = do
  selfPath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ])
  simplePath <- (not . null) <$> evalContext (reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ])
  viewType <- evalContext (viewInstanceType (let (E typ) = view etype v in typ))
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | simplePath -> simplef
    typ
      | isJust viewType -> do
          let b = fromJust viewType
          exp <- runQ [|viewLens :: Lens' $(return typ) $(return b)|]
          substf exp b
    ConT tname -> namedf tname
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf

makeTypeGraph :: DsMonad m => Q [Type] -> m R
makeTypeGraph st = do
  st' <- runQ st
  ti <- typeGraphInfo st'
  es <- runReaderT (makeTypeGraphEdges st') ti
  return $ R { _startTypes = st'
             , _typeInfo = ti
             , _edges = es
             , _graph = graphFromMap es
             , _gsimple = graphFromMap (simpleEdges es)
             , _stack = []
             }
