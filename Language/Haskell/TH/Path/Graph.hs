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
    ( makeTypeGraphEdges
    , makeTypeGraph
    , VertexStatus(..)
    , typeGraphEdges'
    , adjacent
    , typeVertex
    , fieldVertex
    , makePathLenses
    , FoldPathControl(..)
    , foldPath
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#else
import Control.Applicative
#endif
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (ask, MonadReader, runReaderT)
import Control.Monad.State (execStateT, modify, StateT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Default (Default(def))
import Data.Foldable as Foldable (concatMap, elem, mapM_, null, toList)
import Data.Graph hiding (edges)
import Data.List as List (map)
import Data.Map as Map (alter, keys, Map, map, mapWithKey, update)
import Data.Maybe (mapMaybe)
import Data.Maybe (fromJust, isJust)
import Data.Set as Set (difference, empty, filter, fromList, insert, map, member, Set, singleton, toList, unions)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Context (HasSet(getSet, modifySet))
import Language.Haskell.TH.Context.Reify (evalContext, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Path.Core (fieldLensName, SelfPath)
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.Prune (pruneTypeGraph, SinkType)
import Language.Haskell.TH.Path.View (View(viewLens), viewInstanceType)
import Language.Haskell.TH.Syntax (Quasi(..))
import Language.Haskell.TH.TypeGraph.Edges (dissolveM, GraphEdges, isolate, simpleEdges, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Free (freeTypeVars)
import Language.Haskell.TH.TypeGraph.Graph (graphFromMap, TypeGraph(..), typeInfo)
import Language.Haskell.TH.TypeGraph.Info (startTypes, TypeInfo, vertex)
import Language.Haskell.TH.TypeGraph.Shape (unlifted)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex, etype, field, typeNames)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

-- FIXME: pass in ti, pass in makeTypeGraphEdges, remove Q, move to TypeGraph.Graph
makeTypeGraph :: DsMonad m => TypeInfo -> m TypeGraph
makeTypeGraph ti = do
  -- ti <- typeInfo st
  es <- runReaderT makeTypeGraphEdges ti
  return $ TypeGraph
             { _typeInfo = ti
             , _edges = es
             , _graph = graphFromMap es
             , _gsimple = graphFromMap (simpleEdges es)
             , _stack = []
             }

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, MonadReader TypeInfo m) =>
                      m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges = do
  -- im <- view infoMap
  -- Dissolve the vertices for types whose arity is not zero.  Each of
  -- their in-edges become connected to each of their out-edges.
  edges' <- typeGraphEdges >>= pruneTypeGraph >>= dissolveM victim >>= return . removePathsToOrderKeys . removeUnnamedFieldEdges
  let (g, vf, kf) = graphFromMap edges'
  -- Isolate all nodes that are not reachable from the start types.
  kernel <- view startTypes >>= mapM expandType >>= mapM (vertex Nothing) >>= return . mapMaybe kf
  let -- Remove all edges involving unreachable vertices, but not the
      -- vertices themselves - they might still be queried.
      keep :: Set TypeGraphVertex
      keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
      victims = difference (Set.fromList (Map.keys edges')) keep
      edges'' :: GraphEdges hint TypeGraphVertex
      edges'' = isolate victims edges'
  -- trace (pprint edges'') (return ())
  return edges''
    where
      victim :: TypeGraphVertex -> m Bool
      victim v = do
        let (E etyp) = view etype v
        k <- runQ $ inferKind etyp
        fv <- runQ $ freeTypeVars etyp
        prim' <- unlifted etyp
        return $ k /= Right StarT || fv /= Set.empty || prim'
      removeUnnamedFieldEdges :: (GraphEdges hint TypeGraphVertex) -> (GraphEdges hint TypeGraphVertex)
      removeUnnamedFieldEdges =
          -- If the _field field of the goal key is a Left it is a
          -- positional field rather than named - we don't make lenses
          -- for such fields so we don't want their edges in our graph.
          Map.map (\(hint, gkeys) -> (hint, Set.filter (\gkey -> maybe True (\(_, _, fld) -> either (const False) (const True) fld) (view field gkey)) gkeys))
      removePathsToOrderKeys :: (GraphEdges hint TypeGraphVertex) -> (GraphEdges hint TypeGraphVertex)
      removePathsToOrderKeys =
          -- The graph will initially include edges from (Map a b) to
          -- a, and (Order a b) to a, but we only create lenses to the
          -- b values of a map, so remove the edge to a.
          Map.mapWithKey (\key (hint, gkeys) -> (hint, Set.filter (\gkey -> case view etype key of
                                                                              E (AppT (AppT (ConT mtyp) ityp) _etyp) | elem mtyp [''Map, ''Order] -> E ityp /= view etype gkey
                                                                              _ -> True) gkeys))

-- type TypeGraphEdges typ = Map typ (Set typ)

-- | When a VertexStatus value is associated with a Type it describes
-- alterations in the type graph from the usual default.
data VertexStatus typ
    = Vertex      -- ^ normal case
    | Sink        -- ^ out degree zero - don't create any outgoing edges
    | Divert typ  -- ^ replace all outgoing edges with an edge to an alternate type
    | Extra typ   -- ^ add an extra outgoing edge to the given type
    deriving Show

instance Default (VertexStatus typ) where
    def = Vertex

--- type Edges = GraphEdges () TypeGraphVertex

-- | Return the set of edges implied by the subtype relationship among
-- a set of types.  This is just the nodes of the type graph.  The
-- type aliases are expanded by the th-desugar package to make them
-- suitable for use as map keys.
typeGraphEdges'
    :: forall m. (DsMonad m, MonadReader TypeGraph m, HasSet TypeGraphVertex m) =>
       (TypeGraphVertex -> m (Set TypeGraphVertex))
           -- ^ This function is applied to every expanded type before
           -- use, and the result is used instead.  If it returns
           -- NoVertex, no vertices or edges are added to the graph.
           -- If it returns Sink no outgoing edges are added.  The
           -- current use case Substitute is to see if there is an
           -- instance of class @View a b@ where @a@ is the type
           -- passed to @doType@, and replace it with @b@, and use the
           -- lens returned by @View's@ method to convert between @a@
           -- and @b@ (i.e. to implement the edge in the type graph.)
    -> [Type]
    -> m (GraphEdges () TypeGraphVertex)
typeGraphEdges' augment types = do
  execStateT (mapM_ (\typ -> typeVertex typ >>= doNode) types) (mempty :: GraphEdges () TypeGraphVertex)
    where
      doNode v = do
        s <- lift $ getSet
        when (not (member v s)) $
             do lift $ modifySet (insert v)
                doNode' v
      doNode' :: TypeGraphVertex -> StateT (GraphEdges () TypeGraphVertex) m ()
      doNode' typ = do
        addNode typ
        vs <- lift $ augment typ
        mapM_ (addEdge typ) (Set.toList vs)
        mapM_ doNode (Set.toList vs)

      addNode :: TypeGraphVertex -> StateT (GraphEdges () TypeGraphVertex) m ()
      addNode a = modify $ Map.alter (maybe (Just (def, Set.empty)) Just) a

      addEdge :: TypeGraphVertex -> TypeGraphVertex -> StateT (GraphEdges () TypeGraphVertex) m ()
      addEdge a b = modify $ Map.update (\(lbl, s) -> Just (lbl, Set.insert b s)) a

-- | Return the set of adjacent vertices according to the default type
-- graph - i.e. the one determined only by the type definitions, not
-- by any additional hinting function.
adjacent :: forall m. (MonadReader TypeGraph m, DsMonad m) => TypeGraphVertex -> m (Set TypeGraphVertex)
adjacent typ =
    case view etype typ of
      E (ForallT _ _ typ') -> typeVertex typ' >>= adjacent
      E (AppT c e) ->
          typeVertex c >>= \c' ->
          typeVertex e >>= \e' ->
          return $ Set.fromList [c', e']
      E (ConT name) -> do
        info <- qReify name
        case info of
          TyConI dec -> doDec dec
          _ -> return mempty
      _typ -> return $ {-trace ("Unrecognized type: " ++ pprint' typ)-} mempty
    where
      doDec :: Dec -> m (Set TypeGraphVertex)
      doDec dec@(NewtypeD _ tname _ con _) = doCon tname dec con
      doDec dec@(DataD _ tname _ cns _) = Set.unions <$> mapM (doCon tname dec) cns
      doDec (TySynD _tname _tvars typ') = singleton <$> typeVertex typ'
      doDec _ = return mempty

      doCon :: Name -> Dec -> Con -> m (Set TypeGraphVertex)
      doCon tname dec (ForallC _ _ con) = doCon tname dec con
      doCon tname dec (NormalC cname fields) = Set.unions <$> mapM (doField tname dec cname) (zip (List.map Left ([1..] :: [Int])) (List.map snd fields))
      doCon tname dec (RecC cname fields) = Set.unions <$> mapM (doField tname dec cname) (List.map (\ (fname, _, typ') -> (Right fname, typ')) fields)
      doCon tname dec (InfixC (_, lhs) cname (_, rhs)) = Set.unions <$> mapM (doField tname dec cname) [(Left 1, lhs), (Left 2, rhs)]

      doField :: Name -> Dec -> Name -> (Either Int Name, Type) -> m (Set TypeGraphVertex)
      doField tname _dec cname (fld, ftype) = Set.singleton <$> fieldVertex (tname, cname, fld) ftype

-- | Return the TypeGraphVertex associated with a particular type,
-- with no field specified.
typeVertex :: (MonadReader TypeGraph m, DsMonad m) => Type -> m TypeGraphVertex
typeVertex typ = do
        typ' <- expandType typ
        ask >>= runReaderT (vertex Nothing typ') . view typeInfo
        -- magnify typeInfo $ vertex Nothing typ'

-- | Return the TypeGraphVertex associated with a particular type and field.
fieldVertex :: (MonadReader TypeGraph m, DsMonad m) => (Name, Name, Either Int Name) -> Type -> m TypeGraphVertex
fieldVertex fld typ = do
        typ' <- expandType typ
        ask >>= runReaderT (vertex (Just fld) typ') . view typeInfo
        -- magnify typeInfo $ vertex (Just fld) typ'

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
  case runExpanded (view etype v) of
    _ | selfPath -> pathyf
      | simplePath -> simplef
    typ
      | isJust viewType -> do
          let b = fromJust viewType
          expr <- runQ [|viewLens :: Lens' $(return typ) $(return b)|]
          substf expr b
    ConT tname -> namedf tname
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf
