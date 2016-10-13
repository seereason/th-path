-- | Abstract operations on Maps containing graph edges.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.TH.Path.TypeGraph
    ( TypeGraph, graph, gsimple, stack
    , makeTypeGraph
    , graphFromMap

    -- , MaybePair(toMaybePair)
    , HasTGV(asTGV)
    , HasTGVSimple(asTGVSimple)

    -- * TypeGraph queries
    , simplify
    , allPathNodes
    , allPathStarts
    , lensKeys, allLensKeys
    , tgv, tgvSimple, tgvSimple'
    , pathKeys, pathKeys', allPathKeys
    , reachableFrom
    , reachableFromSimple
    , goalReachableFull
    , goalReachableSimple
    , goalReachableSimple'

    , VertexStatus(..)
    -- , adjacent
    , typeGraphVertex
    , typeGraphVertexOfField
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#else
import Control.Applicative
#endif
import Control.Lens
import Control.Monad (foldM)
import qualified Control.Monad.Reader as MTL (ask, ReaderT, runReaderT)
import Control.Monad.Readers (MonadReaders(askPoly, localPoly))
import Control.Monad.States (MonadStates)
import Control.Monad.Trans (lift)
import Data.Default (Default(def))
import Data.Foldable as Fold
import Data.Graph hiding (edges)
import Data.List as List (map)
import Data.Map.Strict as Map (insertWith, Map)
import qualified Data.Map.Strict as Map (toList)
import Data.Maybe (fromJust, mapMaybe)
import Data.Set.Extra as Set (empty, fromList, map, mapM, Set, singleton, toList, union)
import Data.Traversable as Traversable
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.PprLib (ptext, vcat)
import Language.Haskell.TH.Path.Edges (GraphEdges, simpleEdges)
import Language.Haskell.TH.Path.Expand (ExpandMap, expandType)
import Language.Haskell.TH.Path.Prelude (adjacent', pprint1, reachable')
import Language.Haskell.TH.Path.TypeInfo (startTypes, TypeInfo, typeVertex, typeVertex', fieldVertex)
import Language.Haskell.TH.Path.Shape (Field)
import Language.Haskell.TH.Path.Stack (StackElement)
import Language.Haskell.TH.Path.Vertex (TGV'(..), TGV, TGVSimple'(..), TGVSimple, TypeGraphVertex, vsimple)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)

data TypeGraph
    = TypeGraph
      { _graph :: (Graph, Vertex -> ((), TGV', [TGV']), TGV' -> Maybe Vertex)
      , _gsimple :: (Graph, Vertex -> ((), TGVSimple', [TGVSimple']), TGVSimple' -> Maybe Vertex)
      , _stack :: [StackElement]
      }

-- | Build a TypeGraph given a set of edges and the TypeInfo environment
makeTypeGraph :: MonadReaders TypeInfo m => (GraphEdges TGV') -> m TypeGraph
makeTypeGraph es = do
  return $ TypeGraph
             { _graph = graphFromMap es
             , _gsimple = graphFromMap (simpleEdges es)
             , _stack = []
             }

-- | Build a graph from the result of typeGraphEdges, each edge goes
-- from a type to one of the types it contains.  Thus, each edge
-- represents a primitive lens, and each path in the graph is a
-- composition of lenses.
graphFromMap :: forall key. (Ord key) =>
                GraphEdges key -> (Graph, Vertex -> ((), key, [key]), key -> Maybe Vertex)
graphFromMap mp =
    graphFromEdges triples
    where
      triples :: [((), key, [key])]
      triples = List.map (\ (k, ks) -> ((), k, Fold.toList ks)) $ Map.toList mp

$(makeLenses ''TypeGraph)

instance (Monad m, MonadReaders [StackElement] m) => MonadReaders [StackElement] (MTL.ReaderT TypeGraph m) where
    askPoly = lift askPoly
    localPoly f action = MTL.ask >>= MTL.runReaderT (localPoly f (lift action))

instance MonadReaders TypeInfo m => MonadReaders TypeInfo (MTL.ReaderT TypeGraph m) where
    askPoly = lift askPoly
    localPoly f action = MTL.ask >>= MTL.runReaderT (localPoly f (lift action))

instance Ppr TypeGraph where
    ppr = ppr . view graph

instance Ppr Vertex where
    ppr n = ptext ("V" ++ show n)

instance Ppr (Graph, Vertex -> ((), TGV', [TGV']), TGV' -> Maybe Vertex) where
    ppr (g, vf, _) = vcat (List.map (ppr . vf) (vertices g))

instance Ppr (Graph, Vertex -> ((), TGVSimple', [TGVSimple']), TGVSimple' -> Maybe Vertex) where
    ppr (g, vf, _) = vcat (List.map (ppr . vf) (vertices g))

{-
class MaybePair t n where toMaybePair :: (Vertex, n) -> t
instance MaybePair (Vertex, n) n where toMaybePair = id
instance MaybePair t t where toMaybePair = snd
-}

class HasTGV a where asTGV :: a -> TGV'
class HasTGVSimple a where asTGVSimple :: a -> TGVSimple'

-- instance HasTGV TGV where asTGV = id
-- instance HasTGVSimple TGVSimple where asTGVSimple = id

instance HasTGV TGV where asTGV = snd
instance HasTGVSimple TGVSimple where asTGVSimple = snd

-- | All the nodes in the TGV (unsimplified) graph, where each field
-- of a record is a distinct node.
allPathNodes :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m (Set TGV)
allPathNodes = do
  (g, vf, kf) <- askPoly >>= return . view graph
  kernel <- askPoly >>= \ti -> MTL.runReaderT (Traversable.mapM expandType (view startTypes ti) >>= Traversable.mapM typeVertex') ti
  let keep :: Set Vertex
      keep = Set.fromList $ concatMap (reachable g) (mapMaybe kf kernel)
      keep' :: Set TGV
      keep' = Set.map (\v -> (v, view _2 (vf v))) keep
  return keep'

-- | All the nodes in the TGVSimple graph, where each field representa
-- a different type.
allPathStarts :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
                 m (Set TGVSimple)
allPathStarts = do
  ts <- allPathNodes :: m (Set TGV)
  Set.mapM simplify ts

view' :: MonadReaders s m => Getting b s b -> m b
view' lns = view lns <$> askPoly

-- | Each lens represents a single step in a path.  The start point is
-- a simplified vertex and the endpoint is an unsimplified vertex.
allLensKeys :: forall m. (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m (Map TGVSimple (Set TGV))
allLensKeys = do
  (starts :: Set TGVSimple) <- allPathStarts
  foldM (\mp s -> do
           ts <- lensKeys s :: m (Set TGV)
           return $ Fold.foldr (Map.insertWith Set.union s . Set.singleton) mp ts
        ) mempty (Set.toList starts)

-- | Find the node corresponding to the given simple graph node in the
-- full graph.
tgv :: MonadReaders TypeGraph m => Maybe Field -> TGVSimple -> m TGV
tgv mf s =
    do let t = TGV' { _field = mf, _vsimple = asTGVSimple s}
       (_g, vf, kf) <- askPoly >>= return . view graph
       case kf t of
         Just v -> let (_, t', _) = vf v in return (v, t')
         Nothing -> error $ "tgv: " ++ show mf ++ " " ++ show s

-- | Find the simple graph node corresponding to the given type
tgvSimple :: (MonadStates ExpandMap m, DsMonad m, MonadReaders TypeInfo m, MonadReaders TypeGraph m) => Type -> m (Maybe TGVSimple)
tgvSimple t =
    do (_g, _vf, kf) <- askPoly >>= return . view gsimple
       s <- expandType t >>= typeVertex
       return $ fmap (\k -> (k, s)) (kf s)

tgvSimple' :: (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
              Type -> m TGVSimple
tgvSimple' typ =
    tgvSimple typ >>= maybe (error $ "tgvSimple' - no node for " ++ pprint1 typ) pure

-- | Return the nodes adjacent to x in the lens graph.
lensKeys :: (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
            TGVSimple -> m (Set TGV)
lensKeys s = do
  g <- view' graph
  t <- tgv Nothing s
  return $ Set.fromList $ adjacent' g t

simplify :: (MonadReaders TypeGraph m, HasTGV t) => t -> m TGVSimple
simplify t = do
  (_, _, kf) <- view' gsimple
  let s = (view vsimple . asTGV) t
  let v = (fromJust . kf) s
  return (v, s)

-- | Paths go between simple types.
allPathKeys :: (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m (Map TGVSimple (Set TGVSimple))
allPathKeys = do
  starts <- Set.toList <$> allPathStarts
  foldM (\mp s -> pathKeys s >>= return . Fold.foldr (Map.insertWith Set.union s . Set.singleton) mp) mempty starts

-- | Return the nodes reachable from x in the path graph.
pathKeys :: (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGVSimple -> m (Set TGVSimple)
pathKeys s = do
  gs <- view' gsimple
  return $ Set.fromList $ reachable' gs s

-- | Return the nodes reachable from x in the path graph.
pathKeys' :: (DsMonad m, MonadStates ExpandMap m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => TGV -> m (Set TGVSimple)
pathKeys' s = do
#if 0
  g <- view' graph
  Set.fromList <$> Prelude.mapM simplify (reachable' g s)
#else
  s' <- simplify s
  gs <- view' gsimple
  pure $ Set.fromList $ reachable' gs s'
#endif

  -- allPathStarts >>= return . Map.fromList . List.map (\x -> (x, Set.fromList (reachable' gs x))) . Set.toList . Set.map (view vsimple)

reachableFrom :: forall m. (DsMonad m, MonadReaders TypeGraph m) => TGV -> m (Set TGV)
reachableFrom t = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view' graph
  case kf (asTGV t) of
    Nothing -> return Set.empty
    Just v ->
        let vs = Set.fromList (reachable (transposeG g) v) in
        return $ Set.map (\v' -> let (_, t', _) = vf v' in (v', t')) vs

reachableFromSimple :: forall m. (DsMonad m, MonadReaders TypeGraph m) => TGVSimple -> m (Set TGVSimple)
reachableFromSimple s = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view' gsimple
  case kf (asTGVSimple s) of
    Nothing -> return Set.empty
    Just v ->
        let vs = Set.fromList (reachable (transposeG g) v) in
        return $ Set.map (\v' -> let (_, s', _) = vf v' in (v', s')) vs

-- | Can we reach the goal type from the start type in this key?
goalReachableFull :: (Functor m, DsMonad m, MonadReaders TypeGraph m, HasTGV t) => t -> t -> m Bool
goalReachableFull gkey key0 = isReachable (asTGV gkey) (asTGV key0) <$> view' graph

-- | Can we reach the goal type in the simplified graph?
goalReachableSimple :: (Functor m, DsMonad m, MonadReaders TypeGraph m, HasTGVSimple s) => s -> s -> m Bool
goalReachableSimple gkey key0 = isReachable (asTGVSimple gkey) (asTGVSimple key0) <$> view' gsimple

-- | Version of goalReachableSimple that first simplifies its argument nodes
goalReachableSimple' :: (Functor m, DsMonad m, MonadReaders TypeGraph m, HasTGV t) => t -> t -> m Bool
goalReachableSimple' gkey key0 = do
  (_g, _vf, kf) <- view' gsimple
  let gkey' = view vsimple (asTGV gkey)
      key0' = view vsimple (asTGV key0)
  goalReachableSimple (fromJust (kf gkey'), gkey') (fromJust (kf key0'), key0')

isReachable :: TypeGraphVertex key => key -> key -> (Graph, Vertex -> ((), key, [key]), key -> Maybe Vertex) -> Bool
isReachable gkey key0 (g, _vf, kf) = path g (fromJust $ kf key0) (fromJust $ kf gkey)

-- | Return the TGV associated with a particular type,
-- with no field specified.
typeGraphVertex :: ({-MonadReaders TypeGraph m,-} MonadReaders TypeInfo m, MonadStates ExpandMap m, DsMonad m) => Type -> m TGV'
typeGraphVertex typ = do
        typ' <- expandType typ
        askPoly >>= \(ti :: TypeInfo) -> MTL.runReaderT (typeVertex' typ') ti
        -- magnify typeInfo $ vertex Nothing typ'

-- | Return the TGV associated with a particular type and field.
typeGraphVertexOfField :: (MonadReaders TypeInfo m, MonadStates ExpandMap m, DsMonad m) => Field -> Type -> m TGV'
typeGraphVertexOfField fld typ = do
        typ' <- expandType typ
        askPoly >>= \(ti :: TypeInfo) -> MTL.runReaderT (fieldVertex fld typ') ti
        -- magnify typeInfo $ vertex (Just fld) typ'

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

{-
-- | Return the set of adjacent vertices according to the default type
-- graph - i.e. the one determined only by the type definitions, not
-- by any additional hinting function.
adjacent :: forall m. ({-MonadReaders TypeGraph m,-} MonadReaders TypeInfo m, DsMonad m, MonadStates ExpandMap m) => TGV -> m (Set TGV)
adjacent typ =
    case view (vsimple . etype) typ of
      E (ForallT _ _ typ') -> typeGraphVertex typ' >>= adjacent
      E (AppT c e) ->
          typeGraphVertex c >>= \c' ->
          typeGraphVertex e >>= \e' ->
          return $ Set.fromList [c', e']
      E (ConT name) -> do
        info <- qReify name
        case info of
          TyConI dec -> doDec dec
          _ -> return mempty
      _typ -> return $ {-trace ("Unrecognized type: " ++ pprint' typ)-} mempty
    where
      doDec :: Dec -> m (Set TGV)
      doDec dec@(NewtypeD _ tname _ con _) = doCon tname dec con
      doDec dec@(DataD _ tname _ cns _) = Set.unions <$> Traversable.mapM (doCon tname dec) cns
      doDec (TySynD _tname _tvars typ') = singleton <$> typeGraphVertex typ'
      doDec _ = return mempty

      doCon :: Name -> Dec -> Con -> m (Set TGV)
      doCon tname dec (ForallC _ _ con) = doCon tname dec con
      doCon tname dec (NormalC cname fields) = Set.unions <$> Traversable.mapM (doField tname dec cname) (zip (List.map Left ([1..] :: [Int])) (List.map snd fields))
      doCon tname dec (RecC cname fields) = Set.unions <$> Traversable.mapM (doField tname dec cname) (List.map (\ (fname, _, typ') -> (Right fname, typ')) fields)
      doCon tname dec (InfixC (_, lhs) cname (_, rhs)) = Set.unions <$> Traversable.mapM (doField tname dec cname) [(Left 1, lhs), (Left 2, rhs)]

      doField :: Name -> Dec -> Name -> (Either Int Name, Type) -> m (Set TGV)
      doField tname _dec cname (fld, ftype) = Set.singleton <$> typeGraphVertexOfField (tname, cname, fld) ftype
-}