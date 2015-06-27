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
module Language.Haskell.TH.Path.Graph
    ( makeTypeGraphEdges
    , makeTypeGraph
    , VertexStatus(..)
    , typeGraphEdges'
    ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Monoid (mempty)
#else
import Control.Applicative ((<$>))
#endif
import Control.Lens -- (makeLenses, over, view)
import Control.Monad.Reader (ask, MonadReader, runReaderT)
import Control.Monad.State (execStateT, modify, MonadState(get), StateT)
import Control.Monad.Trans (lift)
import Data.Default (Default(def))
import Data.Foldable (mapM_)
import Data.Graph (reachable)
import Data.List as List (map)
import Data.Map as Map (alter, keys, lookup, Map, map, mapWithKey, update)
import Data.Maybe (mapMaybe)
import Data.Set as Set (difference, empty, filter, fromList, insert, map, Set)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.KindInference (inferKind)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Monad (R(..), typeInfo)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.Prune (pruneTypeGraph)
import Language.Haskell.TH.Syntax (Quasi(..))
import Language.Haskell.TH.TypeGraph (unlifted, E(E), expandType, freeTypeVars,
                                      dissolveM, GraphEdges, graphFromMap, isolate,
                                      TypeGraphInfo, typeGraphInfo,
                                      simpleEdges, typeGraphEdges, vertex,
                                      TypeGraphVertex, etype, field)
import Prelude hiding (any, concat, exp, foldr, mapM_, null, or)

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

-- | Build a graph of the subtype relation, omitting any types whose
-- arity is nonzero and any not reachable from the start types.  (We
-- may also want to eliminate nodes that are not on a path from a
-- start type to a goal type, though eventually goal types will be
-- eliminated - all types will be goal types.)
makeTypeGraphEdges :: forall m hint. (DsMonad m, Default hint, Ord hint, MonadReader TypeGraphInfo m) =>
                      [Type] -> m (GraphEdges hint TypeGraphVertex)
makeTypeGraphEdges st = do
  -- im <- view infoMap
  -- Dissolve the vertices for types whose arity is not zero.  Each of
  -- their in-edges become connected to each of their out-edges.
  let victim :: TypeGraphVertex -> m Bool
      victim v = do
        let (E etyp) = view etype v
        k <- runQ $ inferKind etyp
        fv <- runQ $ freeTypeVars etyp
        prim <- unlifted etyp
        return $ k /= Right StarT || fv /= Set.empty || prim
  edges' <- typeGraphEdges >>= pruneTypeGraph >>= dissolveM victim >>= return . removePathsToOrderKeys . removeUnnamedFieldEdges
  let (g, vf, kf) = graphFromMap edges'
  -- Isolate all nodes that are not reachable from the start types.
  kernel <- mapM expandType st >>= mapM (vertex Nothing) >>= return . mapMaybe kf
  let keep :: Set TypeGraphVertex
      keep = Set.map (\(_, key, _) -> key) $ Set.map vf $ Set.fromList $ concatMap (reachable g) kernel
      victims = difference (Set.fromList (Map.keys edges')) keep
      edges'' :: GraphEdges hint TypeGraphVertex
      edges'' = isolate victims edges'
  -- trace (pprint edges'') (return ())
  return edges''
    where
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

-- type Edges = Edges
type Edges = GraphEdges () TypeGraphVertex

-- | Return the set of edges implied by the subtype relationship among
-- a set of types.  This is just the nodes of the type graph.  The
-- type aliases are expanded by the th-desugar package to make them
-- suitable for use as map keys.
typeGraphEdges'
    :: forall m. (DsMonad m, MonadReader R m) =>
       (TypeGraphVertex -> m (VertexStatus Type))
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
    -> m Edges
typeGraphEdges' augment types = do
  execStateT (mapM_ (\typ -> vert typ >>= doNode) types) (mempty :: Edges)
    where
      doNode :: TypeGraphVertex -> StateT Edges m ()
      doNode typ = do
        mp <- get
        status <- lift (augment typ)
        case Map.lookup typ mp of
          Just _ -> return ()
          Nothing ->
            case status of
              Sink -> addNode typ
              (Divert typ') -> vert typ' >>= \typ'' -> addNode typ >> addEdge typ typ'' >> doNode typ''
              (Extra typ') -> vert typ' >>= \typ'' -> addNode typ >> doEdges typ >> addEdge typ typ'' >> doNode typ''
              Vertex -> addNode typ >> doEdges typ

      addNode :: TypeGraphVertex -> StateT Edges m ()
      -- addNode a = expandType a >>= \ a' -> modify $ Map.insertWith (flip const) a' Set.empty
      addNode a = modify $ Map.alter (maybe (Just (def, Set.empty)) Just) a
      addEdge :: TypeGraphVertex -> TypeGraphVertex -> StateT Edges m ()
      addEdge a b = modify $ Map.update (\(lbl, s) -> Just (lbl, Set.insert b s)) a

      doEdges :: TypeGraphVertex -> StateT Edges m ()
      doEdges typ =
          case view etype typ of
            E (ForallT _ _ typ') -> vert typ' >>= \typ'' -> addEdge typ typ'' >> doNode typ''
            E (AppT container element) ->
                vert container >>= \container' ->
                vert element >>= \element' ->
                addEdge typ container' >>
                addEdge typ element' >>
                doNode container' >>
                doNode element'
            E (ConT name) -> do
              info <- qReify name
              case info of
                TyConI dec -> doDec typ dec
                _ -> return ()
            _typ -> return ({-trace ("Unrecognized type: " ++ pprint' typ)-} ())
      doDec :: TypeGraphVertex -> Dec -> StateT Edges m ()
      doDec typ dec@(NewtypeD _ tname _ con _) = doCon typ tname dec con
      doDec typ dec@(DataD _ tname _ cons _) = mapM_ (doCon typ tname dec) cons
      doDec typ (TySynD _tname _tvars typ') = vert typ' >>= \typ'' -> addEdge typ typ'' >> doNode typ''
      doDec _ _ = return ()

      doCon :: TypeGraphVertex -> Name -> Dec -> Con -> StateT Edges m ()
      doCon typ tname dec (ForallC _ _ con) = doCon typ tname dec con
      doCon typ tname dec (NormalC cname fields) = mapM_ (doField typ tname dec cname) (zip (List.map Left ([1..] :: [Int])) (List.map snd fields))
      doCon typ tname dec (RecC cname fields) = mapM_ (doField typ tname dec cname) (List.map (\ (fname, _, typ') -> (Right fname, typ')) fields)
      doCon typ tname dec (InfixC (_, lhs) cname (_, rhs)) = mapM_ (doField typ tname dec cname) [(Left 1, lhs), (Left 2, rhs)]

      doField :: TypeGraphVertex -> Name -> Dec -> Name -> (Either Int Name, Type) -> StateT Edges m ()
      doField typ tname _dec cname (fld, ftype) = fieldVert (tname, cname, fld) ftype >>= \ftype' -> addEdge typ ftype' >> doNode ftype'

      vert :: Type -> StateT Edges m TypeGraphVertex
      vert typ = do
        r <- ask
        typ' <- expandType typ
        runReaderT (vertex Nothing typ') (view typeInfo r)

      fieldVert :: (Name, Name, Either Int Name) -> Type -> StateT Edges m TypeGraphVertex
      fieldVert fld typ = do
        r <- ask
        typ' <- expandType typ
        runReaderT (vertex (Just fld) typ') (view typeInfo r)
