{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
module Language.Haskell.TH.Path.Core
    ( R(..), startTypes, goalTypes, lensHintMap, typeInfo, edges
    , allLensKeys
    , allPathKeys
    , makePathLenses
    , pathHints
    , pathTypeNameFromTypeName
    , pathTypeNames
    , bestPathTypeName
    , pathConNameOfField
    , fieldLensName
    , reachableFrom
    , isReachable
    , goalReachable

    -- * Basic Path Types
    , Path_Pair(..)
    , Path_Maybe(..)
    , Path_Map(..)
    , Path_OMap(..)
    , Path_List(..)
    -- * Hints
    , LensHint(..)
    , Field
    , View(viewLens)

    , FoldPathControl(..)
    , foldPath
    ) where

import Control.Applicative ((<$>))
import Control.Lens -- (makeLenses, over, view)
import Control.Monad (filterM)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.Graph (Graph, reachable, transposeG, Vertex)
import Data.List as List (intercalate, map)
import Data.Map as Map (findWithDefault, keys, Map)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set as Set (difference, fromList, map, minView, Set)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib (ptext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.LensTH (nameMakeLens)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.TypeGraph.Core (Field, pprint')
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType, runExpanded)
import Language.Haskell.TH.TypeGraph.Graph (GraphEdges, graphFromMap)
import Language.Haskell.TH.TypeGraph.Hints (HasVertexHints(hasVertexHints), VertexHint(..))
import Language.Haskell.TH.TypeGraph.Info (TypeGraphInfo)
import Language.Haskell.TH.TypeGraph.Monad (vertex)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype, _etype, field, syns, typeNames)
import Prelude hiding (any, concat, concatMap, elem, exp, foldr, mapM_, null, or)
import Web.Routes.TH (derivePathInfo)

import Data.Foldable
#if ! MIN_VERSION_base(4,8,0)
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True
#endif

-- Primitive path types

data Path_Pair a b = Path_First a | Path_Second b | Path_Pair deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Invalid = Path_Invalid deriving (Eq, Ord, Read, Show, Typeable, Data)

-- data SomePath = SomePath deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Maybe a = Path_Maybe a deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_Map k v = Path_Map k v  deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_List a = Path_Elems deriving (Eq, Ord, Read, Show, Typeable, Data)

data Path_OMap k a = Path_OMap | Path_At k a deriving (Eq, Ord, Read, Show, Typeable, Data) -- FIXME - Path_OMap constructor should be removed

$(derivePathInfo ''Path_Pair)
$(derivePathInfo ''Path_List)
$(derivePathInfo ''Path_Map)
$(derivePathInfo ''Either)
$(derivePathInfo ''Path_Maybe)
$(derivePathInfo ''Path_OMap)

$(deriveSafeCopy 0 'base ''Path_Pair)
$(deriveSafeCopy 0 'base ''Path_List)
$(deriveSafeCopy 0 'base ''Path_Map)
$(deriveSafeCopy 0 'base ''Path_Maybe)
$(deriveSafeCopy 0 'base ''Path_OMap)

data R
    = R
      { _startTypes :: [Type]
      -- ^ Start types
      , _goalTypes :: [Type]
      -- ^ The names of the types we are generating pathTo_lens_*_name functions for
      , _lensHintMap :: Map TypeGraphVertex [LensHint] -- FIXME: [LensHint] -> Set LensHint
      , _typeInfo :: TypeGraphInfo LensHint
      , _edges :: GraphEdges LensHint TypeGraphVertex
      , _graph :: (Graph, Vertex -> (LensHint, TypeGraphVertex, [TypeGraphVertex]), TypeGraphVertex -> Maybe Vertex)
      }

data LensHint
    = Substitute Exp Type
    -- ^ Replace the automatically generated pathToLens
    -- function with this.  The expression is the lens to use,
    -- and the second element of the pair is the 'b' Type of
    -- the lens.
    | Self
    -- ^ Causes the type to be used as its own Path Type.  For
    -- example, a UUID or some enumerated type contained in a record
    -- could be used directly to reference the object that contains it.
    | Normal'
    -- ^ no effect
    | VertexHint VertexHint
    deriving (Eq, Ord)

instance HasVertexHints LensHint where
    hasVertexHints (VertexHint x) = return [x]
    hasVertexHints (Substitute _exp typ) = return [Divert typ]
    hasVertexHints Self = return [Normal] -- FIXME
    hasVertexHints _ = return []

instance Default LensHint where
    def = Normal'

instance Show LensHint where
    show (Substitute exp typ) = "Substitute (" ++ pprint exp ++ ") (" ++ pprint typ ++ ")"
    show Self = "Self"
    show Normal' = "Normal'"
    show (VertexHint x) = "VertexHint " ++ show x

instance Ppr LensHint where
    ppr = ptext . show

$(makeLenses ''R)

-- This is the View class from MIMO, but we can't use it directly
-- until we convert from data-lens to lens.
class View a b | a -> b where
    viewLens :: Lens' a b

-- | Find all the hints that match either the exact key vertex, or the
-- key _field set to Nothing.
pathHints :: (DsMonad m, MonadReader R m) => TypeGraphVertex -> m [(TypeGraphVertex, LensHint)]
pathHints key = do
    mp <- view lensHintMap
    let key' = set field Nothing key
    let s1 = Set.fromList $ Map.findWithDefault [] key mp
        s2 = Set.fromList $ Map.findWithDefault [] key' mp
    return $ if null (Set.difference s1 s2)
             then List.map (key',) (toList s2)
             else List.map (key,) (toList s1)

allLensKeys :: (DsMonad m, MonadReader R m) => m (Set (TypeGraphVertex, TypeGraphVertex))
allLensKeys = do
  (gkeys :: [TypeGraphVertex]) <- view goalTypes >>= \gtypes -> view typeInfo >>= runReaderT (mapM expandType gtypes >>= mapM (vertex Nothing))
  pathKeys <- allPathKeys
  Set.fromList <$> filterM (uncurry goalReachable) [ (g, k) | g <- gkeys, k <- toList pathKeys ]

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
  hints <- pathHints key
  case filter noLensHint (List.map snd hints) of
    [] -> mapM make (toList (typeNames key)) >>= tell
    _ -> return ()
    where
      make tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))
      -- The presence of most hints means we don't need a lens
      noLensHint Self = True
      noLensHint (VertexHint Sink) = True
      noLensHint (Substitute _ _) = True
      noLensHint _ = False

-- Naming conventions

pathTypeNameFromTypeName :: Name -> Name
pathTypeNameFromTypeName tname = mkName $ "Path_" ++ nameBase tname

pathTypeNames :: TypeGraphVertex -> Set Name
pathTypeNames = Set.map pathTypeNameFromTypeName . typeNames

-- | If the type is (ConT name) return name, otherwise return a type
-- synonym name.
bestPathTypeName :: TypeGraphVertex -> Maybe (Name, Set Name)
bestPathTypeName v =
    case view etype v of
      E (ConT tname) -> Just (pathTypeNameFromTypeName tname, Set.map pathTypeNameFromTypeName (view syns v))
      _ -> Set.minView (Set.map pathTypeNameFromTypeName (view syns v))

-- | Path type constructor for the field described by key in the parent type named tname.
pathConNameOfField :: TypeGraphVertex -> Maybe Name
pathConNameOfField key = maybe Nothing (\ (tname, _, Right fname') -> Just $ mkName $ "Path_" ++ nameBase tname ++ "_" ++ nameBase fname') (_field key)

fieldLensName :: Name -> Name -> Name
fieldLensName tname fname' = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname')

reachableFrom :: forall m. (DsMonad m, MonadReader R m) => TypeGraphVertex -> m (Set TypeGraphVertex)
reachableFrom v = do
  -- (g, vf, kf) <- graphFromMap <$> view edges
  (g, vf, kf) <- view graph
  let Just v' = kf v
  return $ Set.map (\(_, key, _) -> key) . Set.map vf $ Set.fromList $ reachable (transposeG g) v'

isReachable :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
isReachable gkey key0 = do
  es <- view edges
  let (g, _vf, kf) = graphFromMap es
  case kf key0 of
    Nothing -> error ("isReachable - unknown key: " ++ pprint' key0)
    Just key -> do
      let gvert = fromMaybe (error $ "Unknown goal type: " ++ pprint' gkey ++ "\n" ++ intercalate "\n  " ("known:" : List.map pprint' (Map.keys es))) (kf gkey)
      -- Can we reach any node whose type matches (ConT gname)?  Fields don't matter.
      return $ elem gvert (reachable g key)

-- | Can we reach the goal type from the start type in this key?
-- | Can we reach the goal type from the start type in this key?
goalReachable :: (Functor m, DsMonad m, MonadReader R m) => TypeGraphVertex -> TypeGraphVertex -> m Bool
goalReachable gkey key0 = isReachable gkey key0

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

foldPath :: (DsMonad m, MonadReader R m) => FoldPathControl m r -> TypeGraphVertex -> [(TypeGraphVertex, LensHint)] -> m r
foldPath (FoldPathControl{..}) v hints = do
  let substs = [x | x@(_, Substitute _ _) <- hints]
  case runExpanded (_etype v) of
    _ | elem Self (List.map snd hints) -> pathyf
      | elem (VertexHint Sink) (List.map snd hints) -> simplef
      | not (null substs) -> let ((_, Substitute exp typ) : _) = substs in substf exp typ
    ConT tname -> namedf tname
    AppT (AppT mtyp ityp) etyp | mtyp == ConT ''Order -> orderf ityp etyp
    AppT ListT etyp -> listf etyp
    AppT (AppT t3 ktyp) vtyp | t3 == ConT ''Map -> mapf ktyp vtyp
    AppT (AppT (TupleT 2) ftyp) styp -> pairf ftyp styp
    AppT t1 vtyp | t1 == ConT ''Maybe -> maybef vtyp
    AppT (AppT t3 ltyp) rtyp | t3 == ConT ''Either -> eitherf ltyp rtyp
    _ -> otherf
