-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

{-# OPTIONS -Wall -fno-warn-unused-imports #-}
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
module Language.Haskell.TH.Path.Decs
    ( pathDecs
    ) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad as List ( mapM )
import Control.Monad.Reader (runReaderT)
import Control.Monad.Readers (askPoly, MonadReaders)
import Control.Monad.State (evalStateT, get, modify, StateT)
import Control.Monad.States (MonadStates(getPoly, putPoly), modifyPoly)
import Control.Monad.Trans as Monad (lift)
import Control.Monad.Writer (MonadWriter, execWriterT, tell, WriterT)
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable (mapM_)
import Data.Foldable as Foldable
import Data.List as List (concatMap, intercalate, isPrefixOf, map)
import Data.Map as Map (Map, toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy
import Data.Set as Set (delete, minView)
import Data.Set.Extra as Set (insert, map, member, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Data.Tree (Tree(Node))
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (mat, IsPathType(idPath), IsPathNode(PVType, pvTree), IsPath(..), Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.Common (bestNames, bestTypeName, bestPathTypeName,
                                             clauses, fieldLensNamePair, fieldLensNameOld, makePathLens,
                                             pathConNameOfField, pathTypeNameFromTypeName, pvName, uncap)
import Language.Haskell.TH.Path.Decs.IsPath (doIsPathNode)
import Language.Haskell.TH.Path.Decs.PathsOf (pathsOfClauses)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Decs.PVType (doPVType)
import Language.Haskell.TH.Path.Decs.ToLens (toLensClauses)
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.Order (lens_omat, Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.View (viewInstanceType, viewLens)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify), VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), unE, ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, allPathStarts, goalReachableSimple, reachableFromSimple, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, field, TGV, TGVSimple, syns, TypeGraphVertex(bestType), typeNames, vsimple)

pathDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
pathDecs = execWriterT $ allPathStarts >>= \ps -> Foldable.mapM_ doNode ps >> doPVType ps

doNode :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  -- generate the path type declarations
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
  viewType <- viewInstanceType (view etype v)
  case () of
    _ | selfPath -> return ()
      | simplePath -> maybe (error $ "pathTypeDecs: simple path type has no name: " ++ pprint' v) doSimplePath (bestNames v)
      | isJust viewType ->
          do let b = fromJust viewType
             viewPath b
    _ -> doNames
  -- generate the lens declarations
  case simplePath of
    False -> mapM makePathLens (Foldable.toList (typeNames v)) >>= {- t1 >>= -} tell . concat
    _ -> return ()
  -- generate the path instance declarations
  pathKeys v >>= Set.mapM_ (pathInstanceDecs v)
    where
      doNames = mapM_ (\tname -> runQ (reify tname) >>= doInfo) (typeNames v)

      doSimplePath :: (Name, Name, Set Name) -> m ()
      doSimplePath (_tname, pname, syns') = do
        a <- runQ $ newName "a"
        runQ (dataD (return []) pname [PlainTV a] [normalC pname []] supers) >>= tell . (: [])
        doIsPathType pname a
        doIsPathNode v
        mapM_ (\psyn -> runQ (newName "a" >>= \a -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) >>= tell . (: [])) (Foldable.toList syns')

      -- viewPath [t|Text|] = data Path_Branding a = Path_Branding (Path_Text a)
      viewPath :: Type -> m ()
      viewPath styp = do
        let Just tname = bestTypeName v
            Just (pname, syns') = bestPathTypeName v
            -- gname = mkName ("Goal_" ++ nameBase pname)
        skey <- expandType styp >>= typeVertex
        a <- runQ $ newName "a"
        ptype <- pathType (varT a) skey
        -- A view type may have a type variable, which
        -- we need to replace with the goal type a.
        let ptype' = substitute (VarT a) ptype
        runQ (sequence (dataD (return []) pname [PlainTV a]
                              [ normalC (mkName (nameBase pname ++ "_View")) [strictType notStrict (pure ptype')]
                              , normalC (mkName (nameBase pname)) []
                              ] supers
                         : List.map (\psyn -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) (Foldable.toList syns'))) >>= tell
        doIsPathType pname a
        doIsPathNode v

      substitute :: Type -> Type -> Type
      substitute gtype (AppT x (VarT _)) = (AppT x gtype)
      substitute gtype (AppT a b) = AppT (substitute gtype a) (substitute gtype b)
      substitute gtype (VarT _) = gtype
      substitute _ x = x

      doInfo (TyConI dec) =
          -- tell [d| z = $(litE (stringL ("doDec " ++ pprint' dec))) |] >>
          doDec dec
      doInfo (FamilyI dec _insts) = doDec dec
      doInfo info = error $ "pathTypeDecs - unexpected Info: " ++ pprint' info ++ "\n  " ++ show info
      doDec :: Dec -> m ()
      -- If we have a type synonym, we can create a path type synonym
      doDec (TySynD _ _ typ') =
          do a <- runQ $ newName "a"
             key' <- expandType typ' >>= typeVertex
             ptype <- pathType (varT a) key'
             mapM_ (\pname ->
                        do -- doIsPathType pname a
                           doIsPathNode v
                           runQ (tySynD pname [PlainTV a] (return ptype)) >>= tell . (: [])) (pathTypeNames' v)
      doDec (NewtypeD _ tname _ con _) = doDataD tname [con]
      doDec (DataD _ tname _ cons _) = doDataD tname cons
      doDec (FamilyD _flavour _name _tvbs _mkind) = return ()
      doDec dec = error $ "doName - unexpected Dec: " ++ pprint dec ++ "\n  " ++ show dec

      doDataD :: Name -> [Con] -> m ()
      doDataD tname cons =
          do a <- runQ $ newName "a"
             mapM (doCon a tname) cons >>= makeDecs a

      makeDecs :: Name -> [[Con]] -> m ()
      makeDecs a pconss =
          case filter (/= []) pconss of
            [pcons] -> mapM_ (\pname -> do let Just tname = bestName v
                                           runQ (dataD (cxt []) pname [PlainTV a] (List.map return (pcons ++ [NormalC pname []])) supers) >>= tell . (: [])
                                           doIsPathType pname a
                                           doIsPathNode v
                             ) (pathTypeNames' v)
            [] | length pconss > 1 -> return () -- enum
            [] -> return ()
                  -- FIXME - if there are paths from several different
                  -- constructors we want an extra path type to
                  -- represent the values for each constructor.
            pconss' ->
                makeDecs a [concat pconss']

      doCon :: (DsMonad m, MonadReaders TypeGraph m) => Name -> Name -> Con -> m [Con]
      doCon a tname (ForallC _ _ con) = doCon a tname con
      doCon _ _ (NormalC _ _) = return []
      doCon _ _ (InfixC _ _ _) = return []
      doCon a tname (RecC cname ts) = concat <$> mapM (doField a tname cname) ts

      -- Each field of the original type turns into zero or more (Con, Clause)
      -- pairs, each of which may or may not have a field representing the path type
      -- of some piece of the field value.  FIXME: This exact code is in PathTypes.hs
      doField :: (DsMonad m, MonadReaders TypeGraph m) => Name -> Name -> Name -> VarStrictType -> m [Con]
      doField a tname cname (fname', _, ftype) =
          do key' <- expandType ftype >>= fieldVertex (tname, cname, Right fname')
             let Just pcname = pathConNameOfField key'
             ptype <- case ftype of
                        ConT ftname -> runQ $ appT (conT (pathTypeNameFromTypeName ftname)) (varT a)
                        -- It would be nice to use pathTypeCall (varT a) key' here, but
                        -- it can't infer the superclasses for (PathType Foo a) - Ord,
                        -- Read, Data, etc.
                        _ -> pathType (varT a) (view vsimple key') -- runQ (appT (appT (conT ''PathType) (pure (view (vsimple . etype . unE) key'))) (varT a))
             case ptype of
               TupleT 0 -> return []
               -- Given the list of clauses for a field's path type, create new
               -- constructor for the field in the parent record and alter the
               -- clauses to match expressions wrapped in this new constructor.
               _ -> (: []) <$> runQ (normalC pcname [strictType notStrict (return ptype)])

      supers :: [Name]
      supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]

      -- Given a vertex in the type graph, return the names of the
      -- corresponding path type and its synonyms.
      pathTypeNames' :: TypeGraphVertex v => v -> Set Name
      pathTypeNames' = Set.map pathTypeNameFromTypeName . typeNames

doIsPathType pname a = runQ [d|instance IsPathType ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathInstanceDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
                     TGVSimple -> TGVSimple -> m ()
pathInstanceDecs key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  tlc <- execWriterT $ evalStateT (toLensClauses key gkey) mempty
  poc <- execWriterT $ evalStateT (pathsOfClauses key gkey) mempty
  -- clauses' <- runQ $ sequence clauses
  -- exp <- thePathExp gkey key ptyp clauses'
  when (not (null tlc)) $
       (runQ $ sequence
             [instanceD (pure []) [t|IsPath $(pure (bestType key)) $(pure (bestType gkey))|]
                [ tySynInstD ''PathType (tySynEqn [pure (bestType key), pure (bestType gkey)] (pure ptyp))
                , funD 'toLens tlc
                , funD 'pathsOf poc
                ]]) >>= tell
    where
      -- Send a single dec to our funky writer monad
      -- tell :: (DsMonad m, MonadWriter [Dec] m) => [DecQ] -> m ()
      -- tell dec = runQ (sequence dec) >>= tell
