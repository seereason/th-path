-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the Path types, and the universal path type.

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
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Maybe (fromJust, isJust)
import Data.Set.Extra as Set (map, Set)
import qualified Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (IsPathType(idPath))
import Language.Haskell.TH.Path.Decs.Common (asConQ, asName, asTypeQ, bestNames, bestPathTypeName, fieldLensNamePair, makePathCon,
                                             ModelType(ModelType), pathConNameOfField, PathType, pathTypeNameFromTypeName)
import Language.Haskell.TH.Path.Decs.IsPath (doIsPathNode)
import Language.Haskell.TH.Path.Decs.PathsOf (pathInstanceDecs)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
-- import Language.Haskell.TH.Path.Decs.PeekType (doPeekType)
import Language.Haskell.TH.Path.Graph (SelfPath, SinkType)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify), VarStrictType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.Prelude (pprint')
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, allPathStarts, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (fieldVertex, TypeInfo, typeVertex)
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, TypeGraphVertex, typeNames, vsimple)

pathDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
pathDecs = execWriterT $ allPathStarts >>= \ps -> Foldable.mapM_ doNode ps {->> doPeekType ps-}

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

      doSimplePath :: (ModelType, PathType, Set PathType) -> m ()
      doSimplePath (_tname, pname, syns') = do
        a <- runQ $ newName "a"
        runQ (dataD (return []) (asName pname) [PlainTV a] [normalC (asName pname) []] supers) >>= tell . (: [])
        doIsPathType pname
        doIsPathNode v
        -- Create path type synonyms for all the regular type synonyms: [d|type $psyn a = $(conT pname) a|]
        mapM_ (\psyn -> runQ (do a <- newName "a"
                                 tySynD (asName psyn) [PlainTV a]
                                        (appT (asTypeQ pname) (varT a))) >>= tell . (: [])) (Foldable.toList syns')

      -- viewPath [t|Text|] = data Path_Branding a = Path_Branding (Path_Text a)
      viewPath :: Type -> m ()
      viewPath styp = do
        let Just (pname, syns') = bestPathTypeName v
            -- gname = mkName ("Goal_" ++ nameBase pname)
        skey <- expandType styp >>= typeVertex
        a <- runQ $ newName "a"
        ptype <- pathType (varT a) skey
        -- A view type may have a type variable, which
        -- we need to replace with the goal type a.
        let ptype' = substitute (VarT a) ptype
        runQ (sequence (dataD (return []) (asName pname) [PlainTV a]
                              [ normalC (asName (makePathCon pname "View")) [strictType notStrict (pure ptype')]
                              , normalC (asName pname) []
                              ] supers
                         : List.map (\psyn -> tySynD (asName psyn) [PlainTV a] (appT (asTypeQ pname) (varT a))) (Foldable.toList syns'))) >>= tell
        doIsPathType pname
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
                        do doIsPathNode v
                           runQ (tySynD (asName pname) [PlainTV a] (return ptype)) >>= tell . (: [])) (pathTypeNames' v)
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
            [pcons] -> mapM_ (\pname -> do runQ (dataD (cxt []) (asName pname) [PlainTV a] (List.map return (pcons ++ [NormalC (asName pname) []])) supers) >>= tell . (: [])
                                           doIsPathType pname
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
                        ConT ftname -> runQ $ appT (asTypeQ (pathTypeNameFromTypeName (ModelType ftname))) (varT a)
                        -- It would be nice to use pathTypeCall (varT a) key' here, but
                        -- it can't infer the superclasses for (PathType Foo a) - Ord,
                        -- Read, Data, etc.
                        _ -> pathType (varT a) (view vsimple key')
             case ptype of
               TupleT 0 -> return []
               -- Given the list of clauses for a field's path type, create new
               -- constructor for the field in the parent record and alter the
               -- clauses to match expressions wrapped in this new constructor.
               _ -> (: []) <$> runQ (normalC (asName pcname) [strictType notStrict (return ptype)])

      supers :: [Name]
      supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]

      -- Given a vertex in the type graph, return the names of the
      -- corresponding path type and its synonyms.
      pathTypeNames' :: TypeGraphVertex v => v -> Set PathType
      pathTypeNames' = Set.map pathTypeNameFromTypeName . Set.map ModelType . typeNames

doIsPathType :: forall m. (ContextM m, MonadWriter [Dec] m) => PathType -> m ()
doIsPathType pname =
  runQ [d|instance IsPathType ($(asTypeQ pname) a) where idPath = $(asConQ pname)|] >>= tell

-- | Make lenses for a type with the names described by fieldLensNamePair, which is a little
-- different from the namer used in th-typegraph (for historical reasons I guess.)
makePathLens :: ContextM m => Name -> m [Dec]
makePathLens tname =
    -- runQ (runIO (putStrLn ("makePathLens " ++ nameBase tname))) >>
    qReify tname >>= execWriterT . doInfo
    where
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec (NewtypeD {}) = lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec (DataD {}) =    lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec _ = return ()

uncap :: String -> String
uncap (n : ame) = toLower n : ame
uncap "" = ""
