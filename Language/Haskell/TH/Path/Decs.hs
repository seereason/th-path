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
    ( allDecs
    ) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Maybe (fromJust, isJust)
import Data.Set.Extra as Set (map)
import qualified Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (IsPathType(idPath))
import Language.Haskell.TH.Path.Decs.Common (asConQ, asName, asTypeQ, bestPathTypeName, fieldLensNamePair,
                                             makeFieldCon, makePathCon, makePathType, ModelType(ModelType), PathType)
import Language.Haskell.TH.Path.Decs.IsPath (peekDecs)
import Language.Haskell.TH.Path.Decs.PathsOf (pathDecs)
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
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, typeNames, vsimple)

allDecs :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => m [Dec]
allDecs = execWriterT $ allPathStarts >>= mapM_ doNode

doNode :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNode v = do
  peekDecs v
  -- generate the IsPath instance declarations
  pathKeys v >>= Set.mapM_ (pathDecs v)
  -- generate the lenses
  mapM makePathLens (toList (typeNames v)) >>= tell . concat
  -- generate the data Path_* declarations
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [let (E typ) = view etype v in typ]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype v in typ]
  viewType <- viewInstanceType (view etype v)
  let Just (pname, psyns) = bestPathTypeName v
  a <- runQ $ newName "a"
  -- e.g. type Path_EpochMilli a = Path_Int64 a
  runQ (mapM (\psyn -> tySynD (asName psyn) [PlainTV a] (appT (asTypeQ pname) (varT a))) (toList psyns)) >>= tell
  case () of
    _ | selfPath -> return ()
      | simplePath -> doSimplePath pname
      | isJust viewType -> viewPath v (fromJust viewType)
      | otherwise -> doNames v

doSimplePath :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) =>
                PathType -> m ()
doSimplePath pname = do
  a <- runQ $ newName "a"
  -- e.g. data Path_Int a = Path_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
  runQ (dataD (pure []) (asName pname) [PlainTV a] [normalC (asName pname) []] supers) >>= tell . (: [])
  runQ [d|instance IsPathType ($(asTypeQ pname) a) where idPath = $(asConQ pname)|] >>= tell

viewPath :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> Type -> m ()
viewPath v styp = do
  let Just (pname, _psyns) = bestPathTypeName v
  skey <- expandType styp >>= typeVertex
  a <- runQ $ newName "a"
  ptype <- pathType (varT a) skey
  -- data Path_MaybeImageFile a =
  --           Path_MaybeImageFile_View (Path_String a) | Path_MaybeImageFile
  --         deriving (Eq, Ord, Read, Show, Typeable, Data)
  runQ (dataD (pure []) (asName pname) [PlainTV a]
          [ normalC (asName (makePathCon pname "View")) [strictType notStrict (pure ptype)]
          , normalC (asName pname) []
          ] supers) >>= tell . (: [])
  runQ [d|instance IsPathType ($(asTypeQ pname) a) where idPath = $(asConQ pname)|] >>= tell

doNames :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNames v = mapM_ (\tname -> runQ (reify tname) >>= doInfo) (typeNames v)
    where
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
             mapM (\pname -> runQ (tySynD (asName pname) [PlainTV a] (pure ptype))) (toList . Set.map makePathType . Set.map ModelType . typeNames $ v) >>= tell
      doDec (NewtypeD _ tname _ con _) = doDataD tname [con]
      doDec (DataD _ tname _ cons _) = doDataD tname cons
      doDec (FamilyD _flavour _name _tvbs _mkind) = return ()
      doDec dec = error $ "doName - unexpected Dec: " ++ pprint dec ++ "\n  " ++ show dec

      doDataD :: Name -> [Con] -> m ()
      doDataD tname cons =
          do a <- runQ $ newName "a"
             mapM (doCon a tname) cons >>= makeDecs a . concat

      makeDecs :: Name -> [Con] -> m ()
      makeDecs _a [] = return ()
      makeDecs a pcons =
                do mapM_ (\pname ->
                              -- data Path_Permissions a =
                              --      Path_Permissions_owner (Path_UserId a) |
                              --      Path_Permissions_writers (Path_UserIds a) |
                              --      Path_Permissions_readers (Path_UserIds a) |
                              --      Path_Permissions
                              --    deriving (Eq, Ord, Read, Show, Typeable, Data)
                              do runQ (dataD (cxt []) (asName pname) [PlainTV a] (List.map return (pcons ++ [NormalC (asName pname) []])) supers) >>= tell . (: []))
                         (Set.map makePathType . Set.map ModelType . typeNames $ v)
                   mapM_ (\pname -> runQ [d|instance IsPathType ($(asTypeQ pname) a) where idPath = $(asConQ pname)|] >>= tell) (Set.map makePathType . Set.map ModelType . typeNames $ v)

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
             let Just pcname = makeFieldCon key'
             ptype <- case ftype of
                        ConT ftname -> runQ $ appT (asTypeQ (makePathType (ModelType ftname))) (varT a)
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
