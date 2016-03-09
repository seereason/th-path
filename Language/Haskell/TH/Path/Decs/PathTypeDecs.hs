-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.PathTypeDecs
    ( pathTypeDecs
    ) where

import Control.Monad.Writer (MonadWriter, tell)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Maybe (isJust)
import Data.Set.Extra as Set (delete, map)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (allPathTypeNames, asConQ, asName, asType, asTypeQ, bestPathTypeName, ModelType(ModelType),
                                        makeFieldCon, makePathCon, makePathType, telld, tells)
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), SelfPath, SinkType)
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Traverse (Control(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, typeNames)

pathTypeDecControl :: (TypeGraphM m) => TGVSimple -> Control m () () ()
pathTypeDecControl v =
    Control
    { _doSimple = undefined
    , _doSelf = undefined
    , _doView = undefined
    , _doOrder = undefined
    , _doMap = undefined
    , _doPair = undefined
    , _doMaybe = undefined
    , _doEither = undefined
    , _doField = undefined
    , _doAlt = undefined
    , _doSyn = undefined
    , _doAlts = undefined
    }

pathTypeDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathTypeDecs v = do
  -- generate the data Path_* declarations
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
  viewTypeMaybe <- viewInstanceType (asType v)
  let pname = bestPathTypeName v
      psyns = Set.delete pname (allPathTypeNames v)
  -- e.g. type Path_EpochMilli a = Path_Int64 a
  runQ (mapM (\psyn -> newName "a" >>= \a -> tySynD (asName psyn) [PlainTV a] (appT (asTypeQ pname) (varT a))) (toList psyns)) >>= tell
  case () of
    _ | selfPath -> pure ()
      | simplePath -> doSimplePath v
      | isJust viewTypeMaybe ->
          do let Just viewType = viewTypeMaybe
             skey <- tgvSimple viewType
             a <- runQ $ newName "a"
             ptype <- pathType (varT a) skey
             -- data Path_MaybeImageFile a =
             --           Path_MaybeImageFile_View (Path_String a) | Path_MaybeImageFile
             --         deriving (Eq, Ord, Read, Show, Typeable, Data)
             tells [dataD (pure []) (asName pname) [PlainTV a]
                    [ normalC (asName (makePathCon pname "View")) [strictType notStrict (pure ptype)]
                    , normalC (asName pname) []
                    ] supers]
             telld [d|instance HasIdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|]
    _ -> doNames v

doNames :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
doNames v = mapM_ (\tname -> runQ (reify tname) >>= doInfo) (typeNames v)
    where
      doInfo (TyConI dec) =
          -- tell [d| z = $(litE (stringL ("doDec " ++ pprint1 dec))) |] >>
          doDec dec
      doInfo (FamilyI dec _insts) = doDec dec
      doInfo info = error $ "pathTypeDecs - unexpected Info: " ++ pprint1 info ++ "\n  " ++ show info
      doDec :: Dec -> m ()
      -- If we have a type synonym, we can create a path type synonym
      doDec (TySynD _ _ typ') =
          do a <- runQ $ newName "a"
             key' <- tgvSimple typ'
             ptype <- pathType (varT a) key'
             mapM (\pname -> runQ (tySynD (asName pname) [PlainTV a] (pure ptype))) (toList . Set.map makePathType . Set.map ModelType . typeNames $ v) >>= tell
      doDec (NewtypeD _ tname _ con _) = doDataD tname [con]
      doDec (DataD _ tname _ cons _) = doDataD tname cons
      doDec (FamilyD _flavour _name _tvbs _mkind) = return ()
      doDec dec = error $ "doName - unexpected Dec: " ++ pprint dec ++ "\n  " ++ show dec

      -- Is this a type declaration with fields we understand?
      isRecC :: Con -> Bool
      isRecC (RecC {}) = True
      isRecC _ = False

      doDataD :: Name -> [Con] -> m ()
      doDataD tname cons
          | any isRecC cons =
              do a <- runQ $ newName "a"
                 mapM (doCon a tname) cons >>= makeDecs a . concat
      doDataD _tname _cons = doSimplePath v

      -- Build the constructors of the path type
      doCon :: Name -> Name -> Con -> m [Con]
      doCon a tname (ForallC _ _ con) = doCon a tname con
      doCon _ _ (NormalC _ _) = return []
      doCon _ _ (InfixC _ _ _) = return []
      doCon a tname (RecC cname ts) = concat <$> mapM (doField a tname cname) ts

      -- Each field of the original type turns into zero or more (Con, Clause)
      -- pairs, each of which may or may not have a field representing the path type
      -- of some piece of the field value.  FIXME: This exact code is in PathTypes.hs
      doField :: Name -> Name -> Name -> VarStrictType -> m [Con]
      doField a tname cname (fname', _, ftype) =
          do skey' <- tgvSimple ftype
             key' <- tgv (Just (tname, cname, Right fname')) skey'
             let Just pcname = makeFieldCon key'
             ptype <- case ftype of
                        ConT ftname -> runQ $ appT (asTypeQ (makePathType (ModelType ftname))) (varT a)
                        -- It would be nice to use pathTypeCall (varT a) key' here, but
                        -- it can't infer the superclasses for (PathType Foo a) - Ord,
                        -- Read, Data, etc.
                        _ -> pathType (varT a) skey'
             case ptype of
               TupleT 0 -> return []
               -- Given the list of clauses for a field's path type, create new
               -- constructor for the field in the parent record and alter the
               -- clauses to match expressions wrapped in this new constructor.
               _ -> (: []) <$> runQ (normalC (asName pcname) [strictType notStrict (return ptype)])

      makeDecs :: Name -> [Con] -> m ()
      makeDecs _a [] = return ()
      makeDecs a pcons =
          mapM_ (\pname ->
                    -- data Path_Permissions a =
                    --      Path_Permissions_owner (Path_UserId a) |
                    --      Path_Permissions_writers (Path_UserIds a) |
                    --      Path_Permissions_readers (Path_UserIds a) |
                    --      Path_Permissions
                    --    deriving (Eq, Ord, Read, Show, Typeable, Data)
                    do tells [dataD (cxt []) (asName pname) [PlainTV a] (List.map return (pcons ++ [NormalC (asName pname) []])) supers]
                       telld [d|instance HasIdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|])
                (Set.map makePathType . Set.map ModelType . typeNames $ v)

doSimplePath :: forall m. (TypeGraphM m, MonadWriter [Dec] m) =>
                TGVSimple -> m ()
doSimplePath v = do
  let pname = bestPathTypeName v
  a <- runQ $ newName "a"
  -- e.g. data Path_Int a = Path_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
  tells [dataD (pure []) (asName pname) [PlainTV a] [normalC (asName pname) []] supers]
  telld [d|instance HasIdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|]

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]
