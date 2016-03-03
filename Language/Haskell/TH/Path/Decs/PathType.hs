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
module Language.Haskell.TH.Path.Decs.PathType
    ( pathType
    , pathTypeDecs
    ) where

import Control.Monad.Writer (MonadWriter, tell)
import Data.Data (Data, Typeable)
import Data.Foldable as Foldable
import Data.List as List (intercalate, map)
import Data.Map as Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Set.Extra as Set (delete, map)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (allPathTypeNames, asConQ, asName, asType, asTypeQ, bestPathTypeName, ModelType(ModelType),
                                        makeFieldCon, makePathCon, makePathType, telld, tells)
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), SelfPath, SinkType,
                                      Path_List, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..))
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (VarStrictType)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.TypeGraph (HasTGVSimple(asTGVSimple), reachableFromSimple, tgv, tgvSimple)
import Language.Haskell.TH.TypeGraph.Vertex (TGVSimple, typeNames)

-- | Given a type, compute the corresponding path type.
pathType :: forall m. TypeGraphM m =>
            TypeQ
         -> TGVSimple -- ^ The type to convert to a path type
         -> m Type
pathType gtyp key = do
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType key]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType key]
  viewType <- viewInstanceType (asType key)
  case asType (asTGVSimple key) of
    _ | selfPath -> return $ asType (asTGVSimple key)
      | simplePath -> runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
      | isJust viewType -> runQ [t|$(asTypeQ (bestPathTypeName key)) $gtyp|]
    ConT tname ->
        runQ $ [t|$(asTypeQ (makePathType (ModelType tname))) $gtyp|]
    AppT (AppT mtyp ityp) etyp
        | mtyp == ConT ''Order ->
            do ipath <- tgvSimple ityp >>= pathType gtyp
               epath <- tgvSimple etyp >>= pathType gtyp
               runQ [t|Path_OMap $(return ipath) $(return epath)|]
    AppT ListT etyp ->
        do epath <- tgvSimple etyp >>= pathType gtyp
           runQ [t|Path_List $(return epath)|]
    AppT (AppT t3 ktyp) vtyp
        | t3 == ConT ''Map ->
            do kpath <- tgvSimple ktyp >>= pathType gtyp
               vpath <- tgvSimple vtyp >>= pathType gtyp
               runQ [t| Path_Map $(return kpath) $(return vpath)|]
    AppT (AppT (TupleT 2) ftyp) styp ->
        do fpath <- tgvSimple ftyp >>= pathType gtyp
           spath <- tgvSimple styp >>= pathType gtyp
           runQ [t| Path_Pair $(return fpath) $(return spath) |]
    AppT t1 vtyp
        | t1 == ConT ''Maybe ->
            do epath <- tgvSimple vtyp >>= pathType gtyp
               runQ [t|Path_Maybe $(return epath)|]
    AppT (AppT t3 ltyp) rtyp
        | t3 == ConT ''Either ->
            do lpath <- tgvSimple ltyp >>= pathType gtyp
               rpath <- tgvSimple rtyp >>= pathType gtyp
               runQ [t| Path_Either $(return lpath) $(return rpath)|]
    _ -> do ks <- reachableFromSimple key
            error $ "pathType otherf: " ++ pprint1 key ++ "\n" ++
                    intercalate "\n  " ("reachable from:" : List.map pprint1 (Foldable.toList ks))


pathTypeDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathTypeDecs v = do
  -- generate the data Path_* declarations
  selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
  viewType <- viewInstanceType (asType v)
  let pname = bestPathTypeName v
      psyns = Set.delete pname (allPathTypeNames v)
  a <- runQ $ newName "a"
  -- e.g. type Path_EpochMilli a = Path_Int64 a
  runQ (mapM (\psyn -> tySynD (asName psyn) [PlainTV a] (appT (asTypeQ pname) (varT a))) (toList psyns)) >>= tell
  case () of
    _ | selfPath -> return ()
      | simplePath -> doSimplePath v
      | isJust viewType -> viewPath v (fromJust viewType)
      | otherwise -> doNames v

doSimplePath :: forall m. (TypeGraphM m, MonadWriter [Dec] m) =>
                TGVSimple -> m ()
doSimplePath v = do
  let pname = bestPathTypeName v
  a <- runQ $ newName "a"
  -- e.g. data Path_Int a = Path_Int deriving (Eq, Ord, Read, Show, Typeable, Data)
  tells [dataD (pure []) (asName pname) [PlainTV a] [normalC (asName pname) []] supers]
  telld [d|instance HasIdPath ($(asTypeQ pname) a) where idPath = $(asConQ pname)|]

viewPath :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> Type -> m ()
viewPath v styp = do
  let pname = bestPathTypeName v
  skey <- tgvSimple styp
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

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]
