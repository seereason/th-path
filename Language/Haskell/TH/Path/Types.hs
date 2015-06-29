{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Language.Haskell.TH.Path.Types
    ( pathTypes
    ) where

import Control.Applicative
import Control.Lens hiding (cons) -- (makeLenses, over, view)
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.RWS (evalRWST)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable
import Data.Generics (Data, Typeable)
import Data.List as List (map)
import Data.Set as Set (empty, map, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (bestPathTypeName, IdPath(idPath), pathConNameOfField, pathTypeNameFromTypeName, pathTypeNames')
import Language.Haskell.TH.Path.Graph (makeTypeGraph)
import Language.Haskell.TH.Path.Monad (allPathKeys, foldPath, FoldPathControl(..), R, typeInfo)
import Language.Haskell.TH.Path.PathType (pathType)
import Language.Haskell.TH.Syntax as TH (Quasi, VarStrictType)
import Language.Haskell.TH.TypeGraph (pprint', expandType, simpleVertex, vertex, TypeGraphVertex, typeNames)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

-- | Construct a graph of all types reachable from the types in the
-- argument, and construct the corresponding path types.
pathTypes :: Q [Type] -> Q [Dec]
pathTypes st = do
  r <- makeTypeGraph st
  (_, decss) <- evalRWST (allPathKeys >>= mapM pathTypeDecs . toList . Set.map simpleVertex) r Set.empty
  runIO . compareSaveAndReturn changeError "GeneratedPathTypes.hs" $ concat decss

-- | Given a type, generate the corresponding path type declarations
pathTypeDecs :: forall m. (DsMonad m, MonadReader R m, MonadWriter [[Dec]] m) => TypeGraphVertex -> m ()
pathTypeDecs key =
  pathTypeDecs'
    where
      pathTypeDecs' = foldPath control key
        where
          control =
            FoldPathControl
              { simplef = maybe (error $ "pathTypeDecs: simple path type has no name: " ++ pprint' key) (uncurry simplePath) (bestPathTypeName key)
              , substf = \_lns styp -> viewPath styp -- maybe (return ()) (uncurry simplePath) (bestPathTypeName key)
              , pathyf = return ()
              , namedf = \_tname -> doNames
              , maybef = \_etyp -> doNames
              , listf = \_etyp -> doNames
              , orderf = \_ityp _etyp -> doNames
              , mapf = \_ktyp _vtyp -> doNames
              , pairf = \_ftyp _styp -> doNames
              , eitherf = \_ltyp _rtyp -> doNames
              , otherf = doNames
              }

      doNames = mapM_ (\tname -> runQ (reify tname) >>= doInfo) (typeNames key)

      simplePath :: Name -> Set Name -> m ()
      simplePath pname syns = do
        runQ (newName "a" >>= \a -> dataD (return []) pname [PlainTV a] [normalC pname []] supers) >>= tell . (: []) . (: [])
        runQ [d|instance IdPath ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell . (: [])
        mapM_ (\psyn -> runQ (newName "a" >>= \a -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) >>= tell . (: []) . (: [])) (toList syns)

      -- viewPath [t|Text|] = data Path_Branding a = Path_Branding (Path_Text a)
      viewPath :: Type -> m ()
      viewPath styp = do
        let Just (pname, syns) = bestPathTypeName key
            -- gname = mkName ("Goal_" ++ nameBase pname)
        skey <- view typeInfo >>= runReaderT (expandType styp >>= vertex Nothing)
        a <- runQ $ newName "a"
        ptype <- pathType (varT a) skey
        -- A view type may have a type variable, which
        -- we need to replace with the goal type a.
        let ptype' = substitute (VarT a) ptype
        runQ (sequence (dataD (return []) pname [PlainTV a]
                              [ normalC (mkName (nameBase pname ++ "_View")) [strictType notStrict (pure ptype')]
                              , normalC (mkName (nameBase pname)) []
                              ] supers
                         : List.map (\psyn -> tySynD psyn [PlainTV a] (appT (conT pname) (varT a))) (toList syns))) >>= tell . (: [])
        runQ [d|instance IdPath ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell . (: [])

      substitute :: Type -> Type -> Type
      substitute gtype (AppT x (VarT _)) = (AppT x gtype)
      substitute gtype (AppT a b) = AppT (substitute gtype a) (substitute gtype b)
      substitute gtype (VarT _) = gtype
      substitute _ x = x

      doInfo (TyConI dec) =
          -- tell [ [d| z = $(litE (stringL ("doDec " ++ pprint' dec))) |] ] >>
          doDec dec
      doInfo (FamilyI dec _insts) = doDec dec
      doInfo info = error $ "pathTypeDecs - unexpected Info: " ++ pprint' info ++ "\n  " ++ show info
      doDec :: Dec -> m ()
      -- If we have a type synonym, we can create a path type synonym
      doDec (TySynD _ _ typ') =
          do a <- runQ $ newName "a"
             key' <- view typeInfo >>= runReaderT (expandType typ' >>= vertex Nothing)
             ptype <- pathType (varT a) key'
             mapM_ (\pname -> tell1 (tySynD pname [PlainTV a] (return ptype))) (pathTypeNames' key)
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
            [pcons] -> mapM_ (\pname -> do tell1 (dataD (cxt []) pname [PlainTV a] (List.map return (pcons ++ [NormalC pname []])) supers)
                                           runQ [d|instance IdPath ($(conT pname) a) where idPath = $(conE (mkName (nameBase pname)))|] >>= tell . (: [])
                             ) (pathTypeNames' key)
            [] | length pconss > 1 -> return () -- enum
            [] -> return ()
                  -- FIXME - if there are paths from several different
                  -- constructors we want an extra path type to
                  -- represent the values for each constructor.
            pconss' ->
                makeDecs a [concat pconss']

      doCon :: (DsMonad m, MonadReader R m) => Name -> Name -> Con -> m [Con]
      doCon a tname (ForallC _ _ con) = doCon a tname con
      doCon _ _ (NormalC _ _) = return []
      doCon _ _ (InfixC _ _ _) = return []
      doCon a tname (RecC cname ts) = concat <$> mapM (doField a tname cname) ts

      -- Each field of the original type turns into zero or more (Con, Clause)
      -- pairs, each of which may or may not have a field representing the path type
      -- of some piece of the field value.  FIXME: This exact code is in PathTypes.hs
      doField :: (DsMonad m, MonadReader R m) => Name -> Name -> Name -> VarStrictType -> m [Con]
      doField a tname cname (fname', _, ftype) =
          do key' <- view typeInfo >>= runReaderT (expandType ftype >>= vertex (Just (tname, cname, Right fname')))
             let Just pcname = pathConNameOfField key'
             ptype <- case ftype of
                        ConT ftname -> runQ $ appT (conT (pathTypeNameFromTypeName ftname)) (varT a)
                        -- It would be nice to use pathTypeCall (varT a) key' here, but
                        -- it can't infer the superclasses for (PathType Foo a) - Ord,
                        -- Read, Data, etc.
                        _ -> pathType (varT a) key'
             case ptype of
               TupleT 0 -> return []
               -- Given the list of clauses for a field's path type, create new
               -- constructor for the field in the parent record and alter the
               -- clauses to match expressions wrapped in this new constructor.
               _ -> (: []) <$> runQ (normalC pcname [strictType notStrict (return ptype)])

supers :: [Name]
supers = [''Eq, ''Ord, ''Read, ''Show, ''Typeable, ''Data]

tell1 :: (Quasi m, MonadWriter [[Dec]] m) => DecQ -> m ()
tell1 dec = runQ (sequence (List.map sequence [[dec]])) >>= tell
