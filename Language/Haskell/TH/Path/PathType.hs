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
module Language.Haskell.TH.Path.PathType
    ( pathType
    , pathTypeCall
    ) where

import Control.Lens hiding (cons)
import Control.Monad.Reader (MonadReader, runReaderT)
import Data.Foldable
import Data.List as List (intercalate, map)
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Core (bestPathTypeName, pathTypeNameFromTypeName, PathType, Path_OMap, Path_List, Path_Map, Path_Pair, Path_Maybe, Path_Either)
import Language.Haskell.TH.Path.Monad (R, typeInfo, pathHints, reachableFrom, FoldPathControl(..), foldPath)
import Language.Haskell.TH.TypeGraph.Core (pprint')
import Language.Haskell.TH.TypeGraph.Expand (E(E), runExpanded)
import Language.Haskell.TH.TypeGraph.Monad (vertex)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex(..), etype)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)

-- | Given a type, generate the corresponding path type.
pathType :: (DsMonad m, MonadReader R m) =>
            TypeQ
         -> TypeGraphVertex -- | The type to convert to a path type
         -> m Type
pathType gtyp key =
  pathHints key >>= pathType'
    where
      pathType' hints = foldPath control key hints
        where
          -- Nest the definition of control so it can see this binding of hints,
          -- and call pathType' with a modified hint list.
          control =
              FoldPathControl
                { simplef = let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
                , substf = \_lns _styp ->
                    -- This is safe because hint types are now required to have a name
                    let Just (pname, _syns) = bestPathTypeName key in runQ [t|$(conT pname) $gtyp|]
                , pathyf = return $ runExpanded $ view etype key
                , namedf = \tname -> runQ $ [t|$(conT (pathTypeNameFromTypeName tname)) $gtyp|]
                , maybef = \etyp -> do
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_Maybe $(return epath)|]
                , listf = \etyp -> do
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_List $(return epath)|]
                , orderf = \ityp etyp -> do
                    ipath <- vert ityp >>= pathType gtyp
                    epath <- vert etyp >>= pathType gtyp
                    runQ [t|Path_OMap $(return ipath) $(return epath)|]
                , mapf = \ktyp vtyp -> do
                    kpath <- vert ktyp >>= pathType gtyp
                    vpath <- vert vtyp >>= pathType gtyp
                    runQ [t| Path_Map $(return kpath) $(return vpath)|]
                , pairf = \ftyp styp -> do
                    fpath <- vert ftyp >>= pathType gtyp
                    spath <- vert styp >>= pathType gtyp
                    runQ [t| Path_Pair $(return fpath) $(return spath) |]
                , eitherf = \ltyp rtyp -> do
                    lpath <- vert ltyp >>= pathType gtyp
                    rpath <- vert rtyp >>= pathType gtyp
                    runQ [t| Path_Either $(return lpath) $(return rpath)|]
                , otherf = do
                    ks <- reachableFrom key
                    error $ "makePathType otherf: " ++ pprint' key ++ "\n" ++
                            intercalate "\n  " ("reachable from:" : List.map pprint' (toList ks))
                }

      vert typ = view typeInfo >>= runReaderT (vertex Nothing (E typ))

-- | Call the type function PathType.
pathTypeCall :: (DsMonad m, MonadReader R m) =>
                TypeQ           -- | The goal type - possibly a type variable
             -> TypeGraphVertex -- | The type to convert to a path type
             -> m Type
pathTypeCall gtyp key = runQ [t|PathType $(let (E typ) = view etype key in return typ) $gtyp|]
