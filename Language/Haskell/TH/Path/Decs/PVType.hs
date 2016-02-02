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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.PVType where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable as Foldable
import Data.List as List (map)
import Data.Set.Extra as Set (Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Decs.Common (bestPathTypeName)
import Language.Haskell.TH.TypeGraph.Expand (unE)
import Language.Haskell.TH.TypeGraph.TypeGraph (pathKeys, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (TypeInfo)
import Language.Haskell.TH.TypeGraph.Vertex (bestName, etype, TGVSimple)

doPVType :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m, MonadWriter [Dec] m) => Set TGVSimple -> m ()
doPVType vs = do
  mapM_ doVert (Foldable.toList vs)
    where
      doVert :: TGVSimple -> m ()
      doVert v = do
        gs <- pathKeys v
        let cons = concat (List.map (doPair v) (Foldable.toList gs))
        case bestName v of
          Just vn -> do
                   let leName = mkName ("PV_" ++ nameBase vn)
                   runQ (dataD (return []) leName [] cons [''Eq, ''Show]) >>= tell . (: [])
          _ -> return ()
      doPair :: TGVSimple -> TGVSimple -> [ConQ]
      doPair v g =
          let Just (vp, _) = bestPathTypeName v in
          case (bestName v, bestName g) of
            (Just vn, Just gn) ->
                [normalC (mkName ("PV_" ++ nameBase vn ++ "_" ++ nameBase gn))
                         [(,) <$> notStrict <*> [t|$(conT vp) $(pure (view (etype . unE) g))|],
                          (,) <$> notStrict <*> pure (view (etype . unE) g)]]
            _ -> []
