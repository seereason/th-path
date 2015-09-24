{-# LANGUAGE FlexibleContexts, TemplateHaskell, TupleSections, CPP #-}

{- |
This is a modifed copy of Data.Lens.Template from
Joel Burget's data-lens-template package.  It changes
the signature of the namer function used in nameMakeLens
and adds nameMakeLenses.

This module provides an automatic Template Haskell
routine to scour data type definitions and generate
accessor objects for them automatically.
-}
module Language.Haskell.TH.Path.Lens
    ( pathLenses
    , fieldLensNameOld
    ) where

import Control.Applicative
import Control.Lens hiding (cons) -- (makeLenses, over, view)
import Control.Monad.Readers (MonadReaders, runReaderT)
import Control.Monad.States (MonadStates)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Char (toLower)
import Data.Foldable as Foldable
import Data.Map as Map (keys)
import Data.Set.Extra as Set (singleton)
-- import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Context (InstMap, reifyInstancesWithContext)
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Graph (pathGraphEdges, SinkType)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.TypeGraph.Expand (E(E), ExpandMap)
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.TypeGraph (allLensKeys, makeTypeGraph, TypeGraph)
import Language.Haskell.TH.TypeGraph.TypeInfo (makeTypeInfo)
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, typeNames)
import Prelude hiding (any, concat, concatMap, elem, foldr, mapM_, null, or)
import System.FilePath.Extra (compareSaveAndReturn, changeError)

pathLenses :: (DsMonad m, MonadStates ExpandMap m, MonadStates InstMap m) => m [Type] -> m [Dec]
pathLenses st = do
  r <- st >>= makeTypeInfo (\t -> maybe mempty singleton <$> runQ (viewInstanceType t)) >>= \ti -> runReaderT (pathGraphEdges >>= makeTypeGraph) ti
  decs <- execWriterT $ flip runReaderT r $
          do lmp <- Map.keys <$> allLensKeys
             Foldable.mapM_ pathLensDecs lmp
  _ <- runQ . runIO . compareSaveAndReturn changeError "GeneratedPathLenses.hs" $ decs
  runQ . runIO $ putStr ("\nPathLenses finished\n")
  return decs

pathLensDecs :: (DsMonad m, MonadReaders TypeGraph m, MonadWriter [Dec] m, MonadStates ExpandMap m, MonadStates InstMap m) =>
                TGVSimple -> m ()
pathLensDecs key = do
  simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [let (E typ) = view etype key in typ]
  case simplePath of
    False -> mapM makePathLens (Foldable.toList (typeNames key)) >>= {- t1 >>= -} tell . concat
    _ -> return ()
    -- where t1 x = trace (pprint' x) (return x)

-- | Make lenses for a type with the names described by fieldLensNamePair, which is a little
-- different from the namer used in th-typegraph (for historical reasons I guess.)
makePathLens :: Quasi m => Name -> m [Dec]
makePathLens tname =
    runQ (runIO (putStrLn ("makePathLens " ++ nameBase tname))) >>
    qReify tname >>= execWriterT . doInfo
    where
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec (NewtypeD {}) = lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) (uncap (nameBase tname)) pairs tname) >>= tell
      doDec (DataD {}) =    lensNamePairs fieldLensNamePair tname >>= \pairs -> runQ (makeClassyFor ("Has" ++ nameBase tname) ("lens_" ++ uncap (nameBase tname)) pairs tname) >>= tell
      doDec _ = return ()

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)

-- | Version of fieldLensName suitable for use as argument to
-- findNames below.
fieldLensNamePair :: Name -> Name -> Name -> (String, String)
fieldLensNamePair tname _cname fname = (nameBase fname, nameBase (fieldLensNameOld tname fname))

uncap :: String -> String
uncap (n : ame) = toLower n : ame
uncap "" = ""
