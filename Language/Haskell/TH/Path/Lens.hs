-- | The HasStack monad used in MIMO to construct lenses that look
-- deep into a record type.  However, it does not involve the Path
-- type mechanism, and is unaware of View instances and other things
-- that modify the type graph.  Lets see how it adapts.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.TH.Path.Lens
    ( makeTypeGraphLenses
    , lensNamePairs
    ) where

import Control.Category ((.))
import Control.Lens as Lens (_2, makeLensesFor, view)
import Control.Monad.Readers (MonadReaders)
import Control.Monad.States (MonadStates)
import Control.Monad.Writer (execWriterT, tell)
import Data.Map as Map (keys, Map)
import Data.Set (Set)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.Desugar (DsMonad)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.Path.Expand (E(E), ExpandMap)
import Language.Haskell.TH.Path.Stack (lensNamer)
import Language.Haskell.TH.Path.TypeGraph (allLensKeys, TypeGraph)
import Language.Haskell.TH.Path.TypeInfo (TypeInfo)
import Language.Haskell.TH.Path.Vertex (etype, TGVSimple, TGV)
import Prelude hiding ((.))

-- | Generate lenses to access the fields of the row types.  Like
-- Control.Lens.TH.makeLenses, but makes lenses for every field, and
-- instead of removing the prefix '_' to form the lens name it adds
-- the prefix "lens" and capitalizes the first letter of the field.
-- The only reason for this function is backwards compatibility,
-- makeLensesFor should be used instead.
makeTypeGraphLenses :: forall m. (DsMonad m, MonadReaders TypeInfo m, MonadReaders TypeGraph m, MonadStates ExpandMap m) => m [Dec]
makeTypeGraphLenses =
    (allLensKeys :: m (Map TGVSimple (Set TGV))) >>= execWriterT . mapM doType . map (view (_2 . etype)) . Map.keys
    where
      doType (E (ConT tname)) = qReify tname >>= doInfo
      doType _ = return ()
#if MIN_VERSION_template_haskell(2,11,0)
      doInfo (TyConI (NewtypeD _ tname _ _ _ _)) = lensNamePairs namer tname >>= \pairs -> runQ (makeLensesFor pairs tname) >>= tell
      doInfo (TyConI (DataD _ tname _ _ _ _)) = lensNamePairs namer tname >>= \pairs -> runQ (makeLensesFor pairs tname) >>= tell
#else
      doInfo (TyConI (NewtypeD _ tname _ _ _)) = lensNamePairs namer tname >>= \pairs -> runQ (makeLensesFor pairs tname) >>= tell
      doInfo (TyConI (DataD _ tname _ _ _)) = lensNamePairs namer tname >>= \pairs -> runQ (makeLensesFor pairs tname) >>= tell
#endif
      doInfo _ = return ()

namer :: Name -> Name -> Name -> (String, String)
namer _tname _cname fname = (nameBase fname, lensNamer (nameBase fname))

-- | Build the list of pairs used by makeLensesFor.
lensNamePairs :: Quasi m => (Name -> Name -> Name -> (String, String)) -> Name -> m [(String, String)]
lensNamePairs namefn tname =
    qReify tname >>= execWriterT . doInfo
    where
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
#if MIN_VERSION_template_haskell(2,11,0)
      doDec (NewtypeD _ _ _ _ con _) = doCon con
      doDec (DataD _ _ _ _ cons _) = mapM_ doCon cons
#else
      doDec (NewtypeD _ _ _ con _) = doCon con
      doDec (DataD _ _ _ cons _) = mapM_ doCon cons
#endif
      doDec (TySynD _ _ _) = return ()
      doDec _ = return ()
      doCon (ForallC _ _ con) = doCon con
      doCon (RecC cname flds) = mapM_ (doField cname) flds
      doCon (NormalC _ _) = return ()
      doCon (InfixC _ _ _) = return ()
      doField cname (fname, _, (ConT fTypeName)) = qReify fTypeName >>= doFieldTypeName cname fname
      doField cname (fname, _, _) = tell [namefn tname cname fname]
      doFieldTypeName _cname _fname (PrimTyConI _ _ _) = return ()
      doFieldTypeName cname fname _ = tell [namefn tname cname fname]