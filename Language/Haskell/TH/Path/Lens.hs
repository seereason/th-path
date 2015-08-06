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
    ( fieldLensName
    , fieldLensNameOld
    , makePathLens
    {- , nameMakeLens -} -- Imported by Appraisal.ReportMap
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Control.Lens (makeLensesFor)
import Control.Monad.Writer (execWriterT, tell)

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)

-- | Version of fieldLensName suitable for use as argument to
-- findNames below.
fieldLensName :: Name -> Name -> Name -> (String, String)
fieldLensName tname _cname fname = (nameBase fname, "lens_" ++ nameBase tname ++ "_" ++ nameBase fname)

-- | Make lenses for a type with the names described by fieldLensName, which is a little
-- different from the namer used in th-typegraph (for historical reasons I guess.)
makePathLens :: Quasi m => Name -> m [Dec]
makePathLens tname =
    qReify tname >>= execWriterT . doInfo
    where
      doInfo (TyConI dec) = doDec dec
      doInfo _ = return ()
      doDec (NewtypeD {}) = lensNamePairs fieldLensName tname >>= \pairs -> runQ (makeLensesFor pairs tname) >>= tell
      doDec (DataD {}) = lensNamePairs fieldLensName tname >>= \pairs -> runQ (makeLensesFor pairs tname) >>= tell
      doDec _ = return ()
