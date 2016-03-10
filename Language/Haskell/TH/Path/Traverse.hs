-- | Return the declarations that implement the IsPath instances, the
-- toLens methods, the PathType types, and the universal path type.

{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -ddump-minimal-imports #-}
module Language.Haskell.TH.Path.Traverse
    ( asP'
    , Control(..)
    , doType
    , substG
    , finishConc
    , finishPair
    , finishEither
    ) where

import Data.Default (Default(def))
import Data.Generics (Data, everywhere, mkT)
import Data.Map as Map (fromList, lookup, Map)
import Data.Maybe (isJust)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Path.Common
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.TypeGraph.Shape (Field)
import Language.Haskell.TH.TypeGraph.TypeGraph

data Control m conc alt r
    = Control
      { _doSimple :: m r
      , _doSelf :: m r
      , _doSyn :: Name -> Type -> m r
      , _doView :: Type -> m r -- Most of these could probably be pure
      , _doOrder :: Type -> Type -> m r
      , _doMap :: Type -> Type -> m r
      , _doList :: Type -> m r
      , _doPair :: Type -> Type -> m r
      , _doMaybe :: Type -> m r
      , _doEither :: Type -> Type -> m r
      , _doField :: Field -> Type -> m conc -- s is temporary
      , _doConcs :: PatQ -> [conc] -> m alt
      , _doAlts :: [alt] -> m r
      }

doType :: forall m conc alt r. (Quasi m, TypeGraphM m, Default r) => Control m conc alt r -> Type -> m r
doType control typ =
  do v <- tgvSimple typ
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case () of
       _ | selfPath -> _doSelf control
         | simplePath -> _doSimple control
         | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                _doView control viewtyp
       _ -> doType' typ []
    where
      doType' :: Type -> [Type] -> m r
      doType' (AppT t1 t2) tps = doType' t1 (t2 : tps)
      doType' (ConT tname) [ityp, vtyp] | tname == ''Order = _doOrder control ityp vtyp
      doType' (ConT tname) [ktyp, vtyp] | tname == ''Map = _doMap control ktyp vtyp
      doType' (TupleT 2) [ftyp, styp] = _doPair control ftyp styp
      doType' (ConT tname) [etyp] | tname == ''Maybe = _doMaybe control etyp
      doType' (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
#if 0
              _doEither control l r >>= \(lconc, rconc) -> doAlts [(conP 'Left [wildP], [lconc]),
                                                                   (conP 'Right [wildP], [rconc])]
#else
              _doEither control ltyp rtyp
#endif
      doType' (ConT tname) tps = doName tps tname
      doType' ListT [etyp] = _doList control etyp
      doType' _ _ = pure def

      doName :: [Type] -> Name -> m r
      doName tps tname = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m r
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo tps (FamilyI dec _insts) = doDec tps dec
      doInfo _ _ = pure def
      doDec :: [Type] -> Dec -> m r
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx tname binds cons _supers) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
            subst = substG bindings
        doCons subst tname cons
      doDec tps (TySynD _tname binds _typ)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (TySynD tname binds syntyp) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
            subst = substG bindings
        _doSyn control tname (subst syntyp)
      doDec _tps dec = error $ "Unexpected dec: " ++ pprint dec

      doCons :: (Type -> Type) -> Name -> [Con] -> m r
      doCons _subst _tname [] = error "No constructors"
      doCons subst tname cons = mapM (doCon subst tname) cons >>= _doAlts control

      doCon :: (Type -> Type) -> Name -> Con -> m alt
      doCon subst tname (ForallC _ _ con) = doCon subst tname con
      doCon subst tname (RecC cname vsts) = do
        flds <- mapM (doNamedField subst tname cname) (zip vsts [1..])
        _doConcs control (recP cname []) flds
      doCon _subst _tname (NormalC cname _sts) = do
#if 1
        _doConcs control (recP cname []) []
#else
        flds <- mapM (doAnonField bindings tname cname) (zip sts [1..])
        doAlts [(recP cname [], flds)]
#endif
      doCon _bindings _tname (InfixC _lhs cname _rhs) = do
#if 1
        _doConcs control (infixP wildP cname wildP) []
#else
        flds <- mapM (doAnonField bindings tname cname) (zip [lhs, rhs] [1..])
        c <- doAlts [(infixP wildP cname wildP, flds)]
        return [c]
#endif

      doNamedField :: (Type -> Type) -> Name -> Name -> ((Name, Strict, Type), Int) -> m conc
      doNamedField subst tname cname ((fname, _, ftype), _fpos) =
          _doField control (tname, cname, Right fname) (subst ftype)

      doAnonField :: (Type -> Type) -> Name -> Name -> ((Strict, Type), Int) -> m conc
      doAnonField subst tname cname ((_, ftype), fpos) =
          _doField control (tname, cname, Left fpos) (subst ftype)

      doAlts :: [(PatQ, [conc])] -> m r
      doAlts alts = mapM (uncurry (_doConcs control)) alts >>= _doAlts control

substG :: Data a => Map Name Type -> a -> a
substG bindings typ = everywhere (mkT (subst1 bindings)) typ

subst1 :: Map Name Type -> Type -> Type
subst1 bindings t@(VarT name) = maybe t id (Map.lookup name bindings)
subst1 _ t = t

-- | Change patterns of the form x@_ to x
asP' :: Name -> PatQ -> PatQ
asP' name patQ = do
  pat <- patQ
  case pat of
    WildP -> varP name
    AsP name' _ | name == name' -> patQ
    _ -> asP name patQ

finishConc :: Monad m => Control m conc alt r -> conc -> m r
finishConc control conc = _doConcs control wildP [conc] >>= \alt -> _doAlts control [alt]

finishPair :: Monad m => Control m conc alt r -> conc -> conc -> m r
finishPair control fconc sconc = _doConcs control wildP [fconc, sconc] >>= \alt -> _doAlts control [alt]

finishEither :: Monad m => Control m conc alt r -> conc -> conc -> m r
finishEither control lconc rconc =
    do lalt <- _doConcs control (conP 'Left [wildP]) [lconc]
       ralt <- _doConcs control (conP 'Right [wildP]) [rconc]
       _doAlts control [lalt, ralt]
