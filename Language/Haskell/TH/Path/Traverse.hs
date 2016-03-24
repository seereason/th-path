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
    , doNode
    , substG
    , finishConcs
    ) where

import Control.Lens (_2, view)
import Data.Generics (Data, everywhere, mkT)
import Data.Map as Map (fromList, lookup, Map)
import Data.Maybe (isJust)
import Data.Set.Extra as Set (delete, map, toList)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Path.Common
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (Order)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.TypeGraph.Expand (unE)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Language.Haskell.TH.TypeGraph.Shape (Field)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (etype, TGVSimple, typeNames)

data Control m conc alt r
    = Control
      { _doSimple :: m r
      , _doSelf :: m r
      , _doSyn :: Name -> Type -> m r
      , _doView :: TGVSimple -> m r
      , _doOrder :: Type -> TGVSimple -> m r
      , _doMap :: Type -> TGVSimple -> m r
      , _doList :: TGVSimple -> m r
      , _doPair :: TGVSimple -> TGVSimple -> m r
      , _doMaybe :: TGVSimple -> m r
      , _doEither :: TGVSimple -> TGVSimple -> m r
      , _doField :: Field -> Type -> m conc
      , _doConcs :: PatQ -> [conc] -> m alt
      , _doAlts :: [alt] -> m r
      , _doSyns :: r -> [r] -> m r
      }

doNode :: forall m conc alt r. (Quasi m, TypeGraphM m) => Control m conc alt r -> TGVSimple -> m r
doNode control v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case () of
       _ | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                _doView control =<< tgvSimple' viewtyp
         | selfPath -> _doSelf control
         | simplePath -> _doSimple control
       _ -> do
         let t0 = view (_2 . etype . unE) v
             ts = Set.toList $ Set.delete t0 $ Set.map ConT $ typeNames v
         r0 <- doType' t0 []
         rs <- mapM (flip doType' []) ts
         _doSyns control r0 rs
    where
      doType' :: Type -> [Type] -> m r
      doType' (AppT t1 t2) tps = doType' t1 (t2 : tps)
      doType' (ConT tname) [ityp, vtyp] | tname == ''Order = uncurry (_doOrder control) =<< ((,) <$> pure ityp <*> tgvSimple' vtyp)
      doType' (ConT tname) [ktyp, vtyp] | tname == ''Map = uncurry (_doMap control) =<< ((,) <$> pure ktyp <*> tgvSimple' vtyp)
      doType' (TupleT 2) [ftyp, styp] = uncurry (_doPair control) =<< ((,) <$> tgvSimple' ftyp <*> tgvSimple' styp)
      doType' (ConT tname) [etyp] | tname == ''Maybe = _doMaybe control =<< (tgvSimple' etyp)
      doType' (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              uncurry (_doEither control) =<< ((,) <$> tgvSimple' ltyp <*> tgvSimple' rtyp)
      doType' (ConT tname) tps = doName tps tname
      doType' ListT [etyp] = _doList control =<< (tgvSimple' etyp)
      doType' typ _ = error $ "doType: unexpected type: " ++ pprint1 typ ++ " (in " ++ pprint1 (asType v) ++ ")"

      doName :: [Type] -> Name -> m r
      doName tps tname = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m r
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo tps (FamilyI dec _insts) = doDec tps dec
      doInfo _ info = error $ "doType: unexpected info: " ++ show info
      doDec :: [Type] -> Dec -> m r
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx tname binds cons _supers) = do
        let bindings = Map.fromList (zip (Prelude.map asName binds) tps)
            subst = substG bindings
        doCons subst tname cons
      doDec tps (TySynD _tname binds _typ)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (TySynD tname binds syntyp) = do
        let bindings = Map.fromList (zip (Prelude.map asName binds) tps)
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

finishConcs :: Monad m => Control m conc alt r -> [(PatQ, [conc])] -> m r
finishConcs control concs = mapM (uncurry (_doConcs control)) concs >>= _doAlts control
