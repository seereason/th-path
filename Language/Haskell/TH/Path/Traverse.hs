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
    , doTGVSimple
    , substG
    ) where

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
import Language.Haskell.TH.TypeGraph.TypeGraph
import Language.Haskell.TH.TypeGraph.Vertex

data Control m conc
    = Control
      { _doView :: TGV -> m conc -- Most of these could probably be pure
      , _doOrder :: TGV -> m conc
      , _doMap :: TGV -> m conc
      , _doPair :: TGV -> TGV -> m (conc, conc)
      , _doMaybe :: TGV -> m conc
      , _doEither :: TGV -> TGV -> m (conc, conc)
      , _doField :: TGV -> m conc -- s is temporary
      , _doAlt :: (PatQ, [conc]) -> m ()
      }

doTGVSimple :: forall m conc. (Quasi m, TypeGraphM m) => Control m conc -> TGVSimple -> m ()
doTGVSimple control v =
  do selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case asType v of
       _ | selfPath -> pure ()
         | simplePath -> pure ()
         | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                w <- tgvSimple viewtyp >>= tgv Nothing
                _doView control w >>= \conc -> doAlts [(wildP, [conc])]
       typ -> doType typ []
    where
      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType (ConT tname) [_ityp, vtyp] | tname == ''Order = tgvSimple vtyp >>= tgv Nothing >>= _doOrder control >>= \conc -> doAlts [(wildP, [conc])]
      doType (ConT tname) [_ktyp, vtyp] | tname == ''Map = tgvSimple vtyp >>= tgv Nothing >>= _doMap control >>= \conc -> doAlts [(wildP, [conc])]
      doType (TupleT 2) [ftyp, styp] = do
        f <- tgvSimple ftyp >>= tgv Nothing -- (Just (''(,), '(,), Left 1))
        s <- tgvSimple styp >>= tgv Nothing -- (Just (''(,), '(,), Left 2))
        _doPair control f s >>= \(fconc, sconc) -> doAlts [(wildP, [fconc, sconc])]
      doType (ConT tname) [etyp] | tname == ''Maybe = tgvSimple etyp >>= tgv Nothing >>= _doMaybe control >>= \conc -> doAlts [(wildP, [conc])]
      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do l <- tgvSimple ltyp >>= tgv Nothing -- (Just (''Either, 'Left, Left 1))
                 r <- tgvSimple rtyp >>= tgv Nothing -- (Just (''Either, 'Right, Left 1))
                 _doEither control l r >>= \(lconc, rconc) -> doAlts [(conP 'Left [wildP], [lconc]),
                                                                      (conP 'Right [wildP], [rconc])]
      doType (ConT tname) tps = doName tps tname
      doType ListT [_etyp] = error "list" {- tell [clause [wildP] (normalB [|error "list"|]) []]-}
      doType _ _ = return ()

      doName :: [Type] -> Name -> m ()
      doName tps tname = qReify tname >>= doInfo tps
      doInfo :: [Type] -> Info -> m ()
      doInfo tps (TyConI dec) = doDec tps dec
      doInfo _ _ = pure ()
      doDec :: [Type] -> Dec -> m ()
      doDec tps (NewtypeD cx tname binds con supers) = doDec tps (DataD cx tname binds [con] supers)
      doDec tps (DataD _cx _tname binds _cons _supers)
          | length tps /= length binds =
              error $ "Arity mismatch: binds: " ++ show binds ++ ", types: " ++ show tps
      doDec tps (DataD _cx tname binds cons _supers) = do
        let bindings = Map.fromList (zip (map asName binds) tps)
            subst = substG bindings
        doCons subst tname cons
      doDec _tps dec = error $ "Unexpected dec: " ++ pprint dec

      doCons :: (Type -> Type) -> Name -> [Con] -> m ()
      doCons _subst _tname [] = error "No constructors"
      doCons subst tname cons = mapM_ (doCon subst tname) cons

      doCon :: (Type -> Type) -> Name -> Con -> m ()
      doCon subst tname (ForallC _ _ con) = doCon subst tname con
      doCon subst tname (RecC cname vsts) = do
        flds <- mapM (doNamedField subst tname cname) (zip vsts [1..])
        doAlts [(recP cname [], flds)]
      doCon _subst _tname (NormalC _cname _sts) = do
#if 1
        pure ()
#else
        flds <- mapM (doAnonField bindings tname cname) (zip sts [1..])
        doAlts [(recP cname [], flds)]
#endif
      doCon _bindings _tname (InfixC _lhs _cname _rhs) = do
#if 1
        pure ()
#else
        flds <- mapM (doAnonField bindings tname cname) (zip [lhs, rhs] [1..])
        c <- doAlts [(infixP wildP cname wildP, flds)]
        return [c]
#endif

      doNamedField :: (Type -> Type) -> Name -> Name -> ((Name, Strict, Type), Int) -> m conc
      doNamedField subst tname cname ((fname, _, ftype), _fpos) =
          do let ftype' = subst ftype
             f <- tgvSimple ftype' >>= tgv (Just (tname, cname, Right fname))
             _doField control f

      doAnonField :: (Type -> Type) -> Name -> Name -> ((Strict, Type), Int) -> m conc
      doAnonField subst tname cname ((_, ftype), fpos) =
          do let ftype' = subst ftype
             f <- tgvSimple ftype' >>= tgv (Just (tname, cname, Left fpos))
             _doField control f

      doAlts :: [(PatQ, [conc])] -> m ()
      doAlts = Prelude.mapM_ (_doAlt control)

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
