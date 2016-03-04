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
module Language.Haskell.TH.Path.Decs.HasPaths (pathDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Generics (Data, everywhere, mkT)
import Data.List as List (concatMap, map)
import Data.Map as Map (fromList, lookup, Map, toList)
import Data.Maybe (isJust)
import Data.Set.Extra as Set (mapM_, member)
import Language.Haskell.TH
import Language.Haskell.TH.Context (reifyInstancesWithContext)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (asConQ, asType, asTypeQ, HasName(asName), makePathCon, makePathType, mconcatQ, ModelType(ModelType), tells)
import Language.Haskell.TH.Path.Core (HasIdPath(idPath), HasPaths(..), ToLens(..), SelfPath, SinkType, Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Instances ()
import Language.Haskell.TH.Path.Order (Order, Path_OMap(..), toPairs)
import Language.Haskell.TH.Path.Traverse (Control(..), doTGVSimple)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.TypeGraph (allPathKeys, pathKeys, tgv, tgvSimple, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (field, TGVSimple, TypeGraphVertex(bestType))

newtype Subst a = Subst {unSubst :: a}

pathDecs :: (TypeGraphM m, MonadWriter [Dec] m) => TGVSimple -> m ()
pathDecs v =
    pathKeys v >>= Set.mapM_ (pathDecs' v)

-- | For a given pair of TGVSimples, compute the declaration of the
-- corresponding Path instance.  Each clause matches some possible value
-- of the path type, and returns a lens that extracts the value the
-- path type value specifies.
pathDecs' :: (TypeGraphM m, MonadWriter [Dec] m) =>
             TGVSimple -> TGVSimple -> m ()
pathDecs' v gkey = do
  ptyp <- pathType (pure (bestType gkey)) v
  x <- runQ (newName "s")
  g <- runQ (newName "g")
  poc <- case v == gkey of
           True -> pure [clause [wildP, wildP] (normalB [| [idPath] |]) []]
           False -> execWriterT (pathsOfClauses (hasPathControl v gkey g) v gkey x g)
  when (not (null poc))
       (tells [ instanceD (pure []) [t|HasPaths $(pure (bestType v)) $(pure (bestType gkey))|]
                [ tySynInstD ''Path (tySynEqn [pure (bestType v), pure (bestType gkey)] (pure ptyp))
                , funD 'pathsOf poc
                ]])

hasPathControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TGVSimple -> TGVSimple -> Name -> Control m (Subst Type, ExpQ) ExpQ
hasPathControl v gkey g =
    Control { _doView =
                  \w -> do
                    s <- runQ $ newName "s"
                    let pcname = makePathCon (makePathType (ModelType (asName v))) "View"
                    pure [(varP s, [(Subst (asType w),
                                     [|map (\a' -> ($(asConQ pcname) {-:: Path $(asTypeQ w) $(asTypeQ gkey) -> Path $(asTypeQ v) $(asTypeQ gkey)-}, a'))
                                           (toListOf (toLens ($(asConQ pcname) (idPath :: Path $(asTypeQ w) $(asTypeQ w)))) $(varE s)) |])])]
            , _doOrder =
                \w -> do
                  s <- runQ $ newName "s"
                  pure [(varP s, [(Subst (asType w), [| map (\(i, x) -> (Path_At i, x)) (toPairs $(varE s)) |])])]
            , _doMap =
                \w -> do
                  s <- runQ $ newName "s"
                  pure [(varP s, [(Subst (asType w), [| map (\(i, x) -> (Path_Look i, x)) (Map.toList $(varE s)) |])])]
            , _doPair =
                \f s -> do
                  s' <- runQ $ newName "s"
                  pure [(varP s', [(Subst (asType f), [| [(Path_First, fst $(varE s'))] |]),
                                   (Subst (asType s), [| [(Path_Second, snd $(varE s'))] |])])]
            , _doMaybe =
                \w -> do
                  s <- runQ $ newName "s"
                  pure [(varP s, [(Subst (asType w), [| case $(varE s) of Nothing -> []; Just a' -> [(Path_Just, a')]|])])]
            , _doEither =
                \l r ->
                    do s <- runQ $ newName "s"
                       pure [(asP s (conP 'Left [wildP]), [(Subst (asType l), [| case $(varE s) of Left a' -> [(Path_Left, a')]; Right _ -> []|])]),
                             (asP s (conP 'Right [wildP]), [(Subst (asType r), [| case $(varE s) of Left _ -> []; Right a' -> [(Path_Right, a')]|])])]
            , _doField =
                \s f ->
                    case view (_2 . field) f of
                      Just (_tname, _cname, Right fname) ->
                          pure (Subst (asType f), [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE s)))] |])
                      Just (_tname, _cname, Left fpos) ->
                          pure (Subst (asType f), [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show fpos))),
                                               $(do p <- newName "p"
                                                    lamE (replicate (fpos-1) wildP ++ [varP p] ++ replicate (2-fpos) wildP) (varE p)) $(varE s))] |])
                      Nothing -> error "Not a field"
            , _doConc =
                \s (Subst typ, asList) ->
                    do isPath <- testIsPath typ gkey
                       case isPath of
                         False -> pure [| [] |]
                         True -> pure [| List.concatMap
                                           (\(p, a') -> (List.map p (pathsOf (a' :: $(pure typ)) $(varE g) {-:: [Path $(pure typ) $(asTypeQ gkey)]-})) {-:: [Path $(pure styp) $(asTypeQ gkey)]-})
                                           ($asList {-:: [(Path $(pure typ) $(asTypeQ gkey) -> Path $(pure styp) $(asTypeQ gkey), $(pure typ))]-}) |]
            , _doAlt =
                \s xpat exps ->
                    tell [clause [xpat, varP g] (normalB (mconcatQ exps)) []]
            }

-- | Build an expression whose value is a list of paths from type S to
-- type A
pathsOfClauses :: forall m alt conc. (TypeGraphM m, MonadWriter [ClauseQ] m, conc ~ (Subst Type, ExpQ), alt ~ (PatQ, [conc])) =>
                  Control m (Subst Type, ExpQ) ExpQ
               -> TGVSimple -- ^ the type whose clauses we are generating
               -> TGVSimple -- ^ the goal type key
               -> Name -- ^ s
               -> Name -- ^ g
               -> m ()
pathsOfClauses control v gkey x g =
  do -- the corresponding path type - first type parameter of ToLens
     -- ptyp <- pathType (pure (bestType gkey)) v
     selfPath <- (not . null) <$> reifyInstancesWithContext ''SelfPath [asType v]
     simplePath <- (not . null) <$> reifyInstancesWithContext ''SinkType [asType v]
     viewTypeMaybe <- viewInstanceType (asType v)
     case asType v of
       _ | v == gkey -> tell [clause [wildP, wildP] (normalB [| [idPath] |]) []]
         | selfPath -> pure ()
         | simplePath -> pure ()
         | isJust viewTypeMaybe ->
             do let Just viewtyp = viewTypeMaybe
                w <- tgvSimple viewtyp >>= tgv Nothing
                _doView control w >>= Prelude.mapM_ doAlt
       typ -> doType typ []
    where
      doType :: Type -> [Type] -> m ()
      doType (AppT t1 t2) tps = doType t1 (t2 : tps)
      doType (ConT tname) [_ityp, vtyp]
          | tname == ''Order =
              do w <- tgvSimple vtyp >>= tgv Nothing
                 -- Return a path for each element of an order, assuming
                 -- there is a path from the element type to the goal.
                 _doOrder control w >>= Prelude.mapM_ doAlt

      doType (ConT tname) [_ktyp, vtyp]
          | tname == ''Map =
              do w <- tgvSimple vtyp >>= tgv Nothing
                 _doMap control w >>= Prelude.mapM_ doAlt
      doType (TupleT 2) [ftyp, styp] = do
        f <- tgvSimple ftyp >>= tgv Nothing
        s <- tgvSimple styp >>= tgv Nothing
        _doPair control f s >>= Prelude.mapM_ doAlt
      doType (ConT tname) [etyp]
          | tname == ''Maybe =
              do w <- tgvSimple etyp >>= tgv Nothing
                 _doMaybe control w >>= Prelude.mapM_ doAlt
      doType (ConT tname) [ltyp, rtyp]
          | tname == ''Either =
              do l <- tgvSimple ltyp >>= tgv Nothing
                 r <- tgvSimple rtyp >>= tgv Nothing
                 _doEither control l r >>= Prelude.mapM_ doAlt
      doType (ConT tname) tps = doName tname tps

      doType _ _ = error $ "pathsOfExpr - unexpected type" {-++ pprint (asTGVSimple v)-}

      doName :: Name -> [Type] -> m ()
      doName tname tps = qReify tname >>= doInfo tps
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
      doDec _ dec = error $ "Unexpected Dec: " ++ pprint dec

      doCons :: (Type -> Subst Type) -> Name -> [Con] -> m ()
      doCons _subst _tname [] = error "No constructors"
      doCons subst _tname cons = Prelude.mapM_ (doCon subst) cons

      doCon :: (Type -> Subst Type) -> Con -> m ()
      doCon subst (ForallC _binds _cx con) = doCon subst con -- Should probably do something here
      doCon subst (InfixC lhs cname rhs) = doInfixC subst cname lhs rhs >>= Prelude.mapM_ doAlt
      doCon subst (NormalC cname binds) = doNormalC subst cname binds >>= Prelude.mapM_ doAlt
      doCon subst (RecC cname vbinds) = doRecC subst cname vbinds >>= Prelude.mapM_ doAlt
      doAlt :: alt -> m ()
      doAlt (spat, pairs) =
          mapM doConc pairs >>= \exps -> tell [clause [spat, varP g] (normalB (mconcatQ exps)) []]

      doConc :: conc
             -> m ExpQ
      doConc (Subst atyp, asList) = do
        isPath <- testIsPath atyp gkey
        case isPath of
          False -> pure [| [] |]
          True -> pure [| List.concatMap (\(p, a') -> (List.map p (pathsOf (a' :: $(pure atyp)) $(varE g) {-:: [Path $(pure atyp) $(asTypeQ gkey)]-})) {-:: [Path $(pure styp) $(asTypeQ gkey)]-})
                                         ($asList {-:: [(Path $(pure atyp) $(asTypeQ gkey) -> Path $(pure styp) $(asTypeQ gkey), $(pure atyp))]-}) |]

      doNormalC :: (Type -> Subst Type) -> Name -> [(Strict, Type)] -> m [(PatQ, [conc])]
      doNormalC subst cname binds = do
        pure $ [(asP x (recP cname []),
                 map (\((_, ftype), pos) ->
                          (subst ftype,
                                     [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show pos))),
                                          ($(do p <- newName "p"
                                                lamE (replicate (pos-1) wildP ++ [varP p] ++ replicate (length binds - pos) wildP) (varE p)) $(varE x)))] |]))
                     (zip binds ([1..] :: [Int])))]

      doRecC :: (Type -> Subst Type) -> Name -> [(Name, Strict, Type)] -> m [(PatQ, [conc])]
      doRecC subst cname vbinds = do
        concs <- mapM (doNamedField subst) vbinds
        pure $ [(asP x (recP cname []), concs)]

      doNamedField :: (Type -> Subst Type) -> (Name, Strict, Type) -> m conc
      doNamedField subst (fname, _, ftype') = do
        let ftype = subst ftype'
        pure (ftype, [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (nameBase fname))), ($(varE fname) $(varE x)))] |])

      doInfixC :: (Type -> Subst Type) -> Name -> (Strict, Type) -> (Strict, Type) -> m [(PatQ, [conc])]
      doInfixC subst cname lhs rhs = do
        pure $ [(asP x (infixP wildP cname wildP),
                 map (\((_, ftype), pos) ->
                          (subst ftype,
                           [| [($(asConQ (makePathCon (makePathType (ModelType (asName v))) (show pos))),
                                ($(do p <- newName "p"
                                      lamE (replicate (pos-1) wildP ++ [varP p] ++ replicate (2-pos) wildP) (varE p)) $(varE x)))] |]))
                     (zip [lhs, rhs] ([1..] :: [Int])))]

substG :: Data a => Map Name Type -> a -> Subst a
substG bindings typ = Subst {unSubst = everywhere (mkT (subst1 bindings)) typ}

subst1 :: Map Name Type -> Type -> Type
subst1 bindings t@(VarT name) = maybe t id (Map.lookup name bindings)
subst1 _ t = t

-- | See if there is a path from typ to gkey.  We need to avoid
-- building expressions for non-existant paths because they will cause
-- "no Path instance" errors.
testIsPath :: TypeGraphM m => Type -> TGVSimple -> m Bool
testIsPath typ gkey = do
  mkey <- tgvSimple' typ
  case mkey of
    Nothing -> pure False
    Just v -> (maybe False (Set.member gkey) . Map.lookup v) <$> allPathKeys
