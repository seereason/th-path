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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.ULens (uLensDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Default (Default(def))
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (fieldLensNameOld, HasName(asName), HasType(asType), HasTypeQ(asTypeQ), makeUFieldCon, tells)
import Language.Haskell.TH.Path.Core (mat, IdPath(idPath), ToLens(toLens), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), U(u, unU'))
import Language.Haskell.TH.Path.Decs.PathType (upathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (field, TGVSimple)

uLensDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
uLensDecs utype v = do
  -- ptyp <- upathType v
  tlc <- execWriterT (do tell [newName "p" >>= \p -> clause [varP p] (guardedB [normalGE [|$(varE p) == idPath|] [|lens u (\s a -> maybe s id (unU' a))|]]) []]
                         toLensClauses utype v)
  tells [instanceD (pure []) [t|ToLens $utype $(asTypeQ v)|]
           [ funD 'toLens tlc
           ] ]

toLensClauses :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
                 TypeQ
              -> TGVSimple
              -> m ()
toLensClauses utype v = do
  x <- runQ (newName "_p")
  let control = toLensControl utype v x :: Control m () () ()
  doNode control v

toLensControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TypeQ -> TGVSimple -> Name -> Control m () () ()
toLensControl utype v x =
    Control
    { _doSimple = pure ()
    , _doSelf = pure ()
    , _doView =
        \w -> do
          ptyp <- upathType v
          lns <- runQ [|viewLens :: Lens' $(return (asType v)) $(asTypeQ w)|]
          -- Ok, we have a type v, and a lens that goes between v and
          -- w, and we need to create a toLens' function for v's path type.
          -- The tricky bit is to extract the path value for w from the path
          -- value we have.
          let ConT pname = ptyp
          doClause utype w (\p -> conP (mkName (nameBase pname ++ "_View")) [p]) (pure lns)
    , _doOrder =
        \_i w -> do
          k <- runQ (newName "k")
          doClause utype w (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
    , _doMap =
        \_i w -> do
          k <- runQ (newName "k")
          doClause utype w (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \f s ->
            do doClause utype f (\p -> [p|Path_First $p|]) [|_1|]
               doClause utype s (\p -> [p|Path_Second $p|]) [|_2|]
               pure def
    , _doMaybe =
        \w -> do
          doClause utype w (\p -> [p|Path_Just $p|]) [|_Just|]
    , _doEither =
        \l r ->
            do doClause utype l (\p -> [p|Path_Left $p|]) [|_Left|]
               doClause utype r (\p -> [p|Path_Right $p|]) [|_Right|]
               pure def
    , _doField =
        \fld typ -> do
          skey <- tgvSimple' typ
          fkey <- tgv (Just fld) skey
          -- ok <- goalReachableSimple gkey skey
          case ({-ok,-} view (_2 . field) fkey) of
            ({-True,-} Just (_tname, _cname, Right fname)) ->
                do -- Build a type expression for the path type, inserting any
                   -- necessary declarations into the state.  Also, build an
                   -- expression for the lens that turns this field value into the
                   -- goal type.
                   let Just pcname = makeUFieldCon fkey
                   -- These are the field's clauses.  Each pattern gets wrapped
                   -- with the field path constructor, and each field lens gets
                   -- composed with the lens produced for the field's type.
                   let hop = varE (fieldLensNameOld (asName v) fname)
                       lns = {-if skey == gkey then hop else-} [|$hop . toLens $(varE x)|]
                   tell [clause [conP (asName pcname) [varP x]] (normalB lns) []]
            ({-True,-} Just (_tname, _cname, Left _fpos)) -> pure ()
            ({-True,-} Nothing) -> pure ()
            -- (False, _) -> pure ()
    , _doConcs = \_ _ -> pure ()
    , _doSyn = \_tname _typ -> pure ()
    , _doAlts = \_ -> pure ()
    , _doSyns = \() _ -> pure ()
    }

-- | Given a function pfunc that modifies a pattern, add a
-- 'Language.Haskell.TH.Clause' (a function with a typically incomplete
-- pattern) to the toLens' method we are building to handle the new
-- pattern.
doClause :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
            TypeQ -> TGVSimple -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause utype v pfunc lns = do
  p <- runQ (newName "v")
  -- ok <- goalReachableSimple gkey v
  let pat = {-bool wildP-} (varP p) {-(v /= gkey)-}
      lns' = {-bool lns-} [|$lns . toLens $(varE p)|] {-(v /= gkey)-}
  tell [clause [pfunc pat] (normalB lns') []]
