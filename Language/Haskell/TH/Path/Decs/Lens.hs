-- | Make the classy lens declarations.

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
{-# LANGUAGE TypeSynonymInstances #-}
module Language.Haskell.TH.Path.Decs.Lens
    ( uLensDecs
    ) where

import Control.Lens hiding (cons, Strict)
import Control.Monad.Writer (MonadWriter, execWriterT, tell)
import Data.Char (toLower)
import Data.Foldable as Foldable
import Data.Proxy (Proxy(Proxy))
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (HasName(asName), HasTypeQ(asTypeQ), makeUFieldCon, tells)
import Language.Haskell.TH.Path.Core (mat, IsPath(idPath), ToLens(toLens), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..), Path_View(..), U(u, unU'))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.Syntax as TH (Quasi(qReify))
import Language.Haskell.TH.TypeGraph.Lens (lensNamePairs)
import Language.Haskell.TH.TypeGraph.TypeGraph (tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (field, TGVSimple, typeNames)

uLensDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
uLensDecs utype v = do
  tlc <- execWriterT (do tell [newName "p" >>= \p -> clause [varP p] (guardedB [normalGE [|$(varE p) == idPath|] [|lens u (\s a -> maybe s id (unU' a))|]]) []]
                         toLensClauses v)
  tells [instanceD (pure []) [t|ToLens $utype $(asTypeQ v)|]
           [ funD 'toLens tlc
           ] ]

toLensClauses :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
                 TGVSimple
              -> m ()
toLensClauses v = do
  x <- runQ (newName "_p")
  let control = toLensControl v x :: Control m () () ()
  doNode control v

toLensControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TGVSimple -> Name -> Control m () () ()
toLensControl v x =
    Control
    { _doSimple = pure ()
    , _doSelf = pure ()
    , _doView =
        \_w -> doClause (\p -> [p|Path_To Proxy $p|]) [|viewLens|]
    , _doOrder =
        \_i _w -> do
          k <- runQ (newName "k")
          doClause (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
    , _doMap =
        \_i _w -> do
          k <- runQ (newName "k")
          doClause (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \_f _s ->
            do doClause (\p -> [p|Path_First $p|]) [|_1|]
               doClause (\p -> [p|Path_Second $p|]) [|_2|]
               pure ()
    , _doMaybe =
        \_w -> do
          doClause (\p -> [p|Path_Just $p|]) [|_Just|]
    , _doEither =
        \_l _r ->
            do doClause (\p -> [p|Path_Left $p|]) [|_Left|]
               doClause (\p -> [p|Path_Right $p|]) [|_Right|]
               pure ()
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
                   let pcname = makeUFieldCon fld
                   -- These are the field's clauses.  Each pattern gets wrapped
                   -- with the field path constructor, and each field lens gets
                   -- composed with the lens produced for the field's type.
                   let hop = varE (fieldLensNameOld (asName v) fname)
                       lns = [|$hop . toLens $(varE x)|]
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
doClause :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) => (PatQ -> PatQ) -> ExpQ -> m ()
doClause pfunc lns = do
  tell [newName "v" >>= \v -> clause [pfunc (varP v)] (normalB [|$lns . toLens $(varE v)|]) []]

fieldLensNameOld :: Name -> Name -> Name
fieldLensNameOld tname fname = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname)
