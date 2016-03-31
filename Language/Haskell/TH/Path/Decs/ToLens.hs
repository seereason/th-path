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
module Language.Haskell.TH.Path.Decs.ToLens (toLensDecs) where

import Control.Lens hiding (cons, Strict)
import Control.Monad (when)
import Control.Monad.Writer (execWriterT, MonadWriter, tell)
import Data.Bool (bool)
import Data.Default (Default(def))
import Data.Proxy (Proxy(Proxy))
import Data.Set.Extra as Set (mapM_)
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (fieldLensNameOld, HasName(asName), HasType(asType), HasTypeQ(asTypeQ), makeFieldCon)
import Language.Haskell.TH.Path.Core (mat, S, A, ToLens(toLens), Path_Map(..), Path_Pair(..), Path_Maybe(..), Path_Either(..))
import Language.Haskell.TH.Path.Decs.PathType (pathType)
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.Order (lens_omat, Path_OMap(..))
import Language.Haskell.TH.Path.Traverse (Control(..), doNode)
import Language.Haskell.TH.Path.View (viewLens)
import Language.Haskell.TH.TypeGraph.TypeGraph (goalReachableSimple, pathKeys, tgv, tgvSimple')
import Language.Haskell.TH.TypeGraph.Vertex (field, TGVSimple, TypeGraphVertex(bestType))

toLensControl :: (TypeGraphM m, MonadWriter [ClauseQ] m) => TypeQ -> TGVSimple -> TGVSimple -> Name -> Control m () () ()
toLensControl utype key gkey x =
    Control
    { _doSimple = pure ()
    , _doSelf = pure ()
    , _doView =
        \w -> do
          ptyp <- pathType (pure (bestType gkey)) key
          lns <- runQ [|viewLens :: Lens' $(return (asType key)) $(asTypeQ w)|]
          -- Ok, we have a type key, and a lens that goes between key and
          -- w, and we need to create a toLens' function for key's path type.
          -- The tricky bit is to extract the path value for w from the path
          -- value we have.
          let (AppT (ConT pname) _gtyp) = ptyp
          doClause utype gkey w (\p -> conP (mkName (nameBase pname ++ "_View")) [if asType w == asType gkey then wildP else p]) (pure lns)
    , _doOrder =
        \_i w -> do
          k <- runQ (newName "k")
          doClause utype gkey w (\p -> [p|Path_At $(varP k) $p|]) [|lens_omat $(varE k)|]
    , _doMap =
        \_i w -> do
          k <- runQ (newName "k")
          doClause utype gkey w (\p -> [p|Path_Look $(varP k) $p|]) [|mat $(varE k)|]
    , _doList =
        \_e -> pure ()
    , _doPair =
        \f s ->
            do doClause utype gkey f (\p -> [p|Path_First $p|]) [|_1|]
               doClause utype gkey s (\p -> [p|Path_Second $p|]) [|_2|]
               pure def
    , _doMaybe =
        \w -> do
          doClause utype gkey w (\p -> [p|Path_Just $p|]) [|_Just|]
    , _doEither =
        \l r ->
            do doClause utype gkey l (\p -> [p|Path_Left $p|]) [|_Left|]
               doClause utype gkey r (\p -> [p|Path_Right $p|]) [|_Right|]
               pure def
    , _doField =
        \fld typ -> do
          skey <- tgvSimple' typ
          fkey <- tgv (Just fld) skey
          ok <- goalReachableSimple gkey skey
          case (ok, view (_2 . field) fkey) of
            (True, Just (_tname, _cname, Right fname)) ->
                do -- Build a type expression for the path type, inserting any
                   -- necessary declarations into the state.  Also, build an
                   -- expression for the lens that turns this field value into the
                   -- goal type.
                   let Just pcname = makeFieldCon fkey
                   -- These are the field's clauses.  Each pattern gets wrapped
                   -- with the field path constructor, and each field lens gets
                   -- composed with the lens produced for the field's type.
                   let hop = varE (fieldLensNameOld (asName key) fname)
                       lns = if skey == gkey then hop else [|$hop . toLens (Proxy :: Proxy $utype) $(varE x)|]
                   tell [clause [conP 'Proxy [], conP (asName pcname) [varP x]] (normalB lns) []]
            (True, Just (_tname, _cname, Left _fpos)) -> pure ()
            (True, Nothing) -> pure ()
            (False, _) -> pure ()
    , _doConcs = \_ _ -> pure ()
    , _doSyn = \_tname _typ -> pure ()
    , _doAlts = \_ -> pure ()
    , _doSyns = \() _ -> pure ()
    }

toLensDecs :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> m ()
toLensDecs utype v =
    pathKeys v >>= Set.mapM_ (toLensDecs' utype v)

toLensDecs' :: forall m. (TypeGraphM m, MonadWriter [Dec] m) => TypeQ -> TGVSimple -> TGVSimple -> m ()
toLensDecs' utype key gkey = do
  ptyp <- pathType (pure (bestType gkey)) key
  tlc <- execWriterT $ toLensClauses utype key gkey
  when (not (null tlc)) $
       (runQ $ sequence
            [ instanceD (pure []) [t|ToLens $utype $(pure ptyp)|]
                [ tySynInstD ''S (tySynEqn [utype, pure ptyp] (pure (bestType key)))
                , tySynInstD ''A (tySynEqn [utype, pure ptyp] (pure (bestType gkey)))
                , funD 'toLens tlc -- [clause [wildP] (normalB (if key == gkey then [|id|] else [|undefined|])) []]
                ] ]) >>= tell


toLensClauses :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
                 TypeQ
              -> TGVSimple -- ^ the type whose clauses we are generating
              -> TGVSimple -- ^ the goal type key
              -> m ()
toLensClauses utype key gkey = do
  case key == gkey of
    True -> tell [clause [conP 'Proxy [], wildP] (normalB [|id|]) []]
    False -> do
      -- Use this to raise errors when the path patterns aren't exhaustive.
      -- That is supposed to be impossible, so this is debugging code.
      -- toLensClauses key gkey ptyp = do
      --   r <- foldPath control key
      --   return $ r ++ [clause [varP x] (normalB [|error ("toLens' (" ++ $(lift (pprint' key)) ++ ") -> (" ++ $(lift (pprint' gkey)) ++ ") - unmatched: " ++ show $(varE x))|]) []]
      x <- runQ (newName "_x")
      let control = toLensControl utype key gkey x :: Control m () () ()
      doNode control key

-- | Given a function pfunc that modifies a pattern, add a
-- 'Language.Haskell.TH.Clause' (a function with a typically incomplete
-- pattern) to the toLens' method we are building to handle the new
-- pattern.
doClause :: forall m. (TypeGraphM m, MonadWriter [ClauseQ] m) =>
            TypeQ -> TGVSimple -> TGVSimple -> (PatQ -> PatQ) -> ExpQ -> m ()
doClause utype gkey key pfunc lns = do
  v <- runQ (newName "v")
  ok <- goalReachableSimple gkey key
  let pat = bool wildP (varP v) (key /= gkey)
      lns' = bool lns [|$lns . toLens (Proxy:: Proxy $utype) $(varE v)|] (key /= gkey)
  when ok $ tell [clause [conP 'Proxy [], pfunc pat] (normalB lns') []]
