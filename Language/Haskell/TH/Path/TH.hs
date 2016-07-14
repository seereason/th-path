{-# LANGUAGE CPP, FlexibleContexts #-}
module Language.Haskell.TH.Path.TH
    ( aOfS
    ) where

import Control.Lens (view, _Left, _Right)
import Control.Monad.State (get, modify, put, StateT, runStateT)
import Control.Monad.Trans (lift)
import Debug.Trace
import Data.Map ((!), Map)
import Data.Maybe (fromJust)
import Language.Haskell.TH -- (Q, ExpQ, Exp(AppE, VarE, TupE, LitE, InfixE))
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Path.Common (makeUNamedFieldCon, PathCon(unPathCon))
import Language.Haskell.TH.Path.Core
#if !__GHCJS__
import Language.Haskell.TH.Path.Instances ()
#endif
import Language.Haskell.TH.Path.View (View(ViewType), viewLens, viewInstanceType)
import Language.Haskell.TH.Syntax (qReify, VarStrictType)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Test.HUnit

-- | Customized modify for our state monad.
modify' :: (ExpQ -> ExpQ) -> StateT Exp Q ()
modify' f = get >>= \e -> lift (f (pure e)) >>= put

-- | Given an expression of type @s -> a@ (in a very specific format)
-- and a type @s@ type, return the type @a@.
--
-- We extract @a@ from the type with guidance from the expression.
-- Specifically, the innermost layer of the expression tells us how to
-- strip the outermost layer of the type and get closer to @a@.
--
-- So we must first traverse to the bottom of the expression.  When we
-- reach @id@ we return the initial @s@ type.  Then as we exit each
-- layer of the expression we strip a layer of the result type.
aOfS :: ExpQ -> TypeQ -> Q (Type, Exp)
aOfS expq stypeq = do
  exp <- expq
  stype <- stypeq
  -- trace ("aOfS (" ++ pprint1 exp ++ ") (" ++ pprint1 stype ++ ")\n  exp=" ++ show exp ++ "\n  typ=" ++ show stype) (pure ())
  [|idPath|] >>= runStateT (doType exp stype)
    where
      doType :: Exp -> Type -> StateT Exp Q Type
      doType exp typ@(ConT tname) = {-trace ("doType1 (" ++ pprint1 exp ++ ") (" ++ pprint1 typ ++ ")") (pure ()) >>-} qReify tname >>= doInfo exp tname >>= lift . expandType
      doType exp@(LamE [VarP x] _) typ = {-trace ("doType2 (" ++ pprint1 exp ++ ") (" ++ pprint1 typ ++ ")") (pure ()) >>-} doExp exp typ >>= lift . expandType
      doType exp typ = error $ "Expecting a lambda expression, saw " ++ pprint1 exp

      doInfo :: Exp -> Name -> Info -> StateT Exp Q Type
      doInfo exp tname (TyConI dec) = {-trace ("doInfo1 (" ++ pprint1 exp ++ ") (" ++ show tname ++ ") (" ++ pprint1 dec ++ ")") (pure ()) >>-} doDec exp tname dec
      doInfo exp tname (FamilyI dec _) = {-trace ("doInfo2 (" ++ pprint1 exp ++ ") (" ++ show tname ++ ") (" ++ pprint1 dec ++ ")") (pure ()) >>-} doDec exp tname dec
      doInfo _ _ info = error $ "Unexpected info: " ++ pprint1 info ++ "\n  " ++ show info

      doDec :: Exp -> Name -> Dec -> StateT Exp Q Type
      doDec exp _ (TySynD _ binds typ) = doType exp typ
      doDec exp tname _ = doExp exp (ConT tname)

      -- Given a lambda expression and the type of its argument,
      -- return the type of its result.  Save the resulting path
      -- expression in the state.  Example:
      --   exp: \x -> fst x
      --   typ: (Int, Char)
      doExp :: Exp -> Type -> StateT Exp Q Type
      -- If the expression is (\x -> x) then return typ.
      doExp (LamE [(VarP x)] (VarE name)) typ | name == x = pure typ
      doExp (LamE [x@(VarP _)] (AppE (VarE fn) exp')) typ
        | fn == 'fst =
            do -- Strip off the outer layer of the lambda expression
               -- and compute the result type.  Then return the
               -- application of that outer layer
               t <- doType (LamE [x] exp') typ -- doType (LamE [x] (VarE x)) (Int, Char) -> Int
               -- t needs to be a pair, return the type of fst
               case t of
                 AppT (AppT (TupleT 2) a) _ -> modify' (\e -> [|Path_First $e|]) >> pure a
                 _ -> error "aOfS fst"
        | fn == 'snd =
            do t <- doType (LamE [x] exp') typ
               case t of
                 AppT (AppT (TupleT 2) _) b -> modify' (\e -> [|Path_Second $e|]) >> pure b
                 _ -> error $ "aOfS snd - t=" ++ show t
      doExp (LamE [x@(VarP _)] (AppE (AppE (VarE fn) (VarE lns)) exp')) typ
        | fn == 'view && lns == '_Left =
            do t <- doType (LamE [x] exp') typ
               case t of
                 AppT (AppT (ConT either) l) _ | either == ''Either -> modify' (\e -> [|Path_Left $e|]) >> pure l
                 _ -> error "aOfS Left"
        | fn == 'view && lns == '_Right =
            do t <- doType (LamE [x] exp') typ
               case t of
                 AppT (AppT (ConT either) _) r | either == ''Either -> modify' (\e -> [|Path_Right $e|]) >> pure r
                 _ -> error "aOfS Right"
      doExp (LamE [x@(VarP _)] (AppE (ConE c) exp')) typ
        | c == 'Just =
            do t <- doType (LamE [x] exp') typ
               case t of
                 AppT (ConT maybe) a | maybe == ''Maybe -> modify' (\e -> [|Path_Just $e|]) >> pure a
                 _ -> error "aOfS Just"
      doExp (LamE [x@(VarP _)] (InfixE (Just exp') (VarE op) (Just key))) typ
        | op == '(!) =
            do t <- doType (LamE [x] exp') typ
               case t of
                 AppT (AppT (ConT mp) k) a | mp == ''Map -> modify' (\e -> [|Path_Look $(pure key) $e|]) >> pure a
                 _ -> error "aOfS Map"
      doExp (LamE [x@(VarP _)] (AppE (VarE fname) exp')) typ =
        do VarI _ (AppT (AppT ArrowT rtype@(ConT tname)) ftype) _ _ <- qReify fname
           t <- doType (LamE [x] exp') typ -- Should return a @ConT tname@
           case t of
             rtype' | rtype' == rtype ->
                        do fld <- lift $ fromJust <$> lookupValueName (nameBase (unPathCon (makeUNamedFieldCon tname fname)))
                           modify' (\e -> [|$(conE fld) $e|]) >> pure ftype
             _ -> error "aOfS field"
      doExp exp typ = error $ "aOfS - exp=" ++ pprint1 exp ++ ", typ=" ++ pprint1 typ

      expandType :: Type -> TypeQ
      expandType (ConT tname) = qReify tname >>= expandInfo tname
      expandType typ = pure typ
      expandInfo tname (TyConI dec) = expandDec tname dec
      expandInfo tname (FamilyI dec _) = expandDec tname dec
      expandInfo tname info = error $ "Unexpected info"
      expandDec _ (TySynD _ binds typ) = expandType typ
      expandDec tname _ = conT tname
