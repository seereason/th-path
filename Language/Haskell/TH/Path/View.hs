{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.TH.Path.View
    ( -- View(viewLens, ViewType)
      View(viewLens)
    , viewInstanceType
    , viewTypes
    ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Monad.State (MonadState)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (InstMap, {-testContext,-} reifyInstancesWithContext, evalContextState)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.TypeGraph (E)
import Language.Haskell.TH.TypeGraph.Core (typeArity)

-- | If there is an instance of View for a pair of types, when @a@
-- appears in the data, the lens returned by 'viewLens' is used to
-- compute the @b@ value, and that value is used instead - to generate
-- a User Interface, for example.  Then the resulting @b@ value is
-- transformed back into an @a@ and stored in the database.  An
-- instance of View is equivalent to a Substitute hint.
#if 0
class View a where
    type ViewType a
    viewLens :: Lens' a (ViewType a)
#else
class View a b | a -> b where
    viewLens :: Lens' a b
#endif

viewInstanceType :: (DsMonad m, MonadState (InstMap (E Pred)) m) => Type -> m (Maybe Type)
viewInstanceType typ =
    do arity <- typeArity typ
       v <- runQ $ VarT <$> newName "v"
       vInsts <- if arity == 0 then reifyInstancesWithContext ''View [typ, v] else return []
       case vInsts of
         [(InstanceD _ (AppT (AppT _view _a) b) _)] -> return $ Just b
         [] -> return Nothing
         _ -> error $ "Unexpected view instance(s): " ++ show vInsts

-- | Any type that is part of a View instance
viewTypes :: Q [Type]
viewTypes = do
  vInsts <- evalContextState $ reifyInstancesWithContext ''View [VarT (mkName "a"), VarT (mkName "b")]
  return $ concatMap (\ (InstanceD _ (AppT (AppT (ConT _view) a) b) _) -> [a, b]) vInsts
