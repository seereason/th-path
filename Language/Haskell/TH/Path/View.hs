{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.TH.Path.View
    ( View(viewLens, ViewType)
    , viewInstanceType
    , viewTypes
    ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Monad.State (MonadState)
import Data.Maybe (catMaybes)
import Data.Set as Set (fromList, Set)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (InstMap, {-testContext,-} reifyInstancesWithContext, evalContextState)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.TypeGraph (E)

-- | If there is an instance of View for a pair of types, when @a@
-- appears in the data, the lens returned by 'viewLens' is used to
-- compute the @b@ value, and that value is used instead - to generate
-- a User Interface, for example.  Then the resulting @b@ value is
-- transformed back into an @a@ and stored in the database.  An
-- instance of View is equivalent to a Substitute hint.
class View a where
    type ViewType a
    viewLens :: Lens' a (ViewType a)

viewInstanceType :: (DsMonad m, MonadState (InstMap (E Pred)) m) => Type -> m (Maybe Type)
viewInstanceType typ =
    do vInsts <- runQ $ reifyInstances ''ViewType [typ]
       case vInsts of
         [TySynInstD _ (TySynEqn [_typ] type2)] -> return $ Just type2
         [] -> return Nothing
         _ -> error $ "Unexpected view instance(s): " ++ show vInsts

-- | Any type that is part of a View instance
viewTypes :: Q (Set Type)
viewTypes = evalContextState $ do
  a <- runQ (newName "a" >>= varT)
  vInsts <- reifyInstancesWithContext ''View [a]
  -- trace ("vInsts a " ++ " -> " ++ show vInsts) (return ())
  let aTypes = concatMap (\ (InstanceD _ (AppT (ConT _view) t) _) -> [t]) vInsts
  bTypes <- catMaybes <$> mapM viewInstanceType aTypes
  return $ Set.fromList $ aTypes ++ bTypes
