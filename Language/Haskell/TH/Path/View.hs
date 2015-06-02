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

-- | If there is an instance of View for a type @a@, then when @a@
-- appears in the data, the lens returned by 'viewLens' is used to
-- compute the value of type @ViewType a@, and that value is used
-- instead.  For exmaple, a 'View' could be used to modify the value
-- obtained from a database so that it is more suitable to the user
-- interface that uses that value.  Then the @ViewType a@ value is
-- transformed back into an @a@ and stored in the database.
class View a where
    type ViewType a
    viewLens :: Lens' a (ViewType a)

-- | Determine whether there is a 'View' instance for a type and if so
-- return @ViewType a@.
viewInstanceType :: (DsMonad m, MonadState (InstMap (E Pred)) m) => Type -> m (Maybe Type)
viewInstanceType typ =
    do vInsts <- runQ $ reifyInstances ''ViewType [typ]
       case vInsts of
         [TySynInstD _ (TySynEqn [_typ] type2)] -> return $ Just type2
         [] -> return Nothing
         _ -> error $ "Unexpected view instance(s): " ++ show vInsts

-- | Return all types which have a 'View' instance.
viewTypes :: Q (Set Type)
viewTypes = evalContextState $ do
  a <- runQ (newName "a" >>= varT)
  vInsts <- reifyInstancesWithContext ''View [a]
  -- trace ("vInsts a " ++ " -> " ++ show vInsts) (return ())
  let aTypes = concatMap (\ (InstanceD _ (AppT (ConT _view) t) _) -> [t]) vInsts
  bTypes <- catMaybes <$> mapM viewInstanceType aTypes
  return $ Set.fromList $ aTypes ++ bTypes
