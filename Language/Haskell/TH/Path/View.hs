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

import Control.Applicative
import Control.Lens (Lens')
import Control.Monad.State (MonadState)
import Data.List (intercalate)
import Data.Set as Set (fromList, Set)
import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Context.Reify (reifyInstancesWithContext, evalContext, S)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.TypeGraph.Shape (pprint')

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
viewInstanceType :: (DsMonad m, MonadState S m) => Type -> m (Maybe Type)
viewInstanceType typ =
    do vInsts <- runQ $ reifyInstances ''ViewType [typ]
       case vInsts of
         [TySynInstD _ (TySynEqn [_typ] type2)] -> return $ Just type2
         [] -> return Nothing
         _ -> error $ "Unexpected view instance(s): " ++ show vInsts

-- | Retrieve every View instance known to the Q monad and return the
-- union of all of their a and b types.
viewTypes :: Q (Set Type)
viewTypes = evalContext $ do
  FamilyI _ tySynInsts <- runQ $ reify ''ViewType
  return $ Set.fromList $ t1 $ concatMap (\ (TySynInstD vt (TySynEqn [a] b)) -> [a, b]) tySynInsts
    where t1 x = trace (intercalate "\n  " ("Language.Haskell.TH.Path.View - viewTypes:" : map show x)) x
