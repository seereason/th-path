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

import Control.Lens (Lens')
import Control.Monad.State (execState, get, modify, put, StateT)
import Control.Monad.States (MonadStates)
import Data.Generics (everywhere, mkT)
-- import Data.Logic.ATP.FOL (subst)
-- import Data.Logic.ATP.Quantified (IsQuantified(..))
-- import Data.Logic.ATP.TH -- barely working Unify instance for Types
-- import Data.Logic.ATP.Unif (unify)
import Data.Map as Map (insert, lookup, Map)
import Data.Maybe (fromMaybe)
import Data.Set as Set (fromList, Set)
import Debug.Trace (trace)
import Language.Haskell.TH hiding (prim)
import Language.Haskell.TH.Context (InstMap)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.TypeGraph.Arity (typeArity)
import Language.Haskell.TH.TypeGraph.Expand (E(E), ExpandMap, expandType)
import Language.Haskell.TH.TypeGraph.Prelude (unlifted)

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
viewInstanceType :: (DsMonad m, MonadStates ExpandMap m) => E Type -> m (Maybe Type)
viewInstanceType (E typ) =
    do prim <- unlifted typ
       arity <- typeArity typ
       case arity == 0 && not prim of
         True -> do
           vInsts <- runQ $ reifyInstances ''ViewType [typ]
           case vInsts of
             [TySynInstD _ (TySynEqn [type1] type2)] ->
                 do (E type1') <- expandType type1
                    (E type2') <- expandType type2
                    -- Unify the original type with type1, and apply
                    -- the resulting bindings to type2.
                    case execState (fakeunify (E typ) (E type1')) (Just mempty) of
                      Nothing -> return Nothing
                      Just bindings -> return $ Just (everywhere (mkT (expandBindings bindings)) type2')
             [] -> return Nothing
             _ -> error $ "Unexpected view instance(s): " ++ show vInsts
         _ -> return Nothing
    where
      -- This is a dangerously weak imitation of unification
      fakeunify :: Monad m => E Type -> E Type -> StateT (Maybe (Map Type Type)) m ()
      fakeunify (E (AppT a b)) (E (AppT c d)) = fakeunify (E a) (E c) >> fakeunify (E b) (E d)
      fakeunify (E (ConT a)) (E (ConT b)) | a == b = return ()
      fakeunify (E a@(VarT _)) (E b) = do
        binding <- maybe Nothing (Map.lookup a) <$> get
        -- We ought to ensure that unexpended bindings don't appear in b
        maybe (modify (fmap (Map.insert a b))) (\a' -> fakeunify (E a') (E b)) binding
      fakeunify (E a) (E b@(VarT _)) = fakeunify (E b) (E a)
      fakeunify (E a) (E b) | a == b = return ()
      fakeunify (E a) (E b) = trace ("Could not unify: " ++ pprint (AppT (AppT EqualityT a) b)) (put Nothing)

expandBindings :: Map Pred Pred -> Pred -> Pred
expandBindings mp x@(VarT _) = fromMaybe x (Map.lookup x mp)
expandBindings _ x = x

-- expandBinding :: Pred -> Pred -> Pred -> Pred
-- expandBinding v a x = if x == v then a else x

-- | Retrieve every View instance known to the Q monad and return the
-- union of all of their a and b types.
viewTypes :: (DsMonad m, MonadStates InstMap m) => m (Set Type)
viewTypes = do
  FamilyI _ tySynInsts <- runQ $ reify ''ViewType
  return $ Set.fromList $ concatMap (\ (TySynInstD _vt (TySynEqn [a] b)) -> [a, b]) tySynInsts
