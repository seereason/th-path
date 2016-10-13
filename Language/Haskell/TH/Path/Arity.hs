{-# LANGUAGE CPP #-}

-- | Function to compute the arity or kind of a Type, the number of
-- type parameters that need to be applied to get a concrete type.
module Language.Haskell.TH.Path.Arity
    ( typeArity
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Desugar ({- instances -})
import Language.Haskell.TH.Syntax (Quasi(qReify))
import Language.Haskell.TH.Path.Prelude (pprint1)

-- | Compute the arity of a type - the number of type parameters that
-- must be applied to it in order to obtain a concrete type.  I'm not
-- quite sure I understand the relationship between this and
-- 'freeTypeVars'.
typeArity :: Quasi m => Type -> m Int
typeArity t0 = typeArity' t0
    where
      typeArity' (ForallT _ _ typ) = typeArity' typ -- Shouldn't a forall affect the arity?
      typeArity' ListT = return 1
      typeArity' (TupleT n) = return n
      typeArity' (VarT _) = return 1
      typeArity' (AppT t _) = typeArity' t >>= \ n -> return $ n - 1
      typeArity' (ConT name) = qReify name >>= infoArity
      typeArity' typ = error $ "typeArity (" ++ pprint1 t0 ++ ") - unexpected type: " ++ show typ
      infoArity (TyConI dec) = decArity dec
      infoArity (PrimTyConI _ _ _) = return 0
      infoArity (FamilyI dec _) = decArity dec
      infoArity info = error $ "typeArity (" ++ pprint1 t0 ++ ")- unexpected info: " ++ show info
#if MIN_VERSION_template_haskell(2,11,0)
      decArity (DataD _ _ _ vs _ _) = return $ length vs
      decArity (NewtypeD _ _ _ vs _ _) = return $ length vs
#else
      decArity (DataD _ _ vs _ _) = return $ length vs
      decArity (NewtypeD _ _ vs _ _) = return $ length vs
#endif
      decArity (TySynD _ vs t) = typeArity' t >>= \ n -> return $ n + length vs
#if MIN_VERSION_template_haskell(2,11,0)
      decArity (DataFamilyD _ vs _mk) = return $ {- not sure what to do with the kind mk here -} length vs
#else
      decArity (FamilyD _ _ vs _mk) = return $ {- not sure what to do with the kind mk here -} length vs
#endif
      decArity dec = error $ "typeArity (" ++ pprint1 t0 ++ ")- unexpected dec: " ++ show dec
