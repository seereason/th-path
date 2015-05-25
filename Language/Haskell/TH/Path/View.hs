{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.TH.Path.View
    ( View(viewLens, ViewType)
    , viewInstanceType
    ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens')
import Control.Monad.State (MonadState)
import Language.Haskell.TH
import Language.Haskell.TH.Context (InstMap, {-testContext,-} reifyInstancesWithContext)
import Language.Haskell.TH.Desugar as DS (DsMonad)
import Language.Haskell.TH.TypeGraph (E)
import Language.Haskell.TH.TypeGraph.Core (typeArity)

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
    do arity <- typeArity typ
       v <- runQ $ VarT <$> newName "v"
       vInsts <- if arity == 0 then reifyInstancesWithContext ''View [typ, v] else return []
       case vInsts of
         [(InstanceD _ (AppT (AppT _view _a) b) _)] -> return $ Just b
         [] -> return Nothing
         _ -> error $ "Unexpected view instance(s): " ++ show vInsts
