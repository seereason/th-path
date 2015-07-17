{-# LANGUAGE TemplateHaskell, TupleSections, CPP #-}

{- |
This is a modifed copy of Data.Lens.Template from
Joel Burget's data-lens-template package.  It changes
the signature of the namer function used in nameMakeLens
and adds nameMakeLenses.

This module provides an automatic Template Haskell
routine to scour data type definitions and generate
accessor objects for them automatically.
-}
module Language.Haskell.TH.Path.Lens
    ( makePathLens
    , fieldLensName -- Imported by Path.Instance
    , nameMakeLens -- Imported by Appraisal.ReportMap
    ) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Lens (_Just, Lens', lens, Traversal')
import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Data.List (nub)

makePathLens :: Quasi m => Name -> m [Dec]
makePathLens tname = runQ (nameMakeLens tname (\ nameA nameB -> Just (nameBase (fieldLensName nameA nameB))))

fieldLensName :: Name -> Name -> Name
fieldLensName tname fname' = mkName ("lens_" ++ nameBase tname ++ "_" ++ nameBase fname')

-- |@nameMakeLens n f@ where @n@ is the name of a data type
-- declared with @data@ and @f@ is a function from names of fields
-- in that data type to the name of the corresponding accessor. If
-- @f@ returns @Nothing@, then no accessor is generated for that
-- field.
nameMakeLens :: Name -> (Name -> Name -> Maybe String) -> Q [Dec]
nameMakeLens t namer = do
    info <- reify t
    case info of
      TyConI reified -> decMakeLens t reified namer
      _ -> return []

decMakeLens :: Name -> Dec -> (Name -> Name -> Maybe String) -> Q [Dec]
decMakeLens t (NewtypeD cx n ps c ss) namer = decMakeLens t (DataD cx n ps [c] ss) namer
decMakeLens t (DataD _ nameA params cons _) namer = do
    decs <- makeAccs (length cons) . nub $ concatMap namedFields cons
    -- when (null decs) $ qReport False nodefmsg
    return decs

    where

    -- nodefmsg = "Warning: No accessors generated from the name " ++ show t
    --       ++ "\n If you are using makeLenses rather than"
    --       ++ "\n nameMakeLens, remember accessors are"
    --       ++ "\n only generated for fields starting with an underscore"

    makeAccs :: Int -> [(Con, VarStrictType)] -> Q [Dec]
    makeAccs ncons vars =
        liftM (concat . catMaybes) $ mapM (\ (con, (fname,_,ftype)) -> makeAccFromName ncons con fname (return ftype)) vars

    transformName :: Name -> Name -> Maybe Name
    transformName nameA' nameB@(Name _occ f) = do
        n <- namer nameA' nameB
        return $ Name (mkOccName n) f

    makeAccFromName :: Int -> Con -> Name -> TypeQ -> Q (Maybe [Dec])
    makeAccFromName ncons con fname ftype =
        case transformName nameA fname of
            Nothing -> return Nothing
            -- Bug fix: Data.Lens.Template uses n instead of mkName (nameBase n),
            -- so the names cannot be used if the lenses are not generated in the
            -- module where the types are declared.
            Just n -> liftM Just $ makeAcc ncons con fname params ftype (mkName (nameBase n))

    -- haddock doesn't grok TH
#ifndef __HADDOCK__

    makeAcc :: Int -> Con -> Name -> [TyVarBndr] -> TypeQ -> Name -> Q [Dec]
    makeAcc ncons con fname params' ftype accName = do
        let params'' = map (\x -> case x of (PlainTV n) -> n; (KindedTV n _) -> n) params'
        let appliedT = foldl appT (conT t) (map varT params'')

        let -- partialName = mkName (nameBase accName ++ "_partial")
            -- totalName = mkName (nameBase accName ++ "_total")
            partialBody =
              [| let getter a =
                       $(if ncons == 1
                         then [| Just ($(varE fname) a) |]
                         else caseE [|a|]
                                  [match (conPat con) (normalB [|Just ($(varE fname) a)|]) [],
                                   match wildP (normalB [|Nothing|]) []])
                     setter a Nothing = a
                     setter a (Just b) =
                       $(if ncons == 1
                         then recUpdE [|a|] [fieldExp fname [|b|]]
                         else caseE [|a|]
                                  [match (conPat con) (normalB (recUpdE [|a|] [fieldExp fname [|b|]])) [],
                                   match wildP (normalB [|a|]) []]) in
                 lens getter setter . _Just |]
            totalBody =
              [| let getter a = $(varE fname) a
                     setter a b = $(recUpdE [|a|] [fieldExp fname [|b|]]) in
                 lens getter setter |]
        let lensType = [t|$(if ncons > 1 then [t|Traversal'|] else [t|Lens'|]) $appliedT $ftype|]
            lensType' = case params'' of
                          [] -> lensType
                          _ -> forallT (map PlainTV params'') (return []) lensType
        sequence [ {- sigD accName lensType'
                 , valD (varP accName) (normalB (if ncons > 1 then [|$(varE partialName)|] else [|$(varE totalName)|])) []
                 , -} sigD accName lensType'
                 , valD (varP accName) (normalB (if ncons > 1 then partialBody else totalBody)) [] ]

    conPat :: Con -> PatQ
    conPat (NormalC name _) = recP name []
    conPat (RecC name _) = recP name []
    conPat (InfixC _ name _) = recP name []
    conPat (ForallC _ _ con) = conPat con
decMakeLens _ _ _ = return []

#endif

namedFields :: Con -> [(Con, VarStrictType)]
namedFields con@(RecC _ fs) = (map (con,) fs)
namedFields (ForallC _ _ c) = namedFields c
namedFields _ = []

#if UNUSED
errmsg :: Show a => a -> [Char]
errmsg t = "Cannot derive accessors for name " ++ show t ++ " because"
         ++ "\n it is not a type declared with 'data' or 'newtype'"
         ++ "\n Did you remember to double-tick the type as in"
         ++ "\n $(makeLenses ''TheType)?"
#endif
