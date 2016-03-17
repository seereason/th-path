{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -ddump-splices #-}
module Editor
    ( editor
    ) where

import ReportPaths
import Appraisal.ReportInstances
import Appraisal.ReportTH
import Appraisal.Report (Report)
import Control.Lens (Lens', toListOf, view)
import Control.Monad.Readers
--import Data.Graph (Graph, Vertex)
--import Data.Set as Set (Set, toList)
import Data.Proxy
import Data.Tree
import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Core (Paths(paths, FromTo), ToLens(toLens), IdPath(idPath))
import Language.Haskell.TH.Path.Graph (TypeGraphM)
import Language.Haskell.TH.Path.View (viewInstanceType)
import Language.Haskell.TH.TypeGraph.Expand (E(E), expandType)
import Language.Haskell.TH.TypeGraph.TypeGraph
import Language.Haskell.TH.TypeGraph.TypeInfo
import Language.Haskell.TH.PprLib (text, hang)

{-
-- | This is what the output of editor should look like.
doReport :: forall m. (TypeGraphM m) =>
            Report -> m (Tree PV_Report)
doReport r =
    Node (PV_Report_Report idPath r) <$> doReportView
    where
      doReportView :: m [Tree PV_Report]
      doReportView = let (p, [x]) = (Path_Report_View idPath, toListOf (toLens p) r) in
                     (: []) <$> Node (PV_Report_ReportView p x) <$> doReportViewFields x
      -- doReportViewFields :: m [Tree PV_Report]
      doReportViewFields x =
          mapM (doReportViewPath x)
                   [{-Path_ReportView__reportName,
                    Path_ReportView__reportDate,
                    Path_ReportView__reportContractDate,
                    Path_ReportView__reportInspectionDate,
                    Path_ReportView__reportEffectiveDate,
                    Path_ReportView__reportAuthors,
                    Path_ReportView__reportPreparer,
                    Path_ReportView__reportPreparerEIN,
                    Path_ReportView__reportPreparerAddress,
                    Path_ReportView__reportPreparerEMail,
                    Path_ReportView__reportPreparerWebsite,
                    Path_ReportView__reportAbbrevs,
                    Path_ReportView__reportTitle,
                    Path_ReportView__reportHeader,
                    Path_ReportView__reportFooter,
                    Path_ReportView__reportValueTypeInfo,
                    Path_ReportView__reportValueApproachInfo,
                    Path_ReportView__reportClientName,
                    Path_ReportView__reportClientAddress,
                    Path_ReportView__reportClientGreeting,
                    Path_ReportView__reportItemsOwnerFull,
                    Path_ReportView__reportItemsOwner,
                    Path_ReportView__reportBriefItems,
                    Path_ReportView__reportInspectionLocation,
                    Path_ReportView__reportBody,
                    Path_ReportView__reportGlossary,
                    Path_ReportView__reportSources,
                    Path_ReportView__reportLetterOfTransmittal,
                    Path_ReportView__reportScopeOfWork,
                    Path_ReportView__reportCertification,
                    Path_ReportView__reportLimitingConditions,
                    Path_ReportView__reportPrivacyPolicy,
                    Path_ReportView__reportPerms,
                    Path_ReportView__reportBranding-}]
      doReportViewPath x p =
          let [y] = toListOf (toLens (p y)) x in
          (: []) <$> Node (PV_Report_ReportView (undefined y))
-}

-- | Build an editor for a named type and a value of that type.  Find
-- the node corresponding to the named type.  Generate a function
-- (lambda expression) that takes a value of that type and outputs the
-- corresponding list of PV_ values.
editor :: forall m. TypeGraphM m =>
          Name -- ^ The name of the type to edit
       -> ExpQ -- ^ An expression of that type
       -> m Exp
editor tname value =
    doType (conT tname) value
    where
      -- Given a type and an expression which is assumed to be of that
      -- type, return a lambda expression that returns the
      -- corresponding list of PV_ values.
      doType :: TypeQ -> ExpQ -> m Exp
      doType typ expr = do
        -- Find the vertex keys adjacent to k
        -- The root node represents the original value
        Just leRootCon <- runQ $ lookupValueName ("PV_" ++ nameBase tname ++ "_" ++ nameBase tname)
        Just pvType <- runQ $ lookupTypeName ("PV_" ++ nameBase tname)
        root <- runQ [| Node ($(conE leRootCon) idPath $expr) [] |]
        -- Now generate the traversal of value.
        E t <- expandType (ConT tname)
        viewType <- viewInstanceType t
        case viewType of
          -- This arc is a view, so there is only one PV value:
          -- \report -> let path = Path_Report ReportView in PV_Report_ReportView path (view path report)
          Just ktype@(ConT kname) ->
              do Just leCon <- runQ $ lookupValueName ("PV_" ++ nameBase tname ++ "_" ++ nameBase kname)
                 runQ [| $(pure root) {subForest = map (\path ->
                                                            let [x] = toListOf (toLens path) $expr :: [$(pure ktype)] in
                                                            Node ($(conE leCon) path x) [])
                                                       (pathsOf $expr (undefined :: Proxy $(conT kname)) :: [Path $(conT tname) $(conT kname)]) :: [Tree $(conT pvType)]} |]
          _ -> runQ [| root :: Tree $(conT pvType) |]
{-
        case ks of
          -- Is typ an instance of View?  Perform a lame test...
          [k1] -> doSingleton typ value (bestName k1) k1
          _ -> doMultiple ks
      doSingleton typ value (Just kname) k
          | mkName (nameBase tname ++ "View") == kname =
              let le = conE (mkName ("PV_" ++ nameBase tname ++ "_" ++ nameBase kname))
                  path = [|toPath $(value) :: PathType $(conT tname) $(conT kname)|] in
              runQ $ [| [$le $path (view (toLens $path) $value)] |]
      doSingleton typ value _ k =
          runQ $ lift ([] :: [PV_Report])
      doMultiple ks = runQ $ lift ([] :: [PV_Report])
-}

#if 0
-- We need a function that takes a type name and returns the
-- declaration of a function of type @Report -> [PV_Report]@.  In
-- order to avoid having to think up names, we use an Editor class and
-- call the function leList.

class Editor a where
    type EditType a
    leList :: TypeGraphM m => a -> m Exp {-a -> [EditType a]-}

editor :: forall m. TypeGraphM m => Name -> m [Dec]
editor name = do
  Just pvName <- (runQ . lookupTypeName) ("PV_" ++ nameBase name)
  -- The vertex of the type we are starting from
  v <- runQ (conT name) >>= expandType >>= typeVertex :: m TGVSimple
  -- The vertex that are adjacent
  ws <- Set.toList <$> adjacent (tgv v)
  x <- runQ $ newName "x"v
  exps <- sequence (map (doArc pvName x v) ws)
  runQ [d|instance Editor $(conT name) where
             type EditType $(conT name) = $(conT pvName)
             leList = \x -> $(listE (map pure exps)) :: $(conT name) -> [$(conT pvName)] |]
    where
      doArc :: Name -> Name -> TGVSimple -> TGV -> m Exp
      doArc pvName x v w = runQ [|\ $(varP x) -> (error "editor2" {-undefined $(litE (stringL ("x=" ++ show (friendlyNames x) ++ ", v=" ++ show (friendlyNames v) ++ ", w=" ++ show (friendlyNames w))))-}) :: [$(conT pvName)] |]
      -- doArc v w = runQ [|\x -> "x=" ++ show x ++ ", v=" ++ show v ++ ", w=" ++ show w|]

{-
      -- Create an expression that turns a value x into 
      doNode :: TGVSimple -> m Exp
      doNode n = do
        ns <- adjacent n
        [|\x -> case 
      doName name = qReify name >>= doInfo
      doInfo (TyConI dec) = doDec dec
      doInfo (FamilyI dec _insts) = doDec dec
      doInfo (PrimTyConI _ _ _) = return ()
      doDec (TySynD _tname _ typ) = doType typ
      doDec (NewtypeD _ tname _ constr _) = doCon tname constr
      doDec (DataD _ tname _ constrs _) = mapM_ (doCon tname) constrs
-}
#endif
