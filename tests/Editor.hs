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
import Data.Tree
import Debug.Trace (trace)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Core (IsPath(toLens, toPaths), IsPathType(idPath), PathType)
import Language.Haskell.TH.Path.View (viewInstanceType)
--import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import Language.Haskell.TH.TypeGraph.TypeGraph
import Language.Haskell.TH.TypeGraph.TypeInfo
import Language.Haskell.TH.TypeGraph.Vertex (bestName, TGV)
import Language.Haskell.TH.PprLib (text, hang)

-- | This is what the output of editor should look like.
doReport :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
            Report -> m (Tree LE_Report)
doReport r =
    Node (LE_Report_Report idPath r) <$> (doSubTypes ''Report)
    where
      doSubTypes :: Name -> m [Tree LE_Report]
      doSubTypes tname = undefined

-- | Build an editor for a named type and a value of that type.  Find
-- the node corresponding to the named type.  Generate a function
-- (lambda expression) that takes a value of that type and outputs the
-- corresponding list of LE_ values.
editor :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) =>
          Name -- ^ The name of the type to edit
       -> ExpQ -- ^ An expression of that type
       -> m Exp
editor tname value =
    doType (conT tname) value
    where
      -- Given a type and an expression which is assumed to be of that
      -- type, return a lambda expression that returns the
      -- corresponding list of LE_ values.
      doType :: TypeQ -> ExpQ -> m Exp
      doType typ value = do
        typ' <- runQ typ
        value' <- runQ value
        trace ("doType " ++ show typ' ++ " " ++ show value') (return ())
        g <- askPoly :: m TypeGraph
        let (_, keyFunction, vertexFunction) = view graph g
        -- Find the vertex key corresponding to typ
        k <- runQ typ >>= typeGraphVertex :: m TGV
        -- Find the vertex corresponding to k
        let Just v = vertexFunction k
        -- Find the vertex keys adjacent to k
        let g@(_, _, ks) = keyFunction v
        trace (show (hang (text "vertex Report:") 2 (ppr g))) (return ())
        trace ("bestNames: " ++ show (map bestName ks)) (return ())
        -- The root node represents the original value
        Just leRootCon <- runQ $ lookupValueName ("LE_" ++ nameBase tname ++ "_" ++ nameBase tname)
        Just leType <- runQ $ lookupTypeName ("LE_" ++ nameBase tname)
        root <- runQ [| Node ($(conE leRootCon) idPath $value) [] |]
        -- Now generate the traversal of value.
        viewType <- viewInstanceType (ConT tname)
        case viewType of
          -- This arc is a view, so there is only one LE value:
          -- \report -> let path = Path_Report ReportView in LE_Report_ReportView path (view path report)
          Just ktype@(ConT kname) ->
              do Just leCon <- runQ $ lookupValueName ("LE_" ++ nameBase tname ++ "_" ++ nameBase kname)
                 runQ [| $(pure root) {subForest = map (\path ->
                                                            let [x] = toListOf (toLens path) $value :: [$(pure ktype)] in
                                                            Node ($(conE leCon) path x) [])
                                                       ({-toPaths $value-} undefined :: [PathType $(conT tname) $(conT kname)]) :: [Tree $(conT leType)]} |]
          _ -> runQ [| root :: Tree $(conT leType) |]
{-
        case ks of
          -- Is typ an instance of View?  Perform a lame test...
          [k1] -> doSingleton typ value (bestName k1) k1
          _ -> doMultiple ks
      doSingleton typ value (Just kname) k
          | mkName (nameBase tname ++ "View") == kname =
              let le = conE (mkName ("LE_" ++ nameBase tname ++ "_" ++ nameBase kname))
                  path = [|toPath $(value) :: PathType $(conT tname) $(conT kname)|] in
              runQ $ [| [$le $path (view (toLens $path) $value)] |]
      doSingleton typ value _ k =
          runQ $ lift ([] :: [LE_Report])
      doMultiple ks = runQ $ lift ([] :: [LE_Report])
-}

#if 0
-- We need a function that takes a type name and returns the
-- declaration of a function of type @Report -> [LE_Report]@.  In
-- order to avoid having to think up names, we use an Editor class and
-- call the function leList.

class Editor a where
    type EditType a
    leList :: (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => a -> m Exp {-a -> [EditType a]-}

editor :: forall m. (ContextM m, MonadReaders TypeGraph m, MonadReaders TypeInfo m) => Name -> m [Dec]
editor name = do
  Just leName <- (runQ . lookupTypeName) ("LE_" ++ nameBase name)
  -- The vertex of the type we are starting from
  v <- runQ (conT name) >>= expandType >>= typeVertex :: m TGVSimple
  -- The vertex that are adjacent
  ws <- Set.toList <$> adjacent (tgv v)
  x <- runQ $ newName "x"
  exps <- sequence (map (doArc leName x v) ws)
  runQ [d|instance Editor $(conT name) where
             type EditType $(conT name) = $(conT leName)
             leList = \x -> $(listE (map pure exps)) :: $(conT name) -> [$(conT leName)] |]
    where
      doArc :: Name -> Name -> TGVSimple -> TGV -> m Exp
      doArc leName x v w = runQ [|\ $(varP x) -> (undefined $(litE (stringL ("x=" ++ show (friendlyNames x) ++ ", v=" ++ show (friendlyNames v) ++ ", w=" ++ show (friendlyNames w))))) :: [$(conT leName)] |]
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
