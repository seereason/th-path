-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- The generated toLens instances will have incomplete patterns where
-- we tried to generate a clause but we found no path to the goal type.

import Control.Monad.States (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.Monoid ((<>))
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, S(S))
import Language.Haskell.TH.Path.Instances
import Language.Haskell.TH.Path.Lens
import Language.Haskell.TH.Path.Types
import System.Exit
import System.FilePath.Extra (compareSaveAndReturn, changeError)
import Test.HUnit

import Common (fixStringLits, stripNames)
import Appraisal.ReportMap
import Appraisal.ReportPathInfo

main :: IO ()
main = do
  -- mapM_ (putStrLn . pprint) actual01
  r <- runTestTT (TestList [{-test01,-} test02])
  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r
{-
test01 :: Test
test01 = TestCase $ assertEqual "path types for TH.Type" expected01 actual01

actual01 :: [Dec]
actual01 =
    (fixStringLits . stripNames)
       $(do (Just dec) <- lookupTypeName "Dec"
            let st = sequence [runQ [t|Type|]]
            (decs1 :: [Dec]) <- evalStateT (execWriterT (pathTypes st)) (S mempty mempty)
            (decs2 :: [Dec]) <- evalStateT (execWriterT (pathLenses st)) (S mempty mempty)
            (decs3 :: [Dec]) <- evalStateT (execWriterT (pathInstances st)) (S mempty mempty)
            lift (decs1 ++ decs2 ++ decs3))

expected01 :: [Dec]
expected01 = []
-}
test02 :: Test
test02 = TestCase $ assertEqual "path types for ReportMap" expected02 actual02

actual02 :: [Dec]
actual02 =
    (fixStringLits . stripNames)
       $(do (Just dec) <- lookupTypeName "Dec"
            let st = sequence [runQ [t|ReportMap|]]
            (decs1 :: [Dec]) <- evalStateT (pathTypes st) (S mempty mempty) >>= (runQ . runIO . compareSaveAndReturn changeError "GeneratedPathTypes.hs")
            (decs2 :: [Dec]) <- evalStateT (pathLenses st) (S mempty mempty) >>= (runQ . runIO . compareSaveAndReturn changeError "GeneratedPathLenses.hs")
            (decs3 :: [Dec]) <- evalStateT (pathInstances st) (S mempty mempty) >>= (runQ . runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs")
            lift (decs1 ++ decs2 ++ decs3))

expected02 :: [Dec]
expected02 = []
