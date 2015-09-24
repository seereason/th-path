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
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile)
import Data.List (sort)
import Data.Monoid ((<>))
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, S(S))
import Language.Haskell.TH.Path.Instances
import Language.Haskell.TH.Path.Lens
import Language.Haskell.TH.Path.Types
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Exit
import System.FilePath.Extra (compareSaveAndReturn, changeError)
import Test.HUnit
import Text.PrettyPrint (text)

import Common (depFiles, fixStringLits, stripNames)
import Appraisal.ReportItem
import Appraisal.ReportMap
import Appraisal.ReportPathInfo hiding (depFiles)

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
test02 = TestCase $ assertString (show (prettyContextDiff (text "expected") (text "actual") text (getContextDiff 2 expected02 actual02)))

actual02 :: [String]
actual02 =
    (lines . pprint . fixStringLits . stripNames)
       $(do -- We need to rebuild this module if any of the library
            -- source files change.  It is not sufficent to re-link the
            -- executable because the libraries affect the code generated
            -- in this template haskell splice.
            depFiles
            (Just dec) <- lookupTypeName "Dec"
            st <- runQ [t|ReportMap|]
            (decs1 :: [Dec]) <- evalStateT (pathTypes [st]) (S mempty mempty) >>= (runQ . runIO . compareSaveAndReturn changeError "GeneratedPathTypes.hs" . sort)
            (decs2 :: [Dec]) <- evalStateT (pathLenses [st]) (S mempty mempty) >>= (runQ . runIO . compareSaveAndReturn changeError "GeneratedPathLenses.hs" . sort)
            (decs3 :: [Dec]) <- evalStateT (pathInstances [st]) (S mempty mempty) >>= (runQ . runIO . compareSaveAndReturn changeError "GeneratedPathInstances.hs" . sort)
            lift (decs1 ++ decs2 ++ decs3))

expected02 :: [String]
expected02 = lines $ toString $(embedFile "tests/expected02")
