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

import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile)
import Language.Haskell.TH
import System.Exit
import Test.HUnit
import Text.PrettyPrint (text)

import Appraisal.ReportTH

main :: IO ()
main = do
  -- mapM_ (putStrLn . pprint) actual01
  writeFile "tests/actual.hs" (unlines (map pprint decs))
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

actual02 :: [String]
actual02 = (lines . unlines . map pprint) decs

test02 :: Test
test02 = TestCase $ assertString (show (prettyContextDiff (text "expected") (text "actual") text (getContextDiff 2 expected02 actual02)))
{-
actual02 :: [String]
actual02 =
    (lines . pprint . fixStringLits . stripNames)
       $(do -- We need to rebuild this module if any of the library
            -- source files change.  It is not sufficent to re-link the
            -- executable because the libraries affect the code generated
            -- in this template haskell splice.
            depFiles
            let save path = (runQ . runIO . compareSaveAndReturn changeError (pprint . friendlyNames) path . sortBy (compare `on` (show . friendlyNames)))
            (Just dec) <- lookupTypeName "Dec"
            st <- runQ [t|ReportMap|]
            (decs1 :: [Dec]) <- runTypeGraphT pathTypes [st] >>= save "ReportPathTypes.hs"
            (decs2 :: [Dec]) <- runTypeGraphT pathLenses [st] >>= save "ReportPathLenses.hs"
            (decs3 :: [Dec]) <- runTypeGraphT pathInstances [st] >>= save "ReportPathInstances.hs"
            lift (decs1 ++ decs2 ++ decs3))
-}
expected02 :: [String]
expected02 = (lines . toString) $(embedFile "tests/expected.hs")
