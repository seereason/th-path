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

import Appraisal.Report
import Appraisal.ReportInstances
import Appraisal.ReportMap
import Control.Lens (Lens')
import Control.Monad.Readers (MonadReaders)
import Data.Algorithm.DiffContext (getContextDiff, prettyContextDiff)
import Data.ByteString.UTF8 (toString)
import Data.FileEmbed (embedFile)
import Data.List (sort)
import Data.Tree
import Debug.Trace
import Editor (editor)
import Language.Haskell.TH
import Language.Haskell.TH.Context (ContextM)
import Language.Haskell.TH.Path.Core (IsPathType(idPath))
import Language.Haskell.TH.Path.Graph (runTypeGraphT)
import Language.Haskell.TH.TypeGraph.Prelude (friendlyNames)
import Language.Haskell.TH.TypeGraph.TypeInfo (startTypes, TypeInfo)
import Language.Haskell.TH.TypeGraph.TypeGraph (adjacent, TypeGraph, typeGraphVertex)
import System.Exit
import Test.HUnit
import Text.PrettyPrint (text)

import Appraisal.ReportTH (decs)
import Report (report)
import ReportPaths

main :: IO ()
main = do
  -- mapM_ (putStrLn . pprint) actual01
  writeFile "tests/actual.hs" actual02
  r <- runTestTT (TestList [test02, test03])
  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r
    where
      actual02 :: String
      actual02 = (unlines . map (pprint . friendlyNames) . sort) decs

      expected02 :: String
      expected02 = toString $(embedFile "tests/expected.hs")

      test02 :: Test
      test02 = TestCase $ assertString $ show $ prettyContextDiff (text "expected") (text "actual") text (getContextDiff 2 (lines expected02) (lines actual02))

      -- Convert a report into a tree of PVs, used to implement an editor.
      actual03 :: Tree PV_Report
      actual03 = $(do (exp :: Exp) <- runQ [t|ReportMap|] >>= runTypeGraphT (editor ''Report [|Report.report|]) . (: [])
                      trace ("exp: " ++ pprint (friendlyNames exp)) (return ())
                      return exp
                  )

      expected03 :: Tree PV_Report
      expected03 = Node (PV_Report_Report idPath Report.report) []

      test03 :: Test
      test03 = TestCase $ assertEqual "editor" expected03 actual03
      -- test03 = TestCase $ assertString $ show $ prettyContextDiff (text "expected") (text "actual") text (getContextDiff 2 (lines expected03) (lines actual03))
