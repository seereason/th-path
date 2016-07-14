{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Tests.MakePath (testMakePath) where

import Control.Lens (view, _Left, _Right)
import Control.Monad.Trans (liftIO)
import Data.Map (Map, (!))
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Lift as TH (lift)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.Graph
import Language.Haskell.TH.Path.View
import Language.Haskell.TH.Syntax (qReify, Loc(..), CharPos)
import Language.Haskell.TH.TypeGraph.Prelude (pprint1)
import Test.HUnit
import Tests.Instances
import TestPaths
-- import ReportPaths
-- import Appraisal.Markup
-- import Appraisal.Report (Author(..))

testMakePath :: Test
testMakePath = TestList [ TestLabel "fieldTest" fieldTest
                        , TestLabel "viewTest" viewTest
                        , TestLabel "fstTest1" fstTest1
                        , TestLabel "aOfS1" aOfS1
                        , TestLabel "aOfS2" aOfS2
                        , TestLabel "aOfS3" aOfS3
                        , TestLabel "pOfS1" pOfS1
{-
                        , fstTest2
                        , sndTest
                        , justTest
                        , nothingTest
                        , leftTest
                        , rightTest
                        , TestList mapTests
-}
                        ]

-- | What do we get when we reify a field name?
fieldTest :: Test
fieldTest = TestCase $ let -- expected = "loc_start :: Loc -> CharPos"
                           expected = VarI 'loc_start (AppT (AppT ArrowT (ConT ''Loc)) (ConT ''CharPos)) Nothing (Fixity 9 InfixL)
                           actual = $(qReify 'loc_start >>= TH.lift) in
                       assertEqual "fieldTest" expected actual

viewTest :: Test
viewTest = TestCase $ let expected = $([t|Integer|] >>= TH.lift) :: Type
                          actual = $(let t = [t|Int|] in t >>= viewInstanceType >>= maybe t pure >>= TH.lift) in
                      assertEqual "viewTest" expected actual

-- Build a path to the first element of a 2-tuple.
fstTest1 :: Test
fstTest1 =
    TestCase $ let expected = "Path_First (idPath :: UPath Univ Double)"
                   actual = $(makePath [t|Univ|] [t|(Double, Char)|] [|\x -> fst x|] >>= TH.lift . pprint1) in
               assertEqual "fstTest" expected actual

aOfS1 :: Test
aOfS1 = TestCase $ do let actual = $(aOfS [t|Univ|] [|\x -> snd (x ! 5)|] [t|Map Int (Int, Char)|] >>= TH.lift)
                      expected <- runQ $ (,) <$> [t|Char|] <*> [|Path_Look 5 (Path_Second idPath)|]
                      assertEqual "aOfS1" expected actual

aOfS2 :: Test
aOfS2 = TestCase $ do let actual = $(aOfS [t|Univ|] [|\x -> snd (loc_end x)|] [t|Loc|] >>= TH.lift)
                      expected <- runQ $ (,) <$> [t|Int|] <*> [|UPath_Loc_loc_end (Path_Second idPath)|]
                      assertEqual "aOfS2" expected actual

aOfS3 :: Test
aOfS3 = TestCase $ do let actual = $(aOfS [t|Univ|] [|\x -> snd (loc_end (view _Left x))|] [t|Either Loc Double|] >>= TH.lift)
                      expected <- runQ $ (,) <$> [t|Int|] <*> [|Path_Left (UPath_Loc_loc_end (Path_Second idPath))|]
                      assertEqual "aOfS3" expected actual

pOfS1 :: Test
pOfS1 = TestCase $ do let expected :: Path_Map Integer (Path_Pair UPath_Int UPath_Int)
                          expected = Path_Look 5 (Path_Second (idPath :: UPath_Int))
                          actual :: Path_Map Integer (Path_Pair UPath_Int UPath_Int)
                          actual = $(let exp = [|\x -> snd (x ! 5)|]
                                         stype = [t|Map Int (Int, Char)|] in
                                     snd <$> aOfS [t|Univ|] exp stype)
                      assertEqual "pOfS1" expected actual

{-
fstTest2 :: Test
fstTest2 =
    TestCase $ let expected = ""
                   actual = $(makePath [t|Univ|] [t|(Int, (Char, Float))|] [|fst (snd id)|] >>= TH.lift . pprint) in
               assertEqual "fstTest" expected actual

sndTest :: Test
sndTest =
    TestCase $ let expected = "runQ [|Path_Second idPath|]"
                   actual = $(makePath [t|Univ|] [t|(Int, Char)|] [|snd id|] >>= TH.lift . pprint) in
               assertEqual "sndTest" expected actual

justTest :: Test
justTest =
    TestCase $ let expected = "runQ [|Path_Just idPath|]"
                   actual = $(makePath [t|Univ|] [t|Maybe Int|] [|Just id|] >>= TH.lift . pprint) in
               assertEqual "justTest" expected actual

-- There are no paths into a Nothing, test the behavior of
-- Path_Maybe, which is an idPath.
nothingTest :: Test
nothingTest =
    TestCase $ let expected = "runQ [|idPath|]"
                   actual = $(makePath [t|Univ|] [t| Maybe Int|] [|id|] >>= TH.lift . pprint) in
               assertEqual "nothingTest" expected actual

leftTest :: Test
leftTest =
    TestCase $ let expected = "runQ [|Path_Left idPath|]"
                   actual = $(makePath [t|Univ|] [t|Either Int Char|] [|Left id|] >>= TH.lift . pprint) in
               assertEqual "leftTest" expected actual

rightTest :: Test
rightTest =
    TestCase $ let expected = "runQ [|Path_Right idPath|]"
                   actual = $(makePath [t|Univ|] [t|Either Int Char|] [|Right id|] >>= TH.lift . pprint) in
               assertEqual "rightTest" expected actual

mapTests :: [Test]
mapTests =
    [TestCase $ let expected = "runQ [|Path_Look 5 idPath|]"
                    actual = $(makePath [t|Univ|] [t|Map Int String|] [|id ! 5|] >>= TH.lift . pprint) in
                assertEqual "mapTest1" expected actual,
     TestCase $ let expected = "runQ [|Path_First (Path_Look 5 idPath)|]"
                    actual = $(makePath [t|Univ|] [t|Map Int (Char, String)|] [|fst (id ! 5)|] >>= TH.lift . pprint) in
                assertEqual "mapTest2" expected actual,
     TestCase $ let expected = "runQ [|Path_First (Path_Look "foo" idPath)|]"
                    actual = $(makePath [t|Univ|] [t|Map String Int|] [|fst (id ! "foo")|] >>= TH.lift . pprint) in
                assertEqual "mapTest3" expected actual]
-}
