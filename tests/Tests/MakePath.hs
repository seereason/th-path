module Tests.MakePath (testMakePath) where

import Data.Map ((!))
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Lift as TH (lift)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.View
import Test.HUnit
import Tests.Instances
import ReportDecs
import Appraisal.Report (Author(..))

testMakePath :: Test
testMakePath = TestList [ fstTest1
                        , fstTest2
                        , sndTest
                        , justTest
                        , nothingTest
                        , leftTest
                        , rightTest
                        , TestList mapTests
                        , viewTest
                        , fieldTest
                        ]

-- Build a path to the first element of a 2-tuple.
fstTest1 :: Test
fstTest1 =
    TestCase $ do expected <- runQ [|Path_First idPath|]
                  actual <- runQ ([|fst id|] >>= makePath)
                  assertEqual "fstTest" expected actual

fstTest2 :: Test
fstTest2 =
    TestCase $ do expected <- runQ [|Path_First (Path_Second idPath)|]
                  actual <- runQ ([|fst (snd id)|] >>= makePath)
                  assertEqual "fstTest" expected actual

sndTest :: Test
sndTest =
    TestCase $ do expected <- runQ [|Path_Second idPath|]
                  actual <- runQ ([|snd id|] >>= makePath)
                  assertEqual "sndTest" expected actual

justTest :: Test
justTest =
    TestCase $ do expected <- runQ [|Path_Just idPath|]
                  actual <- runQ ([|Just id|] >>= makePath)
                  assertEqual "justTest" expected actual

-- There are no paths into a Nothing, test the behavior of
-- Path_Maybe, which is an idPath.
nothingTest :: Test
nothingTest =
    TestCase $ do expected <- runQ [|idPath|]
                  actual <- runQ ([|id|] >>= makePath)
                  assertEqual "nothingTest" expected actual

leftTest :: Test
leftTest =
    TestCase $ do expected <- runQ [|Path_Left idPath|]
                  actual <- runQ ([|Left id|] >>= makePath)
                  assertEqual "leftTest" expected actual

rightTest :: Test
rightTest =
    TestCase $ do expected <- runQ [|Path_Right idPath|]
                  actual <- runQ ([|Right id|] >>= makePath)
                  assertEqual "rightTest" expected actual

mapTests :: [Test]
mapTests =
    [TestCase $ do expected <- runQ [|Path_Look 5 idPath|]
                   actual <- runQ ([|id ! 5|] >>= makePath)
                   assertEqual "mapTest1" expected actual,
     TestCase $ do expected <- runQ [|Path_First (Path_Look 5 idPath)|]
                   actual <- runQ ([|fst (id ! 5)|] >>= makePath)
                   assertEqual "mapTest2" expected actual,
     TestCase $ do expected <- runQ [|Path_First (Path_Look "foo" idPath)|]
                   actual <- runQ ([|fst (id ! "foo")|] >>= makePath)
                   assertEqual "mapTest3" expected actual]

viewTest :: Test
viewTest = TestCase $ let expected = $([t|Integer|] >>= TH.lift) :: Type
                          actual = $(let t = [t|Int|] in t >>= viewInstanceType >>= maybe t pure >>= TH.lift) in
                      assertEqual "viewTest" expected actual

fieldTest :: Test
fieldTest = TestCase $ let expected = ""
                           actual = show $(reify 'authorName >>= TH.lift) in
                       assertEqual "fieldTest" expected actual
