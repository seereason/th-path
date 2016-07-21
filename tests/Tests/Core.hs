module Tests.Core where

import Control.Monad.State
import Language.Haskell.TH
import Language.Haskell.TH.Lift as TH (lift)
import Language.Haskell.TH.Path
import Test.HUnit
import Tests.Instances ()

core :: Test
core = TestList [view1 {-, view2-} ]

view1 :: Test
view1 =
    TestCase $ do
      -- In Tests.Instances there is an instance View Int with type
      -- ViewType Int = Integer.  So when we see an Int we use the
      -- lens returned by viewLens to turn it into an Integer.
      let expected = $([t|Integer|] >>= TH.lift) :: Type
      let actual = $(let t = [t|Int|] in t >>= viewInstanceType >>= maybe t pure >>= TH.lift) :: Type
      liftIO $ assertEqual "view1" expected actual

{-
-- Works, but instance conflicts with instance View String = JSONText
view2 :: Test
view2 =
    TestCase $ do
      -- instance View String where
      --     type ViewType String = Text
      let expected = $([t|Text|] >>= TH.lift) :: Type
      let actual = $(let t = [t|[Char]|] in t >>= viewInstanceType >>= maybe t pure >>= TH.lift) :: Type
      liftIO $ assertEqual "view2 - TypeSynonymInstance" expected actual
-}
