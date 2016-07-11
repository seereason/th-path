{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Tests.Instances where

import Control.Lens
import Control.Monad.State
import Data.Maybe
import Data.Text.Lazy
import Language.Haskell.TH
import Language.Haskell.TH.Lift as TH (lift)
import Language.Haskell.TH.Path.Core
import Language.Haskell.TH.Path.View
import Test.HUnit

instance View Int where
    type ViewType Int = Integer
    viewLens = iso (toInteger :: Int -> Integer) (fromInteger :: Integer -> Int)

instance View String where
    type ViewType String = Text
    viewLens = iso (pack :: String -> Text) (unpack :: Text -> String)
