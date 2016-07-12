{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeSynonymInstances #-}
module Tests.Instances where

import Control.Lens
import Data.Text.Lazy
import Language.Haskell.TH.Path.View

instance View Int where
    type ViewType Int = Integer
    viewLens = iso (toInteger :: Int -> Integer) (fromInteger :: Integer -> Int)

{-
instance View String where
    type ViewType String = Text
    viewLens = iso (pack :: String -> Text) (unpack :: Text -> String)
-}
