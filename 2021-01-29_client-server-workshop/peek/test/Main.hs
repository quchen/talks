{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.QuickCheck
import qualified Data.Text as T

import Lib

main :: IO ()
main = do
    quickCheck (\x -> x == not (not x))
    quickCheck (\str -> reverse (reverse str) == (str :: String))
    quickCheck (\action -> deserialize (serialize action) == Right action)

instance Arbitrary ClientAction where
    arbitrary = oneof
        [ pure Quit
        , do msg <- arbitrary
             pure (Message (T.pack msg))
        ]
