module Main (main) where

import Test.Modules.GPS
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Dzerver"
    [ testProperty "Location GPS data conversion" prop_Convert
    ]
