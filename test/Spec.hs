module Main (main) where

import Test.Modules.GPS
import Test.QuickCheck

main :: IO ()
main = verboseCheck $ withMaxSuccess 1000 prop_Convert
