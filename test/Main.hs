module Main (main) where

import Test.Tasty
import qualified Test.Yosys.Rtl

main :: IO ()
main = defaultMain $ testGroup "Test.Yosys"
  [ testGroup "Rtl" Test.Yosys.Rtl.tests
  ]
