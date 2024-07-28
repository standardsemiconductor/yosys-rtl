module Main (main) where

import Paths_yosys_rtl
import Test.Tasty
import qualified Test.Yosys.Rtl

main :: IO ()
main = do
  dataDir <- getDataDir
  defaultMain $ testGroup "Test.Yosys"
    [ testGroup "Rtl" $ Test.Yosys.Rtl.tests dataDir
    ]
