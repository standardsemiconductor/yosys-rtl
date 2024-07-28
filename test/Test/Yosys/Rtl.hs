{-# LANGUAGE OverloadedStrings #-}

module Test.Yosys.Rtl
  ( tests
  ) where

import Data.String
import Data.Text (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Prettyprinter
import Prettyprinter.Render.Text
import System.Exit
import System.FilePath
import System.IO.Extra
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import Yosys.Rtl

tests :: FilePath -> [TestTree]
tests dataDir =
  [ testGroup "pretty"
    [ prettyTest' dataDir "led" rtlLed
    , prettyTest' dataDir "add" $
        addC
          "\\adder"
          False
          32
          False
          32
          33
          (SigSpecWireId "\\a")
          (SigSpecWireId "\\b")
          (SigSpecWireId "\\y")
    , let three :: Value
          three = Value 2 [B1, B1]
      in testGroup "Value"
           [ prettyUnitTest "Pretty Three" three "2'11"
           , fromStringTest "String Three" "2'11" three
           ]
    , let four :: Constant
          four = ConstantValue $ Value 3 [B1, B0, B0]
      in testGroup "Constant"
           [ prettyUnitTest "Pretty Four" four "3'100"
           , fromStringTest "String Four" "3'100" four
           ]
    , let five :: SigSpec
          five = SigSpecConstant $ ConstantValue $ Value 3 [B1, B0, B1]
      in testGroup "SigSpec"
           [ prettyUnitTest "Pretty Five" five "3'101"
           , fromStringTest "String Five" "3'101" five
           ]
    ]
  , testGroup "synth"
    [ synthTest "led" rtlLed
    ]
  ]

render :: Doc ann -> Text
render = renderStrict . layoutSmart defaultLayoutOptions

prettyTest :: Pretty a => FilePath -> TestName -> a -> TestTree
prettyTest curDir n = goldenVsString n (curDir </> n <.> "pretty")
                        . return . fromString . T.unpack . render . pretty

prettyTest' :: Pretty a => FilePath -> TestName -> a -> TestTree
prettyTest' dataDir = prettyTest $ dataDir </> "Yosys" </> "Rtl" </> "golden"

prettyUnitTest :: Pretty p => TestName -> p -> Text -> TestTree
prettyUnitTest n p t = testCase n $ (render . pretty) p @?= t

synthTest :: TestName -> File -> TestTree
synthTest n rtl = testCase n $ withTempFile $ \t -> do
  TIO.writeFile t $ render $ pretty rtl
  let c = "yosys -q -p \"synth_ice40\" -f rtlil " <> t
  (ExitSuccess @=?) =<< waitForProcess =<< spawnCommand c

fromStringTest
  :: Eq a
  => IsString a
  => Show a
  => TestName
  -> String
  -> a          -- ^ expected
  -> TestTree
fromStringTest n s a = testCase n $ fromString s @?= a

rtlLed :: File
rtlLed = File Nothing
  [ Module
      []
      "\\top"
      [ ModuleBodyWire $ Wire [] $ WireStmt [WireOptionInput  1] "\\clk"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 2] "\\LED_R"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 3] "\\LED_G"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionOutput 4] "\\LED_B"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 26] "\\counter"
      , ModuleBodyWire $ Wire [] $ WireStmt [WireOptionWidth 32] "\\counter_plus_one"
      , ModuleBodyCell $ addC "$increment" False 26 False 32 32
          (SigSpecWireId "\\counter")
          (SigSpecConstant $ ConstantInteger 1)
          (SigSpecWireId "\\counter_plus_one")
      , ModuleBodyCell $ notC "$not$1" False 1 1
          (SigSpecSlice (SigSpecWireId "\\counter") 23 Nothing)
          (SigSpecWireId "\\LED_R")
      , ModuleBodyCell $ notC "$not$2" False 1 1
          (SigSpecSlice (SigSpecWireId "\\counter") 24 Nothing)
          (SigSpecWireId "\\LED_G")
      , ModuleBodyCell $ notC "$not$3" False 1 1
          (SigSpecSlice (SigSpecWireId "\\counter") 25 Nothing)
          (SigSpecWireId "\\LED_B")
      , ModuleBodyProcess $ Process
          []
          "$run"
          (ProcessBody
             []
             Nothing
             []
             [ Sync
                 (SyncStmt Posedge (SigSpecWireId "\\clk"))
                 [ UpdateStmt
                     (DestSigSpec $ SigSpecWireId "\\counter")
                     (SrcSigSpec $ SigSpecSlice
                        (SigSpecWireId "\\counter_plus_one")
                        25
                        (Just 0)
                     )
                 ]
             ]
          )
          ProcEndStmt
      ]
      ModuleEndStmt
  ]
