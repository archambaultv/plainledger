module Ledger.Syntax (
  syntaxTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import qualified Data.ByteString as B
import Plainledger.Ledger


journalPath :: String
journalPath = "test/Ledger/ledger-syntax.yaml"

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "Syntax test - Ledger"
    [ testCase "Decode ledger-syntax.yaml" $ do
       journal <- decodeFileEither journalPath :: IO (Either ParseException Ledger)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode ledger-syntax.yaml" $
       testCase "Encode decode idempotency" $ do
        journal <- decodeFileThrow journalPath :: IO Ledger
        let x = YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeThrow x
        journal @?= journal2
    ]
