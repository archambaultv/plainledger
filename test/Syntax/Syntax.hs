module Syntax.Syntax (
  syntaxTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import qualified Data.ByteString as B
import Plainledger.Journal


journalPath :: String
journalPath = "test/Syntax/journal-syntax.yaml"

tmpPath :: String
tmpPath = "test/Syntax/tmp/journal-syntax2.yaml"

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "journal-syntax.yaml"
    [ testCase "Decode journal-syntax.yaml" $ do
       journal <- decodeFileEither journalPath :: IO (Either ParseException Journal)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode journal-syntax.yaml" $
       testCase "Decode encode decode idempotency" $ do
        journal <- decodeFileThrow journalPath :: IO Journal
        B.writeFile tmpPath $ YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeFileThrow tmpPath :: IO Journal
        journal @?= journal2
    ]
