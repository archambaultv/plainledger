module Journal.Journal (
  journalTestTree
  )where

import Data.Maybe
import Data.List
import Data.Ord
import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import Plainledger.Ledger
import Plainledger.Journal
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS


dir :: String
dir = "test/Journal/"

journalPath :: String
journalPath = "test/Journal/journal-syntax.yaml"

journalTestTree :: TestTree
journalTestTree = testGroup "Journal tests"
              [syntaxTestTree]

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "Journal syntax"
    [ testCase "Decode ledger-syntax.yaml" $ do
       journal <- decodeFileEither journalPath :: IO (Either ParseException JournalFile)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode ledger-syntax.yaml" $
       testCase "Decode encode decode" $ do
        journal <- decodeFileThrow journalPath :: IO JournalFile
        let x = YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeThrow x
        journal @?= journal2,
      after AllSucceed "Decode ledger-syntax.yaml" $
       testCase "Includes" $ do
        journalFile <- decodeFileThrow journalPath :: IO JournalFile
        journal <- journalFileToJournal journalFile
        journal2 <- decodeFileThrow (dir ++ "journal-no-includes.yaml") :: IO Journal
        journal @?= journal2
    ]
