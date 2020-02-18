module Journal.Journal (
  journalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import Plainledger.Ledger


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
        journal <- journalFileToJournal journalPath journalFile
        journal2 <- decodeFileThrow (dir ++ "journal-no-includes.yaml") :: IO Journal
        (jConfiguration journal) @?= (jConfiguration journal2)
        (jAccounts journal) @?= (jAccounts journal2)
        (jTransactions journal) @?= (jTransactions journal2)
        (jBalances journal) @?= (jBalances journal2)
    ]
