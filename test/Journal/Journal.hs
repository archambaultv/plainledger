module Journal.Journal (
  journalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import Plainledger.Ledger
import Plainledger.Journal


dir :: String
dir = "test/Journal/"

journalPath :: String
journalPath = "test/Journal/journal-syntax.yaml"

journalTestTree :: TestTree
journalTestTree = testGroup "Journal tests"
              [syntaxTestTree,
               validationTestTree]

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "Syntax"
    [ testCase "Decode journal-syntax.yaml" $ do
       journal <- decodeFileEither journalPath :: IO (Either ParseException Journal)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode journal-syntax.yaml" $
       testCase "Yaml encode decode" $ do
        journal <- decodeFileThrow journalPath :: IO Journal
        let x = YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeThrow x
        journal @?= journal2
    ]

validationTestTree :: TestTree
validationTestTree =
   testGroup "Validation"
    [ testCase "journal-syntax.yaml is valid" $ do
       ledger <- decodeFileThrow journalPath
       case journalToLedger ledger of
         Left err -> assertFailure err
         Right _ -> return (),
      validationFailure "validate-transaction-id-dup.yaml",
      validationFailure "validate-transaction-transfer-no-date.yaml"
    ]

validationFailure :: String -> TestTree
validationFailure file =
  testCase file $ do
     journal <- decodeFileThrow
               (dir ++ file)
     case journalToLedger journal of
       Left _ -> return ()
       Right _ -> assertFailure
                  $ "Validation of \""
                  ++ file
                  ++ "\" should fail."
