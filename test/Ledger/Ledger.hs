module Ledger.Ledger (
  ledgerTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import Plainledger.Ledger


dir :: String
dir = "test/Ledger/"

ledgerPath :: String
ledgerPath = "test/Ledger/ledger-syntax.yaml"

ledgerTestTree :: TestTree
ledgerTestTree = testGroup "Ledger tests"
              [syntaxTestTree,
               validationTestTree]

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "Syntax"
    [ testCase "Decode ledger-syntax.yaml" $ do
       journal <- decodeFileEither ledgerPath :: IO (Either ParseException Ledger)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode ledger-syntax.yaml" $
       testCase "Yaml encode decode" $ do
        journal <- decodeFileThrow ledgerPath :: IO Ledger
        let x = YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeThrow x
        journal @?= journal2
    ]

validationTestTree :: TestTree
validationTestTree =
   testGroup "Validation"
    [ testCase "ledger-syntax.yaml is valid" $ do
       ledger <- decodeFileThrow ledgerPath
       case validateLedger ledger of
         Left err -> assertFailure err
         Right _ -> return (),
      validationFailure "validate-group-field.yaml",
      validationFailure "validate-account-id.yaml"
    ]

validationFailure :: String -> TestTree
validationFailure file =
  testCase file $ do
     ledger <- decodeFileThrow
               (dir ++ file)
     case validateLedger ledger of
       Left _ -> return ()
       Right _ -> assertFailure
                  $ "Validation of \""
                  ++ file
                  ++ "\" should fail."
