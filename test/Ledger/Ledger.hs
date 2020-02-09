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
               validationTestTree,
               csvTestTree]

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "Journal syntax"
    [ testCase "Decode ledger-syntax.yaml" $ do
       journal <- decodeFileEither ledgerPath :: IO (Either ParseException Journal)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode ledger-syntax.yaml" $
       testCase "Decode encode decode" $ do
        journal <- decodeFileThrow ledgerPath :: IO Journal
        let x = YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeThrow x
        journal @?= journal2
    ]

validationTestTree :: TestTree
validationTestTree =
   testGroup "Validation"
    [ testCase "ledger-syntax.yaml is a valid ledger" $ do
       journal <- decodeFileThrow ledgerPath
       case journalToLedger journal of
         Left err -> assertFailure err
         Right _ -> return (),
      testCase "Transaction without date" $ do
        let f = dir ++ "validate-transaction-no-date.yaml"
        journal <- decodeFileEither f :: IO (Either ParseException Journal)
        case journal of
          Left _ -> return ()
          Right _ -> assertFailure
                     $ "Validation of \""
                     ++ "validate-transaction-no-date.yaml"
                     ++ "\" should fail.",
      validationFailure "validate-group-field.yaml",
      validationFailure "validate-group-field-non-null.yaml",
      validationFailure "validate-default-commodity-non-null.yaml",
      validationFailure "validate-earnings-account-non-null.yaml",
      validationFailure "validate-opening-balance-non-null.yaml",
      validationFailure "validate-account-id-duplicate.yaml",
      validationFailure "validate-account-id-non-null.yaml",
      validationFailure "validate-transaction-id-dup.yaml"
    ]

csvTestTree :: TestTree
csvTestTree =
  testGroup "CSV"
    [ testCase "Accounts : encode decode" $ do
       l <- Y.decodeFileThrow ledgerPath :: IO Journal
       let encodeAcc = encodeAccounts $ lAccounts l
       csvAccounts <- either assertFailure return $ decodeAccounts encodeAcc
       (lAccounts l) @?= csvAccounts,

       testCase "Transactions : encode decode" $ do
          j <- Y.decodeFileThrow ledgerPath
          l <- either assertFailure return $ journalToLedger j
          let encodeAcc = encodeTransactions $ lTransactions l
          csvTransactions <- either assertFailure return $ decodeTransactions encodeAcc
          (lTransactions l) @?= csvTransactions
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

{--
validationTestTree :: TestTree
validationTestTree =
   testGroup "Validation"
    [

      testCase "transaction id" $ do
         journal <- decodeFileThrow (dir ++ "validate-transaction-id-new.yaml")
         case journalToLedger journal of
           Left err -> assertFailure err
           Right ledger ->
            let Transactions = lTransaction ledger
                nbTransactions = length Transactions
            in if nbTransactions /= 7
               then assertFailure
                     $ "Wrong number of Transactions. Got "
                     ++ show nbTransactions
                     ++ ".\n"
                     -- ++ show Transactions
               else traverse validateTransactionId Transactions >> return ()
    ]

validateTransactionId :: Transfer -> IO ()
validateTransactionId t =
  let tId = lookup "Transaction id" $ map tagToTuple $ tfTags t
      tAmount = pAmount t
  in case (tAmount, tId) of
       (_, Nothing) -> assertFailure "No Transaction id tag"
       (1, Just "2019-01-23-100") -> return ()
       (2, Just "2019-01-23-1") -> return ()
       (3, Just "2019-01-23-2") -> return ()
       (4, Just "2019-01-23-2") -> return ()
       (5, Just "2019-01-22-1") -> return ()
       (6, Just "2019-01-24-1") -> return ()
       (7, Just "2019-01-24-2") -> return ()
       (_, Just x) -> assertFailure
                 $ "Incorrect transaction id \""
                 ++ T.unpack x
                 ++ "\" for amount "
                 ++ show tAmount
                 ++ "."
-}
