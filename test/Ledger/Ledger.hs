module Ledger.Ledger (
  ledgerTestTree
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
      validationFailure "validate-transaction-id-dup.yaml",
      testCase "generate transaction id" $ do
         journal <- decodeFileThrow (dir ++ "validate-transaction-id-new.yaml")
         case journalToLedger journal of
           Left err -> assertFailure err
           Right ledger ->
            let transactions = lTransactions ledger
                nbTransactions = length transactions
            in if nbTransactions /= 6
               then assertFailure
                     $ "Wrong number of Transactions. Got "
                     ++ show nbTransactions
                     ++ ".\n"
                     -- ++ show Transactions
               else traverse validateTransactionId transactions >> return ()
    ]

csvTestTree :: TestTree
csvTestTree =
  testGroup "CSV"
    [ testCase "Accounts : encode decode" $ do
       l <- Y.decodeFileThrow ledgerPath :: IO Journal
       let encodeAcc = encodeAccounts $ lAccounts l
       csvAccounts <- either assertFailure return $ decodeAccounts encodeAcc
       (lAccounts l) @?= csvAccounts,

       testCase "JTransactions : encode decode single line" $ do
          j <- Y.decodeFileThrow ledgerPath
          let encodeAcc = encodeTransactions EncodeAsSingleRecord
                        $ lTransactions j
          csvTransactions <- either assertFailure return
                            $ decodeTransactions SingleRecord encodeAcc
          lTransactions j @?= csvTransactions,

     testCase "Transactions : encode decode single line" $ do
        j <- Y.decodeFileThrow ledgerPath
        l <- either assertFailure return $ journalToLedger j
        let txns = map transactionToJTransaction $ lTransactions l
        let encodeAcc = encodeTransactions EncodeAsSingleRecord txns
        csvTransactions <- either assertFailure return
                          $ decodeTransactions SingleRecord encodeAcc
        txns @?= csvTransactions,

      testCase "JTransactions : encode decode multiple lines" $ do
         j <- Y.decodeFileThrow (dir ++ "validate-csv-multiple-lines.yaml")
         let encodeAcc = encodeTransactions EncodeAsMultipleRecords
                       $ lTransactions j
         csvTransactions <- either assertFailure return
                           $ decodeTransactions MultipleRecords encodeAcc
         (sortBy (comparing tTransactionId) (lTransactions j)) @?=
           (sortBy (comparing tTransactionId) csvTransactions),

      testCase "JTransactions : no transaction single line" $ do
         j <- Y.decodeFileThrow (dir ++ "transactions-no-line.yaml")
         let encodeAcc = encodeTransactions EncodeAsSingleRecord
                       $ lTransactions j
         csvTransactions <- either assertFailure return
                           $ decodeTransactions SingleRecord encodeAcc
         lTransactions j @?= csvTransactions,

      csvValidationFailure "csv-multiple-lines-no-transaction-id.csv"
                           MultipleRecords,
      csvValidationFailure "csv-multiple-lines-different-tag-values.csv"
                           MultipleRecords
    ]

csvValidationFailure :: String -> CsvDecodeOptions -> TestTree
csvValidationFailure file opt =
  testCase file $ do
    csv <- BS.readFile (dir ++ file)
    let csvTransactions = decodeTransactions opt csv
    case csvTransactions of
       Left _ -> return ()
       Right _ -> assertFailure
                  $ "Csv decode of \""
                  ++ file
                  ++ "\" should fail."

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

validateTransactionId :: Transaction -> IO ()
validateTransactionId t =
  let tId = tTransactionId t
      amnt = fromJust $ lookup "id" $ map tagToTuple $ tTags t
  in case (amnt, tId) of
       (_, "") -> assertFailure "No Transaction id tag"
       ("1", "2019-01-23-100") -> return ()
       ("2", "2019-01-23-01") -> return ()
       ("3", "2019-01-22-01") -> return ()
       ("4", "2019-01-23-02") -> return ()
       ("5", "2019-01-24-1") -> return ()
       ("6", "2019-01-24-01") -> return ()
       (_, x) -> assertFailure
                 $ "Incorrect transaction id \""
                 ++ T.unpack x
                 ++ "\" for transaction "
                 ++ T.unpack amnt
                 ++ "."
