module Journal.Journal (
  journalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
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
       journal <- decodeFileThrow journalPath
       case journalToLedger journal of
         Left err -> assertFailure err
         Right _ -> return (),
      validationFailure "validate-transaction-id-dup.yaml",
      validationFailure "validate-transaction-transfer-no-date.yaml",
      testCase "transaction id" $ do
         journal <- decodeFileThrow (dir ++ "validate-transaction-id-new.yaml")
         case journalToLedger journal of
           Left err -> assertFailure err
           Right ledger ->
            let transfers = lTransfers ledger
                nbTransfers = length transfers
            in if nbTransfers /= 7
               then assertFailure
                     $ "Wrong number of transfers. Got "
                     ++ show nbTransfers
                     ++ ".\n"
                     -- ++ show transfers
               else traverse validateTransactionId transfers >> return ()
    ]

validateTransactionId :: Transfer -> IO ()
validateTransactionId t =
  let tId = lookup "Transaction id" $ map tagToTuple $ tfTags t
      tAmount = tfAmount t
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
