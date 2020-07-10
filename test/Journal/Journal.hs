module Journal.Journal (
  journalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Yaml.Pretty as YP
import Plainledger.Ledger
import Control.Monad.Except

journalPath :: String
journalPath = "test/Journal/Journal.yaml"

journalTestTree :: TestTree
journalTestTree = testGroup "Journal tests"
              [syntaxTestTree, validationTestTree]

syntaxTestTree :: TestTree
syntaxTestTree =
  testGroup "Journal syntax"
    [ testCase "Decode Journal.yaml" $ do
       journal <- decodeFileEither journalPath :: IO (Either ParseException JournalFile)
       case journal of
         Left err -> assertFailure $ prettyPrintParseException err
         Right _ -> return (),
      after AllSucceed "Decode Journal.yaml" $
       testCase "Decode encode decode" $ do
        journal <- decodeFileThrow journalPath :: IO JournalFile
        let x = YP.encodePretty yamlPrettyConfig journal
        journal2 <- decodeThrow x
        journal @?= journal2,
      after AllSucceed "Decode Journal.yaml" $
       testCase "journalFileToJournal" $ do
        journalFile <- decodeFileThrow journalPath :: IO JournalFile
        j <- runExceptT $ journalFileToJournal journalPath journalFile
        case j of
          Left err -> assertFailure err
          Right _ -> return ()
    ]

validationTestTree :: TestTree
validationTestTree =
   testGroup "Validation"
    [ validationOk,
      validationFailure "transaction-no-date.csv" VTransaction,
      validationFailure "transactions-duplicate-column.csv" VTransaction,
      validationFailure "group-field.csv" VAccount,
      validationFailure "group-field-non-null.csv" VAccount,
      validationFailure "earnings-account-non-null.yaml" VConfig,
      validationFailure "opening-balance-non-null.yaml" VConfig,
      validationFailure "account-id-duplicate.csv" VAccount,
      validationFailure "account-id-non-null.csv" VAccount,
      validationFailure "balance-valid-account-id.csv" VAccount
    ]

data JournalCsv = VTransaction | VBalance | VAccount | VConfig
  deriving (Eq, Show)

validationFailure :: String -> JournalCsv -> TestTree
validationFailure file opt = testCase file $ do
  journalFile <- decodeFileThrow journalPath :: IO JournalFile
  let f = "KO/" ++ file
  jf <- case opt of
         VTransaction -> pure journalFile{jfTransactions = ["opening-balances.csv", f]}
         VBalance -> pure journalFile{jfBalances=[f]}
         VAccount -> pure journalFile{jfAccounts=[f]}
         VConfig -> do
                c <- decodeFileThrow ("test/Journal/" ++ f) :: IO Configuration
                pure $ journalFile{jfConfiguration=c}
  journal <- runExceptT $ journalFileToJournal journalPath jf
  case journal >>= journalToLedger of
    Left _ -> return ()
    Right _ -> assertFailure
               $ "Validation of \""
               ++ file
               ++ "\" should fail."

validationOk :: TestTree
validationOk = testCase "Journal.yaml is a valid ledger" $ do
  journalFile <- decodeFileThrow journalPath :: IO JournalFile
  journal <- runExceptT $ journalFileToJournal journalPath journalFile
  case journal >>= journalToLedger of
     Left err -> assertFailure err
     Right _ -> return ()
