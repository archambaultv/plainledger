module Journal.Journal (
  journalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except


journalTestTree :: TestTree
journalTestTree = testGroup "Journal tests"
              [journalFileTestTree]

journalFileTestTree :: TestTree
journalFileTestTree =
  testGroup "JournalFile"
    [ okConfig "Journal-01.csv" -- With BOM
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' ',' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"],
      okConfig "Journal-02.csv" -- With BOM
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" ',' ';' 1 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"],
      okConfig "Journal-03.csv" -- Without BOM
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' ',' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"],
      okConfig "Journal-04.csv" -- Without BOM
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company, Inc." '.' ',' 1 "comptes.csv" 
          ["transactions 1.csv", "transactions 2.csv", "transactions 3.csv"] 
          ["soldes 1.csv", "soldes 2.csv"],
      okConfig "Journal-05.csv" -- With BOM
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' '\t' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"],
      koConfig "Journal-06.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-06.csv" 1 0 ) InvalidHeaderJournalFile,
      koConfig "Journal-07.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-07.csv" 1 0 ) InvalidHeaderJournalFile,
      koConfig "Journal-08.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-08.csv" 2 2 ) 
        (EmptyFieldInJournalFile "Compte pour les soldes d'ouverture"),
      koConfig "Journal-09.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-09.csv" 0 0 ) 
        (MissingFieldinJournalFile "Compte pour les soldes d'ouverture")
    ]

okConfig :: String -> JournalFile -> TestTree
okConfig filename expectedJournal = 
  testCase ("Decode " ++ filename) $ do
       journal <- runExceptT $ decodeJournalFileIO ("test/Journal/JournalFile/" ++ filename)
       case journal of
         Left err -> assertFailure $ printError err
         Right actual -> assertEqual "" expectedJournal actual

koConfig :: String -> Error -> TestTree
koConfig filename expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       journal <- runExceptT $ decodeJournalFileIO ("test/Journal/JournalFile/" ++ filename)
       case journal of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

-- validationTestTree :: TestTree
-- validationTestTree =
--    testGroup "Validation"
--     [ validationOk,
--       validationFailure "transaction-no-date.csv" VTransaction,
--       validationFailure "transactions-duplicate-column.csv" VTransaction,
--       validationFailure "group-field.csv" VAccount,
--       validationFailure "group-field-non-null.csv" VAccount,
--       validationFailure "earnings-account-non-null.yaml" VConfig,
--       validationFailure "earnings-account-wrong-name.yaml" VConfig,
--       validationFailure "opening-balance-non-null.yaml" VConfig,
--       validationFailure "opening-balance-wrong-name.yaml" VConfig,
--       validationFailure "account-id-duplicate.csv" VAccount,
--       validationFailure "account-id-non-null.csv" VAccount,
--       validationFailure "balance-valid-account-id.csv" VBalance,
--       validationFailure "balance-assertions-wrong-amount.csv" VBalance,
--       validationFailure "trial-balance-assertions-wrong-amount.csv" VTrialBalance,
--       validationFailure "trial-balance-assertions-wrong-id.csv" VTrialBalance
--     ]

-- data JournalCsv = VTransaction | VBalance | VAccount | VConfig | VTrialBalance
--   deriving (Eq, Show)

-- validationFailure :: String -> JournalCsv -> TestTree
-- validationFailure file opt = testCase file $ do
--   journalFile <- decodeFileThrow journalPath :: IO JournalFile
--   let f = "KO/" ++ file
--   jf <- case opt of
--          VTransaction -> pure journalFile{jfTransactions = ["opening-balances.csv", f]}
--          VBalance -> pure journalFile{jfBalances=[f]}
--          VTrialBalance -> pure journalFile{jfTrialBalances=[f]}
--          VAccount -> pure journalFile{jfAccounts=[f]}
--          VConfig -> do
--                 c <- decodeFileThrow ("test/Journal/" ++ f) :: IO Configuration
--                 pure $ journalFile{jfConfiguration=c}
--   journal <- runExceptT $ journalFileToJournal journalPath jf
--   case journal >>= journalToLedger of
--     Left _ -> return ()
--     Right _ -> assertFailure
--                $ "Validation of \""
--                ++ file
--                ++ "\" should fail."

-- validationOk :: TestTree
-- validationOk = testCase "Journal.yaml is a valid ledger" $ do
--   journalFile <- decodeFileThrow journalPath :: IO JournalFile
--   journal <- runExceptT $ journalFileToJournal journalPath journalFile
--   case journal >>= journalToLedger of
--      Left err -> assertFailure err
--      Right _ -> return ()
