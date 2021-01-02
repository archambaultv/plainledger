module Journal.Journal.Journal (
  journalJournalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except


journalJournalTestTree :: TestTree
journalJournalTestTree =
   testGroup "Journal"
    [ validationOk "Journal-01",

      validationKO "Journal-02"
      $ mkError (SourcePos "test/Journal/Journal/Journal-02/TrialBalance.csv" 20 0) 
      $ WrongBalance "Revenu de location" (read "2018-12-31") (-100) (Just (-1000))
    ]


validationOk :: String -> TestTree
validationOk folder = testCase folder $ do
  let journalPath = "test/Journal/Journal/" ++ folder ++ "/Journal.csv"
  journal <- runExceptT 
          $ decodeJournalFileIO journalPath >>= journalFileToJournal
  case journal of
   Left err -> assertFailure $ printErrors err
   Right _ -> return ()

validationKO :: String -> Errors -> TestTree
validationKO folder expectedErr = testCase folder $ do
  let journalPath = "test/Journal/Journal/" ++ folder ++ "/Journal.csv"
  journal <- runExceptT 
          $ decodeJournalFileIO journalPath >>= journalFileToJournal
  case journal of
   Left err -> assertEqual "" expectedErr err
   Right _ -> assertFailure $ "Decoding " ++ folder ++ " should throw an error"