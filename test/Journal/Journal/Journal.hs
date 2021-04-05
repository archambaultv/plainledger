module Journal.Journal.Journal (
  journalJournalTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Plainledger.I18n.I18n
import qualified Data.Text as T

journalJournalTestTree :: TestTree
journalJournalTestTree =
   testGroup "Journal"
    [ validationOk "Journal-01",

      validationKO "Journal-02"
      $ mkError (SourcePos "test/Journal/Journal/Journal-02/TrialBalance.csv" 20 0) 
      $ WrongBalance "Revenu de location" (read "2018-12-31") (-100) (Just (-1000)),

      validationKO "Journal-03"
      $ mkErrorMultiPos 
        [SourcePos "test/Journal/Journal/Journal-03/StatementBalance.csv" 2 0,
         SourcePos "test/Journal/Journal/Journal-03/StatementBalance.csv" 3 0]
        (DuplicateBalance (read "2018-01-31") "Compte chèque")
    ]

validationOk :: String -> TestTree
validationOk folder = testCase folder $ do
 let journalPath = "test/Journal/Journal/" ++ folder ++ "/Journal.csv"
 journal <- runExceptT $ decodeJournal journalPath
 case journal of
   Left (_, err) -> assertFailure $ printErr err
   Right _ -> return ()

validationKO :: String -> Errors -> TestTree
validationKO folder expectedErr = testCase folder $ do
 let journalPath = "test/Journal/Journal/" ++ folder ++ "/Journal.csv"
 journal <- runExceptT $ decodeJournal journalPath
 case journal of
   Left (_, err) -> assertEqual "" expectedErr err
   Right _ -> assertFailure $ "Decoding " ++ folder ++ " should throw an error"

printErr :: [Error] -> String
printErr err = T.unpack
             $ printErrors
             $ map (i18nText En_CA . TError ) err