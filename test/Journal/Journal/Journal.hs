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
      $ WrongBalance "Revenu de location" (read "2018-12-31") (-100) (Just (-1000))
    ]


validationOk :: String -> TestTree
validationOk folder = testCase folder $ do
 let journalPath = "test/Journal/Journal/" ++ folder ++ "/Journal.csv"
 header <- runExceptT $ processJournalFileHeader journalPath
 case header of
   Left err -> assertFailure $ printErr err
   Right x -> do
    journal <- runExceptT 
              $ decodeJournalFile journalPath x
              >>= journalFileToJournal
    case journal of
     Left err -> assertFailure $ printErr err
     Right _ -> return ()

validationKO :: String -> Errors -> TestTree
validationKO folder expectedErr = testCase folder $ do
 let journalPath = "test/Journal/Journal/" ++ folder ++ "/Journal.csv"
 header <- runExceptT $ processJournalFileHeader journalPath
 case header of
   Left err -> assertEqual "" expectedErr err
   Right x -> do
    journal <- runExceptT 
              $ decodeJournalFile journalPath x
              >>= journalFileToJournal
    case journal of
     Left err -> assertEqual "" expectedErr err
     Right _ -> assertFailure $ "Decoding " ++ folder ++ " should throw an error"

printErr :: [Error] -> String
printErr err = T.unpack
                       $ printErrors
                       $ map (i18nText En_CA . TError ) err