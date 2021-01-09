-- |
-- Module      :  Report.Transactions.Transactions
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type


module Report.Transactions.Transactions
(
  transactionsTestTree
)
where

import Data.List
import Data.Time
import Data.Char (ord)
import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Plainledger.I18n.I18n
import Plainledger.Report
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Csv as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

transactionsTestTree :: TestTree
transactionsTestTree =
   testGroup "Transactions"
    [ runReportOk "Journal-01" AllDates Nothing SingleCsvRecord 
      (read "2020-01-01") "Transactions 01.csv",
      runReportOk "Journal-01" (CalendarYear 0) Nothing SingleCsvRecord 
      (read "2018-01-01") "Transactions 02.csv",
      
      transactionEncodeDecode "Journal-01"
    ]

runReportOk :: String -> 
               ReportPeriod -> 
               (Maybe CompareAnotherPeriod) -> 
               TransactionCsvRecordType ->
               Day ->
               String ->
               TestTree
runReportOk folder period compareC lineType today actualTransactions = 
  testCase ("Report for " ++ folder) $ do
     (ledger, txns) <- getTransactionReport period compareC lineType folder today

     let csvSeparator = jfCsvSeparator $ lJournalFile ledger
     let myOptions = C.defaultEncodeOptions {
                       C.encDelimiter = fromIntegral (ord csvSeparator)
                     }
     let csvBS = C.encodeWith myOptions $ V.toList txns
     actualBS <- BS.readFile ("test/Report/Transactions/" ++ actualTransactions)
     assertEqual "" (BL.fromStrict actualBS) csvBS

transactionEncodeDecode ::  String -> 
                            TestTree
transactionEncodeDecode folder = 
  testCase ("Encode - decode for " ++ folder) $ do
    (journal, report) <- getTransactionReport AllDates Nothing SingleCsvRecord folder (read "2020-01-01")
    -- Encode the report as csv and then decode the report back
    -- Ensures the decoded report is equal to the original transactions
    let csvSeparator = jfCsvSeparator $ lJournalFile journal
    let decimalSeparator = jfDecimalSeparator $ lJournalFile journal
    let lang = jfLanguage $ lJournalFile journal
    let myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord csvSeparator)
                    }
    let csvBS = C.encodeWith myOptions $ V.toList report
    let accIds = HS.fromList $ map aId $ lAccounts journal
    let pos = map (\i -> SourcePos folder i 0) [2..]
    let accs = fmap (zip pos)
             $ decodeTransactions lang csvSeparator decimalSeparator csvBS
    let txnsM = accs >>= validateJTransactions accIds
    let actualTxns = sortOn tDate
                   $ map (\t -> t{tPostings = sortOn pFileOrder (tPostings t)}) 
                   $ lTransactions journal
    case txnsM of
      Left err -> assertFailure $ printErr err
      Right txns -> assertEqual "" actualTxns txns


printErr :: [Error] -> String
printErr err = T.unpack
             $ printErrors
             $ map (i18nText En_CA . TError ) err

getTransactionReport :: ReportPeriod -> 
                         (Maybe CompareAnotherPeriod) -> 
                         TransactionCsvRecordType ->
                         String -> 
                         Day ->
                         IO (Ledger, V.Vector (V.Vector T.Text))
getTransactionReport period compareC lineType folder today = do
 let journalPath = "test/Report/Transactions/" ++ folder ++ "/Journal.csv"
 ledger <- runExceptT $ fmap journalToLedger $ decodeJournal journalPath
 let report = fmap (\j -> (j, transactionReport period compareC lineType j today)) ledger
 case report of
     Left (_, err) -> assertFailure $ printErr err
     Right (j, txns) -> return (j, txns)