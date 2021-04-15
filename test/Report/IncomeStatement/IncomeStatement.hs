-- |
-- Module      :  Report.IncomeStatement.IncomeStatement
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type


module Report.IncomeStatement.IncomeStatement
(
  incomeStatementTestTree
)
where

import Data.Time
import Data.Char (ord)
import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Plainledger.Report
import Control.Monad.Except
import Plainledger.I18n.I18n
import qualified Data.Text as T
import qualified Data.Csv as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

incomeStatementTestTree :: TestTree
incomeStatementTestTree =
   testGroup "IncomeStatement"
    [ runReportOk "Journal-01" (FiscalYear 0) Nothing ShowNonZero 
      (read "2018-12-31") "Income Statement 2018.csv",
      runReportOk "Journal-01" (FiscalYear 0) Nothing ShowNonZero 
      (read "2019-12-31") "Income Statement 2019.csv",
      runReportOk "Journal-01" (FiscalYear 0) (Just (PreviousYear 1)) ShowNonZero 
      (read "2019-12-31") "Income Statement 2018 - 2019.csv"
    ]

runReportOk :: String -> 
               ReportPeriod -> 
               (Maybe CompareAnotherPeriod) -> 
               ShowRow ->
               Day ->
               String -> 
               TestTree
runReportOk folder period comparePeriod showRow today actualIncomeStatement = 
  testCase ("Report for " ++ folder) $ do
     (ledger, tb) <- getIncomeStatementReport period comparePeriod showRow today folder
     let csvSeparator = jfCsvSeparator $ lJournalFile ledger
     let myOptions = C.defaultEncodeOptions {
                       C.encDelimiter = fromIntegral (ord csvSeparator)
                     }
     let csvBS = C.encodeWith myOptions tb
     actualBS <- BS.readFile ("test/Report/IncomeStatement/" ++ actualIncomeStatement)
     assertEqual "" (BL.fromStrict actualBS) csvBS

printErr :: [Error] -> String
printErr err = T.unpack
             $ printErrors
             $ map (i18nText En_CA . TError ) err

getIncomeStatementReport :: ReportPeriod -> 
                         (Maybe CompareAnotherPeriod) -> 
                         ShowRow ->
                         Day ->
                         String -> 
                         IO (Ledger, [ReportRow])
getIncomeStatementReport period comparePeriod showRow today folder = do
 let journalPath = "test/Report/IncomeStatement/" ++ folder ++ "/Journal.csv"
 ledger <- runExceptT $ fmap journalToLedger $ decodeJournal journalPath
 let report = fmap (\j -> (j, incomeStatementReport 
                              (AccountTreeParam period 
                              comparePeriod 
                              showRow 
                              Nothing
                              comparisonColumnsDefault)
                              j 
                              today)) ledger
 case report of
     Left (_, err) -> assertFailure $ printErr err
     Right (j, txns) -> return (j, txns)