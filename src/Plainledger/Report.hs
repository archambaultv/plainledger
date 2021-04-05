-- |
-- Module      :  Plainledger.Reports
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the journal data type and reexports all the
-- data types and functions related to the journal file.

module Plainledger.Report 
(
  runReport,
  encodeReport,
  writeReport,
  ReportParams(..),
  module Plainledger.Report.Ledger,
  module Plainledger.Report.TrialBalance,
  module Plainledger.Report.BalanceSheet,
  module Plainledger.Report.IncomeStatement,
  module Plainledger.Report.Transactions,
  module Plainledger.Report.AccountTreeReport,
  module Plainledger.Report.AccountTreeParam
  )
where

import Data.Char (ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import Data.Time
import Plainledger.Journal
import Plainledger.Report.Ledger
import Plainledger.Report.TrialBalance
import Plainledger.Report.BalanceSheet
import Plainledger.Report.IncomeStatement
import Plainledger.Report.Transactions
import Plainledger.Report.AccountTreeReport
import Plainledger.Report.AccountTreeParam
import Plainledger.Internal.Utils

data ReportParams 
  = Transactions ReportPeriod (Maybe CompareAnotherPeriod) TransactionCsvRecordType
  | TrialBalance ReportPeriod (Maybe CompareAnotherPeriod) ShowRow
  | BalanceSheet ReportPeriod (Maybe CompareAnotherPeriod) ShowRow (Maybe GroupByColumns)
                 ComparisonColumns
  | IncomeStatement ReportPeriod (Maybe CompareAnotherPeriod) ShowRow (Maybe GroupByColumns) 
    ComparisonColumns
  deriving (Eq, Show)

runReport :: ReportParams -> Day -> Journal -> [ReportRow]
runReport (Transactions period c b) today j = 
  transactionReport period c b (journalToLedger j) today
runReport (TrialBalance period c showRow) today j = 
  trialBalanceReport period c showRow (journalToLedger j) today
runReport (BalanceSheet period c showRow groupByColumns displayExtraC) today j = 
  balanceSheetReport (AccountTreeParam period c showRow groupByColumns displayExtraC)
  (journalToLedger j) today
runReport (IncomeStatement period c showRow groupByColumns displayExtraC) today j = 
  incomeStatementReport (AccountTreeParam period c showRow groupByColumns displayExtraC)
  (journalToLedger j) today

encodeReport :: Journal -> [ReportRow] -> BL.ByteString
encodeReport j v =
  let csvSeparator = jfCsvSeparator $ jJournalFile j
      myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord csvSeparator)
                    }
  in C.encodeWith myOptions v

writeReport :: FilePath -> Journal -> [ReportRow] -> IO ()
writeReport path j v = 
  let report = encodeReport j v
      bomReport = if jfHasBom $ jJournalFile j
                  then BL.append (BL.fromStrict bom) report
                  else report
  in BS.writeFile path (BL.toStrict bomReport)