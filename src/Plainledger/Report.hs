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
  module Plainledger.Report.Report,
  module Plainledger.Report.TrialBalance,
  module Plainledger.Report.BalanceSheet,
  module Plainledger.Report.IncomeStatement,
  module Plainledger.Report.Transactions
  )
where

import Data.Char (ord)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Time
import Plainledger.Journal
import Plainledger.Report.Report
import Plainledger.Report.TrialBalance
import Plainledger.Report.BalanceSheet
import Plainledger.Report.IncomeStatement
import Plainledger.Report.Transactions
import Plainledger.Internal.Utils

runReport :: ReportParams -> Day -> Journal -> V.Vector (V.Vector T.Text)
runReport (Transactions period _ b) today j = 
  let dateSpan = reportPeriodToSpan period today (journalToLedger j)
  in transactionReport dateSpan b j
runReport (TrialBalance period c showRow) today j = 
  trialBalanceReport period c showRow (journalToLedger j) today
runReport (BalanceSheet period c showRow displayColumns displayExtraC) today j = 
  balanceSheetReport period c showRow displayColumns displayExtraC
  (journalToLedger j) today
runReport (IncomeStatement _ _ _ _ _) _ _ = error "Not Implemented"

encodeReport :: Journal -> V.Vector (V.Vector T.Text) -> BL.ByteString
encodeReport j v =
  let csvSeparator = jfCsvSeparator $ jJournalFile j
      myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord csvSeparator)
                    }
  in C.encodeWith myOptions $ V.toList v

writeReport :: FilePath -> Journal -> V.Vector (V.Vector T.Text) -> IO ()
writeReport path j v = 
  let report = encodeReport j v
      bomReport = if jfHasBom $ jJournalFile j
                  then BL.append (BL.fromStrict bom) report
                  else report
  in BS.writeFile path (BL.toStrict bomReport)