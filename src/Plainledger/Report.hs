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
import Plainledger.Journal
import Plainledger.Report.Report
import Plainledger.Report.TrialBalance
import Plainledger.Report.BalanceSheet
import Plainledger.Report.IncomeStatement
import Plainledger.Report.Transactions

runReport :: Report -> Journal -> V.Vector (V.Vector T.Text)
runReport (Transactions _ _ b) j = transactionReport b j
runReport (TrialBalance _ _ _) _ = error "Not Implemented"
runReport (BalanceSheet _ _ _ _ _) _ = error "Not Implemented"
runReport (IncomeStatement _ _ _ _ _) _ = error "Not Implemented"

encodeReport :: Journal -> V.Vector (V.Vector T.Text) -> BL.ByteString
encodeReport j v =
  let csvSeparator = jfCsvSeparator $ jJournalFile j
      myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord csvSeparator)
                    }
  in C.encodeWith myOptions $ V.toList v

writeReport :: FilePath -> Journal -> V.Vector (V.Vector T.Text) -> IO ()
writeReport path j v = BS.writeFile path $ BL.toStrict $ encodeReport j v