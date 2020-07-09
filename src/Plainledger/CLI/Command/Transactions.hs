-- |
-- Module      :  Plainledger.CLI.Command.Transactions
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the accounts command

module Plainledger.CLI.Command.Transactions
  (
  TransactionsCommand(..),
  CsvEncodeFormat,
  CsvRecordOptions(..)
  )
where

import Plainledger.Reports.Report
import Plainledger.Journal.Transaction (CsvRecordOptions(..))

type CsvEncodeFormat = CsvRecordOptions


data TransactionsCommand = TransactionsCommand {
  tcJournalFile :: String,
  tcOuputFile :: String,
  tcPeriod :: Period,
  tcEncodeFormat :: CsvEncodeFormat,
  tcValidation :: Bool
  }
