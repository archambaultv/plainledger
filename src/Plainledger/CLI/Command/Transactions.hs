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

import Data.Time
import Plainledger.Journal.Transaction (CsvRecordOptions(..))

type CsvEncodeFormat = CsvRecordOptions


data TransactionsCommand = TransactionsCommand {
  tcJournalFile :: String,
  tcCsvFile :: String,
  tcStartDate :: Maybe Day,
  tcEndDate :: Maybe Day,
  tcEncodeFormat :: CsvEncodeFormat,
  tcValidation :: Bool
  }
