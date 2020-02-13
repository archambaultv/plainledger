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
  CsvDecodeOptions(..)
  )
where

import Data.Time
import Plainledger.Ledger.Transaction (CsvDecodeOptions(..))

type CsvEncodeFormat = CsvDecodeOptions


data TransactionsCommand = TransactionsCommand {
  tcYamlFile :: String,
  tcCsvFile :: String,
  tcStartDate :: Maybe Day,
  tcEndDate :: Maybe Day,
  tcEncodeFormat :: CsvEncodeFormat,
  tcValidation :: Bool
  }
