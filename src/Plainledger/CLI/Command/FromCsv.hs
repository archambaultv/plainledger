-- |
-- Module      :  Plainledger.CLI.Command.FromCsv
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the fromcsv command

module Plainledger.CLI.Command.FromCsv
  (
  CsvType(..),
  FromCsvCommand(..),
  )
where

import Plainledger.CLI.Command.Transactions

data CsvType = CsvAccounts | CsvTransactions CsvEncodeFormat

data FromCsvCommand = FromCsvCommand {
  fcsvCsvFile :: String,
  fcsvYamlFile :: String,
  fcsvCsvType :: CsvType
  }
