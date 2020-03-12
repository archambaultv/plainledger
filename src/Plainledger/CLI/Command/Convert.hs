-- |
-- Module      :  Plainledger.CLI.Command.Convert
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the fromcsv command

module Plainledger.CLI.Command.Convert
  (
  CsvType(..),
  ConvertCommand(..),
  )
where

import Plainledger.CLI.Command.Transactions

data CsvType = CsvAccounts | CsvTransactions CsvRecordOptions

data ConvertCommand = ConvertCommand {
  ccFromFile :: String,
  ccToFile :: String,
  ccFromDataType :: CsvType
  }
