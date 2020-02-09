-- |
-- Module      :  Plainledger.CLI.Command.Transfers
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
  )
where

import Data.Time

data TransactionsCommand = TransactionsCommand {
  tcYamlFile :: String,
  tcCsvFile :: String,
  tcStartDate :: Maybe Day,
  tcEndDate :: Maybe Day
  }
