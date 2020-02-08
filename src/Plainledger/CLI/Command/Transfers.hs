-- |
-- Module      :  Plainledger.CLI.Command.Transfers
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the accounts command

module Plainledger.CLI.Command.Transfers
  (
  TransferCommand(..),
  )
where

import Data.Time

data TransferCommand = TransferCommand {
  tcYamlFile :: String,
  tcCsvFile :: String,
  tcStartDate :: Maybe Day,
  tcEndDate :: Maybe Day
  }
