-- |
-- Module      :  Plainledger.CLI.Command.Cashflow
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the trial balance command

module Plainledger.CLI.Command.Cashflow
  (
  CashFlowCommand(..),
  BalanceFormat(..),
  CashFlowOption(..)
  )
where

import Data.Time
import Plainledger.Reports

data CashFlowCommand = CashFlowCommand {
  cfYamlFile :: String,
  cfCsvFile :: String,
  cfStartDate :: Maybe Day,
  cfEndDate :: Maybe Day,
  cfOption :: CashFlowOption
  }
