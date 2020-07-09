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
  CashFlowOption
  )
where

import Plainledger.Reports

data CashFlowCommand = CashFlowCommand {
  cfJournalFile :: String,
  cfOuputFile :: String,
  cfPeriod :: Period,
  cfOption :: CashFlowOption
  }
