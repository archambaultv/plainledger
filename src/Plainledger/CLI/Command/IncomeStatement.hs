-- |
-- Module      :  Plainledger.CLI.Command.IncomeStatement
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the trial balance command

module Plainledger.CLI.Command.IncomeStatement
  (
  IncomeStatementCommand(..),
  IncomeStatementOption
  )
where

import Plainledger.Reports

data IncomeStatementCommand = IncomeStatementCommand {
  isJournalFile :: String,
  isOuputFile :: String,
  isPeriod :: Period,
  isOption :: IncomeStatementOption
  }
