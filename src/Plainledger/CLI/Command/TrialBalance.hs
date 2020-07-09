-- |
-- Module      :  Plainledger.CLI.Command.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the trial balance command

module Plainledger.CLI.Command.TrialBalance
  (
  TrialBalanceCommand(..),
  BalanceFormat(..),
  TrialBalanceOption
  )
where

import Plainledger.Reports

data TrialBalanceCommand = TrialBalanceCommand {
  tbcJournalFile :: String,
  tbcOuputFile :: String,
  tbcPeriod :: Period,
  tbcOption :: TrialBalanceOption
  }
