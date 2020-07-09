-- |
-- Module      :  Plainledger.CLI.Command.BalanceSheet
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the trial balance command

module Plainledger.CLI.Command.BalanceSheet
  (
  BalanceSheetCommand(..),
  BalanceSheetOption(..)
  )
where

import Data.Time
import Plainledger.Reports

data BalanceSheetCommand = BalanceSheetCommand {
  bsJournalFile :: String,
  bsOuputFile :: String,
  bsPeriod :: Period,
  bsOption :: BalanceSheetOption
  }
