-- |
-- Module      :  Plainledger.CLI.Command.AllReports
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the reports command

module Plainledger.CLI.Command.AllReports
  (
  AllReportsCommand(..),
  BalanceSheetOption
  )
where

import Plainledger.Reports

data AllReportsCommand = AllReportsCommand {
  arJournalFile :: String,
  arOuputDir :: String,
  arPeriod :: Period,
  arFlatOption :: FlatReportOption
  }
