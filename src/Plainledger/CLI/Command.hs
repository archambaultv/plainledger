-- |
-- Module      :  Plainledger.CLI.Command
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines all the possible terminal commands

module Plainledger.CLI.Command
  (
  Command(..),
  module Plainledger.CLI.Command.Transactions,
  module Plainledger.CLI.Command.TrialBalance,
  module Plainledger.CLI.Command.BalanceSheet,
  module Plainledger.CLI.Command.IncomeStatement,
  )
where

import Plainledger.CLI.Command.Transactions
import Plainledger.CLI.Command.TrialBalance
import Plainledger.CLI.Command.BalanceSheet
import Plainledger.CLI.Command.IncomeStatement

data Command {
  cJournalFile :: String,
  cReport :: Report
  }

