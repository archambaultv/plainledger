-- |
-- Module      :  Plainledger.CLI.Run
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the CLI commands

module Plainledger.CLI.Run
(
  runCommand
  ) where

import Plainledger.CLI.Command
import Plainledger.CLI.Run.Transactions
import Plainledger.CLI.Run.TrialBalance
import Plainledger.CLI.Run.BalanceSheet
import Plainledger.CLI.Run.IncomeStatement

-- / How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (CTransactions x) = runTransactions x
runCommand (CTrialBalance x) = runTrialBalance x
runCommand (CBalanceSheet x) = runBalanceSheet x
runCommand (CIncomeStatement x) = runIncomeStatement x
