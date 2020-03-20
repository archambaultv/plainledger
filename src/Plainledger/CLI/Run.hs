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
import Plainledger.CLI.Run.Accounts
import Plainledger.CLI.Run.Convert
import Plainledger.CLI.Run.Transactions
import Plainledger.CLI.Run.TrialBalance
import Plainledger.CLI.Run.Cashflow
import Plainledger.CLI.Run.BalanceSheet

-- / How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (CAccounts x) = runAccounts x
runCommand (CFromCsv x) = runFromCsv x
runCommand (CTransactions x) = runTransactions x
runCommand (CTrialBalance x) = runTrialBalance x
runCommand (CCashFlow x) = runCashFlow x
runCommand (CBalanceSheet x) = runBalanceSheet x
