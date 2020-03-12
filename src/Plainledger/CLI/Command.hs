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
  module Plainledger.CLI.Command.Accounts,
  module Plainledger.CLI.Command.Convert,
  module Plainledger.CLI.Command.Transactions,
  module Plainledger.CLI.Command.TrialBalance
  )
where

import Plainledger.CLI.Command.Accounts
import Plainledger.CLI.Command.Convert
import Plainledger.CLI.Command.Transactions
import Plainledger.CLI.Command.TrialBalance

data Command
  = CAccounts AccountsCommand
  | CFromCsv ConvertCommand
  | CTransactions TransactionsCommand
  | CTrialBalance TrialBalanceCommand
