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
  module Plainledger.CLI.Command.FromCsv,
  module Plainledger.CLI.Command.Transfers
  )
where

import Plainledger.CLI.Command.Accounts
import Plainledger.CLI.Command.FromCsv
import Plainledger.CLI.Command.Transfers

data Command
  = CAccounts AccountCommand
  | CFromCsv FromCsvCommand
  | CTransfers TransferCommand
