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
  module Plainledger.CLI.Command.FromCsv
  )
where

import Plainledger.CLI.Command.Accounts
import Plainledger.CLI.Command.FromCsv

data Command
  = CAccounts AccountCommand
  | CFromCsv FromCsvCommand
