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
import Plainledger.CLI.Run.FromCsv
import Plainledger.CLI.Run.Transfers

-- / How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (CAccounts x) = runAccounts x
runCommand (CFromCsv x) = runFromCsv x
runCommand (CTransfers x) = runTransfers x
