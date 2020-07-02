-- |
-- Module      :  Plainledger.CLI.Command.Accounts
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the accounts command

module Plainledger.CLI.Command.Accounts
  (
  AccountsCommand(..),
  )
where

data AccountsCommand = AccountsCommand {
  acJournalFile :: String,
  acCsvFile :: String
  --acUsed :: Used
  }

-- data Used = ALL | USED | UNUSED
