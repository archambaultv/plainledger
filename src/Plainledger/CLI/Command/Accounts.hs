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
  AccountCommand(..),
  )
where

data AccountCommand = AccountCommand {
  acInputFile :: String,
  acOutputFile :: String
  }
