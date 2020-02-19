-- |
-- Module      :  Plainledger.CLI.Command.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the trial balance command

module Plainledger.CLI.Command.TrialBalance
  (
  TrialBalanceCommand(..),
  BalanceFormat(..),
  TrialBalanceOption(..)
  )
where

import Data.Time
import Plainledger.Reports.TrialBalance

data TrialBalanceCommand = TrialBalanceCommand {
  tbcYamlFile :: String,
  tbcCsvFile :: String,
  tbcStartDate :: Maybe Day,
  tbcEndDate :: Maybe Day,
  tbcOption :: TrialBalanceOption
  }
