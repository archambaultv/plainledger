module Plainledger.Commands
  (
  Command(..),
  TrialBalanceCommand(..),
  TransactionsCommand(..),
  IncomeCommand(..),
  ModifyCommand(..),
  BalanceSheetCommand(..),
  commandInputFile
  )
where

import Data.Time
import Plainledger.Data.Type

data Command
  = CModify ModifyCommand
  | CBalanceSheet BalanceSheetCommand
  | CIncome IncomeCommand
  | CTransactions TransactionsCommand
  | CTrialBalance TrialBalanceCommand
  -- | CAccounts --Export accounts

commandInputFile :: Command -> String
commandInputFile (CModify x) = mcInputFile x
commandInputFile (CBalanceSheet x) = bcInputFile x
commandInputFile (CIncome x) = incInputFile x
commandInputFile (CTransactions x) = tcInputFile x
commandInputFile (CTrialBalance x) = tbcInputFile x

data TrialBalanceCommand = TrialBalanceCommand {
  tbcInputFile :: String,
  tbcOutputFile :: Maybe String,
  tbcStart :: Maybe Day,
  tbcEnd :: Maybe Day,
  tbcAccType :: AccountingFormat
  }

data TransactionsCommand = TransactionsCommand {
  tcInputFile :: String,
  tcOutputFile :: Maybe String,
  tcStart :: Maybe Day,
  tcEnd :: Maybe Day,
  tcAccType :: AccountingFormat
  }

data IncomeCommand = IncomeCommand {
  incInputFile :: String,
  incOutputFile :: Maybe String,
  incStart :: Maybe Day,
  incEnd :: Maybe Day
  }

data BalanceSheetCommand = BalanceSheetCommand {
  bcInputFile :: String,
  bcOutputFile :: Maybe String,
  bcStart :: Maybe Day,
  bcEnd :: Maybe Day
  }

data ModifyCommand = ModifyCommand {
  mcInputFile :: String,
  mcOutputFile :: Maybe String,
  mcDryRun :: Bool,
  mcSort :: Bool,
  mcFill :: Bool,
  mcRule :: Maybe String
  }
