module Plainledger.Commands
  (
  Command(..),
  ImportCommand(..),
  TrialBalanceCommand(..),
  TransactionsCommand(..),
  IncomeCommand(..),
  ModifyCommand(..),
  BalanceSheetCommand(..)
  )
where

import Data.Time
import Plainledger.Data.Type

data Command
  = CImport ImportCommand
  | CModify ModifyCommand
  | CBalanceSheet BalanceSheetCommand
  | CIncome IncomeCommand
  | CTransactions TransactionsCommand
  | CTrialBalance TrialBalanceCommand
  -- | CAccounts --Export accounts


data TrialBalanceCommand = TrialBalanceCommand {
  tbcInputFile :: String,
  tbcOutputFile :: Maybe String,
  tbcStart :: Maybe Day,
  tbcEnd :: Maybe Day,
  tbcAccType :: AccountingType
  }
  
data TransactionsCommand = TransactionsCommand {
  tcInputFile :: String,
  tcOutputFile :: Maybe String,
  tcStart :: Maybe Day,
  tcEnd :: Maybe Day,
  tcAccType :: AccountingType
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
  
data ImportCommand = ImportCommand {
  icCsvFile :: String,
  icConfig :: String,
  icDryRun :: Bool,
  icJournalFile :: Maybe String
  }

data ModifyCommand = ModifyCommand {
  mcInputFile :: String,
  mcOutputFile :: Maybe String,
  mcDryRun :: Bool,
  mcSort :: Bool,
  mcFill :: Bool,
  mcRule :: Maybe String
  }
