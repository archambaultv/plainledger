module Plainledger.CLIOptions
  (
  Command(..),
  CommandName(..),
  str2CommandName
  )
where

import Data.Time
import Data.Char
import Plainledger.Data.Type

data Command = Command CommandName FilePath FilePath (Maybe Day) (Maybe Day) AccountingType
             deriving (Show)

data CommandName = BalanceSheet
                 | IncomeStatement
                 | TrialBalance
                 | Transactions
                 deriving (Show)

str2CommandName :: String -> Maybe CommandName
str2CommandName x =
    case (map toLower x) of
      "balancesheet" -> Just BalanceSheet
      "incomestatement" -> Just IncomeStatement
      "trialbalance" -> Just TrialBalance
      "transactions" -> Just Transactions
      _ -> Nothing
