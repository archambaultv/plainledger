module Plainledger.CLIOptions
  (
  Command(..),
  CommandName(..),
  str2CommandName
  )
where

import Data.Time
import Data.Char

data Command = Command CommandName FilePath FilePath (Maybe Day) (Maybe Day) Bool
             deriving (Show)

data CommandName = BalanceSheet
                 | IncomeStatement
                 | TrialBalance
                 | SpendingReport
                 | RevenuReport
                 deriving (Show)

str2CommandName :: String -> Maybe CommandName
str2CommandName x =
    case (map toLower x) of
      "balancesheet" -> Just BalanceSheet
      "incomestatement" -> Just IncomeStatement
      "trialbalance" -> Just TrialBalance
      "spendingreport" -> Just SpendingReport
      "revenureport" -> Just RevenuReport
      _ -> Nothing
