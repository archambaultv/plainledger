module Report.Report (
  reportsTestTree
  )where

import Test.Tasty
import Report.Transactions.Transactions
import Report.TrialBalance.TrialBalance
import Report.BalanceSheet.BalanceSheet
import Report.IncomeStatement.IncomeStatement
import Report.Report.Report


reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [reportTestTree, 
               transactionsTestTree, 
               trialBalanceTestTree,
               balanceSheetTestTree,
               incomeStatementTestTree]