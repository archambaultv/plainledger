module Report.Report (
  reportsTestTree
  )where

import Test.Tasty
import Report.Transactions.Transactions
import Report.Report.Report


reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [reportTestTree, transactionsTestTree]