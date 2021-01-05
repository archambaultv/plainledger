module Report.Report (
  reportsTestTree
  )where

import Test.Tasty
import Report.Transactions.Transactions


reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [transactionsTestTree]