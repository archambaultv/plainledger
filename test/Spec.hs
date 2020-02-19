module Main where

import Test.Tasty
import Journal.Journal
import Ledger.Ledger
import Reports.Reports

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [journalTestTree, ledgerTestTree, reportsTestTree]
