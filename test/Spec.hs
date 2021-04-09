module Main where

import Test.Tasty
import Csv.Csv
import Journal.JournalTests
import Report.Report

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [csvFileTestTree, journalTestTree, reportsTestTree]
