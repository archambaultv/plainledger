module Main where

import Test.Tasty
import Journal.JournalTests
import Report.Report

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [journalTestTree, reportsTestTree]
