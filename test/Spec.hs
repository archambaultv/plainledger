module Main where

import Test.Tasty
import Journal.Journal
import Reports.Reports

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [journalTestTree, reportsTestTree]
