module Main where

import Test.Tasty
import Journal.Journal
import Ledger.Ledger

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [journalTestTree, ledgerTestTree]
