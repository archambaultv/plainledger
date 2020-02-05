module Main where

import Test.Tasty
import Ledger.Ledger
import Journal.Journal

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [ledgerTestTree, journalTestTree]
