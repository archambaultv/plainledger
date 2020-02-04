module Main where

import Test.Tasty
import Ledger.Ledger

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [ledgerTestTree]
