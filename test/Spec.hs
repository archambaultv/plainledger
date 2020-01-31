module Main where

import Test.Tasty
import Syntax.Syntax

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [syntaxTestTree]
