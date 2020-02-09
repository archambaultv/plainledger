-- |
-- Module      :  Main
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- The plainledger command line tool executable

module Main where

import Plainledger.CLI

main :: IO ()
main = cli
