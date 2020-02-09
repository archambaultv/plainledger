-- |
-- Module      :  Plainledger.CLI.Run.Accounts
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.Transactions
(
  runTransactions
  ) where

import Data.Time
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger

-- / Reads the journal file and the exports the transfers in CSV format
runTransactions :: TransactionsCommand -> IO ()
runTransactions c = do
     journal <- Y.decodeFileThrow (tcYamlFile c)
     case journalToLedger journal of
       Left err -> putStrLn err
       Right l -> BL.writeFile
                  (tcCsvFile c)
                  $ encodeTransactions
                  $ filterDate (tcStartDate c) (tcEndDate c)
                  $ lTransactions l

filterDate :: Maybe Day -> Maybe Day -> [Transaction] -> [Transaction]
filterDate Nothing Nothing ts = ts
filterDate (Just s) e ts =
  filterDate Nothing e $ filter (\t -> tDate t >= s) ts
filterDate _ (Just e) ts = filter (\t -> tDate t <= e) ts
