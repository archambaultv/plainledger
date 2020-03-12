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

-- / Reads the journal file and the exports the transactions in CSV format
runTransactions :: TransactionsCommand -> IO ()
runTransactions c = do
     journalFile <- Y.decodeFileThrow (tcYamlFile c)
     journal <- journalFileToJournal (tcYamlFile c) journalFile
     let txns =  if tcValidation c
                 then (map transactionToJTransaction . jTransactions . lJournal)
                      <$> journalToLedger journal
                 else return $ jTransactions journal
     case txns of
       Left err -> putStrLn err
       Right xs ->
        let opt = tcEncodeFormat c
        in BL.writeFile
          (tcCsvFile c)
          $ encodeTransactions opt
          $ filterDate (tcStartDate c) (tcEndDate c)
          $ xs

filterDate :: Maybe Day -> Maybe Day -> [TransactionF p] -> [TransactionF p]
filterDate Nothing Nothing ts = ts
filterDate (Just s) e ts =
  filterDate Nothing e $ filter (\t -> tDate t >= s) ts
filterDate _ (Just e) ts = filter (\t -> tDate t <= e) ts
