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

import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports
import Control.Monad.Except

-- / Reads the journal file and the exports the transactions in CSV format
runTransactions :: TransactionsCommand -> IO ()
runTransactions c = do
     journalFile <- Y.decodeFileThrow (tcJournalFile c)
     journal <- runExceptT $ journalFileToJournal (tcJournalFile c) journalFile
     let txns =  if tcValidation c
                 then (map transactionToJTransaction . jTransactions . lJournal)
                      <$> (journal >>= journalToLedger)
                 else fmap jTransactions journal
     case txns of
       Left err -> putStrLn err
       Right xs -> do
        let opt = tcEncodeFormat c
        let (sd, ed) = maxSpan (tcPeriod c)
        BL.writeFile
          (tcOuputFile c)
          $ encodeTransactions opt
          $ filterDate sd ed
          $ xs

filterDate :: LDate -> LDate -> [TransactionF p] -> [TransactionF p]
filterDate MinDate MaxDate ts = ts
filterDate (Date s) e ts =
    filterDate MinDate e $ filter (\t -> tDate t >= s) ts
filterDate _ (Date e) ts = filter (\t -> tDate t <= e) ts
filterDate _ _ ts = ts
