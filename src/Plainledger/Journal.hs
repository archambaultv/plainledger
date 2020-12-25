-- |
-- Module      :  Plainledger.Journal
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the journal data type and reexports all the
-- data types and functions related to the journal file.

module Plainledger.Journal 
(
  Journal,
  journalFileToJournal,
  -- module Plainledger.Journal.Posting,
  module Plainledger.Journal.Transaction,
  -- module Plainledger.Journal.Balance,
  module Plainledger.Journal.JournalFile,
  module Plainledger.Journal.Account,
  module Plainledger.Journal.Amount,
  module Plainledger.Journal.Day
  )
where


import System.FilePath
import Control.Monad.Except
import Plainledger.Error
--import Plainledger.Journal.Posting
import Plainledger.Journal.Transaction
--import Plainledger.Journal.Balance
import Plainledger.Journal.JournalFile
import Plainledger.Journal.Account
import Plainledger.Journal.Amount
import Plainledger.Journal.Day

data Journal = Journal
  {
    jJournalFile :: JournalFile,
    jAccounts   :: [Account]
   -- jTransactions :: [Transaction],
   -- jBalances :: [Balance]
  }
  deriving (Eq, Show)


-- Reads the include files in the journal file
journalFileToJournal :: JournalFile -> ExceptT Errors IO Journal
journalFileToJournal journalFile = do  
  let dir = takeDirectory $ jfFilePath journalFile
  let csvSeparator = jfCsvSeparator journalFile
  let decimalSeparator = jfDecimalSeparator journalFile

  -- Read the accounts files and check for errors
  let accountPath = dir </> jfAccountFile journalFile
  acc <- decodeAccountsFile accountPath csvSeparator 
         >>= validateAccounts (jfOpeningBalanceAccount journalFile)
              (jfEarningsAccount journalFile)

  -- Read the transactions files and check for errors
  let txnPaths = jfTransactionFiles journalFile

  txns <- fmap concat 
        $ traverse (decodeJTransactionsFile csvSeparator decimalSeparator) 
          (map (dir </>) txnPaths)
        
  -- bals <- fmap concat $ traverse (decodeBalanceFile) (map (dir </>) bi)
  return $ Journal journalFile acc -- txns bals


