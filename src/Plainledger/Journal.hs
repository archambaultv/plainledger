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
  -- JournalFile(..),
  -- journalFileToJournal,
  -- yamlPrettyConfig,
  -- module Plainledger.Journal.Posting,
  -- module Plainledger.Journal.Transaction,
  -- module Plainledger.Journal.Balance,
  module Plainledger.Journal.JournalFile,
  -- module Plainledger.Journal.Account,
  -- module Plainledger.Journal.Amount,
  -- module Plainledger.Journal.Tag,
  -- module Plainledger.Journal.Day
  )
where

import Data.Maybe
import Data.Ord
import Control.Monad.Except
import Plainledger.Error
import qualified Data.Text as T
import Plainledger.Journal.Posting
import Plainledger.Journal.Transaction
import Plainledger.Journal.Balance
import Plainledger.Journal.JournalFile
import Plainledger.Journal.Account
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import System.FilePath

data Journal t  = Journal
  {
    jJournalFile :: JournalFile
   -- jAccounts   :: [Account],
   -- jTransactions :: [Transaction],
   -- jBalances :: [Balance]
  }
  deriving (Eq, Show)


-- -- Reads the include files in the journal file
-- journalFileToJournal :: FilePath -> JournalFile -> ExceptT Error IO Journal
-- journalFileToJournal path (JournalFile c ai ti bi tbi) = do
--   let dir = takeDirectory path
--   acc <- fmap concat $ traverse (decodeAccountsFile) (map (dir </>) ai)
--   txns <- fmap concat $ traverse (decodeJTransactionsFile) (map (dir </>) ti)
--   bals <- fmap concat $ traverse (decodeBalanceFile) (map (dir </>) bi)
--   trialbals <- fmap concat $ traverse (decodeTrialBalanceAssertionFile) (map (dir </>) tbi)
--   return $ Journal c acc txns bals trialbals


