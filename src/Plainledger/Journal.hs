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
  Journal(..),
  journalFileToJournal,
  decodeJournal,
  module Plainledger.Journal.Posting,
  module Plainledger.Journal.Transaction,
  module Plainledger.Journal.Balance,
  module Plainledger.Journal.JournalFile,
  module Plainledger.Journal.Account,
  module Plainledger.Journal.Amount,
  module Plainledger.Journal.Day
  )
where


import System.FilePath
import Control.Monad.Except
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Journal.Posting
import Plainledger.Journal.Transaction
import Plainledger.Journal.Balance
import Plainledger.Journal.JournalFile
import Plainledger.Journal.Account
import Plainledger.Journal.Amount
import Plainledger.Journal.Day

data Journal = Journal
  {
    jJournalFile :: JournalFile,
    jAccounts   :: [Account],
    jTransactions :: [Transaction],
    jBalances :: [Balance]
  }
  deriving (Eq, Show)


-- Reads the include files in the journal file
journalFileToJournal :: JournalFile -> ExceptT Errors IO Journal
journalFileToJournal journalFile = do  
  let dir = takeDirectory $ jfFilePath journalFile
  let csvSeparator = jfCsvSeparator journalFile
  let decimalSeparator = jfDecimalSeparator journalFile
  let lang = jfLanguage journalFile

  -- Read the accounts files and check for errors
  let accountPath = dir </> jfAccountFile journalFile
  acc <- decodeAccountsFile lang accountPath csvSeparator 
         >>= validateAccounts (jfOpeningBalanceAccount journalFile)
              (jfEarningsAccount journalFile)
  let accIds = HS.fromList $ map aId acc

  -- Read the transactions files and check for errors
  let txnPaths = map (dir </>) $ jfTransactionFiles journalFile
  jtxns <- fmap concat 
        $ traverse (decodeJTransactionsFile lang csvSeparator decimalSeparator) txnPaths
  txns <- validateJTransactions accIds jtxns
        
  -- Read the balance assertion files and validate them
  let accMaps = HM.fromList $ map (\a -> (aId a, aType a)) acc
  let accTypef = \a -> accMaps HM.! a

  let balPaths = map (dir </>) $ jfStatementBalanceFiles journalFile
  bals <- fmap concat $ traverse (decodeStatementBalanceFile lang csvSeparator decimalSeparator) balPaths
  _ <- validateStatementBalances accIds txns bals

  let tbalPaths = map (dir </>) $ jfTrialBalanceFiles journalFile
  tbals <- fmap concat $ traverse (decodeTrialBalanceFile lang csvSeparator decimalSeparator) tbalPaths
  _ <- validateTrialBalances accIds accTypef (jfOpeningBalanceAccount journalFile) txns tbals

  return $ Journal journalFile acc txns (map snd bals)


decodeJournal :: FilePath ->
                 ExceptT (Language, Errors) IO Journal
decodeJournal filePath = 
  decodeJournalFile filePath 
  >>= withLang journalFileToJournal 

  where withLang foo jf = withExceptT (jfLanguage jf,) (foo jf)