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
  journalDateSpan,
  journalFileToJournal,
  decodeJournal,
  module Plainledger.Journal.Posting,
  module Plainledger.Journal.Transaction,
  module Plainledger.Journal.Balance,
  module Plainledger.Journal.BalanceMap,
  module Plainledger.Journal.JournalFile,
  module Plainledger.Journal.Account,
  module Plainledger.Journal.Amount,
  module Plainledger.Journal.Day
  )
where


import System.FilePath
import Control.Monad.Except
import qualified Data.HashMap.Strict as HM
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Journal.Posting
import Plainledger.Journal.Transaction
import Plainledger.Journal.Balance
import Plainledger.Journal.BalanceMap
import Plainledger.Journal.JournalFile
import Plainledger.Journal.Account
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Data.Tree

data Journal = Journal
  {
    jJournalFile :: JournalFile,
    jAccounts   :: [Account],
    jTransactions :: [Transaction],
    jChartOfAccount :: [Tree Account],
    jBalanceMap :: BalanceMap 
  }
  deriving (Eq, Show)

journalDateSpan :: Journal -> Maybe DateSpan
journalDateSpan journal =
  case jTransactions journal of
    [] -> Nothing
    txns -> let dates = map tDate txns
            in return (minimum dates, maximum dates)

-- Reads the include files in the journal file
journalFileToJournal :: JournalFile -> ExceptT Errors IO Journal
journalFileToJournal journalFile = do
  let dir = takeDirectory $ jfFilePath journalFile
  let csvSeparator = jfCsvSeparator journalFile
  let decimalSeparator = jfAmountDescriptor journalFile
  let lang = jfLanguage journalFile

  -- Read the accounts files and check for errors
  let accountPath = dir </> jfAccountFile journalFile
  chart <- decodeAccountsFile lang accountPath csvSeparator
                >>= validateAccounts (jfOpeningBalanceAccount journalFile)
                      (jfEarningsAccount journalFile) lang
  let acc = chartToList chart
  let accIds = HM.fromList $ map (\a -> (aIdentifier a, a)) acc

  -- Read the transactions files and check for errors
  let txnPaths = map (dir </>) $ jfTransactionFiles journalFile
  jtxns <- concat <$> traverse (decodeJTransactionsFile lang csvSeparator decimalSeparator) txnPaths
  txns <- validateJTransactions accIds jtxns

  -- Read the balance assertion files and validate them
  let earn = accIds HM.! jfEarningsAccount journalFile
  let open = accIds HM.! jfOpeningBalanceAccount journalFile
  let trialBal = transactionsToBalanceMap open earn txns
  let statementBal = txnsToBalanceMapUsingBalanceDate open earn txns

  let balPaths = map (dir </>) $ jfStatementBalanceFiles journalFile
  bals <- concat <$> traverse (decodeStatementBalanceFile lang csvSeparator decimalSeparator) balPaths
  _ <- validateStatementBalances accIds statementBal bals

  let tbalPaths = map (dir </>) $ jfTrialBalanceFiles journalFile
  tbals <- concat <$> traverse (decodeTrialBalanceFile lang csvSeparator decimalSeparator) tbalPaths
  _ <- validateTrialBalances accIds trialBal tbals

  return $ Journal journalFile acc txns chart trialBal

decodeJournal :: FilePath ->
                 ExceptT (Language, Errors) IO Journal
decodeJournal filePath =
  decodeJournalFile filePath
  >>= withLang journalFileToJournal

  where withLang foo jf = withExceptT (jfLanguage jf,) (foo jf)