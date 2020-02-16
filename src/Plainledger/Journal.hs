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

module Plainledger.Journal (
  LedgerF(..),
  Journal,
  JournalFile(..),
  journalFileToJournal,
  yamlPrettyConfig,
  journalToLedger,
  module Plainledger.Journal.JPosting,
  module Plainledger.Journal.JTransaction
  )
where

import Data.Maybe
import Data.Ord
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as P
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=), (.:?))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Plainledger.Ledger
import Plainledger.Journal.JPosting
import Plainledger.Journal.JTransaction
import Control.Monad.Except
import Plainledger.Error
import System.FilePath


type Journal = LedgerF JTransaction

data JournalFile = JournalFile {
    journal :: Journal,
    accountsInclude :: [String],
    transactionsInclude :: [String],
    balanceInclude :: [String]
} deriving (Show, Eq)

-- Reads the include files in the journal file
journalFileToJournal :: FilePath -> JournalFile -> IO Journal
journalFileToJournal path (JournalFile j ai ti bi) = do
  let dir = takeDirectory path
  acc <- fmap concat $ traverse (decodeAccountsFile) (map (dir </>) ai)
  txns <- fmap concat $ traverse (decodeJTransactionsFile) (map (dir </>) ti)
  bals <- fmap concat $ traverse (decodeBalanceFile) (map (dir </>) bi)
  return j{lAccounts = lAccounts j ++ acc,
           lTransactions = lTransactions j ++ txns,
           lBalances = lBalances j ++ bals}

-- | Converts the journal to a ledger.
-- journalToLedger verifies a series of properties that a valid ledger should
-- satisfies :
-- Configuration :
--  Asserts all members of the group mapping are non null
--  Asserts opening balance account is non null
--  Asserts earnings account is non null
--  Asserts default commodity is non null
-- Accounts :
--  Asserts all accounts group field are in the configuration group mapping.
--  Asserts all accounts Id are unique and non null
--  Asserts configuration earning and opening balance accounts truly exists
-- Transactions :
--  Asserts all transactions have valid unique transaction id
--  Asserts all transactions have valid postings
--  Asserts all transactions have a well defined commodity
--  Asserts all transactions balance to zero for all commodities
-- Balances :
--  Asserts all balance have a valid account field
--  Asserts all balance have a well defined commodity
--  Asserts all balance assertions are correct
journalToLedger :: (MonadError Error m) => Journal -> m Ledger
journalToLedger (Ledger config accounts txns bals) = do
  validateConfig config
  validateAccounts (cGroupMapping config) accounts
  transactions' <- validateJTransactions
                   (cDefaultCommodity config)
                   accounts
                   txns
  balances' <- validateBalances (cDefaultCommodity config) accounts bals
  return $ Ledger config accounts transactions' balances'

-- | The Data.Yaml.Pretty configuration object created so that the
-- fields in the yaml file follow the convention of this software.
yamlPrettyConfig :: P.Config
yamlPrettyConfig = P.setConfCompare (comparing fieldOrder) P.defConfig
 where fieldOrder :: T.Text -> Int
       -- Id always come first
       fieldOrder "id" = 0
       -- Transaction fields
       fieldOrder "date" = 1
       fieldOrder "transaction-id" = 2
       fieldOrder "postings" = 5

       -- Balance fields
       fieldOrder "account" = 8

       -- Transaction fields
       fieldOrder "amount" = 12
       fieldOrder "commodity" = 13
       fieldOrder "balance-date" = 14

       -- Top level fields
       fieldOrder "configuration" = 20
       fieldOrder "accounts" = 21
       fieldOrder "transactions" = 22
       fieldOrder "balance-assertions" = 23
       -- Configuration fields
       fieldOrder "default-commodity" = 30
       fieldOrder "opening-balance-account" = 31
       fieldOrder "earnings-account" = 32
       -- Account fields
       fieldOrder "name" = 40
       fieldOrder "number" = 41
       fieldOrder "group" = 42
       fieldOrder "subgroup" = 43
       fieldOrder "subsubgroup" = 44

       -- Tags always come last
       fieldOrder "tags" = 90

       fieldOrder _ = 99

-- FromJSON instances
instance FromJSON Journal where
  parseJSON (Y.Object v) =
    Ledger
    <$> v .: "configuration"
    <*> v .: "accounts"
    <*> v .: "transactions"
    <*> v .: "balance-assertions"
  parseJSON _ = fail "Expected Object for Ledger value"

instance FromJSON JournalFile where
  parseJSON (Y.Object v) =
    JournalFile
    <$> (Ledger
      <$> v .: "configuration"
      <*> v .: "accounts"
      <*> v .: "transactions"
      <*> v .: "balance-assertions")
    <*> (fromMaybe [] <$> v .:? "accounts-include")
    <*> (fromMaybe [] <$> v .:? "transactions-include")
    <*> (fromMaybe [] <$> v .:? "balance-assertions-include")

  parseJSON _ = fail "Expected Object for Ledger value"

-- To JSON instance
instance ToJSON Journal where
  toJSON (Ledger config accounts txns bals) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "transactions" .= txns,
       "balance-assertions" .= bals]

  toEncoding (Ledger config accounts txns bals) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "transactions" .= txns
    <> "balance-assertions" .= bals

-- To JSON instance
instance ToJSON JournalFile where
  toJSON (JournalFile (Ledger config accounts txns bals)
          accIncl txnsIncl balsIncl) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "accounts-include" .= accIncl,
       "transactions" .= txns,
       "transactions-include" .= txnsIncl,
       "balance-assertions" .= bals,
       "balance-assertions-include" .= balsIncl]

  toEncoding (JournalFile (Ledger config accounts txns bals)
              accIncl txnsIncl balsIncl) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "accounts-include" .= accIncl
    <> "transactions" .= txns
    <> "transactions-include" .= txnsIncl
    <> "balance-assertions" .= bals
    <> "balance-assertions-include" .= balsIncl
