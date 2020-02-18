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
  JournalF(..),
  Journal,
  JournalFile(..),
  journalFileToJournal,
  yamlPrettyConfig,
  module Plainledger.Journal.Posting,
  module Plainledger.Journal.Transaction,
  module Plainledger.Journal.Balance,
  module Plainledger.Journal.Configuration,
  module Plainledger.Journal.Account,
  module Plainledger.Journal.Amount,
  module Plainledger.Journal.Tag,
  module Plainledger.Journal.Day
  )
where

import Data.Maybe
import Data.Ord
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as P
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=), (.:?))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Plainledger.Journal.Posting
import Plainledger.Journal.Transaction
import Plainledger.Journal.Balance
import Plainledger.Journal.Configuration
import Plainledger.Journal.Account
import Plainledger.Journal.Amount
import Plainledger.Journal.Tag
import Plainledger.Journal.Day
import System.FilePath

data JournalF t = Journal
  {jConfiguration :: Configuration,
   jAccounts   :: [Account],
   jTransactions :: [t],
   jBalances :: [Balance]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Journal = JournalF JTransaction

data JournalFile = JournalFile {
    jfJournal :: Journal,
    fjAccountsInclude :: [String],
    fjTransactionsInclude :: [String],
    fjBalanceInclude :: [String]
} deriving (Show, Eq)

-- Reads the include files in the journal file
journalFileToJournal :: FilePath -> JournalFile -> IO Journal
journalFileToJournal path (JournalFile j ai ti bi) = do
  let dir = takeDirectory path
  acc <- fmap concat $ traverse (decodeAccountsFile) (map (dir </>) ai)
  txns <- fmap concat $ traverse (decodeJTransactionsFile) (map (dir </>) ti)
  bals <- fmap concat $ traverse (decodeBalanceFile) (map (dir </>) bi)
  return j{jAccounts = jAccounts j ++ acc,
           jTransactions = jTransactions j ++ txns,
           jBalances = jBalances j ++ bals}



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
    Journal
    <$> v .: "configuration"
    <*> v .: "accounts"
    <*> v .: "transactions"
    <*> v .: "balance-assertions"
  parseJSON _ = fail "Expected Object for Ledger value"

instance FromJSON JournalFile where
  parseJSON (Y.Object v) =
    JournalFile
    <$> (Journal
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
  toJSON (Journal config accounts txns bals) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "transactions" .= txns,
       "balance-assertions" .= bals]

  toEncoding (Journal config accounts txns bals) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "transactions" .= txns
    <> "balance-assertions" .= bals

-- To JSON instance
instance ToJSON JournalFile where
  toJSON (JournalFile (Journal config accounts txns bals)
          accIncl txnsIncl balsIncl) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "accounts-include" .= accIncl,
       "transactions" .= txns,
       "transactions-include" .= txnsIncl,
       "balance-assertions" .= bals,
       "balance-assertions-include" .= balsIncl]

  toEncoding (JournalFile (Journal config accounts txns bals)
              accIncl txnsIncl balsIncl) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "accounts-include" .= accIncl
    <> "transactions" .= txns
    <> "transactions-include" .= txnsIncl
    <> "balance-assertions" .= bals
    <> "balance-assertions-include" .= balsIncl
