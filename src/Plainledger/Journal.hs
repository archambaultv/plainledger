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
import Control.Monad.Except
import Plainledger.Error
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
    jfConfiguration :: Configuration,
    jfAccounts :: [String],
    jfTransactions :: [String],
    jfBalances :: [String]
} deriving (Show, Eq)

-- Reads the include files in the journal file
journalFileToJournal :: FilePath -> JournalFile -> ExceptT Error IO Journal
journalFileToJournal path (JournalFile c ai ti bi) = do
  let dir = takeDirectory path
  acc <- fmap concat $ traverse (decodeAccountsFile) (map (dir </>) ai)
  txns <- fmap concat $ traverse (decodeJTransactionsFile) (map (dir </>) ti)
  bals <- fmap concat $ traverse (decodeBalanceFile) (map (dir </>) bi)
  return $ Journal c acc txns bals

-- | The Data.Yaml.Pretty configuration object created so that the
-- fields in the yaml file follow the convention of this software.
yamlPrettyConfig :: P.Config
yamlPrettyConfig = P.setConfCompare (comparing fieldOrder) P.defConfig
 where fieldOrder :: T.Text -> Int
       -- Top level fields
       fieldOrder "configuration" = 1
       fieldOrder "accounts" = 2
       fieldOrder "transactions" = 3
       fieldOrder "balance-assertions" = 4
       -- Configuration fields
       fieldOrder "opening-balance-account" = 5
       fieldOrder "earnings-account" = 6

       fieldOrder _ = 99

instance FromJSON JournalFile where
  parseJSON (Y.Object v) =
    JournalFile
    <$> v .: "configuration"
    <*> (fromMaybe [] <$> v .:? "accounts")
    <*> (fromMaybe [] <$> v .:? "transactions")
    <*> (fromMaybe [] <$> v .:? "balance-assertions")

  parseJSON _ = fail "Expected Object for Ledger value"

-- To JSON instance
instance ToJSON JournalFile where
  toJSON (JournalFile config accounts txns bals) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "transactions" .= txns,
       "balance-assertions" .= bals]

  toEncoding (JournalFile config accounts txns bals) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "transactions" .= txns
    <> "balance-assertions" .= bals
