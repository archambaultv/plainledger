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
  Journal(..),
  yamlPrettyConfig,
  module Plainledger.Journal.Transaction,
  module Plainledger.Journal.Balance,
  module Plainledger.Journal.Configuration,
  module Plainledger.Journal.Account,
  module Plainledger.Journal.Amount
  )
where

import Data.Ord
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as P
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Plainledger.Journal.Transaction
import Plainledger.Journal.Balance
import Plainledger.Journal.Configuration
import Plainledger.Journal.Account
import Plainledger.Journal.Amount

-- | The Journal data type represents a journal file
data Journal = Journal
  {_configuration :: Configuration,
   _accounts   :: [Account],
   _transactions :: [Transaction],
   _balances :: [Balance]
  }
  deriving (Eq, Show)


-- | The Data.Yaml.Pretty configuration object created so that the
-- fields in the yaml file follow the convention of this software.
yamlPrettyConfig :: P.Config
yamlPrettyConfig = P.setConfCompare (comparing fieldOrder) P.defConfig
 where fieldOrder :: T.Text -> Int
       -- Id always come first
       fieldOrder "id" = 0
       -- Transaction fields
       fieldOrder "date" = 1
       fieldOrder "balance-date" = 2
       fieldOrder "note" = 3
       fieldOrder "counterparty" = 4
       fieldOrder "transfers" = 5
       fieldOrder "tags" = 6
       -- Balance fields
       fieldOrder "account" = 8
       -- Transfer fields
       fieldOrder "from" = 10
       fieldOrder "to" = 11
       fieldOrder "amount" = 12
       fieldOrder "commodity" = 13

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

       fieldOrder _ = 99

-- FromJSON instances
instance FromJSON Journal where
  parseJSON (Y.Object v) =
    Journal
    <$> v .: "configuration"
    <*> v .: "accounts"
    <*> v .: "transactions"
    <*> v .: "balance-assertions"
  parseJSON _ = fail "Expected Object for Journal value"

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
