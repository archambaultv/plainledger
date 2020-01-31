{-# LANGUAGE OverloadedStrings #-}

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
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Text as T
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
  deriving (Show)


-- | The Data.Yaml.Pretty configuration object created so that the
-- fields in the yaml file follow the convention of this software.
yamlPrettyConfig :: P.Config
yamlPrettyConfig = P.setConfCompare (comparing fieldOrder) P.defConfig
 where fieldOrder :: T.Text -> Int
       fieldOrder "id" = 0
       fieldOrder "date" = 1
       fieldOrder "balance-date" = 2
       fieldOrder "transfers" = 3
       fieldOrder "from" = 10
       fieldOrder "to" = 11
       fieldOrder "amount" = 12
       fieldOrder "commodity" = 13
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
