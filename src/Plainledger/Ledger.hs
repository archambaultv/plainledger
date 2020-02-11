-- |
-- Module      :  Plainledger.Ledger
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the journal data type and reexports all the
-- data types and functions related to the journal file.

module Plainledger.Ledger (
  LedgerF(..),
  Ledger,
  Journal,
  yamlPrettyConfig,
  journalToLedger,
  module Plainledger.Ledger.Posting,
  module Plainledger.Ledger.Transaction,
  module Plainledger.Ledger.Balance,
  module Plainledger.Ledger.Configuration,
  module Plainledger.Ledger.Account,
  module Plainledger.Ledger.Amount,
  module Plainledger.Ledger.Tag
  )
where

import Data.Ord
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as P
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Plainledger.Ledger.Posting
import Plainledger.Ledger.Transaction
import Plainledger.Ledger.Balance
import Plainledger.Ledger.Configuration
import Plainledger.Ledger.Account
import Plainledger.Ledger.Amount
import Plainledger.Ledger.Tag
import Control.Monad.Except
import Plainledger.Error

data LedgerF t = Ledger
  {lConfiguration :: Configuration,
   lAccounts   :: [Account],
   lTransactions :: [t],
   lBalances :: [Balance]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Ledger = LedgerF Transaction
type Journal = LedgerF JTransaction

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
  transactions' <- validateTransactions
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
