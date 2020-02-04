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
  Ledger(..),
  yamlPrettyConfig,
  validateLedger,
  module Plainledger.Ledger.Transfer,
  module Plainledger.Ledger.Balance,
  module Plainledger.Ledger.Configuration,
  module Plainledger.Ledger.Account,
  module Plainledger.Ledger.Amount
  )
where

import Data.Ord
import Data.Maybe
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as P
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Plainledger.Ledger.Transfer
import Plainledger.Ledger.Balance
import Plainledger.Ledger.Configuration
import Plainledger.Ledger.Account
import Plainledger.Ledger.Amount
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Control.Monad.Except
import Plainledger.Error

-- | The Ledger data type represents a graph where the accounts are the nodes
-- and the transfers are the edges
data Ledger = Ledger
  {lConfiguration :: Configuration,
   lAccounts   :: [Account],
   lTransfers :: [Transfer],
   lBalances :: [Balance]
  }
  deriving (Eq, Show)

-- | validateLedger verifies a series of properties that a valid ledger should
-- satisfies :
-- Asserts all accounts group field are in the configuration group mapping.
-- Asserts all transfers have valid from and to field
-- Asserts all transfers have positive amount
-- Asserts all transfers have a well defined commodity
-- Asserts all balance have a valid account field
-- Asserts all balance have a well defined commodity
-- Asserts all balance assertions are respected
validateLedger :: (MonadError Error m) => Ledger -> m Ledger
validateLedger l =
  let config = lConfiguration l
      accounts = lAccounts l
      transfers = lTransfers l
      balances = lBalances l
  in do
    validateGroupField (cGroupMapping config) accounts
    transfers' <- validateTransfers (cDefaultCommodity config) accounts transfers
    balances' <- validateBalances (cDefaultCommodity config) accounts balances
    return $ Ledger config accounts transfers' balances'

validateTransfers :: (MonadError Error m) =>
                      Commodity ->
                      [Account] ->
                      [Transfer] ->
                      m [Transfer]
validateTransfers _ _ x = return x

validateBalances :: (MonadError Error m) =>
                      Commodity ->
                      [Account] ->
                      [Balance] ->
                      m [Balance]
validateBalances _ _ x = return x

validateGroupField :: (MonadError Error m) =>
                      HashMap T.Text AccountGroup ->
                      [Account] ->
                      m ()
validateGroupField m accounts =
  let wrong = filter (\a -> isNothing $ HM.lookup (aGroup a) m) accounts
  in case wrong of
      [] -> return ()
      (a:_) -> throwError
               $ "Group \""
               ++ (T.unpack $ aGroup a)
               ++ "\" of account \""
               ++ (T.unpack $ aId a)
               ++ "\" is not in the configuration group-mapping."


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
instance FromJSON Ledger where
  parseJSON (Y.Object v) =
    Ledger
    <$> v .: "configuration"
    <*> v .: "accounts"
    <*> v .: "transfers"
    <*> v .: "balance-assertions"
  parseJSON _ = fail "Expected Object for Journal value"

-- To JSON instance
instance ToJSON Ledger where
  toJSON (Ledger config accounts txns bals) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "transfers" .= txns,
       "balance-assertions" .= bals]

  toEncoding (Ledger config accounts txns bals) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "transfers" .= txns
    <> "balance-assertions" .= bals