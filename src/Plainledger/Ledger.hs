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
  journalToLedger,
  module Plainledger.Ledger.Posting,
  module Plainledger.Ledger.Transaction,
  module Plainledger.Ledger.Balance,
  module Plainledger.Journal
  )
where

import Control.Monad.Except
import Plainledger.Error
import Plainledger.Journal
import Plainledger.Ledger.Balance
import Plainledger.Ledger.Posting
import Plainledger.Ledger.Transaction
import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

data Ledger = Ledger {
  lJournal :: JournalF Transaction,
  lBalanceMap :: BalanceMap,
  lAccounts :: HM.HashMap T.Text Account
} deriving (Eq, Show)

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
journalToLedger (Journal config accounts txns bals) = do
  validateConfig config
  accounts' <- validateAccounts accounts
  let accMap = HM.fromList
              $ map (\a -> (aId a, a)) accounts'
  let accSet = HS.fromList $ map aId accounts'
  (balanceMap, balanceAssertionMap, transactions') <- validateJTransactions
                   accSet
                   txns
  balances' <- validateBalances
               accSet
               balanceAssertionMap
               bals
  return $ Ledger (Journal config accounts' transactions' balances')
                  balanceMap
                  accMap
