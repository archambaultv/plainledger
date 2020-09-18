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
import qualified Data.HashSet as HS

data Ledger = Ledger {
  lJournal :: JournalF Transaction,
  lBalanceMap :: BalanceMap,
  lAccounts :: ChartOfAccount
} deriving (Eq, Show)

-- | Converts the journal to a ledger.
journalToLedger :: (MonadError Error m) => Journal -> m Ledger
journalToLedger (Journal config accounts txns bals trialBals) = do
  validateConfig config
  accounts' <- validateAccounts accounts config
  let accTree = accountsToChartOfAccounts accounts'
  let accSet = HS.fromList $ map aId accounts'
  (balanceMap, balanceAssertionMap, transactions') <- validateJTransactions accSet txns
  _ <- validateBalances accounts' balanceAssertionMap bals
  _ <- validateTrialBalances config accounts' accTree balanceMap trialBals
  return $ Ledger (Journal config accounts' transactions' bals trialBals)
                  balanceMap
                  accTree
