-- |
-- Module      :  Plainledger.Ledger.Transaction
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Transaction data type.

module Plainledger.Ledger.Transaction (
  -- TransactionF(..),
  -- Transaction,
  -- transactionToJTransaction,
  -- validateJTransactions,
  )
where

-- import Data.Function
-- import Data.Ord
-- import Data.List
-- import Data.Time
-- import Plainledger.Ledger.Posting
-- import Plainledger.Journal
-- import Plainledger.Error
-- import Plainledger.Ledger.Balance
-- import Plainledger.Internal.Utils
-- import qualified Data.HashSet as HS
-- import qualified Data.HashMap.Strict as HM
-- import Prelude hiding (lines)
-- import Text.Printf (printf)
-- import qualified Data.Text as T
-- import qualified Data.Map.Strict as M
-- import Control.Monad.Except
-- import Data.Bifunctor


-- transactionToJTransaction :: Transaction -> JTransaction
-- transactionToJTransaction t = fmap postingToJPosting t





-- -- Computes a balanceMap from the transaction date and the balance date
-- computeBalanceTx :: [T.Text] -> [Transaction] -> (BalanceMap, BalanceMap)
-- computeBalanceTx accs txns =
--   let psTx = concatMap
--              -- Overwrite balance date by the transaction date
--              (\t -> map (first (const (tDate t))) (tPostings t))
--              txns

--       psBal = concatMap tPostings txns

--       -- The empty map for all accounts
--       map0 :: BalanceMap
--       map0 = HM.fromList $ zip accs (repeat M.empty)

--       mapTx = computeBalancePs psTx
--       mapBal = computeBalancePs psBal

--   in (HM.union mapTx map0, HM.union mapBal map0)