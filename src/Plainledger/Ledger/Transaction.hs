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



-- -- Compute the balance from postings, assuming they are all from the same
-- -- accounts
-- computeBalance :: [PostingF Day Quantity] -> M.Map Day Quantity
-- computeBalance ps =
--   let
--     deltaMap :: M.Map Day Quantity
--     deltaMap = M.fromListWith (+) $
--                map (\p -> (pBalanceDate p, pAmount p)) ps

--     deltaList :: [(Day, Quantity)]
--     deltaList = M.toAscList deltaMap

--     balanceList :: [Quantity]
--     balanceList = scanl1 (+)
--                 $ map snd deltaList

--   in M.fromList $ zip (map fst deltaList) balanceList

-- -- Sorts the postings by accounts and compute the balance map
-- computeBalancePs :: [PostingF Day Quantity] -> BalanceMap
-- computeBalancePs ps =
--   let byId :: [[PostingF Day Quantity]]
--       byId = groupBy ((==) `on` pAccount)
--            $ sortOn pAccount ps

--       accounts :: [T.Text]
--       accounts = map (pAccount . head) byId

--       balance1 :: [M.Map Day Quantity]
--       balance1 = map computeBalance byId

--   in HM.fromList $ zip accounts balance1

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