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
  TransactionF(..),
  Transaction,
  transactionToJTransaction,
  validateJTransactions,
  )
where

import Data.Function
import Data.Ord
import Data.List
import Data.Time
import Plainledger.Ledger.Posting
import Plainledger.Journal
import Plainledger.Error
import Plainledger.Ledger.Balance
import Plainledger.Internal.Utils
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Prelude hiding (lines)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad.Except
import Data.Bifunctor

type Transaction = TransactionF Posting

transactionToJTransaction :: Transaction -> JTransaction
transactionToJTransaction t = fmap postingToJPosting t

-- | Asserts all transactions have valid unique transaction id
--  Asserts all transactions have valid postings
--  Asserts all transactions balance to zero for all commodities
validateJTransactions :: (MonadError Error m) =>
                      HS.HashSet T.Text ->
                      [JTransaction] ->
                      m (BalanceMap,BalanceMap, [Transaction])
validateJTransactions accs jtransactions = do
    transactions <- traverse jtransactionToTransaction jtransactions
    _ <- traverse (checkTxnAccount accs) transactions
    transactions1 <- validateTransactionsId transactions
    let (bm, bm2) = computeBalanceTx transactions1

    return (bm, bm2, transactions1)

-- Compute the balance from postings, assuming they are all from the same
-- accounts
computeBalance :: [PostingF Day Quantity] -> M.Map Day Quantity
computeBalance ps =
  let
    deltaMap :: M.Map Day Quantity
    deltaMap = M.fromListWith (+) $
               map (\p -> (pBalanceDate p, pAmount p)) ps

    deltaList :: [(Day, Quantity)]
    deltaList = M.toAscList deltaMap

    balanceList :: [Quantity]
    balanceList = scanl1 (+)
                $ map snd deltaList

  in M.fromList $ zip (map fst deltaList) balanceList

-- Sorts the postings by accounts and compute the balance map
computeBalancePs :: [PostingF Day Quantity] -> BalanceMap
computeBalancePs ps =
  let byId :: [[PostingF Day Quantity]]
      byId = groupBy ((==) `on` pAccount)
           $ sortOn pAccount ps

      accounts :: [T.Text]
      accounts = map (pAccount . head) byId

      balance :: [M.Map Day Quantity]
      balance = map computeBalance byId

  in HM.fromList $ zip accounts balance

-- Computes a balanceMap from the transaction date and the balance date
computeBalanceTx :: [Transaction] -> (BalanceMap, BalanceMap)
computeBalanceTx txns =
  let psTx = concatMap
             (\t -> map (first (const (tDate t))) (tPostings t))
             txns

      psBal = concatMap tPostings txns

  in (computeBalancePs psTx, computeBalancePs psBal)

checkTxnAccount :: (MonadError Error m) =>
                    HS.HashSet T.Text -> Transaction -> m ()
checkTxnAccount s t = traverse foo (tPostings t) >> return ()
  where foo p = if HS.member (pAccount p) s
                then return ()
                else throwError
                     $ "Unknown account id \""
                     ++ T.unpack (pAccount p)
                     ++ "\"."

-- Balance postings, fill balance date
jtransactionToTransaction :: (MonadError Error m) =>
                             JTransaction -> m Transaction
jtransactionToTransaction (Transaction d tId p tags) =
  -- We update the balance-date if it is null
  let ps :: [PostingF Day (Maybe Quantity)]
      ps = map (fromBalanceDate d) p
  in do
    -- We balance the postings to zero
    ps2 <- balancePostings ps
    return $ Transaction d tId ps2 tags

-- Create an id of the form YYYY-MM-DD-N where N is a number if the field
-- transaction id is null
validateTransactionsId :: (MonadError Error m) =>
                           [Transaction] -> m [Transaction]
validateTransactionsId ts = do
    let (noId, withId) = partition (T.null . tTransactionId) ts
    validateTransactionsIdNoDup withId
    let knownIds = HS.fromList (map tTransactionId withId)
    -- Now what we have to do :
    -- 1) Group by date
    -- 2) Add a number to the date without clashing with the knownIds to
    --    generate an id of the form YYYY-MM-DD-N
    let groupDate = groupBy ((==) `on` tDate) $ sortBy (comparing tDate) noId
    return $ concatMap (createId knownIds 1) groupDate ++ withId

  where createId :: HS.HashSet T.Text ->
                    Int ->
                    [Transaction] ->
                    [Transaction]
        createId _ _ [] = []
        createId knownIds n (t:tss) =
          let tId = T.pack $ show (tDate t) ++ "-" ++ printf "%02d" n
          in if HS.member tId knownIds
             then createId knownIds (n + 1) (t:tss)
             else t{tTransactionId = tId} : createId knownIds (n + 1) tss

validateTransactionsIdNoDup :: (MonadError Error m) =>
                               [Transaction] ->
                               m ()
validateTransactionsIdNoDup xs =
  let dup = findDuplicates (map tTransactionId xs)
  in if null dup
     then return ()
     else throwError
          $ "Duplicate transaction id : "
          ++ (intercalate " "
             $ map (\k -> "\"" ++ T.unpack k ++ "\"")
             $ dup)
          ++ "."
