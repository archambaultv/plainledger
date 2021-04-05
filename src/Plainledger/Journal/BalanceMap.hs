-- |
-- Module      :  Plainledger.Journal.BalanceMap
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Balance data type representing balance assertions.

module Plainledger.Journal.BalanceMap
(
  BalanceMap(..),
  balanceAtDate,
  balance,
  cashFlow,
  openingBalance,
  journalOpeningBalance,
  earnings,
  postingsToBalanceMap,
  transactionsToBalanceMap,
  txnsToBalanceMapUsingBalanceDate,
  trialBalanceQuantity,
  balanceSheetQuantity
  )
where

import Data.Time
import Data.List ( groupBy, sortOn )
import Data.Function
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Plainledger.Journal.Amount
import Plainledger.Journal.Transaction
import Plainledger.Journal.Posting
import Plainledger.Journal.Account



-- / The BalanceMap type reprensents the balance of each accounts at each day.
-- Only the dates where the balance changes are recorded.
data BalanceMap = BalanceMap {
  bmBalances :: HashMap Account (M.Map Day Quantity),
  bmOpeningBalanceAcc :: Account,
  bmEarningAcc :: Account
  }
  deriving (Eq, Show)

-- Only takes the date of the transaction into account
transactionsToBalanceMap :: Account -> Account -> [Transaction] -> BalanceMap
transactionsToBalanceMap open earn txns =
  let ps = concatMap (tPostings . (\t -> fmap (changePostingDate $ tDate t) t)) txns
      bal = postingsToBalanceMap ps
  in BalanceMap bal open earn

-- Takes the date of the posting (Balance date) into account
txnsToBalanceMapUsingBalanceDate :: Account -> Account -> [Transaction] -> BalanceMap
txnsToBalanceMapUsingBalanceDate open earn txns =
  let ps = concatMap tPostings txns
      bal = postingsToBalanceMap ps
  in BalanceMap bal open earn

-- Sorts the postings by accounts and compute the balance map
postingsToBalanceMap :: [Posting] -> HashMap Account (M.Map Day Quantity)
postingsToBalanceMap ps =
  let byId :: [[Posting]]
      byId = groupBy ((==) `on` pAccount)
           $ sortOn pAccount ps

      accounts :: [Account]
      accounts = map (pAccount . head) byId

      balance1 :: [M.Map Day Quantity]
      balance1 = map computeBalance1 byId

  in HM.fromList $ zip accounts balance1

  where -- Compute the balance from postings, assuming they are all from the sameaccounts
    computeBalance1 :: [Posting] -> M.Map Day Quantity
    computeBalance1 ps1 =
      let
        deltaMap :: M.Map Day Quantity
        deltaMap = M.fromListWith (+) $
                   map (\p -> (pBalanceDate p, pAmount p)) ps1

        deltaList :: [(Day, Quantity)]
        deltaList = M.toAscList deltaMap

        balanceList :: [Quantity]
        balanceList = scanl1 (+)
                    $ map snd deltaList

       in M.fromList $ zip (map fst deltaList) balanceList

getBalance :: BalanceMap ->
             Account ->
             Maybe (M.Map Day Quantity)
getBalance m acc = HM.lookup acc (bmBalances m)

-- | balanceAtDate m d acc returns the balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceAtDate :: BalanceMap ->
                 Account ->
                 Day ->
                 Maybe (Day, Quantity)
balanceAtDate m acc d = getBalance m acc >>= M.lookupLE d

-- Computes the balance at the end of the day
-- For the opening balance account see journalOpeningBalance
balance :: BalanceMap -> Account -> Day -> Quantity
balance m acc d = maybe 0 snd $ balanceAtDate m acc d

-- Computes the balance at the end of the previous day
-- For the opening balance account see journalOpeningBalance
openingBalance :: BalanceMap -> Account -> Day -> Quantity
openingBalance m acc d =
  maybe 0 snd $ balanceAtDate m acc (addDays (-1) d)


-- cashFlow m acc (d1, d2) computes the cashflow from the start of d1 to the end of d2.
cashFlow :: BalanceMap -> Account -> (Day, Day) -> Quantity
cashFlow m acc (d1, d2) = balance m acc d2 - openingBalance m acc d1

-- | Computes the opening balance
journalOpeningBalance :: BalanceMap -> Day -> Quantity
journalOpeningBalance m d =
  let incomeStatementBal = map snd
                          $ filter (isIncomeStatementType . aAccountType . fst)
                          $ HM.toList (bmBalances m)
  in sum $ map snd $ mapMaybe (M.lookupLE (addDays (-1) d)) incomeStatementBal

-- | Computes the earnings
earnings :: BalanceMap -> (Day, Day) -> Quantity
earnings m (d1, d2) =
  let incomeStatementBal = map snd
                          $ filter (isIncomeStatementType . aAccountType . fst)
                          $ HM.toList (bmBalances m)
      end = sum $ map snd $ mapMaybe (M.lookupLE d2) incomeStatementBal
      start = sum $ map snd $ mapMaybe (M.lookupLE (addDays (-1) d1)) incomeStatementBal
  in end - start

-- Computes the quantity that will show up in the trial balance
trialBalanceQuantity :: BalanceMap ->
                        Account ->
                        (Day, Day) ->
                        Quantity
trialBalanceQuantity m acc (sd, ed)
  | isIncomeStatementType (aAccountType acc) = cashFlow m acc (sd, ed)
  | bmOpeningBalanceAcc m == acc = balance m acc ed + journalOpeningBalance m sd
  | otherwise = balance m acc ed

-- | Does not check if the account is trully a balance sheet account
balanceSheetQuantity :: BalanceMap ->
                        Account ->
                        (Day, Day) ->
                        Quantity
balanceSheetQuantity m acc (sd, ed)
  | bmEarningAcc m == acc = balance m acc ed + earnings m (sd, ed)
  | bmOpeningBalanceAcc m == acc = balance m acc ed + journalOpeningBalance m sd
  | otherwise = balance m acc ed