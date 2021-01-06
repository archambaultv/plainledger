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
  BalanceMap,
  balanceAtDate,
  balance,
  openingBalance,
  cashFlow,
  journalOpeningBalance,
  earnings,
  postingsToBalanceMap,
  transactionsToBalanceMap,
  trialBalanceQuantity
  )
where

import Data.Time
import Data.List
import Data.Function
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Plainledger.Journal.Amount
import Plainledger.Journal.Transaction
import Plainledger.Journal.Posting
import Plainledger.Journal.Account



-- / The BalanceMap type reprensents the balance of each accounts at each day.
-- Only the dates where the balance changes are recorded.
type BalanceMap = HashMap
                   T.Text
                  (M.Map Day Quantity)

transactionsToBalanceMap :: [Transaction] -> BalanceMap
transactionsToBalanceMap txns =
  let ps = concatMap tPostings
         $ map (\t -> fmap (changePostingDate $ tDate t) t) txns
  in postingsToBalanceMap ps

-- Sorts the postings by accounts and compute the balance map
postingsToBalanceMap :: [Posting] -> BalanceMap
postingsToBalanceMap ps =
  let byId :: [[Posting]]
      byId = groupBy ((==) `on` pAccount)
           $ sortOn pAccount ps

      accounts :: [T.Text]
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
             T.Text ->
             M.Map Day Quantity 
getBalance m acc =
  case HM.lookup acc m of
    Nothing -> error
               $ "balanceAtDate : Account \""
               ++ T.unpack acc
               ++ "\" is not in the balance map."
    Just m2 -> m2

-- | balanceAtDate m d acc returns the balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceAtDate :: BalanceMap ->
                 T.Text ->
                 Day ->
                 Maybe (Day, Quantity)
balanceAtDate m acc d = 
  let m2 = getBalance m acc
  in M.lookupLE d m2

-- Computes the balance at the end of the day
-- For the opening balance account see journalOpeningBalance
balance :: BalanceMap -> T.Text -> Day -> Quantity
balance m acc d = maybe 0 snd $ balanceAtDate m acc d

-- Computes the balance at the end of the previous day
-- For the opening balance account see journalOpeningBalance
openingBalance :: BalanceMap -> T.Text -> Day -> Quantity
openingBalance m acc d =
  maybe 0 snd $ balanceAtDate m acc (addDays (-1) d)


-- cashFlow m acc (d1, d2) computes the cashflow from the start of d1 to the end of d2.
cashFlow :: BalanceMap -> T.Text -> (Day, Day) -> Quantity
cashFlow m acc (d1, d2) = balance m acc d2 - openingBalance m acc d1

-- | Computes the opening balance
-- | Nothing means
journalOpeningBalance :: (T.Text -> AccountType) -> BalanceMap -> Day -> Quantity
journalOpeningBalance accTypeF m d = 
  let incomeStatementBal = map snd
                          $ filter (isIncomeStatementType . accTypeF . fst) 
                          $ HM.toList m
  in sum $ map snd $ mapMaybe (M.lookupLE d) incomeStatementBal

-- | Computes the earnings
earnings :: (T.Text -> AccountType) -> BalanceMap -> (Day, Day) -> Quantity
earnings accTypeF m (d1, d2) = 
  let incomeStatementBal = map snd
                          $ filter (isIncomeStatementType . accTypeF . fst) 
                          $ HM.toList m
      end = sum $ map snd $ mapMaybe (M.lookupLE d2) incomeStatementBal
      start = sum $ map snd $ mapMaybe (M.lookupLE (addDays (-1) d1)) incomeStatementBal
  in end - start

-- Computes the quantity that will show up in the trial balance
trialBalanceQuantity :: T.Text -> 
                        (T.Text -> AccountType) ->
                        BalanceMap -> 
                        T.Text -> 
                        (Day, Day) -> 
                        Quantity
trialBalanceQuantity openAcc accTypef m acc (sd, ed)
  | openAcc == acc = balance m acc ed + journalOpeningBalance accTypef m sd
  | isIncomeStatementType (accTypef acc) = cashFlow m acc (sd, ed)
  | otherwise = balance m acc ed