-- |
-- Module      :  Plainledger.Ledger.Balance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Ledger.Balance (
  BalanceMap,
  balanceAtDate,
  validateBalances,
  minDate,
  maxDate,
  module Plainledger.Journal.Balance
  )
where

import Data.Time
import Data.Maybe
import Control.Monad.Except
import qualified Data.Text as T
import Plainledger.Journal.Balance
import Plainledger.Journal
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Plainledger.Error
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

-- / The BalanceMap type reprensents the balance of each accounts at each day.
-- Only the dates where the balance changes are recorded.
type BalanceMap = HashMap
                  T.Text
                  (M.Map Day Quantity)

-- The maximum date in the balance map
maxDate :: BalanceMap -> Maybe Day
maxDate m =
  case map fst $ mapMaybe M.lookupMax $ HM.elems m of
    [] -> Nothing
    days -> Just (maximum days)

-- The minimum date in the balance map
minDate :: BalanceMap -> Maybe Day
minDate m =
  case map fst $ mapMaybe M.lookupMax $ HM.elems m of
    [] -> Nothing
    days -> Just (minimum days)

balanceDate :: BalanceMap ->
                 T.Text ->
                 (M.Map Day Quantity -> Maybe (Day, Quantity)) ->
                 Maybe (Day, Quantity)
balanceDate m acc f =
  case HM.lookup acc m of
    Nothing -> error
               $ "balanceAtDate : Account \""
               ++ T.unpack acc
               ++ "\" is not in the balance map."
    Just m2 -> f m2

-- | balanceAtDate m d acc returns the balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceAtDate :: BalanceMap ->
                 T.Text ->
                 LDate ->
                 Maybe (Day, Quantity)
balanceAtDate m acc d =
  case d of
    MinDate -> balanceDate m acc M.lookupMin
    Date d2 -> balanceDate m acc (M.lookupLE d2)
    MaxDate -> balanceDate m acc M.lookupMax

-- Balances :
--  Asserts all balance have a valid account field
--  Asserts all balance assertions are correct
validateBalances :: (MonadError Error m) =>
                      HashSet T.Text ->
                      BalanceMap ->
                      [Balance] ->
                      m [Balance]
validateBalances accs balMap bs = do
    _ <- traverse (checkBalanceAccount accs) bs
    _ <- traverse (checkBalanceAmount balMap) bs
    return bs

  where checkBalanceAccount :: (MonadError Error m) =>
                                HashSet T.Text -> Balance -> m ()
        checkBalanceAccount s b =
          if HS.member (bAccount b) s
          then return ()
          else throwError
               $ "Unknown account id \""
               ++ T.unpack (bAccount b)
               ++ "\"."

        checkBalanceAmount :: (MonadError Error m) =>
                              BalanceMap -> Balance -> m ()
        checkBalanceAmount m b =
          let bAtDate = snd
                      <$> balanceAtDate m (bAccount b) (Date $ bDate b)
          in case (bAtDate, bAmount b) of
               (Nothing, 0) -> return ()
               (Nothing, x) ->
                      throwError
                      $ "Balance assertion failed for account \""
                      ++ T.unpack (bAccount b)
                      ++ "\" on date "
                      ++ show (bDate b)
                      ++ ".\nAsserted balance : "
                      ++ show x
                      ++ "\nComputed balance : 0 (no transaction for \
                         \this account)"
               (Just y, x) | y /= x ->
                    throwError
                     $ "Balance assertion failed for account \""
                     ++ T.unpack (bAccount b)
                     ++ "\" on date "
                     ++ show (bDate b)
                     ++ ".\nAsserted balance : "
                     ++ show x
                     ++ "\nComputed balance : "
                     ++ show y
                     ++ "."
                     ++ "\nDifference       : "
                     ++ show (x - y)
               _ -> return ()
