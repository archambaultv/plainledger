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
  balanceBeforeDate,
  balanceDelta,
  validateBalances,
  flattenBalanceAtDate,
  flattenBalanceDelta,
  module Plainledger.Journal.Balance
  )
where

import Data.Time
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
                  (HashMap Commodity (M.Map Day Quantity))

-- | balanceAtDate m d acc returns the balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceDate :: BalanceMap ->
                 T.Text ->
                 Commodity ->
                 (M.Map Day Quantity -> Maybe (Day, Quantity)) ->
                 Maybe (Day, Quantity)
balanceDate m acc comm f =
  case HM.lookup acc m of
    Nothing -> error
               $ "balanceAtDate : Account \""
               ++ T.unpack acc
               ++ "\" is not in the balance map."
    Just m2 -> HM.lookup comm m2 >>= f

balanceBeforeDate :: BalanceMap ->
                 T.Text ->
                 Commodity ->
                 LDate ->
                 Maybe (Day, Quantity)
balanceBeforeDate m acc comm d =
  case d of
    MinDate -> Nothing
    Date d2 -> balanceDate m acc comm (M.lookupLT d2)
    MaxDate -> do
      (d2, _) <- balanceDate m acc comm M.lookupMax
      balanceDate m acc comm (M.lookupLT d2)

balanceAtDate :: BalanceMap ->
                 T.Text ->
                 Commodity ->
                 LDate ->
                 Maybe (Day, Quantity)
balanceAtDate m acc comm d =
  case d of
    MinDate -> balanceDate m acc comm M.lookupMin
    Date d2 -> balanceDate m acc comm (M.lookupLE d2)
    MaxDate -> balanceDate m acc comm M.lookupMax

flattenBalanceAtDate :: Commodity ->
                 BalanceMap ->
                 LDate ->
                 [(T.Text, Commodity, Maybe (Day, Quantity))]
flattenBalanceAtDate defComm m d =
  let defIfNull x = if null x then [defComm] else x

      keys :: [(T.Text, Commodity)]
      keys = concatMap (\(t, c) -> map (t, ) (defIfNull $ HM.keys c))
           $ HM.toList m

  in map (\(t,c) -> (t,c,) (balanceAtDate m t c d)) keys

flattenBalanceDelta :: Commodity ->
                 BalanceMap ->
                 LDate ->
                 LDate ->
                 [(T.Text, Commodity, Maybe (Day, Quantity))]
flattenBalanceDelta defComm m b e =
  let defIfNull x = if null x then [defComm] else x

      keys :: [(T.Text, Commodity)]
      keys = concatMap (\(t, c) -> map (t, ) (defIfNull $ HM.keys c))
           $ HM.toList m

  in map (\(t,c) -> (t,c,) (balanceDelta m t c b e)) keys

-- | balanceDelta m d acc returns the difference in balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceDelta :: BalanceMap ->
                T.Text ->
                Commodity ->
                LDate ->
                LDate ->
                Maybe (Day, Quantity)
balanceDelta m acc comm d1 d2 =
  let md1 = balanceBeforeDate m acc comm d1
      md2 = balanceAtDate m acc comm d2
  in case (md1, md2) of
      (_, Nothing) -> Nothing
      (Nothing, x) -> x
      (Just (_, m1), Just (d, m2)) ->
        Just (d, (m2 - m1))

-- Balances :
--  Asserts all balance have a valid account field
--  Asserts all balance have a well defined commodity
--  Asserts all balance assertions are correct
validateBalances :: (MonadError Error m) =>
                      Commodity ->
                      HashSet T.Text ->
                      BalanceMap ->
                      [Balance] ->
                      m [Balance]
validateBalances defComm accs balMap bs =
  let b1 = fillCommodity bs
  in do
    _ <- traverse (checkBalanceAccount accs) b1
    _ <- traverse (checkBalanceAmount balMap) b1
    return b1

  where checkBalanceAccount :: (MonadError Error m) =>
                                HashSet T.Text -> Balance -> m ()
        checkBalanceAccount s b =
          if HS.member (bAccount b) s
          then return ()
          else throwError
               $ "Unknown account id \""
               ++ T.unpack (bAccount b)
               ++ "\"."

        fillCommodity xs =
               map (\c -> if T.null $ bCommodity c
                          then c{bCommodity = defComm}
                          else c)
                   xs

        checkBalanceAmount :: (MonadError Error m) =>
                              BalanceMap -> Balance -> m ()
        checkBalanceAmount m b =
          let bAtDate = snd
                      <$> balanceAtDate m (bAccount b) (bCommodity b)
                          (Date $ bDate b)
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
               _ -> return ()
