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
  balanceDelta,
  validateBalances,
  dateAmount,
  balanceDateAmount,
  module Plainledger.Journal.Balance
  )
where

import Data.Maybe
import Data.Time
import Control.Monad.Except
import qualified Data.Text as T
import Plainledger.Journal.Amount
import Plainledger.Journal.Balance
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
                  (HashMap Commodity (M.Map Day (Quantity, Quantity)))

dateAmount :: (Quantity, Quantity) -> Quantity
dateAmount = fst

balanceDateAmount :: (Quantity, Quantity) -> Quantity
balanceDateAmount = snd

-- | balanceAtDate m d acc returns the balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceAtDate :: BalanceMap ->
                 T.Text ->
                 Commodity ->
                 Day ->
                 Maybe (Quantity, Quantity)
balanceAtDate m acc comm d =
  case HM.lookup acc m of
    Nothing -> error
               $ "balanceAtDate : Account \""
               ++ T.unpack acc
               ++ "\" is not in the balance map."
    Just m2 -> HM.lookup comm m2 >>= M.lookupLE d >>= pure . snd

-- | balanceDelta m d acc returns the difference in balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
balanceDelta :: BalanceMap ->
                Day ->
                Day ->
                T.Text ->
                Commodity ->
                (Quantity, Quantity)
balanceDelta m d1 d2 acc comm =
  let (m11, m12) = fromMaybe (0,0) $ balanceAtDate m acc comm d1
      (m21, m22) = fromMaybe (0,0) $ balanceAtDate m acc comm d2
  in (m21 - m11, m22 - m12)

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
          let bAtDate = balanceDateAmount
                      <$> balanceAtDate m (bAccount b) (bCommodity b) (bDate b)
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
