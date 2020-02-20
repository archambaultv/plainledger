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
                  (HashMap Commodity (M.Map Day Quantity))

maxDate :: BalanceMap -> Maybe Day
maxDate m =
  let m1 :: HashMap
            T.Text
            (HashMap Commodity (Maybe Day))
      m1 = fmap (fmap (fmap fst . M.lookupMax)) m

      m2 :: HashMap
            T.Text
            [Day]
      m2 = fmap (catMaybes . HM.elems) m1

      days :: [Day]
      days = concat $ HM.elems m2

  in if null days then Nothing else Just (maximum days)

minDate :: BalanceMap -> Maybe Day
minDate m =
  let m1 :: HashMap
            T.Text
            (HashMap Commodity (Maybe Day))
      m1 = fmap (fmap (fmap fst . M.lookupMin)) m

      m2 :: HashMap
            T.Text
            [Day]
      m2 = fmap (catMaybes . HM.elems) m1

      days :: [Day]
      days = concat $ HM.elems m2

  in if null days then Nothing else Just (minimum days)

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

-- | balanceAtDate m d acc returns the balance of account acc at the date d. The
-- balance is Nothing if the date is prior than any date in the map for this account.
-- Calls error if the account does not exists
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

balanceDelta :: Maybe (Day, Quantity) ->
                Maybe (Day, Quantity) ->
                Maybe (Day, Quantity)
balanceDelta md1 md2 =
  case (md1, md2) of
      (_, Nothing) -> Nothing
      (Nothing, x) -> x
      (Just (_, m1), Just (d, m2)) -> Just (d, (m2 - m1))

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
