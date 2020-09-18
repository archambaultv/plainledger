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
  validateTrialBalances,
  minDate,
  maxDate,
  cashFlow,
  balance,
  openingBalance,
  ledgerOpeningBalance,
  earnings,
  module Plainledger.Journal.Balance
  )
where

import Data.Time
import Data.Maybe
import Control.Monad.Except
import Data.Functor.Foldable
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

-- Computes the balance at the end of the day
balance :: BalanceMap -> T.Text -> LDate -> Quantity
balance m acc d = maybe 0 snd $ balanceAtDate m acc d

-- Computes the balance at the end of the previous day
openingBalance :: BalanceMap -> T.Text -> LDate -> Quantity
openingBalance _ _ MinDate = 0
openingBalance m acc (Date d) =
  maybe 0 snd $ balanceAtDate m acc (Date $ addDays (-1) d)
openingBalance m acc MaxDate =
  case balanceAtDate m acc MaxDate of
    Nothing -> 0
    Just (d, _) -> openingBalance m acc (Date d)

-- cashFlow m acc (d1, d2) computes the cashflow from the start of d1 to the end of d2.
cashFlow :: BalanceMap -> T.Text -> (LDate, LDate) -> Quantity
cashFlow m acc (d1, d2) = balance m acc d2 - openingBalance m acc d1

-- Balances :
--  Asserts all balance have a valid account field
--  Asserts all balance assertions are correct
validateBalances :: forall m . (MonadError Error m) =>
                      [Account] ->
                      BalanceMap ->
                      [Balance] ->
                      m ()
validateBalances accs balMap bs = do
    let accSet = HS.fromList $ map aId accs
    _ <- traverse (checkBalanceAccount accSet) (map bAccount bs)
    _ <- traverse checkBalanceAmount bs
    return ()

  where checkBalanceAmount :: Balance -> m ()
        checkBalanceAmount b =
          let d = bDate b
              bAtDate = snd <$> balanceAtDate balMap (bAccount b) (Date d)
          in checkBalance (bAccount b) (bDate b) (bAtDate, bAmount b)

checkBalance :: (MonadError Error m) => T.Text -> Day -> (Maybe Quantity, Quantity) -> m ()
checkBalance accId date z =
  case z of
       (Nothing, 0) -> return ()
       (Nothing, x) ->
              throwError
              $ "Balance assertion failed for account \""
              ++ T.unpack accId
              ++ "\" on date "
              ++ show date
              ++ ".\nAsserted balance : "
              ++ show x
              ++ "\nComputed balance : 0 (no transaction for \
                 \this account)"
       (Just y, x) | y /= x ->
            throwError
             $ "Balance assertion failed for account \""
             ++ T.unpack accId
             ++ "\" on date "
             ++ show date
             ++ ".\nAsserted balance : "
             ++ show x
             ++ "\nComputed balance : "
             ++ show y
             ++ "."
             ++ "\nDifference       : "
             ++ show (x - y)
       _ -> return ()

checkBalanceAccount :: (MonadError Error m) => HashSet T.Text -> T.Text -> m ()
checkBalanceAccount s b =
        if HS.member b s
        then return ()
        else throwError
             $ "Unknown account id \""
             ++ T.unpack b
             ++ "\"."

validateTrialBalances :: forall m . (MonadError Error m) =>
                      Configuration ->
                      [Account] ->
                      ChartOfAccount ->
                      BalanceMap ->
                      [TrialBalanceAssertion] ->
                      m ()
validateTrialBalances config accs accTree balMap bs = do
  let accSet = HS.fromList $ map aId accs
  let accMap = HM.fromList $ map (\a -> (aId a, a)) accs
  _ <- traverse (checkBalanceAccount accSet) (map tbaAccount bs)
  _ <- traverse (checkBalanceAmount accMap) bs
  return ()

  where checkBalanceAmount :: HashMap T.Text Account -> TrialBalanceAssertion -> m ()
        checkBalanceAmount accMap b =
          let sd = tbaStartDate b
              ed = tbaEndDate b
              accId = (tbaAccount b)
              startBal = Just $ openingBalance balMap accId (Date sd)
              endBal = snd <$> balanceAtDate balMap accId (Date ed)
              group = aGroup $ accMap HM.! accId
              amount = if isIncomeStatementGroup group
                       then ((-) <$> endBal <*> startBal)
                       else if accId == cOpeningBalanceAccount config
                            then (+) <$> endBal <*> Just (ledgerOpeningBalance accTree balMap (Date sd))
                            else endBal
          in checkBalance (tbaAccount b) (tbaEndDate b) (amount, tbaAmount b)


-- | Computes the opening balance
ledgerOpeningBalance :: ChartOfAccount -> BalanceMap -> LDate -> Quantity
ledgerOpeningBalance acc m d = cata alg acc
  where alg :: TreeF ChartNode Quantity -> Quantity
        alg = sumIncomeStatement (\a -> openingBalance m (aId a) d)


-- -- | Computes the earnings
earnings :: ChartOfAccount -> BalanceMap -> (LDate, LDate) -> Quantity
earnings acc m d = cata alg acc
  where alg :: TreeF ChartNode Quantity -> Quantity
        alg = sumIncomeStatement (\a -> cashFlow m (aId a) d)

-- Helper for ledgerOpeningBalance and earnings
sumIncomeStatement :: (Account -> Quantity) ->
                      TreeF ChartNode Quantity ->
                      Quantity
sumIncomeStatement f (NodeF (CAccount a) _) = f a
sumIncomeStatement _ (NodeF (Group _ x) xs) | isIncomeStatementGroup x = sum xs
sumIncomeStatement _ (NodeF (Group _ _) _) = 0
sumIncomeStatement _ (NodeF _ xs) = sum xs
