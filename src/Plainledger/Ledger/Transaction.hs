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

type Transaction = TransactionF Posting

transactionToJTransaction :: Transaction -> JTransaction
transactionToJTransaction t = fmap postingToJPosting t

-- | Asserts all transactions have valid unique transaction id
--  Asserts all transactions have valid postings
--  Asserts all transactions have a well defined commodity
--  Asserts all transactions balance to zero for all commodities
validateJTransactions :: (MonadError Error m) =>
                      Commodity ->
                      HS.HashSet T.Text ->
                      [JTransaction] ->
                      m (BalanceMap,BalanceMap, [Transaction])
validateJTransactions defComm accs jtransactions = do
    transactions <- traverse (jtransactionToTransaction defComm) jtransactions
    _ <- traverse (checkTxnAccount accs) transactions
    transactions1 <- validateTransactionsId transactions
    let (bm, bm2) = computeBalanceMap
             accs
             transactions1

    return (bm, bm2, transactions1)

computeBalanceMap :: HS.HashSet T.Text ->
                     [Transaction] ->
                     (BalanceMap, BalanceMap)
computeBalanceMap accs txns =
  let m0 = HM.fromList $ zip (HS.toList accs) (repeat HM.empty)

      postings = concatMap (tPostings) txns
      postingsByAccount = groupBy ((==) `on` pAccount)
                        $ sortBy (comparing pAccount) postings
  in foldr addAccount (m0, m0) postingsByAccount

  where addAccount :: [Posting] ->
                      (BalanceMap, BalanceMap) ->
                      (BalanceMap, BalanceMap)
        addAccount ps (m1, m2) =
          let ps' = groupBy ((==) `on` pCommodity)
                  $ sortBy (comparing pCommodity) ps
              acc = pAccount $ head ps
          in foldr (addCommodity acc) (m1, m2) ps'

        addCommodity :: T.Text ->
                        [Posting] ->
                        (BalanceMap, BalanceMap) ->
                        (BalanceMap, BalanceMap)
        addCommodity acc ps (m1, m2) =
          let comm = pCommodity $ head ps
              m1' = addDate ps acc comm pDate m1
              m2' = addDate ps acc comm pBalanceDate m2
          in (m1', m2')

        addDate :: [Posting] ->
                   T.Text ->
                   Commodity ->
                   (Posting -> Day) ->
                   BalanceMap ->
                   BalanceMap
        addDate ps acc comm f m =
          let psDate = reverse
                  $ groupBy ((==) `on` f)
                  $ sortBy (comparing f) ps

              dateTotal :: [(Day, Quantity)]
              dateTotal = reverse $ foldr (sumDate f) [] psDate

              dateMap = M.fromList dateTotal
              commMap = HM.singleton comm dateMap

          in HM.insertWith HM.union acc commMap m

        sumDate :: (Posting -> Day) ->
                   [Posting] ->
                   [(Day, Quantity)] ->
                   [(Day, Quantity)]
        sumDate _ [] acc = acc
        sumDate getDate ps acc =
          let d = getDate $ head ps
              a = sum $ map pAmount ps
              previousBalance = if null acc
                                then 0
                                else snd (head acc)
          in (d, a + previousBalance) : acc


checkTxnAccount :: (MonadError Error m) =>
                    HS.HashSet T.Text -> Transaction -> m ()
checkTxnAccount s t = traverse foo (tPostings t) >> return ()
  where foo p = if HS.member (pAccount p) s
                then return ()
                else throwError
                     $ "Unknown account id \""
                     ++ T.unpack (pAccount p)
                     ++ "\"."

-- Balance postings, fill default commodity and balance date
jtransactionToTransaction :: (MonadError Error m) =>
                             Commodity -> JTransaction -> m Transaction
jtransactionToTransaction defComm (Transaction d tId p tags) =
  -- First we update the balance-date and commodity field of each posting,
  -- then group the postings by commodity
  let ps :: [PostingF Day Day (Maybe Quantity)]
      ps = map (setPostingDate d . fromCommodity defComm . fromBalanceDate d) p

      psGroup = groupBy ((==) `on` pCommodity)
                $ sortBy (comparing pCommodity) ps
  in do
    -- Now for each commodity we balance the postings to zero
    ps2 <- concat <$> traverse balancePostings psGroup
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
