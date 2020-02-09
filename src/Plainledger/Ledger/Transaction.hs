-- |
-- Module      :  Plainledger.Ledger.Transfer
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Transaction and Transfer data type.

module Plainledger.Ledger.Transaction (
  TransactionF(..),
  Transaction,
  JTransaction,
  validateTransactions,
  encodeTransactions,
  decodeTransactions
  )
where

import Data.Function
import Data.List
import Data.Maybe
import Control.Monad.Except
import Data.Aeson (pairs)
import Data.ByteString.Lazy (ByteString)
import Data.Csv (Record, Field, ToField(..),toRecord)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Scientific
import Data.Time
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import GHC.Generics hiding (to, from)
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Ledger.Amount
import Plainledger.Ledger.Day
import Plainledger.Ledger.Tag
import Plainledger.Ledger.Posting
import Plainledger.Ledger.Account
import Plainledger.Internal.Utils
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y

data TransactionF p = Transaction
  {
    tDate :: Day,
    tTransactionId :: T.Text,
    tPostings :: [p],
    tTags :: [Tag]
  } deriving (Eq, Show, Generic)

type Transaction = TransactionF Posting
type JTransaction = TransactionF JPosting

-- | Asserts all transactions have valid unique transaction id
--  Asserts all transactions have valid postings
--  Asserts all transactions have a well defined commodity
--  Asserts all transactions balance to zero for all commodities
validateTransactions :: (MonadError Error m) =>
                      Commodity ->
                      [Account] ->
                      [JTransaction] ->
                      m [Transaction]
validateTransactions defComm accs jtransactions = do
    transactions <- traverse (jtransactionToTransaction defComm) jtransactions
    transactions1 <- validateTransactionsId transactions
    return transactions1


jtransactionToTransaction :: (MonadError Error m) =>
                             Commodity -> JTransaction -> m Transaction
jtransactionToTransaction defComm (Transaction d tId p tags) =
  -- First we update the balance-date and commodity field of each posting,
  -- then group the postings by commodity
  let ps :: [PostingF Day (Maybe Quantity)]
      ps = map (fromCommodity defComm . fromBalanceDate d) p

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
          let tId = T.pack $ show (tDate t) ++ "-" ++ show n
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

instance ToJSON JTransaction where
  toJSON (Transaction date tId postings tags) =
    Y.object
    $ ["date" .= date,
       "postings" .= postings]
   ++ (if T.null tId then [] else ["transaction-id" .= tId])
   ++ (if null tags then [] else ["tags" .= tags])

  toEncoding (Transaction date tId postings tags) =
    pairs
    $ "date" .= date
    <> (if T.null tId then mempty else "transaction-id" .= tId)
    <> "postings" .= postings
    <> (if null tags then mempty else "tags" .= tags)

instance FromJSON JTransaction where
  parseJSON (Y.Object v) =
    Transaction
    <$> (v .: "date")
    <*> (fromMaybe "" <$> v .:? "transaction-id")
    <*> v .: "postings"
    <*> (fromMaybe [] <$> v .:? "tags")
  parseJSON _ = fail "Expected Object for Transaction value"

-- CSV functions
coreHeader :: [Field]
coreHeader = ["date",
              "transaction-id",
              "group",
              "subgroup",
              "subsubgroup",
              "account id",
              "account number",
              "account name",
              "amount",
              "quantity",
              "commodity"]

-- / Encode a list of transaction as a Csv. The first line is the header
encodeTransactions :: [Transaction] -> ByteString
encodeTransactions xs = ""
  -- let tagH = tagHeader $ concatMap tfTags xs
  --     header = toRecord $ coreHeader ++ tagH
  --     lines = header : map (toLine tagH) xs
  -- in C.encode lines
  --
  -- where toLine :: [Field] -> Transfer -> Record
  --       toLine tagH t =
  --         let coreLine = [toField $ toISO8601 $ tfDate t,
  --                         toField $ toISO8601 $ pBalanceDateFrom t,
  --                         toField $ toISO8601 $ pBalanceDateTo t,
  --                         toField $ tfFrom t,
  --                         toField $ tfTo t,
  --                         toField $ (realToFrac (pAmount t) :: Scientific),
  --                         toField $ pCommodity t]
  --         in toRecord $ coreLine ++ tagLine (tfTags t) tagH

-- | The first line is the header
decodeTransactions :: (MonadError Error m) => ByteString -> m [Transaction]
decodeTransactions bs = return []
-- do
--   csv <- either throwError return $ C.decode C.NoHeader bs
--   csvToData (csv :: C.Csv) fromLine
--
--   where fromLine :: (MonadError Error m) =>
--                     HM.HashMap Field Field -> m Transfer
--         fromLine m = do
--           date <- findColumnM "date" m parseISO8601M
--           dateFrom <- findColumnDefaultM date "balance-date-from" m
--                       parseISO8601M
--           dateTo <- findColumnDefaultM date "balance-date-to" m parseISO8601M
--           from <- findColumn "from" m
--           to <- findColumn "to" m
--           amount <- (\x -> realToFrac (x :: Scientific))
--                     <$> findColumn "amount" m
--           comm <- findColumnDefault "" "commodity" m
--           tags <- recordToTags m (HS.fromList coreHeader)
--           return $ Transfer date dateFrom dateTo from to amount comm tags
