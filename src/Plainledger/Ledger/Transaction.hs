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
  JTransaction,
  validateTransactions,
  transactionToJTransaction,
  encodeTransactions,
  decodeTransactions,
  CscEncodeOptions(..),
  CsvDecodeOptions(..)
  )
where

import Data.Function
import Data.List hiding (lines)
import Data.Maybe
import Text.Printf (printf)
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
import qualified Data.Set as S
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Vector as V

data TransactionF p = Transaction
  {
    tDate :: Day,
    tTransactionId :: T.Text,
    tPostings :: [p],
    tTags :: [Tag]
  } deriving (Show, Generic, Functor)

type Transaction = TransactionF Posting
type JTransaction = TransactionF JPosting

-- / We sort the tags when comparing two TransactionF
-- The Eq instance is mainly used in the unittests.
instance (Eq p) => Eq (TransactionF p) where
  a1 == a2 =  tDate a1 == tDate a2
           && tTransactionId a1 == tTransactionId a2
           && tPostings a1 == tPostings a2
           && sortBy (comparing tagId) (tTags a1) ==
              sortBy (comparing tagId) (tTags a2)

transactionToJTransaction :: Transaction -> JTransaction
transactionToJTransaction t = fmap postingToJPosting t

-- | Asserts all transactions have valid unique transaction id
--  Asserts all transactions have valid postings
--  Asserts all transactions have a well defined commodity
--  Asserts all transactions balance to zero for all commodities
validateTransactions :: (MonadError Error m) =>
                      Commodity ->
                      [Account] ->
                      [JTransaction] ->
                      m [Transaction]
validateTransactions defComm _ jtransactions = do
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
data CscEncodeOptions
  = EncodeAsSingleRecord
  | EncodeAsMultipleRecords


postingToLine :: JPosting -> [Field]
postingToLine p = [toField $ pAccount p,
                   toField $ (realToFrac <$> pAmount p :: Maybe Scientific),
                   toField $ pCommodity p,
                   toField $ toISO8601 <$> pBalanceDate p]

postingsHeader :: Int -> Int -> [Field]
postingsHeader n maxN | n > maxN = []
postingsHeader n maxN =
    [toField $ "account id (" ++ show n ++ ")",
     toField $ "amount (" ++ show n ++ ")",
     toField $ "commodity (" ++ show n ++ ")",
     toField $ "balance date ("++ show n ++ ")"]
    ++ postingsHeader (n + 1) maxN

multipleRecordsHeader :: [Field]
multipleRecordsHeader = ["date",
                         "transaction id",
                         "account id",
                         "amount",
                         "commodity",
                         "balance date"]

-- / Encode a list of transaction as a Csv. The first line is the header
encodeTransactions :: CscEncodeOptions -> [JTransaction] -> ByteString
encodeTransactions EncodeAsSingleRecord xs =
  let tagH = tagHeader $ concatMap tTags xs
      maxPostings = maximum $ 2 : map (length . tPostings) xs
      header = toRecord
             $ ["date", "transaction id"]
             ++ postingsHeader 1 maxPostings
             ++ tagH
      lines = header : map (toLine maxPostings tagH) xs
  in C.encode lines

  where toLine :: Int -> [Field] -> JTransaction -> Record
        toLine maxPostings tagH t =
          let psLine = concatMap postingToLine (tPostings t)
              txLine = [toField $ toISO8601 $ tDate t,
                        toField $ tTransactionId t]
                       ++ psLine
                       -- To take into accounts the extra space in the
                       -- header due to other trasactions having more postings
                       ++ take (4 * (maxPostings - length (tPostings t)))
                               (repeat "")

          in toRecord $ txLine  ++ tagLine (tTags t) tagH

encodeTransactions (EncodeAsMultipleRecords) xs =
  let tagH = tagHeader $ concatMap tTags xs
      header = toRecord $ multipleRecordsHeader  ++ tagH
      lines = header : concatMap (toMultiLine tagH) xs
  in C.encode lines

  where
        toMultiLine :: [Field] -> JTransaction -> [Record]
        toMultiLine tagH t =
                let front = [toField $ toISO8601 $ tDate t,
                             toField $ tTransactionId t]
                    ps = map postingToLine (tPostings t)
                    tags = tagLine (tTags t) tagH
                    txLines = map (\x -> front ++ x ++ tags) ps
                in map toRecord txLines

data CsvDecodeOptions = SingleRecord
                      | MultipleRecords

-- | The first line is the header
decodeTransactions :: (MonadError Error m) =>
                      CsvDecodeOptions ->
                      ByteString ->
                      m [JTransaction]
decodeTransactions SingleRecord bs = do
  csv <- either throwError return $ C.decode C.NoHeader bs
  if V.null csv
  then return []
  else
    let notTags = HS.fromList
                  $ ["date", "transaction id"]
                  ++ postingHeader (HS.fromList $ V.toList $ csv V.! 0) 1
    in csvToData (csv :: C.Csv) (fromLine notTags)

  where fromLine :: (MonadError Error m) =>
                    HS.HashSet Field ->
                    HM.HashMap Field Field -> m JTransaction
        fromLine notTags m = do
          date <- findColumnM "date" m parseISO8601M
          tId <- findColumn "transaction id" m
          ps <- lineToPostings m 1
          tags <- recordToTags m  notTags
          return $ Transaction date tId ps tags

        postingHeader :: HS.HashSet Field -> Int -> [Field]
        postingHeader m n =
          let accountKey =  toField $ "account id (" ++ show n ++ ")"
              amountKey =  toField $ "amount (" ++ show n ++ ")"
              commodityKey =  toField $ "commodity (" ++ show n ++ ")"
              balanceDateKey =  toField $ "balance date ("++ show n ++ ")"
          in case HS.member accountKey m of
              False -> []
              True ->
                let xs = postingHeader m (n + 1)
                in accountKey
                   : amountKey
                   : commodityKey
                   : balanceDateKey
                   : xs

        lineToPostings :: (MonadError Error n) =>
                          HM.HashMap Field Field -> Int -> n [JPosting]
        lineToPostings m n =
          let accountKey =  toField $ "account id (" ++ show n ++ ")"
              amountKey =  toField $ "amount (" ++ show n ++ ")"
              commodityKey =  toField $ "commodity (" ++ show n ++ ")"
              balanceDateKey =  toField $ "balance date ("++ show n ++ ")"
          in
            case findColumn accountKey m of
              Left _ -> return []
              Right acc | acc == "" -> return []
              Right acc -> do
                ps <- lineToPostings m (n + 1)
                amnt <- findColumnM amountKey m
                        (\case  { Nothing -> return Nothing;
                                  Just v -> return
                                            $ Just
                                            $ (realToFrac :: Scientific -> Quantity)
                                             v})
                comm <- findColumn commodityKey m
                bal <- findColumnDefaultM Nothing balanceDateKey m
                        (\case  {"" -> return Nothing;
                                 v -> Just <$> parseISO8601M v})

                let p = Posting bal acc amnt comm
                return $ p : ps

decodeTransactions MultipleRecords bs = do
  csv <- either throwError return $ C.decode C.NoHeader bs
  txns <- csvToData (csv :: C.Csv) fromLine
  let txnsGroup = groupBy ((==) `on` tTransactionId)
                $ sortBy (comparing tTransactionId) txns
  traverse regroupTransactions txnsGroup

  where fromLine :: (MonadError Error m) =>
                    HM.HashMap Field Field -> m JTransaction
        fromLine m = do
          date <- findColumnM "date" m parseISO8601M
          tId <- findColumn "transaction id" m
          when (T.null tId) (throwError
                $ "Unexpected null transaction id. \
                  \All lines must have a transaction id.")
          acc <- findColumn "account id" m
          amnt <- findColumnM "amount" m
                  (\case  { Nothing -> return Nothing;
                            Just v -> return
                                      $ Just
                                      $ (realToFrac :: Scientific -> Quantity)
                                       v})
          comm <- findColumn "commodity" m
          bal <- findColumnDefaultM Nothing "balance date" m
                  (\case  {"" -> return Nothing;
                           v -> Just <$> parseISO8601M v})
          tags <- recordToTags m  (HS.fromList multipleRecordsHeader)
          return $ Transaction date tId [Posting bal acc amnt comm] tags

        regroupTransactions :: (MonadError Error m) =>
                               [JTransaction] -> m JTransaction
        regroupTransactions [] = error "regroupTransactions empty list"
        regroupTransactions [x] =
            throwError $ "Expecting at least two postings for transaction id \""
                       ++ T.unpack (tTransactionId x)
                       ++ "\"."
        regroupTransactions xs = do
          let nbDates = S.size $ S.fromList $ map tDate xs
          when (nbDates > 1) (throwError
                  $ "Expecting the same date for all postings of transaction id \""
                  ++ T.unpack (tTransactionId $ head xs)
                  ++ "\".")
          let ps = concatMap tPostings xs
          (_, tags) <- validateTags $ map (\x -> (tTransactionId x, tTags x)) xs
          return $ Transaction
                   (tDate $ head xs)
                   (tTransactionId $ head xs)
                   ps
                   tags

        validateTags :: (MonadError Error m) =>
                     [(T.Text, [Tag])] -> m (T.Text, [Tag])
        validateTags xs =
          foldM (\t1  t2 -> if snd t1 == snd t2
                             then return t1
                             else throwError
                                 $ "Expecting the same tag values for all \
                                   \postings of transaction id\""
                                 ++ T.unpack (fst t1)
                                 ++ "\".")
                 (head xs)
                 (tail xs)
