-- |
-- Module      :  Plainledger.Csv.Transaction
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Transaction and Transfer data type.

module Plainledger.Csv.Transaction (
  CsvTransfer(..),
  txnsToCsvTransfers,
  txnToCsvTransfers,
  csvTransfersToTxns,
  csvBalanceDate
  )
where

import Data.Time
import Data.List
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Csv as C
import qualified Data.Vector as V
import Data.Csv (FromRecord(..),
                 FromNamedRecord(..),
                 ToRecord(..),
                 ToNamedRecord(..),
                 ToField(..),
                 (.!),
                 (.=),
                 record,
                 namedRecord,
                 DefaultOrdered)
import qualified Data.Text as T
import Control.Monad (mzero)
import Plainledger.Journal.Transaction

-- | The CsvTransfer data type reprensents a Transfer with additional
-- information coming from the Transaction data type so that each
-- record in the CSV file is complete. It also contains a transaction
-- identifier string so we can map back the CsvTransfer to a
-- Transaction object.
data CsvTransfer = CsvTransfer
  {_cTransactionId :: T.Text,
   _cDate :: Day,
   _cDateBalance :: Maybe Day, -- | The date use for balance assertion
   _cTransfer :: Transfer
  }
  deriving (Show)

csvBalanceDate :: CsvTransfer -> Day
csvBalanceDate c = maybe (_cDate c) id $ _cDateBalance c

-- | txnsToCsvTransfers flatten each transaction as a series of
-- CsvTransfer augmented with the date, balance date and an identifier
-- to allow the function csvTranfersToTxns to reverse the process.
txnsToCsvTransfers :: [Transaction] -> [CsvTransfer]
txnsToCsvTransfers ts =
  let sortedTs = sortBy (comparing _tDate) ts
      groupedTs = groupBy ((==) `on` _tDate) sortedTs
      zipTs :: [[(Int, Transaction)]]
      zipTs = map (zip [1..]) groupedTs
  in concatMap (concatMap $ uncurry txnToCsvTransfers) zipTs

-- | txnToCsvTransfers transform the transaction as a series of
-- CsvTransfer augmented with the date, balance date and an identifier
-- built from the Int input to allow the function csvTranfersToTxns to
-- reverse the process.
txnToCsvTransfers :: Int -> Transaction -> [CsvTransfer]
txnToCsvTransfers n t =
  let id1 = T.pack $ show (_tDate t) ++ "-" ++ show n
      f = CsvTransfer id1 (_tDate t) (_tDateBalance t)
  in map f (_tTranfers t)

-- | The inverse function of txnsToCsvTransfers
csvTransfersToTxns :: [CsvTransfer] -> [Transaction]
csvTransfersToTxns cs =
  let sortedCs = sortBy (comparing _cTransactionId) cs
      groupedCs = groupBy ((==) `on` _cTransactionId) sortedCs
  in map mkTransaction groupedCs

  where mkTransaction [] = error "Csv.Transaction.mkTransaction empty list"
        mkTransaction xs@(x:_) = Transaction
                                 (_cDate x)
                                 (_cDateBalance x)
                                 (map _cTransfer xs)
                                 Nothing
                                 Nothing
                                 []

-- Csv instances
instance FromRecord CsvTransfer where
    parseRecord v
        | length v == 7 = CsvTransfer
                          <$> v .! 0
                          <*> (read <$> v .! 1)
                          <*> (readBalanceDate <$> v .! 2)
                          <*> parseRecord (V.drop 3 v)
        | otherwise     = mzero

instance FromNamedRecord CsvTransfer where
    parseNamedRecord m =
      CsvTransfer
      <$> m C..: "id"
      <*> (read <$> m C..: "date")
      <*> (readBalanceDate <$> m C..: "balance date")
      <*> parseNamedRecord m

readBalanceDate :: String -> Maybe Day
readBalanceDate "" = Nothing
readBalanceDate x = Just $ read x

instance ToRecord CsvTransfer where
    toRecord (CsvTransfer id1 date balDate xfer) =
      record
      $ [toField id1,
         toField (show date),
         toField (maybe "" show balDate)]
      ++ transferRecordList xfer

instance ToNamedRecord CsvTransfer where
    toNamedRecord  (CsvTransfer id1 date balDate xfer) =
      namedRecord
      $ [ "id" .= id1,
          "date" .= (show date),
          "balance date" .= (maybe "" show balDate)]
      ++ transferNamedRecordList xfer

instance DefaultOrdered CsvTransfer where
  headerOrder _ = record $ ["id","date","balance date"] ++ transferHeader
