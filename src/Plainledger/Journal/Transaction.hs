{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Plainledger.Journal.Transaction
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Transaction and Transfer data type.

module Plainledger.Journal.Transaction (
  Transfer(..),
  Transaction(..),
  Tag(..),
  transferRecordList,
  transferNamedRecordList,
  transferHeader,
  balanceDate
  )
where

import Data.Time
import Data.Scientific
import Data.ByteString (ByteString)
import qualified Data.Csv as C
import Data.Csv (FromRecord(..),
                 FromNamedRecord(..),
                 ToRecord(..),
                 ToNamedRecord(..),
                 ToField(..),
                 (.!),
                 record,
                 namedRecord,
                 DefaultOrdered)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Control.Monad (mzero)
import Plainledger.Journal.Amount

-- | The Transfer data type reprensents the flow of one commodity from
-- one account to another. Quantity should always be positive.
data Transfer = Transfer
  {_tfFrom :: T.Text,
   _tfTo   :: T.Text,
   _tfAmount :: Quantity,
   _tfCommodity :: Maybe Commodity,
   _tfNote :: Maybe T.Text
  }
  deriving (Show)

-- | A transaction is a collection of transfers on a specific date
data Transaction = Transaction
  {_tDate :: Day,
   _tDateBalance :: Maybe Day, -- | The date use for balance assertion
   _tTranfers :: [Transfer],
   _tNote :: Maybe T.Text,
   _tConterparty :: Maybe T.Text,
   _tags :: [Tag]
  }
  deriving (Show)

-- | dateBalance returns the date field if the DateBalance field is empty
balanceDate :: Transaction -> Day
balanceDate t = maybe (_tDate t) id $ _tDateBalance t

-- | Tags can be associated with a transaction
data Tag = Tag
  {_tagKey :: T.Text,
   _tagValue :: Maybe T.Text
  }
  deriving (Show)

-- FromJSON instances
instance FromJSON Transfer where
  parseJSON (Y.Object v) =
    Transfer
    <$> v .: "from"
    <*> v .: "to"
    <*> (v .: "amount" >>= Y.withScientific "amount" (return . realToFrac))
    <*> v .:? "commodity"
    <*> v .:? "note"
  parseJSON _ = fail "Expected Object for Transfer value"

instance ToJSON Transfer where
  toJSON (Transfer from to amnt com note) =
    Y.object
    $ ["from" .= from,
       "to" .= to,
       "amount" .= (realToFrac amnt :: Scientific)]
    ++ maybe [] (\x -> ["commodity" .= x]) com
    ++ maybe [] (\x -> ["note" .= x]) note

  toEncoding (Transfer from to amnt com note) =
    pairs
    $ "from"   .= from
    <> "to"   .= to
    <> "amount" .= show amnt
    <> (maybe mempty (\x -> "commodity" .= x) com)
    <> (maybe mempty (\x -> "note" .= x) note)

instance FromJSON Transaction where
  parseJSON (Y.Object v) =
    Transaction
    <$> v .: "date"
    <*> v .:? "balance-date"
    <*> v .: "transfers"
    <*> v .:? "note"
    <*> v .:? "conterparty"
    <*> ((v .:? "tags") >>= maybe (return []) return)
  parseJSON _ = fail "Expected Object for Transaction value"

instance ToJSON Transaction where
  toJSON (Transaction date balDate xfers note conterparty tags) =
    Y.object
    $ ["date" .= date, "transfers" .= xfers]
    ++ maybe [] (\x -> ["balance-date" .= x]) balDate
    ++ maybe [] (\x -> ["note" .= x]) note
    ++ maybe [] (\x -> ["conterparty" .= x]) conterparty
    ++ if null tags then [] else ["tags" .= tags]

  toEncoding (Transaction date balDate xfers note conterparty tags) =
    pairs
    $ "date" .= date
    <> (maybe mempty (\x -> "balance-date" .= x) balDate)
    <> (maybe mempty (\x -> "note" .= x) note)
    <> (maybe mempty (\x -> "conterparty" .= x) conterparty)
    <> "transfers" .= xfers
    <> (if null tags then mempty else "tags" .= tags)

-- Csv instances for Transfer

-- For Csv instances of Transaction, see Plainledger.Csv.Transaction
instance FromRecord Transfer where
    parseRecord v
        | length v == 5 = Transfer
                          <$> v .! 0
                          <*> v .! 1
                          <*> (read <$> v .! 2)
                          <*> v .! 3
                          <*> v .! 4
        | otherwise     = mzero

instance FromNamedRecord Transfer where
    parseNamedRecord m =
      Transfer
      <$> m C..: "from"
      <*> m C..: "to"
      <*> (read <$> m C..: "amount")
      <*> m C..: "commodity"
      <*> m C..: "note"


instance ToRecord Transfer where
    toRecord t = record $ transferRecordList t

transferRecordList :: Transfer -> [ByteString]
transferRecordList (Transfer from to amnt com note) =
  [toField from,
   toField to,
   toField (show amnt),
   toField com,
   toField note]

instance ToNamedRecord Transfer where
    toNamedRecord t = namedRecord $ transferNamedRecordList t

transferNamedRecordList :: Transfer -> [(ByteString, ByteString)]
transferNamedRecordList (Transfer from to amnt com note) =
  ["from" C..= from,
   "to" C..= to,
   "amount" C..= (show amnt),
   "commodity" C..= com,
   "note" C..= note]

instance DefaultOrdered Transfer where
  headerOrder _ = record transferHeader

transferHeader :: [ByteString]
transferHeader = ["from","to","amount","commodity","note"]

instance FromJSON Tag where
  parseJSON (Y.Object v) =
    Tag
    <$> v .: "id"
    <*> v .:? "value"
  parseJSON _ = fail "Expected Object for Tag value"

instance ToJSON Tag where
  toJSON (Tag key value) =
    Y.object
    $ ["id" .= key]
    ++ maybe [] (\x -> ["value" .= x]) value

  toEncoding (Tag key value) =
    pairs
    $ "id" .= key
    <> (maybe mempty (\x -> "value" .= x) value)
