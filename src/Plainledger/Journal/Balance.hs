-- |
-- Module      :  Plainledger.Journal.Balance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Balance data type representing balance assertions.

module Plainledger.Journal.Balance (
  Balance(..)
  )
where

import Data.Time
import Data.Scientific
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
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.=), (.:?))
import qualified Data.Text as T
import Control.Monad (mzero)
import Data.Aeson (pairs)
import Plainledger.Journal.Amount

-- | The Balance data type reprensents an assertion about the total of an
-- account at a particular date.
data Balance = Balance
  {_bDate :: Day,
   _bAccount   :: T.Text,
   _bAmount :: Quantity,
   _bCommodity :: Maybe Commodity
  }
  deriving (Eq, Show)

-- FromJSON instances
instance FromJSON Balance where
  parseJSON (Y.Object v) =
    Balance
    <$> v .: "date"
    <*> v .: "account"
    <*> (v .: "amount" >>= Y.withScientific "amount" (return . realToFrac))
    <*> v .:? "commodity"
  parseJSON _ = fail "Expected Object for Balance value"

instance ToJSON Balance where
  toJSON (Balance date acc amnt com) =
    Y.object
    $ ["date" .= date,
       "account" .= acc,
       "amount" .= (realToFrac amnt :: Scientific)]
    ++ maybe [] (\x -> ["commodity" .= x]) com

  toEncoding (Balance date acc amnt com) =
    pairs
    $ "date"   .= date
    <> "account"   .= acc
    <> "amount" .= (realToFrac amnt :: Scientific)
    <> (maybe mempty (\x -> "commodity" .= x) com)

instance FromRecord Balance where
    parseRecord v
        | length v == 4 = Balance
                          <$> (read <$> v .! 0)
                          <*> v .! 1
                          <*> (read <$> v .! 2)
                          <*> v .! 3
        | otherwise     = mzero

instance FromNamedRecord Balance where
    parseNamedRecord m =
      Balance
      <$> (read <$> m C..: "date")
      <*> m C..: "account"
      <*> (read <$> m C..: "amount")
      <*> m C..: "commodity"


instance ToRecord Balance where
    toRecord (Balance d acc amnt com) =
      record [
      toField (show d),
      toField acc,
      toField (show amnt),
      toField com]

instance ToNamedRecord Balance where
    toNamedRecord (Balance d acc amnt com) =
      namedRecord [
        "date" C..= (show d),
        "account" C..= acc,
        "amount" C..= (show amnt),
        "commodity" C..= com]

instance DefaultOrdered Balance where
  headerOrder _ = record ["date","account","amount","commodity"]
