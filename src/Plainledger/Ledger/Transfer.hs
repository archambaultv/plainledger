-- |
-- Module      :  Plainledger.Ledger.Transfer
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Transaction and Transfer data type.

module Plainledger.Ledger.Transfer (
  Transfer(..)
  )
where

import Data.Char
import Data.Time
import Data.Scientific
import Data.ByteString (ByteString)
import qualified Data.Csv as C
import Data.Csv (FromRecord(..),
                 FromNamedRecord(..),
                 ToRecord(..),
                 ToNamedRecord(..),
                 ToField(..),
                 FromField(..),
                 (.!),
                 record,
                 namedRecord,
                 DefaultOrdered)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Aeson (pairs, fieldLabelModifier, omitNothingFields)
import Data.Bifunctor
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import GHC.Generics
import Control.Monad (mzero)
import Plainledger.Ledger.Amount
import Plainledger.Ledger.Tag
import Plainledger.Ledger.Day

-- | The TransferF data type reprensents the flow of one commodity from
-- one account to another.
data Transfer = Transfer
  {
    tfDate :: Day,
    tfBalanceDateFrom :: Day,
    tfBalanceDateTo :: Day,
    tfFrom :: T.Text,
    tfTo   :: T.Text,
    tfAmount :: Quantity,
    tfCommodity :: Commodity,
    tfTags :: [Tag]
  } deriving (Eq, Show, Generic)


instance ToJSON Transfer where
  toJSON (Transfer date balDateFrom balDateTo from to amnt comm tags) =
    Y.object
    $ ["date" .= date,
       "from" .= from,
       "to" .= to,
       "amount" .= (realToFrac amnt :: Scientific)]
   ++ (if balDateFrom == date then [] else ["balance-date-from" .= balDateFrom])
   ++ (if balDateTo == date then [] else ["balance-date-to" .= balDateTo])
   ++ (if T.null comm then [] else ["commodity" .= comm])
   ++ (if null tags then [] else ["tags" .= tags])

  toEncoding (Transfer date balDateFrom balDateTo from to amnt comm tags) =
    pairs
    $ "date" .= date
    <> (if balDateFrom == date
        then mempty
        else "balance-date-from" .= balDateFrom)
    <> (if balDateTo == date
        then mempty
        else "balance-date-to" .= balDateTo)
    <> "from" .= from
    <> "to" .= to
    <> "amount" .= (realToFrac amnt :: Scientific)
    <> (if T.null comm then mempty else "commodity" .= comm)
    <> (if null tags then mempty else "tags" .= tags)

instance FromJSON Transfer where
  parseJSON (Y.Object v) =
    Transfer
    <$> v .: "date"
    <*> (v .:? "balance-date-from" >>= maybe (v .: "date") return)
    <*> (v .:? "balance-date-to" >>= maybe (v .: "date") return)
    <*> v .: "from"
    <*> v .: "to"
    <*> (v .: "amount" >>= Y.withScientific "amount" (return . realToFrac))
    <*> (maybe "" id <$> v .:? "commodity")
    <*> (maybe [] id <$> v .:? "tags")
  parseJSON _ = fail "Expected Object for Transfer value"
