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
  Transfer(..),
  decodeTransfers,
  encodeTransfers
  )
where

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
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y

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
  } deriving (Show, Generic)

-- / We sort the tags when comparing two transfers
-- The Eq instance is mainly used in the unittests.
instance Eq Transfer where
  t1 == t2 =  tfDate t1 == tfDate t2
           && tfBalanceDateFrom t1 == tfBalanceDateFrom t2
           && tfBalanceDateTo t1 == tfBalanceDateTo t2
           && tfFrom t1 == tfFrom t2
           && tfTo t1 == tfTo t2
           && tfAmount t1 == tfAmount t2
           && tfCommodity t1 == tfCommodity t2
           && sortBy (comparing tagId) (tfTags t1) ==
              sortBy (comparing tagId) (tfTags t2)

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

-- CSV functions
coreHeader :: [Field]
coreHeader = ["date",
              "balance-date-from",
              "balance-date-to",
              "from",
              "to",
              "amount",
              "commodity"]

-- / Encode a list of transfers as a Csv. The first line is the header
encodeTransfers :: [Transfer] -> ByteString
encodeTransfers xs =
  let tagH = tagHeader $ concatMap tfTags xs
      header = toRecord $ coreHeader ++ tagH
      lines = header : map (toLine tagH) xs
  in C.encode lines

  where toLine :: [Field] -> Transfer -> Record
        toLine tagH t =
          let coreLine = [toField $ toISO8601 $ tfDate t,
                          toField $ toISO8601 $ tfBalanceDateFrom t,
                          toField $ toISO8601 $ tfBalanceDateTo t,
                          toField $ tfFrom t,
                          toField $ tfTo t,
                          toField $ (realToFrac (tfAmount t) :: Scientific),
                          toField $ tfCommodity t]
          in toRecord $ coreLine ++ tagLine (tfTags t) tagH

-- | The first line is the header
decodeTransfers :: (MonadError Error m) => ByteString -> m [Transfer]
decodeTransfers bs = do
  csv <- either throwError return $ C.decode C.NoHeader bs
  csvToData (csv :: C.Csv) fromLine

  where fromLine :: (MonadError Error m) =>
                    HM.HashMap Field Field -> m Transfer
        fromLine m = do
          date <- findColumnM "date" m parseISO8601M
          dateFrom <- findColumnDefaultM date "balance-date-from" m
                      parseISO8601M
          dateTo <- findColumnDefaultM date "balance-date-to" m parseISO8601M
          from <- findColumn "from" m
          to <- findColumn "to" m
          amount <- (\x -> realToFrac (x :: Scientific))
                    <$> findColumn "amount" m
          comm <- findColumnDefault "" "commodity" m
          tags <- recordToTags m (HS.fromList coreHeader)
          return $ Transfer date dateFrom dateTo from to amount comm tags
