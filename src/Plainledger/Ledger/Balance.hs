-- |
-- Module      :  Plainledger.Ledger.Balance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Balance data type representing balance assertions.

module Plainledger.Ledger.Balance (
  Balance(..),
  validateBalances,
  decodeBalanceFile
  )
where

import Control.Monad.Except
import Data.Time
import Data.Scientific
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
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
import Plainledger.Ledger.Amount
import Plainledger.Ledger.Day
import Plainledger.Error
import Plainledger.Internal.Utils
import Plainledger.Ledger.Account

-- | The Balance data type reprensents an assertion about the total of an
-- account at a particular date.
data Balance = Balance
  {bDate :: Day,
   bAccount   :: T.Text,
   bAmount :: Quantity,
   bCommodity :: Commodity
  }
  deriving (Eq, Show)

-- Balances :
--  Asserts all balance have a valid account field
--  Asserts all balance have a well defined commodity
--  Asserts all balance assertions are correct
validateBalances :: (MonadError Error m) =>
                      Commodity ->
                      [Account] ->
                      [Balance] ->
                      m [Balance]
validateBalances defComm _ x =
  let b1 = map (\c -> if T.null $ bCommodity c
                      then c{bCommodity = defComm}
                      else c)
           x
  in return b1

-- FromJSON instances
instance FromJSON Balance where
  parseJSON (Y.Object v) =
    Balance
    <$> v .: "date"
    <*> v .: "account"
    <*> (v .: "amount" >>= Y.withScientific "amount" (return . realToFrac))
    <*> (maybe "" id <$> (v .:? "commodity"))
  parseJSON _ = fail "Expected Object for Balance value"

instance ToJSON Balance where
  toJSON (Balance date acc amnt com) =
    Y.object
    $ ["date" .= date,
       "account" .= acc,
       "amount" .= (realToFrac amnt :: Scientific),
       "commodity" .= com]

  toEncoding (Balance date acc amnt com) =
    pairs
    $  "date"  .= date
    <> "account"   .= acc
    <> "amount" .= (realToFrac amnt :: Scientific)
    <> "commodity" .= com

instance FromRecord Balance where
    parseRecord v
        | length v == 4 = Balance
                          <$> (v .! 0 >>= either fail return . parseISO8601M)
                          <*> v .! 1
                          <*> (realToFrac <$>
                               (v .! 2 >>= C.parseField :: C.Parser Scientific))
                          <*> v .! 3
        | otherwise     = mzero

instance FromNamedRecord Balance where
    parseNamedRecord m =
      Balance
      <$> (m C..: "date" >>= either fail return . parseISO8601M)
      <*> m C..: "account"
      <*> (read <$> m C..: "amount")
      <*> m C..: "commodity"

instance ToRecord Balance where
    toRecord (Balance d acc amnt com) =
      record [
      toField (toISO8601 d),
      toField acc,
      toField (realToFrac amnt :: Scientific),
      toField com]

instance ToNamedRecord Balance where
    toNamedRecord (Balance d acc amnt com) =
      namedRecord [
        "date" C..= (toISO8601 d),
        "account" C..= acc,
        "amount" C..= (realToFrac amnt :: Scientific),
        "commodity" C..= com]

instance DefaultOrdered Balance where
  headerOrder _ = record ["date","account","amount","commodity"]

decodeBalanceFile :: String -> IO [Balance]
decodeBalanceFile f = do
  fType <- either fail return $ isDecodableFile f
  case fType of
    YamlFile -> Y.decodeFileThrow f
    CsvFile -> do
        csvBS <- BL.readFile f
        either fail (return . V.toList . snd) $ C.decodeByName csvBS
