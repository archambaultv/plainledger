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
  Balance(..),
  decodeBalanceFile
  )
where

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
import qualified Data.Text as T
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Control.Monad.Except
import Plainledger.Error

-- | The Balance data type reprensents an assertion about the total of an
-- account at a particular date.
data Balance = Balance
  {bDate :: Day,
   bAccount   :: T.Text,
   bAmount :: Quantity
  }
  deriving (Eq, Show)

instance FromRecord Balance where
    parseRecord v
        | length v == 4 = Balance
                          <$> (v .! 0 >>= either fail return . parseISO8601M)
                          <*> v .! 1
                          <*> (realToFrac <$>
                               (v .! 2 >>= C.parseField :: C.Parser Scientific))
        | otherwise     = mzero

instance FromNamedRecord Balance where
    parseNamedRecord m =
      Balance
      <$> (m C..: "date" >>= either fail return . parseISO8601M)
      <*> m C..: "account"
      <*> (read <$> m C..: "amount")

instance ToRecord Balance where
    toRecord (Balance d acc amnt) =
      record [
      toField (toISO8601 d),
      toField acc,
      toField (realToFrac amnt :: Scientific)]

instance ToNamedRecord Balance where
    toNamedRecord (Balance d acc amnt) =
      namedRecord [
        "date" C..= (toISO8601 d),
        "account" C..= acc,
        "amount" C..= (realToFrac amnt :: Scientific)]

instance DefaultOrdered Balance where
  headerOrder _ = record ["date","account","amount"]

decodeBalanceFile :: String -> ExceptT Error IO [Balance]
decodeBalanceFile f = do
    csvBS <- liftIO $ BL.readFile f
    either throwError (return . V.toList . snd) $ C.decodeByName csvBS
