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
  TrialBalanceAssertion(..),
  decodeBalanceFile,
  decodeTrialBalanceAssertionFile
  )
where

import Data.Time
import Data.Scientific
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Csv as C
import Data.Csv (FromNamedRecord(..),
                 ToNamedRecord(..),
                 record,
                 namedRecord,
                 DefaultOrdered)
import qualified Data.Text as T
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Control.Monad.Except
import Plainledger.Error

-- | The Balance data type reprensents an assertion about the total of an
-- account at a particular date. Used for bank reconciliation
data Balance = Balance
  {bDate :: Day,
   bAccount   :: T.Text,
   bAmount :: Quantity
  }
  deriving (Eq, Show)

instance FromNamedRecord Balance where
    parseNamedRecord m =
      Balance
      <$> (m C..: "date" >>= either fail return . parseISO8601M)
      <*> m C..: "account"
      <*> (read <$> m C..: "amount")

instance ToNamedRecord Balance where
    toNamedRecord (Balance d acc amnt) =
      namedRecord [
        "date" C..= (toISO8601 d),
        "account" C..= acc,
        "amount" C..= (realToFrac amnt :: Scientific)]

instance DefaultOrdered Balance where
  headerOrder _ = record ["date","account","amount"]

data TrialBalanceAssertion = TrialBalanceAssertion
  {tbaStartDate :: Day,
   tbaEndDate   :: Day,
   tbaAccount :: T.Text,
   tbaAmount :: Quantity
  }
  deriving (Eq, Show)

instance FromNamedRecord TrialBalanceAssertion where
    parseNamedRecord m =
      TrialBalanceAssertion
      <$> (m C..: "start date" >>= either fail return . parseISO8601M)
      <*> (m C..: "end date" >>= either fail return . parseISO8601M)
      <*> m C..: "account"
      <*> (read <$> m C..: "amount")

instance ToNamedRecord TrialBalanceAssertion where
    toNamedRecord (TrialBalanceAssertion startdate enddate acc amnt) =
      namedRecord [
        "start date" C..= (toISO8601 startdate),
        "end date" C..= (toISO8601 enddate),
        "account" C..= acc,
        "amount" C..= (realToFrac amnt :: Scientific)]

instance DefaultOrdered TrialBalanceAssertion where
  headerOrder _ = record ["start date","end date", "account","amount"]

decodeBalanceFile :: String -> ExceptT Error IO [Balance]
decodeBalanceFile f = do
    csvBS <- liftIO $ BL.readFile f
    either throwError (return . V.toList . snd) $ C.decodeByName csvBS

decodeTrialBalanceAssertionFile :: String -> ExceptT Error IO [TrialBalanceAssertion]
decodeTrialBalanceAssertionFile f = do
    csvBS <- liftIO $ BL.readFile f
    either throwError (return . V.toList . snd) $ C.decodeByName csvBS
