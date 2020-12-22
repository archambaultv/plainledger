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
  -- Balance(..),
  -- decodeBalanceFile,
  -- decodeTrialBalanceAssertionFile
  )
where

-- import Data.Time
-- import Data.Scientific
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Vector as V
-- import qualified Data.Csv as C
-- import Data.Csv (FromNamedRecord(..),
--                  ToNamedRecord(..),
--                  record,
--                  namedRecord,
--                  DefaultOrdered)
-- import qualified Data.Text as T
-- import Plainledger.Journal.Amount
-- import Plainledger.Journal.Day
-- import Control.Monad.Except
-- import Plainledger.Error

-- -- | The Balance data type reprensents an assertion about the total of an
-- -- account at a particular date. Used for bank reconciliation
-- data Balance = Balance
--   {bDate :: Day,
--    bAccount   :: T.Text,
--    bAmount :: Quantity,
--    bStartDate :: Maybe Day
--   }
--   deriving (Eq, Show)

-- instance FromNamedRecord Balance where
--     parseNamedRecord m =
--       Balance
--       <$> (m C..: "Date" >>= either fail return . parseISO8601M)
--       <*> m C..: "Compte"
--       <*> (read <$> m C..: "Montant")
--       <*> (m C..: "Date de début" >>= either fail return . parseISO8601M)

-- instance ToNamedRecord Balance where
--     toNamedRecord (Balance d acc amnt) =
--       namedRecord [
--         "Date" C..= (toISO8601 d),
--         "Compte" C..= acc,
--         "Montant" C..= (realToFrac amnt :: Scientific),
--         "Date de début" C..= (toISO8601 startdate)]

-- instance DefaultOrdered Balance where
--   headerOrder _ = record ["Date","Compte","Montant","Date de début"]


-- decodeBalanceFile :: String -> ExceptT Error IO [Balance]
-- decodeBalanceFile f = do
--     csvBS <- liftIO $ BL.readFile f
--     either (throwError . mkCustomErr) (return . V.toList . snd) $ C.decodeByName csvBS
