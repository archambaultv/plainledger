-- |
-- Module      :  Plainledger.Journal.Configuration
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Configuration object of the journal file

module Plainledger.Journal.Configuration (
  Configuration(..)
  )
where

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Text as T
import Data.Aeson (pairs)
import Plainledger.Journal.Amount

-- | The Configuration of the journal file
data Configuration = Configuration
  {_openingBalanceAccount :: T.Text,
   _earningsAccount :: T.Text,
   _defaultCommodity :: Commodity
  }
  deriving (Eq, Show)

-- FromJSON instances
instance FromJSON Configuration where
  parseJSON (Y.Object v) =
    Configuration
    <$> v .: "opening-balance-account"
    <*> v .: "earnings-account"
    <*> v .: "default-commodity"
  parseJSON _ = fail "Expected Object for Configuration value"

-- To JSON instance
instance ToJSON Configuration where
  toJSON (Configuration openBal earnAcc defComm) =
    Y.object
    $ ["opening-balance-account" .= openBal,
       "earnings-account" .= earnAcc,
       "default-commodity" .= defComm]

  toEncoding (Configuration openBal earnAcc defComm) =
    pairs
    $ "opening-balance-account"   .= openBal
    <> "earnings-account"   .= earnAcc
    <> "default-commodity" .= defComm
