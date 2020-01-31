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
import Data.Yaml (FromJSON(..), (.:))
import qualified Data.Text as T
import Plainledger.Journal.Amount

-- | The Configuration of the journal file
data Configuration = Configuration
  {_openingBalanceAccount :: T.Text,
   _earningsAccount :: T.Text,
   _defaultCommodity :: Commodity
  }
  deriving (Show)

-- FromJSON instances
instance FromJSON Configuration where
  parseJSON (Y.Object v) =
    Configuration
    <$> v .: "opening-balance-account"
    <*> v .: "earnings-account"
    <*> v .: "default-commodity"
  parseJSON _ = fail "Expected Object for Configuration value"
