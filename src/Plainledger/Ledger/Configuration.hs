-- |
-- Module      :  Plainledger.Ledger.Configuration
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Configuration object of the journal file

module Plainledger.Ledger.Configuration (
  Configuration(..)
  )
where

import Data.Char (toLower)
import Data.Tuple (swap)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.=))
import qualified Data.Text as T
import Data.Aeson as A
import Plainledger.Ledger.Amount
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import GHC.Generics

-- | The top level grouping of an account. Must be Asset, Liability,
-- Equity, Revenue or Expense.
data AccountGroup = Asset
                  | Liability
                  | Equity
                  | Revenue
                  | Expense
                  deriving (Show, Eq, Ord, Generic)

instance ToJSON AccountGroup where
  toEncoding = A.genericToEncoding A.defaultOptions {
    A.fieldLabelModifier = map toLower
    }
instance FromJSON AccountGroup
instance ToJSONKey AccountGroup
instance FromJSONKey AccountGroup
instance Hashable AccountGroup

-- | The Configuration of the journal file
data Configuration = Configuration
  {cOpeningBalanceAccount :: T.Text,
   cEarningsAccount :: T.Text,
   cDefaultCommodity :: Commodity,
   cGroupMapping :: HashMap T.Text AccountGroup
  }
  deriving (Eq, Show)

-- FromJSON instances
instance FromJSON Configuration where
  parseJSON (Y.Object v) =
    Configuration
    <$> v .: "opening-balance-account"
    <*> v .: "earnings-account"
    <*> v .: "default-commodity"
    <*> (v .: "group-mapping" >>= \v2 ->
            toGroupMapping
            <$> v2 .: "asset"
            <*> v2 .: "liability"
            <*> v2 .: "equity"
            <*> v2 .: "revenue"
            <*> v2 .: "expense" )
  parseJSON _ = fail "Expected Object for Configuration value"

-- To JSON instance
instance ToJSON Configuration where
  toJSON (Configuration openBal earnAcc defComm gmap) =
    Y.object
    $ ["opening-balance-account" .= openBal,
       "earnings-account" .= earnAcc,
       "default-commodity" .= defComm,
       "group-mapping" .= let x = fromGroupMapping gmap
                          in Y.object
                            ["asset" .= HM.lookup Asset x,
                             "liability" .= HM.lookup Liability x,
                             "equity" .= HM.lookup Equity x,
                             "revenue" .= HM.lookup Revenue x,
                             "expense" .= HM.lookup Expense x]]
  toEncoding (Configuration openBal earnAcc defComm gmap) =
    pairs
    $ "opening-balance-account"   .= openBal
    <> "earnings-account"   .= earnAcc
    <> "default-commodity" .= defComm
    <> "group-mapping" .= let x = fromGroupMapping gmap
                          in Y.object
                             ["asset" .= HM.lookup Asset x,
                              "liability" .= HM.lookup Liability x,
                              "equity" .= HM.lookup Equity x,
                              "revenue" .= HM.lookup Revenue x,
                              "expense" .= HM.lookup Expense x]

toGroupMapping :: [T.Text] ->
                  [T.Text] ->
                  [T.Text] ->
                  [T.Text] ->
                  [T.Text] ->
                  HashMap T.Text AccountGroup
toGroupMapping a l eq r ex =
  HM.fromList
  $ map (,Asset) a
  ++ map (,Liability) l
  ++ map (,Equity) eq
  ++ map (,Revenue) r
  ++ map (,Expense) ex

fromGroupMapping :: HashMap T.Text AccountGroup ->
                    HashMap AccountGroup [T.Text]
fromGroupMapping m = HM.fromListWith (++)
             $ map (fmap (:[]))
             $ map swap
             $ HM.toList m
