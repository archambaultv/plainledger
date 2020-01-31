{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Plainledger.Journal.Account
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the account data type.

module Plainledger.Journal.Account (
  Account(..),
  AccountGroup(..)
  )
where

import qualified Data.Csv as C
import Data.Csv (FromRecord(..),
                 FromNamedRecord(..),
                 ToRecord(..),
                 ToNamedRecord(..),
                 ToField(..),
                 (.!),
                 (.=),
                 record,
                 namedRecord,
                 DefaultOrdered)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?))
import qualified Data.Text as T
import Control.Monad (mzero)

-- | The Account data type serves as aggregation point for commodities
-- relating to a particuliar purpose.
data Account = Account
  {_aId :: T.Text,
   _name :: T.Text,
   _number   :: Int,
   _group :: AccountGroup,
   _subgroup :: Maybe T.Text,
   _subsubgroup :: Maybe T.Text
  }
  deriving (Show)

-- | The top level grouping of an account. Must be Asset, Liability,
-- Equity, Revenue or Expense.
data AccountGroup = Asset
                  | Liability
                  | Equity
                  | Revenue
                  | Expense
                  deriving (Show, Eq, Ord, Read)

-- FromJSON instances
instance FromJSON Account where
  parseJSON (Y.Object v) =
    Account
    <$> v .: "id"
    <*> ((v .:? "name") >>= maybe (v .: "id") return)
    <*> v .: "number"
    <*> v .: "group"
    <*> v .:? "subgroup"
    <*> v .:? "subsubgroup"
  parseJSON _ = fail "Expected Object for Account value"

instance FromJSON AccountGroup where
  parseJSON (Y.String "Asset") = return Asset
  parseJSON (Y.String "Liability") = return Liability
  parseJSON (Y.String "Equity") = return Equity
  parseJSON (Y.String "Revenue") = return Revenue
  parseJSON (Y.String "Expense") = return Expense
  parseJSON (Y.String _) =
    fail "Expected Asset, Liability, Equity, Revenue or Expense"
  parseJSON _ = fail "Expected String for AccountGroup value"

instance FromRecord Account where
    parseRecord v
        | length v == 6 = Account
                          <$> (v .! 0)
                          <*> (v .! 1)
                          <*> v .! 2
                          <*> (read <$> v .! 3)
                          <*> (strToMaybe <$> v .! 4)
                          <*> (strToMaybe <$> v .! 5)
        | otherwise     = mzero

      where strToMaybe "" = Nothing
            strToMaybe x = Just x

instance FromNamedRecord Account where
    parseNamedRecord m =
      Account
      <$> m C..: "id"
      <*> m C..: "name"
      <*> m C..: "number"
      <*> (read <$> m C..: "group")
      <*> (strToMaybe <$> m C..: "subgroup")
      <*> (strToMaybe <$> m C..: "subsubgroup")

      where strToMaybe "" = Nothing
            strToMaybe x = Just x

instance ToRecord Account where
    toRecord (Account aId name num grp subgrp subsubgrp) =
      record [
      toField aId,
      toField name,
      toField num,
      toField (show grp),
      toField (maybe "" id subgrp),
      toField (maybe "" id subsubgrp)]

instance ToNamedRecord Account where
    toNamedRecord (Account aId name num grp subgrp subsubgrp) =
      namedRecord [
        "id" .= aId,
        "name" .= name,
        "number" .= num,
        "group" .= (show grp),
        "subgroup" .= (maybe "" id subgrp),
        "subsubgroup" .= (maybe "" id subsubgrp)]

instance DefaultOrdered Account where
  headerOrder _ = record ["id", "name","number","group","subgroup","subsubgroup"]
