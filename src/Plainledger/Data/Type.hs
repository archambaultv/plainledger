{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Data.Type
where

-- Most data types are defined here.
-- For the functions related to each type, see the corresponding modules

import Data.Text as T
import Data.Decimal
import Data.Time
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Megaparsec (SourcePos)

data Tag = Tag {
  tagKey :: Text,
  tagValue :: Maybe Text,
  tagSourcePos :: SourcePos
  } deriving (Show)

-- Name of an account
type AccountName = Text

-- The account's parents name and its name
-- ex : Asset:Banking:Bank Foo:Chequing ->
--        ["Asset","Banking","Bank Foo","Chequing"]
type QualifiedName = [AccountName]

qualifiedName2Text :: QualifiedName -> Text 
qualifiedName2Text = T.intercalate ":"

qualifiedName2String :: QualifiedName -> String
qualifiedName2String = T.unpack . qualifiedName2Text

data AccountType = Asset
                 | Liability
                 | Equity
                 | Revenue
                 | Expense
                 deriving (Show, Eq, Ord)

type Quantity = Decimal

type Commodity = Text

data Amount = Amount {
  aCommodity :: Commodity,
  aQuantity :: Quantity
  }
  deriving (Show)

data Posting = Posting {
  pAccount :: QualifiedName,
  pAmount :: Amount,
  pTransaction :: Transaction, -- Parent transaction.
  pSourcePos :: SourcePos
  }
  deriving (Show)

data Transaction = Transaction {
  tDate :: Day,
  tTags :: [Tag],
  tPostings :: [Posting],
  tSourcePos :: SourcePos
  }
  deriving (Show)

type Balance = M.Map Commodity Quantity

data AccountInfo = AccountInfo {
  aOpenDate :: Day,
  aCloseDate :: Maybe Day,
  aName :: QualifiedName,
  aTags :: [Tag],
  aType :: AccountType,
  aNumber :: Maybe Integer,
  aAllowedCommodities :: S.Set Commodity,
  aDefaultCommodity :: Commodity,
  aBalance :: Balance
  }
  deriving (Show)

data Configuration = Configuration {
  cDefaultCommodity :: Commodity,
  cAccountTypeMapping :: M.Map AccountName AccountType,
  cOpeningBalanceAccount :: QualifiedName,
  cSourcePos :: SourcePos
  }
  deriving (Show)

data Ledger = Ledger {
  lStartDate :: Day,
  lEndDate :: Day,
  lConfiguration :: Configuration,
  lTransactions :: [Transaction],
  lAccountInfos :: M.Map QualifiedName AccountInfo
  }
  deriving (Show)

-- The following datatypes represents what the user can input in its
-- journal file. They are like the above types but some fields are
-- optional or not yet computed for making parsing easier.

data BalanceAssertion = BalanceAssertion {
  baDate :: Day,
  baAccount :: QualifiedName,
  baAmount :: Amount,
  baSourcePos :: SourcePos
  }
  deriving (Show)

data OpenAccount = OpenAccount {
  oaDate :: Day,
  oaName :: QualifiedName,
  oaTags :: [Tag],
  oaNumber :: Maybe Integer,
  oaCommodity :: [Commodity],
  oaSourcePos :: SourcePos
  }
  deriving (Show)

data CloseAccount = CloseAccount {
  caDate :: Day,
  caAccount :: QualifiedName,
  caSourcePos :: SourcePos
  }
  deriving (Show)

data RawAmount = RawAmount {
  raCommodity :: Maybe Commodity,
  raQuantity :: Maybe Quantity
  }
  deriving (Show)

data RawQuantity = RawQuantity {
  rqCommodity :: Maybe Commodity,
  rqQuantity :: Quantity
  }
  deriving (Show)

data RawPosting = RawPosting {
  rpAccount :: QualifiedName,
  rpAmount :: RawAmount,
  rpSourcePos :: SourcePos}

data RawTransaction = RawTransaction {
  rtDate :: Day,
  rtTags :: [Tag],
  rtPostings :: [RawPosting],
  rtSourcepos :: SourcePos
  }

data RawBalanceAssertion = RawBalanceAssertion {
  rbaDate :: Day,
  rbaAccount :: QualifiedName,
  rbaAmount :: RawQuantity,
  rbaSourcePos :: SourcePos
  }
  deriving (Show)

data RawJournal = RawJournal {
  rjConfiguration :: Maybe Configuration,
  rjTransactions :: [RawTransaction],
  rjBalanceAssertions :: [RawBalanceAssertion],
  rjOpenAccounts :: [OpenAccount],
  rjCloseAccounts :: [CloseAccount]
  }
