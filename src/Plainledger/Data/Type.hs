{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Data.Type
where

-- Most data types are defined here.
-- For the functions related to each type, see the corresponding modules

import qualified Data.Text as T
import Data.Decimal
import Data.Time
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Megaparsec (SourcePos)

-- The internal representation of an account is done
-- using the plus (inflow) and minus (outflow) convention.
-- But for reporting purposes it is usefull to convert
-- to the standard debit credit convention
data AccountingType = PlusMinus | DebitCredit
                    deriving (Eq, Show)

data Tag = Tag {
  tagKey :: T.Text,
  tagValue :: Maybe T.Text,
  tagSourcePos :: SourcePos
  } deriving (Show)

-- Name of an account
type AccountName = T.Text

-- The account's parents name and its name
-- ex : Asset:Banking:Bank Foo:Chequing ->
--        ["Asset","Banking","Bank Foo","Chequing"]
type QualifiedName = [AccountName]

qualifiedName2Text :: QualifiedName -> T.Text 
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

type Commodity = T.Text

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
type BalanceDebitCredit = M.Map Commodity (Quantity, Quantity)

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


totalBalance :: [AccountInfo] -> Balance
totalBalance = foldl' (\b x -> M.unionWith (+) b (aBalance x)) M.empty

totalBalanceDebitCredit :: [AccountInfo] -> BalanceDebitCredit
totalBalanceDebitCredit =                
 foldl' (\b x -> M.unionWith (\(d1, c1) (d2, c2) -> (d1 + d2, c1 + c2))
                             b
                             (fmap toDebitCredit' (aBalance x)))
        M.empty

toDebitCredit :: Quantity -> AccountType -> (Quantity, Quantity)
toDebitCredit q a = if isDebit q a
                    then (q, 0)
                    else (0, q)
                        
toDebitCredit' :: Quantity -> (Quantity, Quantity)
toDebitCredit' q = if isDebit' q
                   then (q, 0)
                   else (0, q)

-- A positive amount is a debit
-- A negative amount is a credit
-- When the amount is equal to zero, we use it the account type
isDebit :: Quantity -> AccountType -> Bool
isDebit q _ | q > 0 = True
isDebit q _ | q < 0 = False
isDebit _ a = a `elem` [Asset, Expense]

-- Same as isDebit, but a zero amount is considered
-- a debit. Usefull for summing balances for example
isDebit' :: Quantity -> Bool
isDebit' q = q >= 0
                     
isCreditAccount :: AccountInfo -> Bool
isCreditAccount a = aType a `elem` [Liability, Equity, Revenue]

isDebitAccount :: AccountInfo -> Bool
isDebitAccount a = aType a `elem` [Asset, Expense]

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
