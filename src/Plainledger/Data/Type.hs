{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}

-- For simplicity, all the datatypes are defined here.
-- See the corresponding modules for the related functions.
module Plainledger.Data.Type (
  Algebra,
  CoAlgebra,
  RCoAlgebra,

  AccountingType(..),

  Tag(..),
  TagDescription(..),
  
  Quantity,
  Commodity,

  PostingF(..),
  Posting,
  
  TransactionF(..),
  Transaction,

  Balance,

  TreeF(..),
  AccountType(..),
  AccountInfo(..),
  AnnAccount,
  Account,
  AccountMap,
  AccountName,
  QualifiedName,
  
  Configuration(..),
  Ledger(..),

  Journal,
  JournalEntry(..),
  OpenAccountEntry(..),
  OpenEntry(..),
  CloseAccountEntry(..),
  TransactionEntry,
  BalanceEntry(..),
  PostingEntry,

)
where


import qualified Data.Text as T
import Data.Tree
import Data.Decimal
import Data.Time
import Data.Bifunctor.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
--import Data.Functor.Compose
import Data.Functor.Classes
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.SExpresso.Parse.Location

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a
type RCoAlgebra f t a = a -> f (Either t a)

data AccountingType = PlusMinus | DebitCredit
                    deriving (Eq, Ord, Show)

data Tag = Tag {
  tagKeyword :: T.Text,
  tagValue :: Maybe (T.Text)
  } deriving (Show)

-- Tags are compared and sorted based on their keys
instance Eq Tag where
  t1 == t2 = tagKeyword t1 == tagKeyword t2

instance Ord Tag where
  compare t1 t2 = compare (tagKeyword t1) (tagKeyword t2)

data TagDescription = TagDescription {
  tdTagKeyword :: T.Text,
  tdHeader :: T.Text,
  tdUnique :: Bool,
  tdForeignKey :: Maybe T.Text,
  tdDefaultValue :: T.Text,
  tdValueIfMising :: T.Text
  }
  deriving (Eq, Show)


-- Name of an account
type AccountName = T.Text

-- The account's parents name and its name
-- ex : Asset:Banking:Bank Foo:Chequing ->
--        ["Asset","Banking","Bank Foo","Chequing"]
type QualifiedName = [AccountName]

data AccountType = Asset
                 | Liability
                 | Equity
                 | Revenue
                 | Expense
                 deriving (Show, Eq, Ord)

type Quantity = Decimal

type Commodity = T.Text

data PostingF q c = Posting {
  pAccount :: QualifiedName,
  pQuantity :: q, --Quantity,
  pCommodity :: c -- Commodity
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(deriveBifunctor ''PostingF)
$(deriveBifoldable ''PostingF)
$(deriveBitraversable ''PostingF)

type Posting = PostingF Quantity Commodity
type PostingEntry = PostingF (Maybe Quantity) (Maybe Commodity)

data TransactionF t p = Transaction {
  tDate :: Day,
  tTags :: [t],
  tPostings :: [p]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(deriveBifunctor ''TransactionF)
$(deriveBifoldable ''TransactionF)
$(deriveBitraversable ''TransactionF)

type Transaction = TransactionF Tag Posting
type TransactionEntry = TransactionF (Located Tag) (Located PostingEntry)

-- Both debit and credit are positif integer
type Balance = M.Map Commodity (Quantity, Quantity)

data AccountInfo
  = VirtualAccount {
   aQName :: QualifiedName
   }
  | RealAccount {
   aOpenDate :: Day,
   aCloseDate :: Maybe Day,
   aQName :: QualifiedName,
   aNumber :: Maybe Integer,
   aDefaultCommodity :: Commodity,
   aAllowedCommodities :: Maybe (S.Set Commodity),
   aBalance :: Balance
   }
  deriving (Show)

makeBaseFunctor ''Tree

instance Show1 (TreeF (a, AccountInfo)) where
  liftShowsPrec _ _ _ (NodeF (_, n) _) = shows n
  
type AnnAccount a = Fix (TreeF (a, AccountInfo))
type Account = Tree AccountInfo
type AccountMap = M.Map QualifiedName AccountInfo

data Configuration = Configuration {
  cDefaultCommodity :: Commodity,
  cAccountTypeMapping :: M.Map AccountName AccountType,
  cOpeningBalanceAccount :: QualifiedName,
  cEarningsAccount :: QualifiedName,
  cTagDescription :: M.Map T.Text TagDescription,
  cTagDefaultValue :: T.Text,
  cTagValueIfMissing :: T.Text
  }
  deriving (Show)

data Ledger = Ledger {
  lConfiguration :: Configuration,
  lTransactions :: [Transaction],
  lAccounts :: AccountMap
  }
  deriving (Show)

---------

data JournalEntry
  = JEConfiguration Configuration
  | JEOpenAccount OpenEntry
  | JETransaction TransactionEntry
  | JEBalance BalanceEntry
  | JECloseAccount CloseAccountEntry
  deriving (Show)

type Journal = [Located JournalEntry]

data CloseAccountEntry = CloseAccountEntry {caDate :: Day,
                                            caNames :: [Located QualifiedName]
                                           }
                         deriving (Show)

data BalanceEntry = BalanceEntry {bDate :: Day,
                                  bName :: QualifiedName,
                                  bQuantity :: Quantity,
                                  bCommodity :: (Maybe Commodity)
                                 }
                    deriving (Show)


  
data OpenEntry = OpenEntry {oaDate :: Day,
                            oaAccountEntry :: [Located OpenAccountEntry]
                           }
                 deriving (Show)
                 
data OpenAccountEntry = OpenAccountEntry {
  oaName :: QualifiedName,
  oaNumber :: Maybe Integer,
  oaAllowedCommodities :: Maybe [Commodity],
  oaAllowAnyCommodities :: Bool,
  oaDefaultCommodity :: Maybe Commodity
  }
  deriving (Show)
