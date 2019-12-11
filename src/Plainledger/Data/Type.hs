{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

-- For simplicity, all the datatypes are defined here.
-- See the corresponding modules for the related functions.
module Plainledger.Data.Type (
  Algebra,
  CoAlgebra,
  RCoAlgebra,
  Located,
  SourceOffset,

  AccountingFormat(..),
  SignConvention(..),  

  TagF(..),
  Tag,
  TagDescription(..),
  
  Quantity,
  Commodity,

  PostingF(..),
  Posting,
  
  TransactionF(..),
  Transaction,

  Balance,

  TreeF(..),
--  TreeAnn,
--  pattern CNode,
  AccountType(..),
--  VirtualAccount(..),
  AccountInfo(..),
  AccountTree,
  AccountMap,
  AccountName,
  AccountTypeMap,
  QualifiedName,
  
  Configuration(..),
  Ledger(..),

  Journal,
  JournalEntry(..),
  OpenAccountEntry(..),
  OpenEntry(..),
--  NodeA,
  CloseAccountEntry(..),
  TransactionEntry,
  BalanceEntry(..),
  PostingEntry,

  FieldType(..),
  CsvConfiguration(..),
  CsvStatementF(..),
  CsvStatement,
  CsvStatementAnn,
  CsvPrim(..),
  CsvExpr(..),
  CsvExprF(..),
  CsvExprAnn,
--  pattern CVar,
  CsvValue(..)
)
where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Tree
import Data.Decimal
import Data.Time
import Data.Bifunctor.TH
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Compose
import Data.Functor.Classes
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.SExpresso.Parse (SourceOffset)

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a
type RCoAlgebra f t a = a -> f (Either t a)

type Located a = (SourceOffset, a)

data AccountingFormat
  = OneColumnSignedNumber SignConvention
  | TwoColumnsDebitCredit
  deriving (Eq, Ord, Show)

data SignConvention
  = SignDependsOnNetBalance -- The number will have the same sign as (Debit - Credit)
  | SignDependsOnAccountType -- Inverse of DependsOnNetBalance for Liabilities, Equity and Revenue
  deriving (Eq, Ord, Show)

data TagF a = Tag {
  tagKeyword :: T.Text,
  tagValue :: a
  } deriving (Show, Functor, Foldable, Traversable)

type Tag = TagF (Maybe (T.Text))

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
type QualifiedName = NE.NonEmpty AccountName

data AccountType = Asset
                 | Liability
                 | Equity
                 | Revenue
                 | Expense
                 deriving (Show, Eq, Ord)

type Quantity = Decimal

type Commodity = T.Text

data PostingF n q c = Posting {
  pAccount :: n,
  pQuantity :: q, --Quantity,
  pCommodity :: c -- Commodity
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(deriveBifunctor ''PostingF)
$(deriveBifoldable ''PostingF)
$(deriveBitraversable ''PostingF)

type Posting = PostingF QualifiedName Quantity Commodity
type PostingEntry = PostingF QualifiedName (Maybe Quantity) (Maybe Commodity)

data TransactionF d t p = Transaction {
  tDate :: d,
  tTags :: [t],
  tPostings :: [p]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(deriveBifunctor ''TransactionF)
$(deriveBifoldable ''TransactionF)
$(deriveBitraversable ''TransactionF)

type Transaction = TransactionF Day Tag Posting
type TransactionEntry = TransactionF Day (Located Tag) (Located PostingEntry)

-- Both debit and credit are positif integer
type Balance = M.Map Commodity (Quantity, Quantity)

data AccountInfo
  -- = VirtualAccount {
  --  vQName :: Maybe QualifiedName
  --  }
  --  RealAccount {
  = RealAccount {
   aOpenDate :: Day,
   aCloseDate :: Maybe Day,
   aQName :: QualifiedName,
   aNumber :: Maybe Integer,
   aDefaultCommodity :: Commodity,
   aAllowedCommodities :: Maybe (S.Set Commodity),
   aBalance :: Balance
   }
  deriving (Show)

data VirtualAccount
  = VRoot
  | VNode QualifiedName
  deriving (Show)

makeBaseFunctor ''Tree

instance Show1 (TreeF (a, AccountInfo)) where
  liftShowsPrec _ _ _ (NodeF (_, n) _) = shows n
  
-- type TreeAnn info a = Fix (Compose ((,) info) (TreeF a))

-- pattern CNode :: info -> a -> [TreeAnn info a] -> TreeAnn info a
-- pattern CNode info a as = Fix (Compose (info, NodeF a as))

type AccountTree = Tree (Either [AccountName] AccountInfo)
type AccountMap = M.Map QualifiedName AccountInfo

type AccountTypeMap = M.Map AccountName AccountType

data Configuration = Configuration {
  cDefaultCommodity :: Commodity,
  cAccountTypeMapping :: AccountTypeMap,
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

------
data CsvStatementF text expr
  = CsvDefine text [text] expr
  | CsvEvalRules text
  | CsvExpr expr
  deriving (Eq, Show)

type CsvStatement = CsvStatementF T.Text CsvExpr

data CsvPrim
  = OpMinus
  | OpPlus
  | OpMult
  | OpDiv
  | OpRound
  | OpGT
  | OpLT
  | OpEQ
  | OpNEQ
  | OpAnd
  | OpOr
  | OpNot
  | OpIf
  | OpYear
  | OpMonth
  | OpDay
  | OpDate
  | OpPass
  | OpSkipline
  | OpTransaction
  | OpPosting
  | OpTag
  deriving (Eq, Show)

data CsvExpr
  = EVar T.Text
  | EBool Bool
  | EPrim CsvPrim
  | EDate Day
  | EString T.Text
  | EQName QualifiedName
  | ENumber Decimal
  | ECall CsvExpr [CsvExpr]
  deriving (Eq, Show)

makeBaseFunctor ''CsvExpr

type CsvStatementAnn info = CsvStatementF (info, T.Text) (CsvExprAnn info)
type CsvExprAnn info = Fix (Compose ((,) info) CsvExprF)

-- pattern CVar :: info -> T.Text -> CsvExprAnn info
-- pattern CVar info x = Fix (Compose (info, EVarF x))

data CsvValue
  -- Values available in the journal file
  = VDate Day
  | VString T.Text
  | VNumber Decimal
  | VQualifiedName QualifiedName
  | VIdentifier T.Text
  -- Extra values for this little language  
  | VBool Bool  
  | VTransaction
  | VSkipLine
  | VPass
  deriving (Eq, Show)


-- Csv configuration
data FieldType
  = FText
  | FNumber
  | FDate
  deriving (Show, Eq)

data CsvConfiguration = CsvConfiguration {
  columnDelimiter :: Char,
  skip :: Integer,
  fields :: [(T.Text, Integer, FieldType)]
  }
  deriving (Show, Eq)
