{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Data.Type
where

-- Most data types are defined here.
-- For the functions related to each type, see the corresponding modules

import qualified Data.Text as T
import Data.Decimal
import Data.Time
import Data.List
import Data.Tree
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

data Account = VirtualAccount {
  vName :: AccountName,
  vType :: AccountType,
  vBalanceWithSubAccounts :: Balance,
  vNumber :: Maybe Integer -- The smallest account number of real account children
  } |
  RealAccount {
  rAccountInfo :: AccountInfo,
  rBalanceWithSubAccounts :: Balance
  }
  deriving (Show)

accType :: Account -> AccountType
accType VirtualAccount{vType=n} = n
accType RealAccount{rAccountInfo=i} = aType i

accName :: Account -> AccountName
accName VirtualAccount{vName=n} = n
accName RealAccount{rAccountInfo=i} = last (aName i)

accNumber :: Account -> Maybe Integer
accNumber a =
  case a of
    (VirtualAccount _ _ _ n) -> n
    (RealAccount i _) -> aNumber i

accBalance :: Account -> Balance
accBalance a =
  case a of
    (VirtualAccount _ _ _ _) -> M.empty
    (RealAccount i _) -> aBalance i

accBalanceWithSubAccounts :: Account -> Balance
accBalanceWithSubAccounts a =
  case a of
    (VirtualAccount _ _ b _) -> b
    (RealAccount _ b) -> b
    
totalBalance :: [AccountInfo] -> Balance
totalBalance = totalBalance' . map aBalance

totalBalance' :: [Balance] -> Balance
totalBalance' = foldl' (\b x -> M.unionWith (+) b x) M.empty

totalBalanceDebitCredit :: [AccountInfo] -> BalanceDebitCredit
totalBalanceDebitCredit =                
 foldl' (\b x -> M.unionWith (\(d1, c1) (d2, c2) -> (d1 + d2, c1 - c2))
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
                     
isCreditAccount :: AccountType -> Bool
isCreditAccount a = a `elem` [Liability, Equity, Revenue]

isDebitAccount :: AccountType -> Bool
isDebitAccount a = a `elem` [Asset, Expense]

depth :: Tree a -> Integer
depth (Node _ []) = 1
depth (Node _ xs) = 1 + maximum (map depth xs)

filterEmptyAccounts :: Forest Account -> Forest Account
filterEmptyAccounts [] = []
filterEmptyAccounts (x : siblings) =
  let fs = filterEmptyAccounts siblings
  in case x of
       Node VirtualAccount{} [] -> filterEmptyAccounts fs
       n@(Node VirtualAccount{} children) ->
         case filterEmptyAccounts children of
           [] -> fs
           cs -> n{subForest = cs} : fs
       n@(Node (RealAccount i _) children) ->
         case filterEmptyAccounts children of
           [] -> if M.empty == aBalance i then fs else n : fs
           cs -> n{subForest = cs} : fs

-- Returns the accounts in a tree structure from the qualified name
-- and a subtotal for each node
toAccountTree :: [AccountInfo] -> Forest Account
toAccountTree accounts =
  let sortedAccounts = sortBy (\a1 a2 -> compare (aType a1, aName a1) (aType a2, aName a2)) accounts
  in makeForest $ map (\a -> (aName a, a)) sortedAccounts

  where
    makeForest :: [(QualifiedName, AccountInfo)] -> Forest Account
    makeForest [] = []
    makeForest xs =
      let groupAccounts = groupBy (\(k1, _) (k2, _) -> head k1 == head k2) xs
      in  map makeTree groupAccounts      

    makeTree ::[(QualifiedName, AccountInfo)] -> Tree Account
    makeTree [] = error "list must not be empty"
    makeTree xs@((k, x):_) =
      let (parent,children) = partition (\(k1, _) -> tail k1 == []) xs
          parentName = head k
          childrenForest = makeForest $ map (\(k1, a) -> (tail k1, a)) children
          b = totalBalance' $ map (accBalanceWithSubAccounts . rootLabel) childrenForest
      in  case parent of
            [] -> let ns = foldl' (\b1 n1 -> case n1 of Nothing -> b1; Just x1 -> x1 : b1) [] $  map (accNumber . rootLabel) childrenForest
                      n = if null ns then Nothing else Just (minimum ns)
                  in Node (VirtualAccount parentName (aType x) b n)
                          childrenForest
            [(_,p)] -> Node (RealAccount p (totalBalance' [(aBalance p), b]))
                       childrenForest
            _ -> error "More than one parent"
          

data Configuration = Configuration {
  cDefaultCommodity :: Commodity,
  cAccountTypeMapping :: M.Map AccountName AccountType,
  cOpeningBalanceAccount :: QualifiedName,
  cEarningsAccount :: QualifiedName,
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
