-- |
-- Module      :  Plainledger.Journal.Account
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the account data type.

{-# LANGUAGE TupleSections #-}
module Plainledger.Journal.Account
(
  AccountF(..),
  JAccount,
  Account,
  ChartOfAccounts,
  chartToList,
  decodeAccounts,
  validateAccounts,
  -- accountsToHashMap,
  decodeAccountsFile,
  AccountType(..),
  decodeAccountType,
  topAccounts,
  isBalanceSheetType,
  isIncomeStatementType,
  isCreditType,
  isDebitType,
  dummyAccount
  )
where

import System.FilePath
import Data.Char (ord)
import Data.Function
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.List hiding (group, lines)
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Internal.Utils
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Tree
import Data.Hashable ( Hashable(hashWithSalt) )
import Data.Bifunctor (second)
import Data.Maybe (mapMaybe)

-- | The top level grouping of an account. Must be Asset, Liability,
-- Equity, Revenue or Expense.
data AccountType = Asset
                | Liability
                | Equity
                | Revenue
                | Expense
                deriving (Show, Eq, Ord)

i18nAccountType :: Language -> [T.Text]
i18nAccountType lang = [i18nText lang TAsset,
                        i18nText lang TLiability,
                        i18nText lang TEquity,
                        i18nText lang TRevenue,
                        i18nText lang TExpense]

decodeAccountType :: Language -> T.Text -> Maybe AccountType
decodeAccountType lang x | x == i18nText lang TAsset = return Asset
decodeAccountType lang x | x == i18nText lang TLiability = return Liability
decodeAccountType lang x | x == i18nText lang TEquity = return Equity
decodeAccountType lang x | x == i18nText lang TRevenue = return Revenue
decodeAccountType lang x | x == i18nText lang TExpense = return Expense
decodeAccountType _ _ = Nothing

isBalanceSheetType :: AccountType -> Bool
isBalanceSheetType a = a `elem` [Asset, Liability, Equity]

isIncomeStatementType :: AccountType -> Bool
isIncomeStatementType = not . isBalanceSheetType

isCreditType :: AccountType -> Bool
isCreditType a = a `elem` [Liability, Equity, Revenue]

isDebitType :: AccountType -> Bool
isDebitType = not . isCreditType

data AccountF p t = Account {
  aId :: Int, -- Generated by the program, not the user. Number 1 to 5 corresponds
              -- to the fire AccountType
  aIdentifier :: T.Text, -- Unique to each account. Used in the CSV files
  aDisplayName :: T.Text, -- Many accounts can have the same display name
  aNumber :: Maybe Int, -- Provided by the user
  aParent :: p, -- Identifier of the parent, as defined in the account CSV files
  aAccountType :: t -- The account type of the account. This property is not known when
                    -- parsing the accounts file.
}
  deriving (Show)

type JAccount = AccountF T.Text ()
type Account = AccountF Int AccountType

dummyAccount :: a -> AccountF Int a
dummyAccount = Account 0 "" "" Nothing 0

topAccounts :: Language -> [Account]
topAccounts lang =
  [Account 1 (i18nText lang TAsset) (i18nText lang TAsset) Nothing 0 Asset,
   Account 2 (i18nText lang TLiability) (i18nText lang TLiability) Nothing 0 Liability,
   Account 3 (i18nText lang TEquity) (i18nText lang TEquity) Nothing 0 Equity,
   Account 4 (i18nText lang TRevenue) (i18nText lang TRevenue) Nothing 0 Revenue,
   Account 5 (i18nText lang TExpense) (i18nText lang TExpense) Nothing 0 Expense]

instance Eq (AccountF p t) where
  a1 == a2 = aId a1 == aId a2

instance Hashable (AccountF p t) where
  hashWithSalt k a = hashWithSalt k (aId a)

instance Ord (AccountF p t) where
  compare a1 a2 = compare (aId a1) (aId a2)

type ChartOfAccounts = [Tree Account]

chartToList :: ChartOfAccounts -> [Account]
chartToList [] = []
chartToList (x:xs) = treeToList x ++ chartToList xs
  where treeToList (Node y ys) = y : concatMap treeToList ys

-- accountsToHashMap :: [Account] -> HashMap T.Text Account
-- accountsToHashMap = HM.fromList . map (\a -> (aId a, a))

validateAccounts :: (MonadError Errors m) =>
                    T.Text ->
                    T.Text ->
                    Language ->
                    [(SourcePos, JAccount)] ->
                    m ChartOfAccounts
validateAccounts openingAccount earningAccount lang accounts = do
  validateAccountIdentNonNull accounts
  validateAccountIdentNoDup accounts
  validateAccountIdentNonTopAccount lang accounts
  validateOpeningBalanceAccount accounts openingAccount
  validateEarningsAccount accounts earningAccount
  makeChartOfAccounts lang accounts


validateAccountIdentNonNull :: (MonadError Errors m) =>
                            [(SourcePos, JAccount)] ->
                            m ()
validateAccountIdentNonNull accounts =
  let nullId :: [(SourcePos, T.Text)]
      nullId = filter (T.null . snd)
             $ map (fmap aIdentifier) accounts
  in if null nullId
     then return ()
     else throwError
          $ mkErrorMultiPos (map fst nullId)
            ZeroLengthAccountId

validateAccountIdentNonTopAccount :: (MonadError Errors m) =>
                            Language ->
                            [(SourcePos, JAccount)] ->
                            m ()
validateAccountIdentNonTopAccount lang accounts =
  let bad :: [(SourcePos, T.Text)]
      bad = filter (\(_,a) -> a `elem` i18nAccountType lang)
          $ map (fmap aIdentifier) accounts
  in if null bad
     then return ()
     else throwError
          $ mkErrorMultiPos (map fst bad)
            (InvalidIdentifier (map (T.unpack . snd) bad))

validateAccountIdentNoDup :: (MonadError Errors m) =>
                      [(SourcePos, JAccount)] ->
                      m ()
validateAccountIdentNoDup accounts =
  let dup :: [[(SourcePos, T.Text)]]
      dup = filter (not . null . tail)
          $ groupBy ((==) `on` snd)
          $ sortBy (compare `on` snd)
          $ map (fmap aIdentifier) accounts
      mkErr ls = mkErrorMultiPos (map fst ls)
                 (DuplicateAccountId $ T.unpack $ snd $ head ls)
  in if null dup
     then return ()
     else throwError $ concatMap mkErr dup

validateOpeningBalanceAccount :: (MonadError Errors m) =>
                           [(SourcePos, JAccount)] ->
                           T.Text ->
                           m ()
validateOpeningBalanceAccount accounts openingAccount =
  if openingAccount `elem` map (aIdentifier . snd) accounts
  then return ()
  else throwError
       $ mkErrorNoPos
       $ OpeningBalanceNotDefined
       $ T.unpack openingAccount

validateEarningsAccount :: (MonadError Errors m) =>
                           [(SourcePos, JAccount)] ->
                           T.Text ->
                           m ()
validateEarningsAccount accounts earningAccount =
  if earningAccount `elem` map (aIdentifier . snd) accounts
  then return ()
  else throwError
       $ mkErrorNoPos
       $ EarningsAccountNotDefined
       $ T.unpack earningAccount

validateParentAccount :: (MonadError Errors m) =>
                         [(SourcePos, JAccount)] ->
                         Language ->
                         m [(SourcePos, AccountF Int ())]
validateParentAccount accounts lang =
  let parents = map (\(_,x) -> (aIdentifier x, aId x)) accounts
              ++ map (\x -> (aIdentifier x, aId x)) (topAccounts lang)
      findParent (pos, a) =
        let p = aParent a
        in case find (\x -> fst x == p) parents of
             Just x -> return (pos, a{aParent = snd x})
             Nothing ->
                  let aIdent = T.unpack $ aIdentifier a
                      aPar = T.unpack p
                      err = InvalidParent aIdent aPar
                  in throwError $ mkError pos err
  in traverse findParent accounts

makeChartOfAccounts :: (MonadError Errors m) =>
                        Language ->
                        [(SourcePos, JAccount)] ->
                        m ChartOfAccounts
makeChartOfAccounts lang accounts = do
  -- We use an algorithm similar to this technique :
  --  https://stackoverflow.com/questions/444296/how-to-efficiently-build-a-tree-from-a-flat-structure
  -- First we populate a Map Int (Account, Bool, [Int]) with
  --  every account and (account, False, []) as value
  --  The values mean (Self, InChart, [List of children])
  -- Then for each account, we add this account to the list of its parent
  -- Then we start from the 5 top accounts (Asset, Liability, etc.)
  --  and build the node with its children, setting the in cycle from the map
  --  to false and recurse onto the children
  -- If at the end there is still accounts in the map with in cycle set to true, 
  -- then it means there is a cycle.

  -- First we make sure all accounts have a valid parent
  okAccounts <- validateParentAccount accounts lang

  -- Then we set Asset as a dummy account type
  -- to create Account from JAccount
  -- accs :: [Account]
  let accs = topAccounts lang
           ++ map (\a -> (snd a){aAccountType = Asset}) okAccounts

  -- parents :: HM.HashMap Int (Account, [Int])
  let parents = HM.fromList $ map (\a -> (aId a, (a, False, []))) accs

  -- For each accounts in the account file, we identify the parent
  -- and add this account to its children. (We do not)
  -- children :: HM.HashMap Int (Account, Bool, [Int])
  let children = foldl (\p (_,a) -> HM.adjust (second (aId a :)) (aParent a) p)
                       parents
                       okAccounts

  -- Now we build the tree starting from the five topAccount
  let (m, chart) = mkChart children

  -- Check if there is any account left with a False flage
  let cycles = map ((\(x,_,_) -> x) . snd)
             $ HM.toList
             $ HM.filter (\(_,x,_) -> not x) m

  if null cycles
  then return $ sortOn (aId . rootLabel) chart
  else 
    let pos = map fst
            $ mapMaybe (\c -> find (\(_, a) -> aId a == aId c) accounts) cycles
        xs = map (T.unpack . aIdentifier) cycles
        err = CycleInParents xs
    in throwError $ mkErrorMultiPos pos err

  where
    mkChart :: HM.HashMap Int (Account, Bool, [Int]) ->
               (HM.HashMap Int (Account, Bool, [Int]), [Tree Account])
    mkChart m =
      let topId = map (\a -> (aAccountType a, aId a)) (topAccounts lang)
          foo (m', ns') (at, n) = second (:ns') $ mkTree m' at n
      in foldl foo (m, []) topId

    mkList :: HM.HashMap Int (Account, Bool, [Int]) ->
              AccountType ->
              [Int] ->
              (HM.HashMap Int (Account, Bool, [Int]), [Tree Account])
    mkList m at ns = let foo (m', ns') n = second (:ns') $ mkTree m' at n
                     in foldl foo (m, []) ns

    mkTree :: HM.HashMap Int (Account, Bool, [Int]) ->
              AccountType ->
              Int ->
              (HM.HashMap Int (Account, Bool, [Int]), Tree Account)
    mkTree m at n = let (self, _, xs) = m HM.! n
                        m1 = HM.adjust (\(x,_,y) -> (x,True,y)) n m
                        (m', xs') = mkList m1 at xs
                    in (m', Node self{aAccountType = at} xs')

-- | The first line is the header
decodeAccounts :: (MonadError Errors m)
               => Language -> Char -> ByteString -> m [JAccount]
decodeAccounts lang csvSeparator bs = do
  -- Read the CSV file as vector of T.Text
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }
  csv <- either (throwError . mkErrorNoPos . ErrorMessage) return
      $ C.decodeWith opts C.NoHeader bs

  -- Decode the header to know the index of columns
  let myFilter t = t `elem` [i18nText lang TAccountIdent,
                             i18nText lang TAccountName,
                             i18nText lang TAccountNumber,
                             i18nText lang TAccountParent]
  (csvData, indexes) <- processColumnIndexes csv myFilter

  identIdx <- columnIndex indexes (i18nText lang TAccountIdent)
  let numberIdx = optionalColumnIndex indexes (i18nText lang TAccountNumber)
  parentIdx <- columnIndex indexes (i18nText lang TAccountParent)
  let nameIdx = optionalColumnIndex indexes (i18nText lang TAccountName)

 -- Add row information to the CSV line
  let csvWithRowNumber = zip [2..] $ V.toList csvData

  accs <- mapM (parseLine identIdx nameIdx numberIdx parentIdx) csvWithRowNumber

  -- Fill DisplayName if it was missing
  let accWithNames = map
                     (\a -> if T.null (aDisplayName a) then a{aDisplayName = aIdentifier a} else a)
                     accs
  return accWithNames

  where
    -- Function to parse a line into an Account 
    -- We use the row number as the unique id for the account    
    parseLine :: (MonadError Errors m) =>
                 ColumnIndex ->
                 Maybe ColumnIndex ->
                 Maybe ColumnIndex ->
                 ColumnIndex ->
                 (Int, V.Vector T.Text) ->
                 m JAccount
    parseLine identIdx nameIdx numberIdx parentIdx (row, line) =
          let p = do
                ident <- columnData identIdx line
                name <- optionalColumnData ident nameIdx line
                number <- optionalColumnDataM Nothing numberIdx line parseIntMaybe
                parent <- columnData parentIdx line
                -- The first 5 id number are reserved for the top five accounts
                return $ Account (row + 5) ident name number parent ()
          in p -- `catchError` (throwError . setSourcePosRowIfNull row)

decodeAccountsFile :: Language -> FilePath -> Char -> ExceptT Errors IO  [(SourcePos, JAccount)]
decodeAccountsFile lang filePath csvSeparator =
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap (snd . removeBom) $ liftIO $ BS.readFile filePath
      accs <- decodeAccounts lang csvSeparator (BL.fromStrict csvBS)
      let pos = map (\i -> SourcePos filePath i 0) [2..]
      return $ zip pos accs
