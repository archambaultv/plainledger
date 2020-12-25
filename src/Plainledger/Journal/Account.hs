-- |
-- Module      :  Plainledger.Journal.Account
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the account data type.

module Plainledger.Journal.Account 
(
  Account(..),
  decodeAccounts,
  validateAccounts,
  -- accountsToHashMap,
  decodeAccountsFile,
  AccountType(..),
  isBalanceSheetType,
  isIncomeStatementType,
  isCreditType,
  isDebitType,
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
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Internal.Utils
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.Text as T
import qualified Data.Vector as V

-- | The top level grouping of an account. Must be Asset, Liability,
-- Equity, Revenue or Expense.
data AccountType = Asset
                | Liability
                | Equity
                | Revenue
                | Expense
                deriving (Show, Eq, Ord)

decodeAccountType :: (MonadError Errors m) => T.Text -> m AccountType
decodeAccountType "Actif" = return Asset
decodeAccountType "Passif" = return Liability
decodeAccountType "Capital" = return Equity
decodeAccountType "Revenu" = return Revenue
decodeAccountType "Dépense" = return Expense
decodeAccountType s = throwError
                    $ mkErrorNoPos
                    $ UnknownAccountType (T.unpack s)

isBalanceSheetType :: AccountType -> Bool
isBalanceSheetType a = a `elem` [Asset, Liability, Equity]

isIncomeStatementType :: AccountType -> Bool
isIncomeStatementType = not . isBalanceSheetType

isCreditType :: AccountType -> Bool
isCreditType a = a `elem` [Liability, Equity, Revenue]

isDebitType :: AccountType -> Bool
isDebitType = not . isCreditType

data Account = Account {
  aId :: T.Text,
  aDisplayName :: T.Text,
  aNumber :: Int,
  aType :: AccountType,
  aGroup :: T.Text, -- | Empty means no subgoup
  aSubGroup :: T.Text -- | Empty means no subsubgroup
}
  deriving (Eq, Show)

-- accountsToHashMap :: [Account] -> HashMap T.Text Account
-- accountsToHashMap = HM.fromList . map (\a -> (aId a, a))

validateAccounts :: (MonadError Errors m) =>
                    T.Text ->
                    T.Text ->
                    [(SourcePos, Account)] ->
                    m [Account]
validateAccounts openingAccount earningAccount accounts = do
  validateAccountIdNonNull accounts
  validateAccountIdNoDup accounts
  validateOpeningBalanceAccount accounts openingAccount
  validateEarningsAccount accounts earningAccount
  return $ map snd accounts


validateAccountIdNonNull :: (MonadError Errors m) =>
                            [(SourcePos, Account)] ->
                            m ()
validateAccountIdNonNull accounts =
  let nullId :: [(SourcePos, T.Text)]
      nullId = filter (T.null . snd)
             $ map (fmap aId) accounts
  in if null nullId
     then return ()
     else throwError
          $ mkErrorMultiPos (map fst nullId)
            ZeroLengthAccountId

validateAccountIdNoDup :: (MonadError Errors m) =>
                      [(SourcePos, Account)] ->
                      m ()
validateAccountIdNoDup accounts =
  let dup :: [[(SourcePos, T.Text)]]
      dup = filter (not . null . tail)
          $ groupBy ((==) `on` snd)
          $ sortBy (compare `on` snd)
          $ map (fmap aId) accounts
      mkErr ls = mkErrorMultiPos (map fst ls)
                 (DuplicateAccountId $ T.unpack $ snd $ head ls)
  in if null dup
     then return ()
     else throwError $ concatMap mkErr dup

validateOpeningBalanceAccount :: (MonadError Errors m) =>
                           [(SourcePos, Account)] ->
                           T.Text ->
                           m ()
validateOpeningBalanceAccount accounts openingAccount =
  if openingAccount `elem` (map (aId . snd) accounts)
  then return ()
  else throwError
       $ mkErrorNoPos 
       $ OpeningBalanceNotDefined 
       $ T.unpack 
       $ openingAccount
     
validateEarningsAccount :: (MonadError Errors m) =>
                           [(SourcePos, Account)] ->
                           T.Text ->
                           m ()
validateEarningsAccount accounts earningAccount =
  if earningAccount `elem` (map (aId . snd) accounts)
  then return ()
  else throwError
       $ mkErrorNoPos 
       $ EarningsAccountNotDefined
       $ T.unpack
       $ earningAccount

-- | The first line is the header
decodeAccounts :: forall m . (MonadError Errors m) 
               => Char -> ByteString -> m [Account]
decodeAccounts csvSeparator bs = do
  -- Read the CSV file as vector of T.Text
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }
  csv <- either (throwError . mkErrorNoPos . ErrorMessage) return 
      $ C.decodeWith opts C.NoHeader bs

  -- Decode the header to know the index of columns
  let myFilter t = t `elem` ["Id", "Numéro", "Type", "Nom", "Groupe", "Sous-groupe"]
  (csvData, indexes) <- processColumnIndexes csv myFilter

  idIdx <- columnIndex indexes "Id"
  numberIdx <- columnIndex indexes "Numéro"
  typeIdx <- columnIndex indexes "Type"
  let nameIdx = optionalColumnIndex indexes "Nom"
  let groupIdx = optionalColumnIndex indexes "Groupe"
  let subGroupIdx = optionalColumnIndex indexes "Sous-groupe"

 -- Add row information to the CSV line
  let csvWithRowNumber = zip [2..] $ V.toList csvData
  
  -- Function to parse a line into an Account                            
  let parseLine (row, line) = 
          let p = do
                id' <- columnData idIdx line
                name <- optionalColumnData id' nameIdx line
                number <- columnDataM numberIdx line parseInt
                type1 <- columnDataM typeIdx line decodeAccountType
                group <- optionalColumnData "" groupIdx line
                subgroup <- optionalColumnData "" subGroupIdx line
                return $ Account id' name number type1 group subgroup
          in p `catchError` (throwError . setSourcePosRowIfNull row)
  

  accs <- mapM parseLine csvWithRowNumber

  -- Fill DisplayName if it was missing
  let accWithNames = map
                     (\a -> if T.null (aDisplayName a) then a{aDisplayName = aId a} else a)
                     accs
  return accWithNames

decodeAccountsFile :: FilePath -> Char -> ExceptT Errors IO  [(SourcePos, Account)]
decodeAccountsFile filePath csvSeparator = 
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap removeBom $ liftIO $ BS.readFile filePath
      accs <- decodeAccounts csvSeparator (BL.fromStrict csvBS)
      let pos = map (\i -> SourcePos filePath i 0) [2..]
      return $ zip pos accs
