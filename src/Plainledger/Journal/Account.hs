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
  encodeAccounts,
  decodeAccounts,
  validateAccounts,
  accountsToHashMap,
  decodeAccountsFile,
  AccountGroup(..),
  validateConfig,
  isBalanceSheetGroup,
  isIncomeStatementGroup,
  isCreditGroup,
  isDebitGroup
  )
where

import Data.Char (toLower)
import Data.Hashable (Hashable)
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Csv (Record, Field, ToField(..),toRecord, FromField(..))
import Data.HashMap.Strict (HashMap)
import Data.List hiding (group, lines)
import Data.Ord (comparing)
import GHC.Generics
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Journal.Configuration
import Plainledger.Journal.Tag
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

-- | The top level grouping of an account. Must be Asset, Liability,
-- Equity, Revenue or Expense.
data AccountGroup = Asset
                  | Liability
                  | Equity
                  | Revenue
                  | Expense
                  deriving (Show, Eq, Ord, Generic)

instance Hashable AccountGroup
instance ToField AccountGroup where
  toField Asset = toField ("Asset" :: String)
  toField Liability = toField ("Liability" :: String)
  toField Equity = toField ("Equity" :: String)
  toField Revenue = toField ("Revenue" :: String)
  toField Expense = toField ("Expense" :: String)

instance FromField AccountGroup where
  parseField x = do
    s <- parseField x
    case (map toLower s) of
         "asset" -> return Asset
         "liability" -> return Liability
         "equity" -> return Equity
         "revenue" -> return Revenue
         "expense" -> return Expense
         _ -> fail $ "Unknown account group \"" ++ s ++ "\". Must be one of the following\n" ++
                   " Asset\n" ++
                   " Liability\n" ++
                   " Equity\n" ++
                   " Revenue\n" ++
                   " Expense"

isBalanceSheetGroup :: AccountGroup -> Bool
isBalanceSheetGroup a = a `elem` [Asset, Liability, Equity]

isIncomeStatementGroup :: AccountGroup -> Bool
isIncomeStatementGroup = not . isBalanceSheetGroup

isCreditGroup :: AccountGroup -> Bool
isCreditGroup a = a `elem` [Liability, Equity, Revenue]

isDebitGroup :: AccountGroup -> Bool
isDebitGroup = not . isCreditGroup

-- | The Account data type serves as aggregation point for commodities
-- relating to a particuliar purpose.
data Account = Account
  {aId :: T.Text,
   aName :: T.Text,
   aNumber :: Int,
   aGroup :: AccountGroup,
   aSubgroup :: T.Text, -- | Empty means no subgoup
   aSubsubgroup :: T.Text, -- | Empty means no subsubgroup
   aTags :: [Tag]
  }
  deriving (Show, Generic)

-- / We sort the accounts when comparing two accounts
-- The Eq instance is mainly used in the unittests. In a validated ledger,
-- you can rely on the aId to identify an account.
instance Eq Account where
  a1 == a2 =  aId a1 == aId a2
           && aName a1 == aName a2
           && aNumber a1 == aNumber a2
           && aGroup a1 == aGroup a2
           && aSubgroup a1 == aSubgroup a2
           && aSubsubgroup a1 == aSubsubgroup a2
           && sortBy (comparing tagId) (aTags a1) ==
              sortBy (comparing tagId) (aTags a2)

accountsToHashMap :: [Account] -> HashMap T.Text Account
accountsToHashMap = HM.fromList . map (\a -> (aId a, a))

validateAccounts :: (MonadError Error m) =>
                      [Account] ->
                      m [Account]
validateAccounts accounts = do
  validateAccountIdNonNull accounts
  validateAccountIdNoDup accounts
  let accWithNames = map
                     (\a -> if T.null (aName a) then a{aName = aId a} else a)
                     accounts
  return accWithNames

validateAccountIdNonNull :: (MonadError Error m) =>
                            [Account] ->
                            m ()
validateAccountIdNonNull accounts =
  let nullId = filter
               (T.null . fst)
               (map (\a -> (aId a, aName a)) accounts)
  in if null nullId
     then return ()
     else throwError
          $ "Unallowed zero length account id for the following accounts : "
          ++ (intercalate " "
             $ map (\k -> "\"" ++ T.unpack k ++ "\"")
             $ map snd nullId)
          ++ "."

validateAccountIdNoDup :: (MonadError Error m) =>
                      [Account] ->
                      m ()
validateAccountIdNoDup accounts =
  let dup = HM.filter (/= (1 :: Int))
          $ HM.fromListWith (+)
          $ zip (map aId accounts) (repeat 1)
  in if HM.size dup /= 0
     then throwError
          $ "Duplicate account id : "
          ++ (intercalate " "
             $ map (\k -> "\"" ++ T.unpack k ++ "\"")
             $ HM.keys dup)
          ++ "."
     else return ()

-- CSV functions
coreHeader :: [Field]
coreHeader = ["id", "name", "number", "group", "subgroup", "subsubgroup"]

-- / Encode a list of accounts as a Csv. The first line is the header
encodeAccounts :: [Account] -> ByteString
encodeAccounts accs =
  let tagH = tagHeader $ concatMap aTags accs
      header = toRecord $ coreHeader ++ tagH
      lines = header : map (toLine tagH) accs
  in C.encode lines

  where toLine :: [Field] -> Account -> Record
        toLine tagH a =
          let coreLine = [toField $ aId a,
                          toField $ aName a,
                          toField $ aNumber a,
                          toField $ aGroup a,
                          toField $ aSubgroup a,
                          toField $ aSubsubgroup a]
          in toRecord $ coreLine ++ tagLine (aTags a) tagH

-- | The first line is the header
decodeAccounts :: (MonadError Error m) => ByteString -> m [Account]
decodeAccounts bs = do
  csv <- either throwError return $ C.decode C.NoHeader bs
  csvToData (csv :: C.Csv) fromLine

  where fromLine :: (MonadError Error m) =>
                    HM.HashMap Field Field -> m Account
        fromLine m = do
          id' <- findColumn "id" m
          name <- findColumnDefault "" "name" m
          number <- findColumn "number" m
          group <- findColumn "group" m
          subgroup <- findColumnDefault "" "subgroup" m
          subsubgroup <- findColumnDefault "" "subsubgroup" m
          tags <- recordToTags m (HS.fromList coreHeader)
          return $ Account id' name number group subgroup subsubgroup tags

decodeAccountsFile :: FilePath -> ExceptT Error IO  [Account]
decodeAccountsFile f = do
        csvBS <- liftIO $ BL.readFile f
        decodeAccounts csvBS
