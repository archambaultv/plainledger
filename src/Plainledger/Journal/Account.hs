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
  -- encodeAccounts,
  decodeAccounts,
  validateAccounts,
  -- accountsToHashMap,
  decodeAccountsFile,
  AccountType(..),
  isBalanceSheetType,
  isIncomeStatementType,
  isCreditType,
  isDebitType,
  -- ChartOfAccount,
  -- ChartNode(..),
  -- TreeF(..),
  -- accountsToChartOfAccounts,
  -- nodeNumber,
  -- nodeGroup,
  -- nodeSubGroup,
  -- nodeSubSubGroup,
  -- nodeName
  -- cataChart,
  -- GroupF(..),
  -- SubGroupF(..),
  -- SubSubGroupF(..),
  -- Group,
  -- SubGroup,
  -- SubSubGroup
  )
where

import System.FilePath
import Data.Char (ord)
--import Data.Hashable (Hashable)
import Data.Function
import Control.Monad.Except
--import Data.Tree
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
--import Data.Csv (Record, Field, ToField(..),toRecord, FromField(..))
--import Data.HashMap.Strict (HashMap)
import Data.List hiding (group, lines)
--import Data.Ord (comparing)
--import GHC.Generics
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Internal.Utils
import Plainledger.Journal.JournalFile
import Prelude hiding (lines)
import qualified Data.Csv as C
--import qualified Data.HashMap.Strict as HM
--import qualified Data.HashSet as HS
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
                    JournalFile ->
                    [(SourcePos, Account)] ->
                    m [Account]
validateAccounts config accounts = do
  validateAccountIdNonNull accounts
  validateAccountIdNoDup accounts
  validateOpeningBalanceAccount accounts config
  validateEarningsAccount accounts config
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
                           JournalFile ->
                           m ()
validateOpeningBalanceAccount accounts config =
  if jfOpeningBalanceAccount config `elem` (map (aId . snd) accounts)
  then return ()
  else throwError
       $ mkErrorNoPos 
       $ OpeningBalanceNotDefined 
       $ T.unpack 
       $ jfOpeningBalanceAccount config
     
validateEarningsAccount :: (MonadError Errors m) =>
                           [(SourcePos, Account)] ->
                           JournalFile ->
                           m ()
validateEarningsAccount accounts config =
  if jfEarningsAccount config `elem` (map (aId . snd) accounts)
  then return ()
  else throwError
       $ mkErrorNoPos 
       $ EarningsAccountNotDefined
       $ T.unpack
       $ jfEarningsAccount config

-- -- CSV functions
-- coreHeader :: [Field]
-- coreHeader = ["Nom", "Nom pour les rapports", "Numéro", "Type", "Groupe", "Sous-groupe"]

-- -- / Encode a list of accounts as a Csv. The first line is the header
-- encodeAccounts :: [Account] -> ByteString
-- encodeAccounts accs =
--   let header = toRecord $ coreHeader
--       lines = header : map toLine accs
--   in C.encode lines

--   where toLine :: Account -> Record
--         toLine a =
--           let coreLine = [toField $ aId a,
--                           toField $ aDisplayName a,
--                           toField $ aNumber a,
--                           toField $ aType a,
--                           toField $ aGroup a,
--                           toField $ aSubGroup a]
--           in toRecord coreLine

-- | The first line is the header
decodeAccounts :: forall m . (MonadError Errors m) 
               => Char -> ByteString -> m [Account]
decodeAccounts csvSeparator bs = do
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }
  csv <- either (throwError . mkErrorNoPos . ErrorMessage) return 
      $ C.decodeWith opts C.NoHeader bs
  (csvData, columnIndexes) <- processColumnIndexes csv
                              ["Id", "Nom", "Numéro", "Type", "Groupe","Sous-groupe"]
  let csvWithRowNumber = zip [2..] $ V.toList csvData
  accs <- mapM (parseLine columnIndexes) csvWithRowNumber
  let accWithNames = map
                     (\a -> if T.null (aDisplayName a) then a{aDisplayName = aId a} else a)
                     accs
  return accWithNames

  where parseLine :: V.Vector Int -> (Int, V.Vector T.Text) -> m Account
        parseLine idx (row, line) = 
          let p = do
                id' <- findColumn (idx V.! 0) line "Id"
                name <- findColumnDefault id' (idx V.! 1) line
                number <- findColumnM (idx V.! 2) line parseInt "Numéro"
                type1 <- findColumnM (idx V.! 3) line decodeAccountType "Type"
                group <- findColumnDefault "" (idx V.! 4) line
                subgroup <- findColumnDefault "" (idx V.! 5) line
                return $ Account id' name number type1 group subgroup
          in p `catchError` (throwError . setSourcePosRowIfNull row)

decodeAccountsFile :: FilePath -> Char -> ExceptT Errors IO  [(SourcePos, Account)]
decodeAccountsFile filePath csvSeparator = 
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap removeBom $ liftIO $ BS.readFile filePath
      accs <- decodeAccounts csvSeparator (BL.fromStrict csvBS)
      let pos = map (\i -> SourcePos filePath i 0) [2..]
      return $ zip pos accs

-- type ChartOfAccount = Tree ChartNode

-- data ChartNode
--   = Root
--   | Group Int AccountType
--   | SubGroup Int AccountType T.Text
--   | SubSubGroup Int AccountType T.Text T.Text
--   | CAccount Account
--   deriving (Eq, Show)

-- nodeGroup :: ChartNode -> AccountType
-- nodeGroup Root = error "nodeGroup called on Root"
-- nodeGroup (Group _ x) = x
-- nodeGroup (SubGroup _ x _) = x
-- nodeGroup (SubSubGroup _ x _ _) = x
-- nodeGroup (CAccount x) = aType x

-- nodeSubGroup :: ChartNode -> T.Text
-- nodeSubGroup Root = error "nodeSubGroup called on Root"
-- nodeSubGroup (Group _ _) = error "nodeSubGroup called on Group"
-- nodeSubGroup (SubGroup _ _ x) = x
-- nodeSubGroup (SubSubGroup _ _ x _) = x
-- nodeSubGroup (CAccount x) = aSubGroup x

-- nodeSubSubGroup :: ChartNode -> T.Text
-- nodeSubSubGroup Root = error "nodeSubSubGroup called on Root"
-- nodeSubSubGroup (Group _ _) = error "nodeSubSubGroup called on Group"
-- nodeSubSubGroup (SubGroup _ _ _) = error "nodeSubSubGroup called on SubGroup"
-- nodeSubSubGroup (SubSubGroup _ _ _ x) = x
-- nodeSubSubGroup (CAccount x) = aSubGroup x

-- nodeNumber :: ChartNode -> Int
-- nodeNumber Root = error "nodeNumber called on Root"
-- nodeNumber (Group x _) = x
-- nodeNumber (SubGroup x _ _) = x
-- nodeNumber (SubSubGroup x _ _ _) = x
-- nodeNumber (CAccount x) = aNumber x

-- nodeName :: ChartNode -> T.Text
-- nodeName Root = error "nodeName called on Root"
-- nodeName (Group _ a) = T.pack $ show a
-- nodeName (SubGroup _ _ x) = x
-- nodeName (SubSubGroup _ _ _ x) = x
-- nodeName (CAccount x) = aDisplayName x

-- -- data GroupF a = Group {
-- --   gGroup :: AccountType,
-- --   gChildren :: [Either Account a]
-- -- } deriving (Eq, Show, Functor)
-- -- type Group = GroupF SubGroup
-- --
-- -- data SubGroupF a = SubGroup {
-- --   sgName :: T.Text,
-- --   sgChildren :: NE.NonEmpty (Either Account a)
-- -- } deriving (Eq, Show, Functor)
-- -- type SubGroup = SubGroupF SubSubGroup
-- --
-- -- data SubSubGroupF a = SubSubGroup {
-- --   ssgName :: T.Text,
-- --   ssgChildren :: NE.NonEmpty a
-- -- } deriving (Eq, Show, Functor)
-- -- type SubSubGroup = SubSubGroupF Account

-- makeBaseFunctor ''Tree

-- -- The list of the main groups, always in the order of AccountType
-- accountsToChartOfAccounts :: [Account] -> ChartOfAccount
-- accountsToChartOfAccounts acc =
--          Node Root
--          $ map mkGroupTree
--          $ groupBy ((==) `on` aType)
--          $ sortOn aType acc

-- -- All accounts must have the same AccountType
-- mkGroupTree :: [Account] -> Tree ChartNode
-- mkGroupTree [] = error "mkGroupTree received the empty list"
-- mkGroupTree xs =
--   let accGroup = aType $ head xs
--       (acc, subgroups) = partition (\a -> aGroup a == "") xs

--       sg :: [Tree ChartNode]
--       sg = map (mkSubGroupTree accGroup)
--          $ groupBy ((==) `on` aGroup)
--          $ sortOn aGroup subgroups

--       ac :: [Tree ChartNode]
--       ac = map (\a -> Node (CAccount a) []) acc

--       n :: Int
--       n = minimum $ map (nodeNumber . rootLabel) (sg ++ ac)

--       children :: [Tree ChartNode]
--       children = sortOn (nodeNumber . rootLabel)
--                $ sg ++ ac
--   in Node (Group n accGroup) children

-- -- All accounts must have the same SubGroup
-- mkSubGroupTree :: AccountType -> [Account] -> Tree ChartNode
-- mkSubGroupTree _ [] = error "mkSubGroupTree received the empty list"
-- mkSubGroupTree accGroup xs =
--   let accSubGroup = aGroup $ head xs
--       (acc, subsubgroups) = partition (\a -> aSubGroup a == "") xs

--       sg :: [Tree ChartNode]
--       sg = map (mkSubSubGroupTree accGroup accSubGroup)
--          $ groupBy ((==) `on` aSubGroup)
--          $ sortOn aSubGroup subsubgroups

--       ac :: [Tree ChartNode]
--       ac = map (\a -> Node (CAccount a) []) acc

--       n :: Int
--       n = minimum $ map (nodeNumber . rootLabel) (sg ++ ac)

--       children :: [Tree ChartNode]
--       children = sortOn (nodeNumber . rootLabel)
--                $ sg ++ ac

--   in Node (SubGroup n accGroup accSubGroup) children

-- -- All accounts must have the same SubSubGroup
-- mkSubSubGroupTree :: AccountType -> T.Text -> [Account] -> Tree ChartNode
-- mkSubSubGroupTree _ _ [] = error "mkSubSubGroupTree received the empty list"
-- mkSubSubGroupTree accGroup accSubGroup xs =
--   let n = minimum $ map aNumber xs
--       accSubSubGroup = aSubGroup $ head xs

--       ac :: [Tree ChartNode]
--       ac = sortOn (nodeNumber . rootLabel)
--          $ map (\a -> Node (CAccount a) []) xs

--   in Node (SubSubGroup n accGroup accSubGroup accSubSubGroup) ac

-- -- Catamorphism for the chart of accounts
-- cataChart :: forall a b c d e.
--              (Account -> a) ->
--              (SubSubGroupF a -> b) ->
--              (SubGroupF b -> c) ->
--              (GroupF c -> d) ->
--              ([d] -> e) ->
--              ChartOfAccountF x y z ->
--              e
-- cataChart fAcc fssg fsg fg froot cs =
--   let cs :: [(x, GroupF (Either Account (y, SubGroupF (Either Account (z, SubSubGroup)))))]
--
--       cs1 :: [(x, GroupF ((y, SubGroupF ((z, SubSubGroupF a)))))]
--       cs1 = fmap (fmap (fmap (fmap (fmap (fmap (fmap fAcc)))))) cs
--
--       cs2 :: [(x, GroupF ((y, SubGroupF b)))]
--       cs2 = fmap (fmap (fmap (fssg))) cs1
--
--       cs3 :: [(x, GroupF c)]
--       cs3 = fmap (fmap fsg) cs2
--
--       cs4 :: [d]
--       cs4 = fmap fg cs3
--
--   in froot cs4
