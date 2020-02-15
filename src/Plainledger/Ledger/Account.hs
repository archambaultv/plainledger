-- |
-- Module      :  Plainledger.Ledger.Account
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the account data type.

module Plainledger.Ledger.Account (
  Account(..),
  encodeAccounts,
  decodeAccounts,
  validateAccounts,
  accountsToHashMap,
  decodeAccountsFile
  )
where

import Control.Monad.Except
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)
import Data.Csv (Record, Field, ToField(..),toRecord)
import Data.HashMap.Strict (HashMap)
import Data.List hiding (group, lines)
import Data.Maybe
import Data.Ord (comparing)
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import GHC.Generics
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Internal.Utils
import Plainledger.Ledger.Configuration
import Plainledger.Ledger.Tag
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y

-- | The Account data type serves as aggregation point for commodities
-- relating to a particuliar purpose.
data Account = Account
  {aId :: T.Text,
   aName :: T.Text,
   aNumber   :: Int,
   aGroup :: T.Text,
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

-- JSON instances
instance ToJSON Account where
  toJSON (Account aId name number group subgroup subsubgroup tags) =
    Y.object
    $ ["id" .= aId,
      "number" .= number,
      "group" .= group]

   ++ (if T.null name then [] else ["name" .= name])
   ++ (if T.null subgroup then [] else ["subgroup" .= subgroup])
   ++ (if T.null subsubgroup then [] else ["subsubgroup" .= subsubgroup])
   ++ (if null tags then [] else ["tags" .= tags])

  toEncoding (Account aId name number group subgroup subsubgroup tags) =
    pairs
    $ "id" .= aId
    <> (if T.null name then mempty else "name" .= name)
    <> "number" .= number
    <> "group" .= group
    <> (if T.null subgroup then mempty else "subgroup" .= subgroup)
    <> (if T.null subsubgroup then mempty else "subsubgroup" .= subsubgroup)
    <> (if null tags then mempty else "tags" .= tags)

instance FromJSON Account where
  parseJSON (Y.Object v) =
    Account
    <$> v .: "id"
    <*> (v .:? "name" >>= maybe (v .: "id") return)
    <*> v .: "number"
    <*> v .: "group"
    <*> (maybe "" id <$> (v .:? "subgroup"))
    <*> (maybe "" id <$> (v .:? "subsubgroup"))
    <*> (maybe [] id <$> (v .:? "tags"))
  parseJSON _ = fail "Expected Object for Account value"

accountsToHashMap :: [Account] -> HashMap T.Text Account
accountsToHashMap = HM.fromList . map (\a -> (aId a, a))

validateAccounts :: (MonadError Error m) =>
                      HashMap T.Text AccountGroup ->
                      [Account] ->
                      m ()
validateAccounts m accounts = do
  validateGroupField m accounts
  validateAccountIdNonNull accounts
  validateAccountIdNoDup accounts
  return ()

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

-- | Asserts all accounts group field are in the configuration group mapping.
validateGroupField :: (MonadError Error m) =>
                      HashMap T.Text AccountGroup ->
                      [Account] ->
                      m ()
validateGroupField m accounts =
  let wrong = filter (\a -> isNothing $ HM.lookup (aGroup a) m) accounts
  in case wrong of
      [] -> return ()
      (a:_) -> throwError
               $ "Group \""
               ++ (T.unpack $ aGroup a)
               ++ "\" of account \""
               ++ (T.unpack $ aId a)
               ++ "\" is not in the configuration group-mapping."

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
          name <- findColumn "name" m
          number <- findColumn "number" m
          group <- findColumn "group" m
          subgroup <- findColumnDefault "" "subgroup" m
          subsubgroup <- findColumnDefault "" "subsubgroup" m
          tags <- recordToTags m (HS.fromList coreHeader)
          return $ Account id' name number group subgroup subsubgroup tags

decodeAccountsFile :: String -> IO [Account]
decodeAccountsFile f = do
  fType <- either fail return $ isDecodableFile f
  case fType of
    YamlFile -> Y.decodeFileThrow f
    CsvFile -> do
        csvBS <- BL.readFile f
        either fail return $ decodeAccounts csvBS
