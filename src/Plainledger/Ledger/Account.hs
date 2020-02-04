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
  )
where

import Data.Char (toLower)
import qualified Data.Csv as C
import Data.Csv (FromNamedRecord(..),
                 ToNamedRecord(..),
                 ToField(..),
                 record,
                 namedRecord,
                 DefaultOrdered)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Text as T
import Data.Aeson as A
import Plainledger.Ledger.Tag
import qualified Data.HashMap.Strict as HM
import GHC.Generics

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
  deriving (Eq, Show, Generic)

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

-- CSV instances
instance FromNamedRecord Account where
  parseNamedRecord m =
        Account
        <$> m C..: "id"
        <*> m C..: "name"
        <*> m C..: "number"
        <*> m C..: "group"
        <*> m C..: "subgroup"
        <*> m C..: "subsubgroup"
        <*> (traverse (\(k,v) -> do
                    kText <- C.parseField k
                    vText <- C.parseField v
                    return $ Tag kText vText)
            $ HM.toList
            $ HM.filterWithKey
              (\k _ -> not $ k `elem` coreHeader)
              m)

instance ToNamedRecord Account where
  toNamedRecord t =
    namedRecord
    $ ["id" C..= aId t,
       "name" C..= aName t,
       "number" C..= aNumber t,
       "group" C..= aGroup t,
       "subgroup" C..= aSubgroup t,
       "subsubgroup" C..= aSubsubgroup t]
    ++ map (\(Tag k v) -> (toField k) C..= v)
           (aTags t)

instance DefaultOrdered Account where
  headerOrder t = record
                $ coreHeader
                ++ map (\(Tag k _) -> toField k) (aTags t)

coreHeader :: [C.Field]
coreHeader = ["id", "name", "number", "group", "subgroup", "subsubgroup"]
