-- |
-- Module      :  Plainledger.Ledger.Tag
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Tags that can strore user information not needed by
-- plainledger

module Plainledger.Ledger.Tag (
  Tag(..),
  tagToHashMap,
  tagToTuple
  )
where

import Data.Char
import qualified Data.Csv as C
import Data.Csv (FromRecord(..),
                 FromNamedRecord(..),
                 ToRecord(..),
                 ToNamedRecord(..),
                 DefaultOrdered)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Text as T
import qualified Data.Aeson as A
import Control.Monad.Fail as Fail
import GHC.Generics
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

-- | Tags
data Tag = Tag
  {tagId :: T.Text,
   tagValue :: T.Text -- | Empty text indicates no value
  }
  deriving (Eq, Show, Generic)

-- JSON instances
instance ToJSON Tag where
  toJSON (Tag key value) =
    Y.object
    $ ["id" .= key]
    ++ (if T.null value then [] else ["value" .= value])

  toEncoding (Tag key value) =
    A.pairs
    $ "id" .= key
    <> (if T.null value then mempty else "value" .= value)

instance FromJSON Tag where
  parseJSON (Y.Object v) =
    Tag
    <$> v .: "id"
    <*> ((v .:? "value") >>= maybe (return "") valueToText)

valueToText :: (MonadFail m) => Y.Value -> m T.Text
valueToText (Y.String t) = return t
valueToText (Y.Number n) = return $ T.pack $ show n
valueToText Y.Null = return ""
valueToText (Y.Bool b) = return $ T.pack $ show b
valueToText _ = Fail.fail "Expected String, Number, Null or Bool for a tag value field"

-- Csv instances.
csvTransferOptions :: C.Options
csvTransferOptions = C.defaultOptions {
 C.fieldLabelModifier = map toLower . drop 3 }
instance FromRecord Tag
instance ToRecord Tag
instance FromNamedRecord Tag where
 parseNamedRecord = C.genericParseNamedRecord csvTransferOptions
instance ToNamedRecord Tag where
 toNamedRecord = C.genericToNamedRecord csvTransferOptions
instance DefaultOrdered Tag where
 headerOrder = C.genericHeaderOrder csvTransferOptions

tagToTuple :: Tag -> (T.Text, T.Text)
tagToTuple (Tag k v) = (k, v)

tagToHashMap :: [Tag] -> HashMap T.Text T.Text
tagToHashMap = HM.fromList . map tagToTuple