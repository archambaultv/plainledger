-- |
-- Module      :  Plainledger.Internal.Csv
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
--
-- This module defines functions to encode and decode objects to CSV when the
-- number of columns vary from object to object. Account and Transfer objects
-- with their tags are examples of such objects, since each object can a
-- different number of tags.

module Plainledger.Internal.Csv
(
  tagHeader,
  tagLine,
  recordToTags,
  recordToHashMap,
  csvToData,
  findColumn,
  findColumnDefault
) where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Plainledger.Ledger.Tag
import Plainledger.Error
import Data.Csv
import Control.Monad.Except

-- / Returns the header associated with the tags. The header is sorted
-- alphabetically
tagHeader :: [Tag] -> [Field]
tagHeader t = map toField
            $ sort
            $ HS.toList
            $ HS.fromList
            $ map tagId t

-- / 'tagLine ts header' builds the record corresponding to the header from the
-- provided list of tags. For each field in the header, it looks if a tag has
-- the same field as id and use its value. If the tag is missing, a blanck field
-- is inserted. Extra tags are ignored.
tagLine :: [Tag] -> [Field] -> [Field]
tagLine ts tagHeader' =
  let m :: HM.HashMap Field Field
      m = HM.fromList
        $ map tagToField ts
  in map (\k -> fromMaybe "" $ HM.lookup k m) tagHeader'

        -- Tag without value are given the tagId as value in the CSV file
  where tagToField (Tag k "") = (toField k, toField k)
        tagToField (Tag k v) = (toField k, toField v)

-- / Returns the list of Tag made from all the fields that are not in the
-- HashSet.
recordToTags :: (MonadError Error m) =>
                HashMap Field Field -> HashSet Field -> m [Tag]
recordToTags m s = either throwError return
                 $ runParser
                 $ traverse tupleToTagM
                 $ HM.toList
                 $ HM.filterWithKey
                   (\k v -> not (HS.member k s || BS.null v))
                   m
  where tupleToTagM (k, v) = do
            k' <- parseField k
            v' <- parseField v
            -- Tag without value in the YAML file are encoded with the value
            -- being tagId in the CSV file
            if v' == k'
              then return $ Tag k' ""
              else return $ Tag k' v'

-- / 'recordToHashMap header record' translate a record to hashMap using the
-- provided header
recordToHashMap :: Record -> Record -> HashMap Field Field
recordToHashMap h l = HM.fromList $ V.toList $ V.zip h l

-- / Takes a CSV where the first line is the header, a function that can convert
-- a line from a HashMap to an object and returns the list of objects
csvToData :: (MonadError Error m) =>
             Csv ->
             (HashMap Field Field -> m a) ->
             m [a]
csvToData csv foo
  | V.null csv = return []
  | otherwise =
    let headerRecord = csv V.! 0
    in V.toList <$> traverse (foo . recordToHashMap headerRecord) (V.tail csv)

-- / Find the value in the HashMap or prints an friendly error message
findColumn :: (MonadError Error m, FromField a) =>
              Field -> HashMap Field Field -> m a
findColumn x m =
  case HM.lookup x m of
    Nothing -> throwError
               $ "\""
               ++ (show x)
               ++ " is not in the CSV header."
    Just v -> either throwError return $ runParser $ parseField v

findColumnDefault :: (MonadError Error m, FromField a) =>
                     a -> Field -> HashMap Field Field -> m a
findColumnDefault v x m =
  case HM.lookup x m of
    Nothing -> return v
    Just v2 -> either throwError return $ runParser $ parseField v2
