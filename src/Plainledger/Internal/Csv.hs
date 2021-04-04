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
-- number of columns vary from object to object. Account and TransactionF objects
-- with their tags are examples of such objects, since each object can a
-- different number of tags.

module Plainledger.Internal.Csv
(
  columnData,
  columnDataM,
  optionalColumnData,
  optionalColumnDataM,
  processColumnIndexes,
  ColumnIndex,
  ColumnIndexes,
  columnIndex,
  optionalColumnIndex,
  parseInt,
  parseIntMaybe
) where

import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Plainledger.Error
import Control.Monad.Except

-- Column name and column number (zero based)
type ColumnIndex = (T.Text, Int)
type ColumnIndexes = M.HashMap T.Text Int


columnIndex :: (MonadError Errors m) =>
                     ColumnIndexes ->
                     T.Text ->
                     m ColumnIndex
columnIndex m key =
  let err = throwError
          $ mkError (SourcePos "" 1 0)
          $ MissingCsvColumn (T.unpack key)
  in fmap (key,) $ maybe err return $ M.lookup key m

optionalColumnIndex :: ColumnIndexes ->
                     T.Text ->
                     Maybe ColumnIndex
optionalColumnIndex m key = fmap (key,) $ M.lookup key m

-- Return a map where the zero based index represents the position
-- in the V.Vector T.Text line
processColumnIndexes :: forall m . (MonadError Errors m)
                     => V.Vector (V.Vector T.Text)
                     -> (T.Text -> Bool)
                     -> m (V.Vector (V.Vector T.Text), ColumnIndexes)
processColumnIndexes csv _ | V.null csv = throwError $ mkErrorNoPos EmptyCsvFile
processColumnIndexes csv keepF  =
  let header = filter (keepF . fst) $ flip zip [0..] $ V.toList $ V.head csv
      csvData = V.tail csv
      dup = filter (not . null . tail)
          $ groupBy (==)
          $ sortBy compare
          $ map fst header
      indexes = M.fromList header
      mkErrorDup x = mkError (SourcePos "" 1 0) (DuplicateCsvColumn $ T.unpack $ head x)
  in case dup of
        [] -> return (csvData, indexes)
        xs -> throwError $ concatMap mkErrorDup xs

  -- where columnData1 :: Bool -> V.Vector T.Text -> T.Text -> m Int
  --       columnData1 isOptional header column =
  --         let is = V.findIndices ((==) column) header
  --         in case V.length is of
  --              0 -> if isOptional
  --                   then return (-1)
  --                   else throwError
  --                         $ mkError (SourcePos "" 1 0) 
  --                         $ MissingCsvColumn (T.unpack column)

columnData :: (MonadError Errors m) =>
              ColumnIndex -> V.Vector T.Text -> m T.Text
columnData i v = columnDataM i v return

columnDataM :: (MonadError Errors m) =>
               ColumnIndex -> V.Vector T.Text -> (T.Text -> m a) -> m a
columnDataM i v f =
  let err = throwError
            $ mkErrorNoPos
            $ MissingCsvColumnData (T.unpack $ fst i)
  in columnDataBase (Just i) v f err

-- If the column is missing or if its value is null, then return the
-- default value.
optionalColumnData :: (MonadError Errors m) =>
                    T.Text -> Maybe ColumnIndex -> V.Vector T.Text -> m T.Text
optionalColumnData d i v = optionalColumnDataM d i v return

optionalColumnDataM :: (MonadError Errors m) =>
                    a -> Maybe ColumnIndex -> V.Vector T.Text -> (T.Text -> m a) -> m a
optionalColumnDataM d i v f
  = columnDataBase i v (\t -> if T.null t then return d else f t) (return d)

columnDataBase :: (MonadError Errors m)
                => Maybe ColumnIndex -> V.Vector T.Text -> (T.Text -> m a) -> m a -> m a
columnDataBase Nothing _ _ notFound = notFound
columnDataBase (Just (_,i)) v found notFound =
  case v V.!? i of
    Nothing -> notFound
    Just x -> found x `catchError` (throwError . setSourcePosColIfNull (i + 1))

parseInt ::  (MonadError Errors m) => T.Text -> m Int
parseInt x =
  case T.decimal x of
    Right (n, "") -> return n
    _ -> throwError $ mkErrorNoPos $ ParseIntErr (T.unpack x)

-- Returns nothing if the argument is the null text
parseIntMaybe ::  (MonadError Errors m) => T.Text -> m (Maybe Int)
parseIntMaybe x =
  if T.null x
  then return Nothing
  else case T.decimal x of
          Right (n, "") -> return $ Just n
          _ -> throwError $ mkErrorNoPos $ ParseIntErr (T.unpack x)