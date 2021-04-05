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
  parseIntMaybe,
  readCsvFile
) where

import Data.List ( sort, group )
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Plainledger.Error
import Control.Monad.Except
import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as C
import Data.Char (ord)
import Data.Word (Word8)

-- Column name and column number (zero based)
type ColumnIndex = (T.Text, Int)
type ColumnIndexes = M.HashMap T.Text Int

-- Reads the bytestring CSV file, skipping
-- white row and only delimiter row and
-- returns the line number
-- Assumes UTF8 encoding
readCsvFile :: (MonadError Errors m) =>
               Char -> 
               B.ByteString ->
               m [(Int, V.Vector T.Text)]
readCsvFile csvSeparator bs =
  -- Read the CSV file as vector of T.Text
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }

      nl:: Word8
      nl = 10

      cr :: Word8
      cr = 13

      -- Returns only the non empty lines with their number
      csvLines :: Int -> B.ByteString -> [(Int, B.ByteString)]
      csvLines _ ps | B.null ps = []
      csvLines i ps = 
        case B.elemIndex nl ps of
           Nothing -> [(i, ps)]
           Just 0 -> csvLines (i+1) (dropCarReturn $ B.tail ps)
           Just n  -> 
             let x = B.take n ps
                 xs = dropCarReturn $ B.drop (n+1) ps
             in (i, x) : csvLines (i+1) xs

      dropCarReturn :: B.ByteString -> B.ByteString
      dropCarReturn xs | B.null xs = xs
      dropCarReturn xs | B.head xs == cr = B.drop 1 xs
      dropCarReturn xs = xs

      parseCsv :: (MonadError Errors m) => 
                  B.ByteString -> 
                  m (V.Vector T.Text)
      parseCsv xs = either (throwError . mkErrorNoPos . ErrorMessage) 
                  (return . checkSingleLine)
                  $ C.decodeWith opts C.NoHeader xs

      checkSingleLine :: V.Vector (V.Vector T.Text) -> V.Vector T.Text
      checkSingleLine v | V.null v = error "Plainledger internal error, vector is null"
      checkSingleLine v | V.length v == 1 = v V.! 0
      checkSingleLine v = error $ "Plainledger internal error, vector has more than 1 element" ++ show v

      isEmptyLine :: V.Vector T.Text -> Bool
      isEmptyLine = V.all T.null

  in filter (not . isEmptyLine . snd)
     <$> traverse (\(i,x) -> (i,) <$> parseCsv x) 
         (csvLines 1 bs)

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
optionalColumnIndex m key = (key,) <$> M.lookup key m

-- Return a map where the zero based index represents the position
-- in the V.Vector T.Text line
processColumnIndexes :: forall m . (MonadError Errors m)
                     => [(Int, V.Vector T.Text)]
                     -> (T.Text -> Bool)
                     -> m ([(Int, V.Vector T.Text)], ColumnIndexes)
processColumnIndexes [] _ = throwError $ mkErrorNoPos EmptyCsvFile
processColumnIndexes csv keepF  =
  let header = filter (keepF . fst) $ flip zip [0..] $ V.toList $ snd $ head csv
      csvData = tail csv
      dup = filter (not . null . tail)
          $ group
          $ sort
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