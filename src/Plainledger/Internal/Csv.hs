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
  findColumn,
  findColumnM,
  findColumnDefault,
  findColumnDefaultM,
  processColumnIndexes,
  parseInt
) where

import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Plainledger.Error
import Control.Monad.Except


processColumnIndexes :: forall m . (MonadError Errors m) 
                     => V.Vector (V.Vector T.Text)
                     -> [T.Text]
                     -> m (V.Vector (V.Vector T.Text), V.Vector Int)
processColumnIndexes csv _ | V.null csv = throwError $ mkErrorNoPos EmptyCsvFile
processColumnIndexes csv columns = 
  let header = V.head csv
      csvData = V.tail csv
      indexes = fmap (V.fromList . reverse) 
              $ foldM (findColumn1 header) [] columns
  in indexes >>= (return . (csvData,))

  where findColumn1 :: V.Vector T.Text -> [Int] -> T.Text -> m [Int]
        findColumn1 header x column =
          let is = V.findIndices ((==) column) header
          in case V.length is of
               0 -> throwError
                  $ mkError (SourcePos "" 1 0) 
                  $ MissingCsvColumn (T.unpack column)
               1 -> return $ V.head is : x
               _ -> throwError
                  $ mkError (SourcePos "" 1 0)
                  $ DuplicateCsvColumn (T.unpack column)
                         
findColumn :: (MonadError Errors m) =>
              Int -> V.Vector T.Text -> String -> m T.Text
findColumn i v c = findColumnM i v return c

findColumnM :: (MonadError Errors m) =>
               Int -> V.Vector T.Text -> (T.Text -> m a) -> String -> m a
findColumnM i v f c =
  let err = throwError
            $ mkErrorNoPos 
            $ MissingCsvColumnData c
  in findColumnBase i v f err

findColumnDefault :: (MonadError Errors m) =>
                    T.Text -> Int -> V.Vector T.Text -> m T.Text
findColumnDefault d i v = findColumnDefaultM d i v return

findColumnDefaultM :: (MonadError Errors m) =>
                    a -> Int -> V.Vector T.Text -> (T.Text -> m a) -> m a
findColumnDefaultM d i v f = findColumnBase i v f (return d)

findColumnBase :: (MonadError Errors m) 
                => Int -> V.Vector T.Text -> (T.Text -> m a) -> m a -> m a
findColumnBase i v found notFound =
  case v V.!? i of
    Nothing -> notFound
    Just x -> found x `catchError` (throwError . setSourcePosColIfNull i)

parseInt ::  (MonadError Errors m) => T.Text -> m Int
parseInt x =
  case T.decimal x of
    Right (n, "") -> return n
    _ -> throwError $ mkErrorNoPos $ ParseIntErr (T.unpack x)