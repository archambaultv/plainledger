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
  -- recordToHashMap,
  -- csvToData,
  -- findColumn,
  -- findColumnM,
  -- findColumnDefault,
  -- findColumnDefaultM
) where

-- import Data.List (sort)
-- import Data.Maybe (fromMaybe)
-- import qualified Data.HashMap.Strict as HM
-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashSet as HS
-- import Data.HashSet (HashSet)
-- import qualified Data.Vector as V
-- import qualified Data.ByteString as BS
-- import Plainledger.Error
-- import Data.Csv
-- import Control.Monad.Except


-- -- / 'recordToHashMap header record' translate a record to hashMap using the
-- -- provided header
-- recordToHashMap :: Record -> Record -> HashMap Field Field
-- recordToHashMap h l = HM.fromList $ V.toList $ V.zip h l

-- -- / Takes a CSV where the first line is the header, a function that can convert
-- -- a line from a HashMap to an object and returns the list of objects
-- csvToData :: (MonadError Error m) =>
--              Csv ->
--              (HashMap Field Field -> m a) ->
--              m [a]
-- csvToData csv foo
--   | V.null csv = return []
--   | otherwise =
--     let headerRecord = csv V.! 0
--     in V.toList <$> traverse (foo . recordToHashMap headerRecord) (V.tail csv)

-- -- / Find the value in the HashMap or prints an friendly error message
-- findColumn :: (MonadError Error m, FromField a) =>
--               Field -> HashMap Field Field -> m a
-- findColumn x m = findColumnM x m return

-- findColumnM :: (MonadError Error m, FromField b) =>
--                Field -> HashMap Field Field -> (b -> m a) -> m a
-- findColumnM x m f =
--   case HM.lookup x m of
--     Nothing -> throwError
--                $ "Field "
--                ++ (show x)
--                ++ " is not in the CSV header."
--     Just v -> do
--       b <- either throwError return $ runParser $ parseField v
--       f b

-- findColumnDefault :: (MonadError Error m, FromField a) =>
--                      a -> Field -> HashMap Field Field -> m a
-- findColumnDefault v x m = findColumnDefaultM v x m return

-- findColumnDefaultM :: (MonadError Error m, FromField b) =>
--                      a -> Field -> HashMap Field Field -> (b -> m a) -> m a
-- findColumnDefaultM v x m f =
--   case HM.lookup x m of
--     Nothing -> return v
--     Just v2 -> do
--       b <- either throwError return $ runParser $ parseField v2
--       f b
