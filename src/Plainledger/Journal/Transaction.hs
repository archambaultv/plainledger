-- |
-- Module      :  Plainledger.Journal.Transaction
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the JTransaction data type.

module Plainledger.Journal.Transaction 
(
  TransactionF(..),
  JTransaction,
  Transaction,
  decodeJTransactionsFile,
  decodeTransactions,
  validateJTransactions,
)
where

import Data.Char (ord)
import Data.Function
import Data.List hiding (lines)
import Data.Maybe
import Control.Monad.Except
import Data.ByteString.Lazy (ByteString)
import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Journal.Posting
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Plainledger.Internal.Utils
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

data TransactionF p = Transaction
  {
    tDate :: Day,
    tComment :: T.Text,
    tCounterParty :: T.Text,
    tTag :: T.Text,
    tPostings :: [p]
  } deriving (Eq, Show, Functor)

type JTransaction = TransactionF JPosting
type Transaction = TransactionF Posting

decodeJTransactionsFile :: Language ->
                           Char -> 
                           Char -> 
                           FilePath -> 
                           ExceptT Errors IO [(SourcePos, JTransaction)]
decodeJTransactionsFile lang csvSeparator decimalSeparator filePath = 
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap (snd . removeBom) $ liftIO $ BS.readFile filePath
      accs <- decodeTransactions lang csvSeparator decimalSeparator (BL.fromStrict csvBS)
      let pos = map (\i -> SourcePos filePath i 0) [2..]
      return $ zip pos accs


-- | The first line is the header
decodeTransactions :: forall m . (MonadError Errors m) =>
                      Language ->
                      Char ->
                      Char -> 
                      ByteString ->
                      m [JTransaction]
decodeTransactions lang csvSeparator decimalSeparator bs = do
  -- Read the CSV file as vector of T.Text
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }
  csv <- either (throwError . mkErrorNoPos . ErrorMessage) return 
      $ C.decodeWith opts C.NoHeader bs


  let accPrefix = (T.append (i18nText lang TTransactionAccountPrefix) " ")
  let amntPrefix = (T.append (i18nText lang TTransactionAmountPrefix) " ")
  let datePrefix = (T.append (i18nText lang TTransactionBalanceDatePrefix) " ")

  -- Decode the header to know the index of columns
  let myFilter t
        = t `elem` [i18nText lang TTransactionDate, 
                    i18nText lang TTransactionComment, 
                    i18nText lang TTransactionCounterparty, 
                    i18nText lang TTransactionTag]
        || T.isPrefixOf accPrefix t
        || T.isPrefixOf amntPrefix t
        || T.isPrefixOf datePrefix t
  (csvData, indexes) <- processColumnIndexes csv myFilter

  dateIdx <- columnIndex indexes (i18nText lang TTransactionDate)
  let commentIdx = optionalColumnIndex indexes (i18nText lang TTransactionComment)
  let counterPartyIdx = optionalColumnIndex indexes (i18nText lang TTransactionCounterparty)
  let tagIdx = optionalColumnIndex indexes (i18nText lang TTransactionTag)

  -- Check for postings columns based on the account column
  -- Sorts them by appearance in the header
  let accSufix = filter (T.isPrefixOf accPrefix . fst)
               $ sortOn snd
               $ HM.toList indexes
  let postingIdx = map (postingIndexes 
                        (accPrefix, amntPrefix, datePrefix)
                        indexes)
                   accSufix

  -- Add row information to the CSV line
  let csvWithRowNumber = zip [2..] $ V.toList csvData
  
  -- Function to parse a line into a JTransaction                            
  let parseLine (row, line) =
          let p = do
                 date <- columnDataM dateIdx line (parseISO8601M . T.unpack)
                 comment <- optionalColumnData "" commentIdx line 
                 counterParty <- optionalColumnData "" counterPartyIdx line 
                 tag <- optionalColumnData "" tagIdx line

                 ps <- fmap catMaybes 
                       $ mapM (postingColumns date line) 
                       $ zip [1..] postingIdx
                 if null ps || null (tail ps)
                  then throwError $ mkErrorNoPos $ ZeroOrOnePostingOnly
                  else 
                   return $ Transaction date comment counterParty tag ps

          in p `catchError` (throwError . setSourcePosRowIfNull row)

  mapM parseLine csvWithRowNumber

  where postingIndexes :: (T.Text, T.Text, T.Text) -> 
                          ColumnIndexes ->
                          ColumnIndex -> 
                          (ColumnIndex, Maybe ColumnIndex, Maybe ColumnIndex)
        postingIndexes (accPrefix, amntPrefix, balPrefix) m acc = 
          let keySuf = T.drop (T.length accPrefix) (fst acc)
              amnt = T.append amntPrefix keySuf
              bl = T.append balPrefix keySuf
          in (acc, optionalColumnIndex m amnt, optionalColumnIndex m bl)

        postingColumns :: Day -> 
                          V.Vector T.Text -> 
                          (Int, (ColumnIndex, Maybe ColumnIndex, Maybe ColumnIndex)) -> 
                          m (Maybe JPosting)
        postingColumns d l (i, (aIdx, amIdx, bIdx)) = do
          acc <- optionalColumnData "" (Just aIdx) l
          if T.null acc
           then return Nothing
           else do
               amount <- optionalColumnDataM Nothing amIdx l 
                        (fmap Just . parseAmount decimalSeparator)
               balanceDate <- optionalColumnDataM d bIdx l (parseISO8601M . T.unpack)
               return $ Just $ Posting i balanceDate acc amount

--  Asserts all transactions have valid postings
--  Asserts all transactions balance to zero
validateJTransactions :: (MonadError Errors m) =>
                      HS.HashSet T.Text ->
                      [(SourcePos, JTransaction)] ->
                      m [Transaction]
validateJTransactions accs jtransactions = do
    transactions <- traverse balanceTransaction jtransactions
    _ <- traverse (checkTxnAccount accs) transactions
    return $ map snd transactions

-- Balance postings
balanceTransaction :: (MonadError Errors m) =>
                             (SourcePos , JTransaction) -> 
                             m (SourcePos, Transaction)
balanceTransaction (pos, t) = do
    ps2 <- (balancePostings $ tPostings $ t)
           `catchError` (throwError . setSourcePosIfNull pos)
    return $ (pos, t{tPostings = ps2})

checkTxnAccount :: (MonadError Errors m) =>
                    HS.HashSet T.Text -> (SourcePos, Transaction) -> m ()
checkTxnAccount s (pos, t) = traverse foo (tPostings t) >> return ()
  where foo p = if HS.member (pAccount p) s
                then return ()
                else throwError 
                      $ mkError pos
                      $ AccountIdNotInAccountFile
                      $ T.unpack 
                      $ pAccount p