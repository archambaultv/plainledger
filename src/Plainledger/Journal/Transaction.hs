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
)
where

import Data.Char (ord)
import Data.Function
import Data.List hiding (lines)
import Data.Maybe
import Control.Monad.Except
import Data.ByteString.Lazy (ByteString)
import Data.Csv (Record, Field, ToField(..),toRecord)
import Data.Ord (comparing)
import Data.Scientific
import Data.Time
import Plainledger.Error
import Plainledger.Internal.Csv
import Plainledger.Journal.Posting
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Plainledger.Internal.Utils
import GHC.Generics hiding (to, from)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (lines)
import qualified Data.Csv as C
import qualified Data.Set as S
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
  } deriving (Eq, Show)

type JTransaction = TransactionF JPosting
type Transaction = TransactionF Posting

-- postingToLine :: JPosting -> [Field]
-- postingToLine p = [toField $ pAccount p,
--                    toField $ (realToFrac <$> pAmount p :: Maybe Scientific),
--                    toField $ toISO8601 <$> pBalanceDate p]

-- postingsHeader :: Int -> Int -> [Field]
-- postingsHeader n maxN | n > maxN = []
-- postingsHeader n maxN =
--     [toField $ "account id (" ++ show n ++ ")",
--      toField $ "amount (" ++ show n ++ ")",
--      toField $ "balance date ("++ show n ++ ")"]
--     ++ postingsHeader (n + 1) maxN

-- multipleRecordsHeader :: [Field]
-- multipleRecordsHeader = ["date",
--                          "transaction id",
--                          "account id",
--                          "amount",
--                          "balance date"]

-- -- / Encode a list of transaction as a Csv. The first line is the header
-- encodeTransactions :: CsvRecordOptions -> [JTransaction] -> ByteString
-- encodeTransactions SingleRecord xs =
--   let tagH = tagHeader $ concatMap tTags xs
--       maxPostings = maximum $ 2 : map (length . tPostings) xs
--       header = toRecord
--              $ ["date", "transaction id"]
--              ++ postingsHeader 1 maxPostings
--              ++ tagH
--       lines = header : map (toLine maxPostings tagH) xs
--   in C.encode lines

--   where toLine :: Int -> [Field] -> JTransaction -> Record
--         toLine maxPostings tagH t =
--           let psLine = concatMap postingToLine (tPostings t)
--               txLine = [toField $ toISO8601 $ tDate t,
--                         toField $ tTransactionId t]
--                        ++ psLine
--                        -- To take into accounts the extra space in the
--                        -- header due to other trasactions having more postings
--                        ++ take (4 * (maxPostings - length (tPostings t)))
--                                (repeat "")

--           in toRecord $ txLine  ++ tagLine (tTags t) tagH

-- encodeTransactions MultipleRecords xs =
--   let tagH = tagHeader $ concatMap tTags xs
--       header = toRecord $ multipleRecordsHeader  ++ tagH
--       lines = header : concatMap (toMultiLine tagH) xs
--   in C.encode lines

--   where
--         toMultiLine :: [Field] -> JTransaction -> [Record]
--         toMultiLine tagH t =
--                 let front = [toField $ toISO8601 $ tDate t,
--                              toField $ tTransactionId t]
--                     ps = map postingToLine (tPostings t)
--                     tags = tagLine (tTags t) tagH
--                     txLines = map (\x -> front ++ x ++ tags) ps
--                 in map toRecord txLines

-- data CsvRecordOptions = SingleRecord
--                       | MultipleRecords
--     deriving (Eq, Show)

-- -- | Test if the file has a SingleRecord header or a MultipleRecords header
-- decodeHeader :: (MonadError Error m) =>
--                 ByteString ->
--                 m CsvRecordOptions
-- decodeHeader bs = do
--   csv <- either throwError return $ C.decode C.NoHeader bs
--   if V.null (csv :: C.Csv)
--   then throwError "Empty file"
--   else
--     let header = csv V.! 0
--     in if "account id" `V.elem` header
--        then return MultipleRecords
--        else if "account id (1)" `V.elem` header
--             then return SingleRecord
--             else throwError "Expecting \"account id\" or \"account id (1)\" \
--                             \in the CSV header"


-- decodeJTransactionsFile :: String -> ExceptT Error IO [JTransaction]
-- decodeJTransactionsFile f = do
--         csvBS <- liftIO $ BL.readFile f
--         h <- decodeHeader csvBS
--         decodeTransactions h csvBS


decodeJTransactionsFile :: Char -> 
                           Char -> 
                           FilePath -> 
                           ExceptT Errors IO [(SourcePos, JTransaction)]
decodeJTransactionsFile csvSeparator decimalSeparator filePath = 
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap removeBom $ liftIO $ BS.readFile filePath
      accs <- decodeTransactions csvSeparator decimalSeparator (BL.fromStrict csvBS)
      let pos = map (\i -> SourcePos filePath i 0) [2..]
      return $ zip pos accs


-- | The first line is the header
decodeTransactions :: forall m . (MonadError Errors m) =>
                      Char ->
                      Char -> 
                      ByteString ->
                      m [JTransaction]
decodeTransactions csvSeparator decimalSeparator bs = do
  -- Read the CSV file as vector of T.Text
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }
  csv <- either (throwError . mkErrorNoPos . ErrorMessage) return 
      $ C.decodeWith opts C.NoHeader bs


  -- Decode the header to know the index of columns
  let myFilter t
        = t `elem` ["Date", "Commentaire", "Contrepartie", "Étiquette"]
        || T.isPrefixOf "Compte " t
        || T.isPrefixOf "Montant " t
        || T.isPrefixOf "Date du relevé " t
  (csvData, indexes) <- processColumnIndexes csv myFilter

  dateIdx <- columnIndex indexes "Date"
  commentIdx <- columnIndex indexes "Commentaire"
  counterPartyIdx <- columnIndex indexes "Contrepartie"
  tagIdx <- columnIndex indexes "Étiquette"

  -- Check for postings columns based on the account column
  let accSufix = filter (T.isPrefixOf "Compte " . fst)
               $ HM.toList indexes
  let postingIdx = map (postingIndexes 
                        ("Compte ", "Montant ", "Date du relevé ")
                        indexes)
                   accSufix

  -- Add row information to the CSV line
  let csvWithRowNumber = zip [2..] $ V.toList csvData
  
  -- Function to parse a line into a JTransaction                            
  let parseLine (row, line) =
          let p = do
                 date <- columnDataM dateIdx line (parseISO8601M . T.unpack)
                 comment <- columnData commentIdx line 
                 counterParty <- columnData counterPartyIdx line 
                 tag <- columnData tagIdx line

                 ps <- fmap catMaybes $ mapM (postingColumns date line) postingIdx
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
                          (ColumnIndex, Maybe ColumnIndex, Maybe ColumnIndex) -> 
                          m (Maybe JPosting)
        postingColumns d l (aIdx, amIdx, bIdx) = do
          acc <- optionalColumnData "" (Just aIdx) l
          if T.null acc
           then return Nothing
           else do
               amount <- optionalColumnDataM Nothing amIdx l 
                        (fmap Just . parseAmount decimalSeparator)
               balanceDate <- optionalColumnDataM d bIdx l (parseISO8601M . T.unpack)
               return $ Just $ Posting balanceDate acc amount

 --  where fromLine :: (MonadError Error m) =>
 --                    HS.HashSet Field ->
 --                    HM.HashMap Field Field -> m JTransaction
 --        fromLine notTags m = do
 --          date <- columnDataM "date" m parseISO8601M
 --          tId <- optionalColumnData "" "transaction id" m
 --          ps <- lineToPostings m 1
 --          tags <- recordToTags m  notTags
 --          return $ Transaction date tId ps tags

 --        postingHeader :: HS.HashSet Field -> Int -> [Field]
 --        postingHeader m n =
 --          let accountKey =  toField $ "account id (" ++ show n ++ ")"
 --              amountKey =  toField $ "amount (" ++ show n ++ ")"
 --              balanceDateKey =  toField $ "balance date ("++ show n ++ ")"
 --          in case HS.member accountKey m of
 --              False -> []
 --              True ->
 --                let xs = postingHeader m (n + 1)
 --                in accountKey
 --                   : amountKey
 --                   : balanceDateKey
 --                   : xs

 --        lineToPostings :: (MonadError Error n) =>
 --                          HM.HashMap Field Field -> Int -> n [JPosting]
 --        lineToPostings m n =
 --          let accountKey =  toField $ "account id (" ++ show n ++ ")"
 --              amountKey =  toField $ "amount (" ++ show n ++ ")"
 --              balanceDateKey =  toField $ "balance date ("++ show n ++ ")"
 --          in
 --            case columnData accountKey m of
 --              Left _ -> return []
 --              Right acc | acc == "" -> return []
 --              Right acc -> do
 --                ps <- lineToPostings m (n + 1)
 --                amnt <- columnDataM amountKey m
 --                        (\case  { Nothing -> return Nothing;
 --                                  Just v -> return
 --                                            $ Just
 --                                            $ (realToFrac :: Scientific -> Quantity)
 --                                             v})
 --                bal <- optionalColumnDataM Nothing balanceDateKey m
 --                        (\case  {"" -> return Nothing;
 --                                 v -> Just <$> parseISO8601M v})

 --                let p = Posting bal acc amnt
 --                return $ p : ps

