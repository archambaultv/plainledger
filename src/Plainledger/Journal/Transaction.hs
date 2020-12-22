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
  -- TransactionF(..),
  -- JTransaction,
  -- decodeJTransactionsFile,
  -- encodeTransactions,
  -- decodeTransactions,
  -- CsvRecordOptions(..)
)
where

-- import Data.Function
-- import Data.List hiding (lines)
-- import Data.Maybe
-- import Control.Monad.Except
-- import Data.ByteString.Lazy (ByteString)
-- import Data.Csv (Record, Field, ToField(..),toRecord)
-- import Data.Ord (comparing)
-- import Data.Scientific
-- import Data.Time
-- import Plainledger.Error
-- import Plainledger.Internal.Csv
-- import Plainledger.Journal.Posting
-- import Plainledger.Journal.Amount
-- import Plainledger.Journal.Day
-- import Plainledger.Internal.Utils
-- import GHC.Generics hiding (to, from)
-- import qualified Data.ByteString.Lazy as BL
-- import Prelude hiding (lines)
-- import qualified Data.Csv as C
-- import qualified Data.Set as S
-- import qualified Data.HashSet as HS
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.Text as T
-- import qualified Data.Vector as V

-- data TransactionF p = Transaction
--   {
--     tDate :: Day,
--     tTransactionId :: T.Text,
--     tPostings :: [p],
--     tTags :: [Tag]
--   } deriving (Show, Generic, Functor)

-- -- / We sort the tags when comparing two TransactionF
-- -- The Eq instance is mainly used in the unittests. In a validated ledger,
-- -- you can rely on the aId to identify an account.
-- instance (Eq p) => Eq (TransactionF p) where
--   a1 == a2 =  tDate a1 == tDate a2
--            && tTransactionId a1 == tTransactionId a2
--            && tPostings a1 == tPostings a2
--            && sortBy (comparing tagId) (tTags a1) ==
--               sortBy (comparing tagId) (tTags a2)
-- type JTransaction = TransactionF JPosting

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

-- -- | The first line is the header
-- decodeTransactions :: (MonadError Error m) =>
--                       CsvRecordOptions ->
--                       ByteString ->
--                       m [JTransaction]
-- decodeTransactions SingleRecord bs = do
--   csv <- either throwError return $ C.decode C.NoHeader bs
--   if V.null csv
--   then return []
--   else
--     let notTags = HS.fromList
--                   $ ["date", "transaction id"]
--                   ++ postingHeader (HS.fromList $ V.toList $ csv V.! 0) 1
--         headerDup :: String
--         headerDup = intercalate ", "
--                   $ findDuplicates
--                   $ map show
--                   $ V.toList
--                   $ csv V.! 0
--     in if null headerDup
--        then csvToData (csv :: C.Csv) (fromLine notTags)
--        else throwError $ "Two or more column with the same header (" ++
--                          headerDup ++ ")"


--   where fromLine :: (MonadError Error m) =>
--                     HS.HashSet Field ->
--                     HM.HashMap Field Field -> m JTransaction
--         fromLine notTags m = do
--           date <- findColumnM "date" m parseISO8601M
--           tId <- findColumnDefault "" "transaction id" m
--           ps <- lineToPostings m 1
--           tags <- recordToTags m  notTags
--           return $ Transaction date tId ps tags

--         postingHeader :: HS.HashSet Field -> Int -> [Field]
--         postingHeader m n =
--           let accountKey =  toField $ "account id (" ++ show n ++ ")"
--               amountKey =  toField $ "amount (" ++ show n ++ ")"
--               balanceDateKey =  toField $ "balance date ("++ show n ++ ")"
--           in case HS.member accountKey m of
--               False -> []
--               True ->
--                 let xs = postingHeader m (n + 1)
--                 in accountKey
--                    : amountKey
--                    : balanceDateKey
--                    : xs

--         lineToPostings :: (MonadError Error n) =>
--                           HM.HashMap Field Field -> Int -> n [JPosting]
--         lineToPostings m n =
--           let accountKey =  toField $ "account id (" ++ show n ++ ")"
--               amountKey =  toField $ "amount (" ++ show n ++ ")"
--               balanceDateKey =  toField $ "balance date ("++ show n ++ ")"
--           in
--             case findColumn accountKey m of
--               Left _ -> return []
--               Right acc | acc == "" -> return []
--               Right acc -> do
--                 ps <- lineToPostings m (n + 1)
--                 amnt <- findColumnM amountKey m
--                         (\case  { Nothing -> return Nothing;
--                                   Just v -> return
--                                             $ Just
--                                             $ (realToFrac :: Scientific -> Quantity)
--                                              v})
--                 bal <- findColumnDefaultM Nothing balanceDateKey m
--                         (\case  {"" -> return Nothing;
--                                  v -> Just <$> parseISO8601M v})

--                 let p = Posting bal acc amnt
--                 return $ p : ps

-- decodeTransactions MultipleRecords bs = do
--   csv <- either throwError return $ C.decode C.NoHeader bs
--   let  headerDup = intercalate ", "
--                  $ findDuplicates
--                  $ map show
--                  $ V.toList
--                  $ csv V.! 0
--   if null headerDup
--     then return ()
--     else throwError $ "Two or more column with the same header (" ++
--                       headerDup ++ ")"
--   txns <- csvToData (csv :: C.Csv) fromLine
--   let txnsGroup = groupBy ((==) `on` tTransactionId)
--                 $ sortBy (comparing tTransactionId) txns
--   traverse regroupTransactions txnsGroup

--   where fromLine :: (MonadError Error m) =>
--                     HM.HashMap Field Field -> m JTransaction
--         fromLine m = do
--           date <- findColumnM "date" m parseISO8601M
--           tId <- findColumn "transaction id" m
--           when (T.null tId) (throwError
--                 $ "Unexpected null transaction id. \
--                   \All lines must have a transaction id.")
--           acc <- findColumn "account id" m
--           amnt <- findColumnM "amount" m
--                   (\case  { Nothing -> return Nothing;
--                             Just v -> return
--                                       $ Just
--                                       $ (realToFrac :: Scientific -> Quantity)
--                                        v})
--           bal <- findColumnDefaultM Nothing "balance date" m
--                   (\case  {"" -> return Nothing;
--                            v -> Just <$> parseISO8601M v})
--           tags <- recordToTags m  (HS.fromList multipleRecordsHeader)
--           return $ Transaction date tId [Posting bal acc amnt] tags

--         regroupTransactions :: (MonadError Error m) =>
--                                [JTransaction] -> m JTransaction
--         regroupTransactions [] = error "regroupTransactions empty list"
--         regroupTransactions [x] =
--             throwError $ "Expecting at least two postings for transaction id \""
--                        ++ T.unpack (tTransactionId x)
--                        ++ "\"."
--         regroupTransactions xs = do
--           let nbDates = S.size $ S.fromList $ map tDate xs
--           when (nbDates > 1) (throwError
--                   $ "Expecting the same date for all postings of transaction id \""
--                   ++ T.unpack (tTransactionId $ head xs)
--                   ++ "\".")
--           let ps = concatMap tPostings xs
--           (_, tags) <- validateTags $ map (\x -> (tTransactionId x, tTags x)) xs
--           return $ Transaction
--                    (tDate $ head xs)
--                    (tTransactionId $ head xs)
--                    ps
--                    tags

--         validateTags :: (MonadError Error m) =>
--                      [(T.Text, [Tag])] -> m (T.Text, [Tag])
--         validateTags xs =
--           foldM (\t1  t2 -> if snd t1 == snd t2
--                              then return t1
--                              else throwError
--                                  $ "Expecting the same tag values for all \
--                                    \postings of transaction id\""
--                                  ++ T.unpack (fst t1)
--                                  ++ "\".")
--                  (head xs)
--                  (tail xs)

-- decodeJTransactionsFile :: String -> ExceptT Error IO [JTransaction]
-- decodeJTransactionsFile f = do
--         csvBS <- liftIO $ BL.readFile f
--         h <- decodeHeader csvBS
--         decodeTransactions h csvBS
