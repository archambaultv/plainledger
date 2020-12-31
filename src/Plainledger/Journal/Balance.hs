-- |
-- Module      :  Plainledger.Journal.Balance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Balance data type representing balance assertions.

module Plainledger.Journal.Balance 
(
  Balance(..),
  decodeBalanceFile,
  decodeBalances,
  validateBalances
  )
where

import Data.Time
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Csv as C
import qualified Data.Text as T
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Plainledger.Journal.BalanceMap
import Plainledger.Internal.Csv
import Plainledger.Internal.Utils
import Plainledger.Journal.Account
import Plainledger.Journal.Transaction
import Control.Monad.Except
import Plainledger.Error
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

-- | The Balance data type reprensents an assertion about the total of an
-- account at a particular date. Used for bank reconciliation
data Balance = Balance
  {bDate :: Day,
   bAccount   :: T.Text,
   bAmount :: Quantity,
   bStartDate :: Maybe Day
  }
  deriving (Eq, Show)

decodeBalanceFile :: Char -> Char -> FilePath ->  ExceptT Errors IO [(SourcePos, Balance)]
decodeBalanceFile csvSeparator decimalSeparator filePath = 
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap removeBom $ liftIO $ BS.readFile filePath
      accs <- decodeBalances csvSeparator decimalSeparator (BL.fromStrict csvBS)
      let pos = map (\i -> SourcePos filePath i 0) [2..]
      return $ zip pos accs

-- | The first line is the header
decodeBalances :: forall m . (MonadError Errors m) 
               => Char -> Char -> ByteString -> m [Balance]
decodeBalances csvSeparator decimalSeparator bs = do
  -- Read the CSV file as vector of T.Text
  let opts = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord csvSeparator)
                }
  csv <- either (throwError . mkErrorNoPos . ErrorMessage) return 
      $ C.decodeWith opts C.NoHeader bs

  -- Decode the header to know the index of columns
  let myFilter t = t `elem` ["Date", "Compte", "Montant", "Date de début"]
  (csvData, indexes) <- processColumnIndexes csv myFilter

  dateIdx <- columnIndex indexes "Date"
  accountIdx <- columnIndex indexes "Compte"
  amountIdx <- columnIndex indexes "Montant"
  let startDateIdx = optionalColumnIndex indexes "Date de début"

 -- Add row information to the CSV line
  let csvWithRowNumber = zip [2..] $ V.toList csvData
  
  -- Function to parse a line into an Account                            
  let parseLine (row, line) = 
          let p = do
                date <- columnDataM dateIdx line (parseISO8601M . T.unpack)
                acc <- columnData accountIdx line
                amount <- columnDataM amountIdx line
                        (parseAmount decimalSeparator)
                sd <- optionalColumnDataM Nothing startDateIdx line
                       (fmap Just . parseISO8601M . T.unpack)
                return $ Balance date acc amount sd
          in p `catchError` (throwError . setSourcePosRowIfNull row)
  

  bals <- mapM parseLine csvWithRowNumber

  return bals

-- Balances :
--  Asserts all balance have a valid account field
--  Asserts all balance assertions are correct
validateBalances :: forall m . (MonadError Errors m) =>
                      HS.HashSet T.Text ->
                      (T.Text -> AccountType) ->
                      T.Text ->
                      [Transaction] ->
                      [(SourcePos, Balance)] ->
                      m ()
validateBalances accSet accType openAcc txns bs = do
    _ <- traverse (checkBalanceAccount accSet) bs
    let balMap = postingsToBalanceMap $ concatMap tPostings txns
    _ <- traverse (checkBalanceAmount accType openAcc balMap) bs
    return ()


checkBalanceAmount :: forall m . (MonadError Errors m) =>
                      (T.Text -> AccountType) -> 
                      T.Text ->
                      BalanceMap -> 
                      (SourcePos, Balance) -> 
                      m ()
checkBalanceAmount accTypef openAcc balMap (pos, b) =
  let endDate = bDate b
      acc = bAccount b
      mustHaveStartDate = acc == openAcc 
                        || isIncomeStatementType (accTypef acc)
      startDateM = 
        case (bStartDate b, mustHaveStartDate) of
          (Nothing, True) -> throwError 
                          $ mkError pos
                          $ MissingStartDateInBalance
                          $ T.unpack 
                          $ acc
          _ -> return $ maybe endDate id $ bStartDate b

      computeAmount sd =
        if HM.member acc balMap
        then Just $ trialBalanceQuantity openAcc accTypef balMap 
                             acc (sd, endDate)
        else Nothing
  in do
    startDate <- startDateM
    let computedAmount = computeAmount startDate
    checkBalance pos acc endDate (computedAmount, bAmount b)

checkBalance :: forall m . (MonadError Errors m) =>
                SourcePos -> 
                T.Text -> 
                Day -> 
                (Maybe Quantity, Quantity) -> m ()
checkBalance pos accId date z =
  case z of
       (Nothing, 0) -> return ()
       (Nothing, x) ->
              throwError
              $ mkError pos
              $ WrongBalance (T.unpack accId) date x Nothing
       (Just y, x) | y /= x ->
              throwError
              $ mkError pos
              $ WrongBalance (T.unpack accId) date x (Just y)
       _ -> return ()

checkBalanceAccount :: (MonadError Errors m) => 
                       HS.HashSet T.Text -> 
                       (SourcePos, Balance) -> 
                       m ()
checkBalanceAccount accSet (pos, b) = foo (bAccount b)
  where foo x = if HS.member x accSet
                then return ()
                else throwError 
                      $ mkError pos
                      $ AccountIdNotInAccountFile
                      $ T.unpack 
                      $ x