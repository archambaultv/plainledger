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
  BalanceF(..),
  JBalance,
  Balance,
  decodeStatementBalanceFile,
  decodeTrialBalanceFile,
  decodeBalances,
  validateTrialBalances,
  validateStatementBalances,
  )
where

import Data.Time ( Day )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Plainledger.I18n.I18n
    ( I18nText(TBalanceStartDate, TBalanceDate, TBalanceEndDate,
               TBalanceAccount, TBalanceAmount),
      Language,
      i18nText )
import Plainledger.Journal.Amount ( Quantity, parseAmount )
import Plainledger.Journal.Day ( parseISO8601M )
import Plainledger.Journal.BalanceMap
    ( BalanceMap(bmOpeningBalanceAcc, bmBalances),
      balanceAtDate,
      trialBalanceQuantity )
import Plainledger.Internal.Csv
    ( columnIndex,
      optionalColumnIndex,
      processColumnIndexes,
      columnData,
      columnDataM,
      optionalColumnDataM, readCsvFile )
import Plainledger.Internal.Utils ( removeBom )
import Plainledger.Journal.Account
    ( isIncomeStatementType,
      Account,
      AccountF(aAccountType, aIdentifier) )
import Control.Monad.Except
    ( withExceptT, MonadIO(liftIO), MonadError(..), ExceptT )
import Plainledger.Error
    ( SourcePos(SourcePos),
      ErrorType(AccountIdNotInAccountFile,
                MissingStartDateInBalance, WrongBalance, DuplicateBalance),
      Errors,
      mkError,
      setSourcePosFileIfNull,
      setSourcePosRowIfNull, mkErrorMultiPos, SourceRow)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (groupBy, sortBy)

-- | The Balance data type reprensents an assertion about the total of an
-- account at a particular date. Used for bank statement assertion and trial
-- balance assertion
data BalanceF a = Balance
  {bDate :: Day,
   bAccount :: a,
   bAmount :: Quantity,
   bStartDate :: Maybe Day
  }
  deriving (Eq, Show)

type JBalance = BalanceF T.Text
type Balance = BalanceF Account

decodeStatementBalanceFile :: Language ->
                              Char ->
                              Char ->
                              FilePath ->
                              ExceptT Errors IO [(SourcePos, JBalance)]
decodeStatementBalanceFile lang csvSeparator decimalSeparator filePath =
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap (snd . removeBom) $ liftIO $ BS.readFile filePath
      bals <- decodeBalances lang True csvSeparator decimalSeparator (BL.fromStrict csvBS)
      let bals1 = map (\(i, b) -> (i, b{bStartDate = Nothing})) bals
      let withPos = map (\(i, a) -> (SourcePos filePath i 0, a)) bals1
      return withPos

decodeTrialBalanceFile :: Language ->
                          Char ->
                          Char ->
                          FilePath ->
                          ExceptT Errors IO [(SourcePos, JBalance)]
decodeTrialBalanceFile lang csvSeparator decimalSeparator filePath =
  withExceptT (setSourcePosFileIfNull filePath) $ do
      csvBS <- fmap (snd . removeBom) $ liftIO $ BS.readFile filePath
      bals <- decodeBalances lang False csvSeparator decimalSeparator (BL.fromStrict csvBS)
      let withPos = map (\(i, a) -> (SourcePos filePath i 0, a)) bals
      return withPos

-- | The first line is the header
decodeBalances :: forall m . (MonadError Errors m)
               => Language -> Bool -> Char -> Char -> ByteString -> m [(SourceRow, JBalance)]
decodeBalances lang statementBalance csvSeparator decimalSeparator bs = do
  -- Read the CSV file as vector of T.Text
  csv <- readCsvFile csvSeparator bs

  -- Decode the header to know the index of columns
  let mainDate = if statementBalance
                  then i18nText lang TBalanceDate
                  else i18nText lang TBalanceEndDate
  let myFilter t = t `elem` [mainDate,
                             i18nText lang TBalanceAccount,
                             i18nText lang TBalanceAmount,
                             i18nText lang TBalanceStartDate]
  (csvData, indexes) <- processColumnIndexes csv myFilter

  dateIdx <- columnIndex indexes mainDate
  accountIdx <- columnIndex indexes (i18nText lang TBalanceAccount)
  amountIdx <- columnIndex indexes (i18nText lang TBalanceAmount)
  let startDateIdx = optionalColumnIndex indexes (i18nText lang TBalanceStartDate)

  -- Function to parse a line into an Account                            
  let parseLine (row, line) =
          let p = (row,) <$> do
                date <- columnDataM dateIdx line (parseISO8601M . T.unpack)
                acc <- columnData accountIdx line
                amount <- columnDataM amountIdx line
                        (parseAmount decimalSeparator)
                sd <- optionalColumnDataM Nothing startDateIdx line
                       (fmap Just . parseISO8601M . T.unpack)
                return $ Balance date acc amount sd
          in p `catchError` (throwError . setSourcePosRowIfNull row)


  mapM parseLine csvData

validateStatementBalances :: forall m . (MonadError Errors m) =>
                      HM.HashMap T.Text Account ->
                      BalanceMap ->
                      [(SourcePos, JBalance)] ->
                      m ()
validateStatementBalances accMap bm = 
  validateBalances (checkStatementBalanceAmount bm) accMap

validateTrialBalances :: forall m . (MonadError Errors m) =>
                      HM.HashMap T.Text Account ->
                      BalanceMap ->
                      [(SourcePos, JBalance)] ->
                      m ()
validateTrialBalances accMap bm = do
    validateBalances (checkTrialBalanceAmount bm) accMap

validateBalances :: forall m . (MonadError Errors m) =>
                      ((SourcePos, Balance) -> m ()) ->
                      HM.HashMap T.Text Account ->
                      [(SourcePos, JBalance)] ->
                      m ()
validateBalances checkAmount accMap bs = do
  checkDuplicateBalance bs
  bals <- traverse (checkBalanceAccount accMap) bs
  traverse_ checkAmount bals

checkDuplicateBalance :: (MonadError Errors m) =>
                        [(SourcePos, JBalance)] ->
                        m ()
checkDuplicateBalance balances =
  let dup :: [[(SourcePos, (Day, T.Text))]]
      dup = filter (not . null . tail)
          $ groupBy ((==) `on` snd)
          $ sortBy (compare `on` snd)
          $ map (fmap (\b -> (bDate b, bAccount b))) balances
      mkErr ls = let d = fst $ snd $ head ls
                     a = T.unpack $ snd $ snd $ head ls
                     pos = map fst ls
                 in mkErrorMultiPos pos
                    (DuplicateBalance d a)
  in if null dup
     then return ()
     else throwError $ concatMap mkErr dup

checkTrialBalanceAmount :: forall m . (MonadError Errors m) =>
                      BalanceMap ->
                      (SourcePos, Balance) ->
                      m ()
checkTrialBalanceAmount balMap (pos, b) =
  let endDate = bDate b
      acc = bAccount b
      accType = aAccountType acc
      mustHaveStartDate = acc == bmOpeningBalanceAcc balMap
                        || isIncomeStatementType accType
      startDateM =
        case (bStartDate b, mustHaveStartDate) of
          (Nothing, True) -> throwError
                          $ mkError pos
                          $ MissingStartDateInBalance
                          $ T.unpack
                          $ aIdentifier acc
          _ -> return $ fromMaybe endDate $ bStartDate b

      computeAmount sd =
        if HM.member acc (bmBalances balMap)
        then Just $ trialBalanceQuantity balMap acc (sd, endDate)
        else Nothing
  in do
    startDate <- startDateM
    let computedAmount = computeAmount startDate
    checkBalance pos (aIdentifier acc) endDate (computedAmount, bAmount b)

checkStatementBalanceAmount :: forall m . (MonadError Errors m) =>
                      BalanceMap ->
                      (SourcePos, Balance) ->
                      m ()
checkStatementBalanceAmount balMap (pos, b) =
  let date = bDate b
      acc = bAccount b
      computedAmount = snd <$> balanceAtDate balMap acc date
  in
    checkBalance pos (aIdentifier acc) date (computedAmount, bAmount b)

checkBalance :: forall m . (MonadError Errors m) =>
                SourcePos ->
                T.Text ->
                Day ->
                (Maybe Quantity, Quantity) -> m ()
checkBalance pos accIdent date z =
  case z of
       (Nothing, 0) -> return ()
       (Nothing, x) ->
              throwError
              $ mkError pos
              $ WrongBalance (T.unpack accIdent) date x Nothing
       (Just y, x) | y /= x ->
              throwError
              $ mkError pos
              $ WrongBalance (T.unpack accIdent) date x (Just y)
       _ -> return ()

checkBalanceAccount :: (MonadError Errors m) =>
                       HM.HashMap T.Text Account ->
                       (SourcePos, JBalance) ->
                       m (SourcePos, Balance)
checkBalanceAccount accMap (pos, b) =
  case HM.lookup (bAccount b) accMap of
    Just x -> return (pos, b{bAccount = x})
    Nothing -> throwError
                $ mkError pos
                $ AccountIdNotInAccountFile
                $ T.unpack
                $ bAccount b