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
  decodeBalances
  )
where

import Data.Time
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Csv as C
import qualified Data.Text as T
import Plainledger.Journal.Amount
import Plainledger.Journal.Day
import Plainledger.Internal.Csv
import Plainledger.Internal.Utils
import Control.Monad.Except
import Plainledger.Error
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)

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