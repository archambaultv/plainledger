{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Run
(
  run
  ) where

import Data.Char
import Data.Csv
import qualified Data.List as DL
import qualified Data.Csv.Parser.Megaparsec as M
import Data.Time
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy as L
import Data.Bifunctor ( first )
import Control.Monad.Except
import Text.Megaparsec
import Plainledger.Data.Type
import Plainledger.Parser.Journal
import Plainledger.Parser.Import
import Plainledger.Parser.Rule
import Plainledger.Commands
import Plainledger.Error
import Plainledger.Data.Ledger
import Plainledger.Data.Transaction
import Plainledger.Printer.Printer

run :: Command -> IO ()
run c = runExceptT (runT c) >>= either putStrLn return

runT :: Command -> ExceptT Error IO () 
runT command = 
  case command of
    CImport c -> runImport c
    CModify c -> runModify c
    CBalanceSheet c -> runBalanceSheet c
    CIncome c -> runIncome c
    CTrialBalance c -> runTrialBalance c --return $ printTrialBalance startDate endDate adjustedLedger accountingType
    CTransactions c -> runTransactions c

getLedger :: String -> Maybe Day -> Maybe Day -> ExceptT Error IO Ledger
getLedger input startDate endDate = do
  stream <- lift $ readFile input
  j <- liftEither $ first errorBundlePretty $ parse journal input stream
  ledger <- liftEither $ journalToLedger j
  return $ adjustDate ledger startDate endDate

printStream :: Maybe String -> B.ByteString -> ExceptT Error IO ()
printStream f s =
  case f of
    Nothing -> lift $ putStrLn $ L.unpack $ EL.decodeUtf8 s
    Just output -> lift $ B.writeFile output s

printString :: Maybe String -> String -> ExceptT Error IO ()
printString f s =
  case f of
    Nothing -> lift $ putStrLn $ s
    Just output -> lift $ writeFile output s
    
runImport :: ImportCommand -> ExceptT Error IO ()
runImport c = do
  -- Read configuration
  stream <- lift $ readFile (icConfig c)
  config <- liftEither $ first errorBundlePretty $ parse importConf (icConfig c) stream

  -- Read the rules files (if any)
  rules <- concat <$> traverse parseRuleFile (icRules c)

  -- Read the csv file
  csvStream <- lift $ B.readFile (icCsvFile c)
  let csvOpts = defaultDecodeOptions {
        decDelimiter = fromIntegral (ord (columnDelimiter config))
        }
  csvData <- liftEither $ first errorBundlePretty $ M.decodeWith csvOpts NoHeader (icCsvFile c) csvStream

  -- Build transactions
  ts <- liftEither $ importTransactions config csvData

  -- Apply modification rules

  -- Print transactions
  printString (icJournalFile c) (DL.intercalate "\n\n" $ map printTransaction ts)

  where parseRuleFile :: String -> ExceptT Error IO [TransactionRule]
        parseRuleFile input = do
          stream <- lift $ readFile input
          liftEither $ first errorBundlePretty $ parse parseRules input stream

runModify :: ModifyCommand -> ExceptT Error IO ()
runModify _ = return ()

runBalanceSheet :: BalanceSheetCommand -> ExceptT Error IO ()
runBalanceSheet c =  do
  l <- getLedger (bcInputFile c) (bcStart c) (bcEnd c)
  printStream (bcOutputFile c) $ printBalanceSheet (bcStart c) (bcEnd c) l

runIncome :: IncomeCommand -> ExceptT Error IO ()
runIncome c = do
  l <- getLedger (incInputFile c) (incStart c) (incEnd c)
  printStream (incOutputFile c) $ printIncomeStatement (incStart c) (incEnd c) l

runTransactions :: TransactionsCommand -> ExceptT Error IO ()
runTransactions c = do
  l <- getLedger (tcInputFile c) (tcStart c) (tcEnd c)
  printStream (tcOutputFile c) $ printTransactions (tcStart c) (tcEnd c) l (tcAccType c)

runTrialBalance :: TrialBalanceCommand -> ExceptT Error IO ()
runTrialBalance c = do
  l <- getLedger (tbcInputFile c) (tbcStart c) (tbcEnd c)
  printStream (tbcOutputFile c) $ printTrialBalance (tbcStart c) (tbcEnd c) l (tbcAccType c)
