{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Run
(
  run
  ) where

import Data.Time
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy as L
import Data.Bifunctor ( first )
import Control.Monad.Except
import Text.Megaparsec
import System.IO
import Plainledger.Parser.Lexer
import Plainledger.Data.Type
import Plainledger.Parser.Journal
import Plainledger.Commands
import Plainledger.Error
import Plainledger.Data.Ledger
import Plainledger.Printer.Printer

run :: Command -> IO ()
run c = runExceptT (runT c) >>= either (hPutStr stderr) return

runT :: Command -> ExceptT Error IO ()
runT command =
  case command of
    CModify c -> runModify c
    CBalanceSheet c -> runBalanceSheet c
    CIncome c -> runIncome c
    CTrialBalance c -> runTrialBalance c --return $ printTrialBalance startDate endDate adjustedLedger accountingType
    CTransactions c -> runTransactions c

getLedger :: String -> Maybe Day -> Maybe Day -> ExceptT Error IO Ledger
getLedger input startDate endDate = do
  stream <- lift $ readFile input
  tokens1 <- liftEither $ first errorBundlePretty $ parse (lexer :: Lexer String) input stream
  j <- liftEither $ first errorBundlePretty $ parse journal input tokens1
  ledger <- liftEither $ journalToLedger j
  return $ adjustDate ledger startDate endDate

printStream :: Maybe String -> B.ByteString -> ExceptT Error IO ()
printStream f s =
  case f of
    Nothing -> lift $ putStrLn $ L.unpack $ EL.decodeUtf8 s
    Just output -> lift $ B.writeFile output s

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
