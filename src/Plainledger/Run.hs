{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Run
(
  run
  ) where


import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Bifunctor ( first )
import Control.Monad.Except
import Text.Megaparsec
import Plainledger.Parser.Journal
import Plainledger.CLIOptions
import Plainledger.Error
import Plainledger.Data.Ledger
import Plainledger.Printer.Printer

run :: Command -> IO ()
run c = runExceptT (runT c) >>= either putStrLn return

runT :: Command -> ExceptT Error IO () 
runT (Command command input output startDate endDate accountingType) = do

  lift $ putStrLn "Parsing input file"
  stream <- lift $ readFile input
  j <- liftEither $ first errorBundlePretty $ parse journal input stream

  lift $ putStrLn "Building the ledger"
  ledger <- liftEither $ journalToLedger j
  let adjustedLedger = adjustDate ledger startDate endDate

  lift $ putStrLn "Executing the command"  
  res <- case command of
           BalanceSheet -> return $ printBalanceSheet startDate endDate adjustedLedger
           IncomeStatement -> return $ printIncomeStatement startDate endDate adjustedLedger
           TrialBalance -> return $ printTrialBalance startDate endDate adjustedLedger accountingType
           Transactions -> return $ printTransactions startDate endDate adjustedLedger accountingType
  case null output of
    True -> lift $ putStrLn $ L.unpack $ E.decodeUtf8 res
    False -> lift $ B.writeFile output res
