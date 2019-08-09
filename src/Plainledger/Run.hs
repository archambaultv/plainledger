{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Run
(
  run
  ) where


import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import Data.Bifunctor ( first )
import Control.Monad.Except
import Text.Megaparsec
import Plainledger.Parser.Parser
import Plainledger.CLIOptions
import Plainledger.Error
import Plainledger.Data.Ledger
import Plainledger.Printer.Printer

run :: Command -> IO ()
run c = runExceptT (runT c) >>= either putStrLn return

runT :: Command -> ExceptT Error IO () 
runT (Command command input output startDate endDate) = do
  lift $ putStrLn "Reading input file"
  stream <- lift $ T.readFile input

  lift $ putStrLn "Parsing s-expressions in input file"
  sexps <- liftEither $ first errorBundlePretty $ parse literateSexpParser input stream

  lift $ putStrLn "Building the raw journal"
  rawJournal <- liftEither $ sexps2RawJournal sexps

  lift $ putStrLn "Building the ledger"
  ledger <- liftEither $ rawJournal2Ledger rawJournal
  let adjustedLedger = adjustDate ledger startDate endDate

  lift $ putStrLn "Executing the command"
  res <- case command of
           BalanceSheet -> return $ printBalanceSheet adjustedLedger
           IncomeStatement -> return "NotImplemented"
           TrialBalance -> return "NotImplemented"
           SpendingReport -> return "NotImplemented"
           RevenuReport -> return "NotImplemented"
  case null output of
    True -> lift $ putStrLn $ L.unpack $ E.decodeUtf8 res
    False -> lift $ B.writeFile output res
