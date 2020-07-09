-- |
-- Module      :  Plainledger.CLI.Run.Cashflow
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.Cashflow
(
  runCashFlow
  ) where

import qualified Data.Csv as C
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports
import Control.Monad.Except

-- / Reads the journal file and the exports the transactions in CSV format
runCashFlow :: CashFlowCommand -> IO ()
runCashFlow c = do
     journalFile <- Y.decodeFileThrow (cfJournalFile c)
     journal <- runExceptT $ journalFileToJournal (cfJournalFile c) journalFile
     case journal >>= journalToLedger of
       Left err -> putStrLn err
       Right l -> do
          let report = Report (cfPeriod c) (cfJournalFile c) l
          let tb = reportToCashFlow (cfOption c) report
          BL.writeFile (cfOuputFile c) (C.encode tb)
