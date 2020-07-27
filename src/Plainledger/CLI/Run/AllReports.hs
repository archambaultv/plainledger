-- |
-- Module      :  Plainledger.CLI.Run.AllReports
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.AllReports
(
  runAllReports
  ) where

import qualified Data.Csv as C
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports
import Plainledger.CLI.Run.Transactions
import Control.Monad.Except
import System.FilePath

-- / Reads the journal file and the exports the transactions in CSV format
runAllReports :: AllReportsCommand -> IO ()
runAllReports c = do
     journalFile <- Y.decodeFileThrow (arJournalFile c)
     journal <- runExceptT $ journalFileToJournal (arJournalFile c) journalFile
     case journal >>= journalToLedger of
       Left err -> putStrLn err
       Right l -> do
          let report = Report (arPeriod c) (arJournalFile c) l
          let grOption = GroupReportOption
                       $ frShowInactiveAccounts
                       $ arFlatOption c
          -- Balance sheet
          let bs = reportToBalanceSheet grOption report
          BL.writeFile (arOuputDir c </> "Balance sheet.csv") (C.encode bs)
          -- Income statement
          let is = reportToIncomeStatement grOption report
          BL.writeFile (arOuputDir c </> "Income statement.csv") (C.encode is)
          -- Trial balance
          let tb = reportToTrialBalance (arFlatOption c) report
          BL.writeFile (arOuputDir c </> "Trial balance.csv") (C.encode tb)
          -- Cashflow
          let cf = reportToCashFlow (arFlatOption c) report
          BL.writeFile (arOuputDir c </> "Cashflow.csv") (C.encode cf)
          -- Transactions
          let (sd, ed) = maxSpan (arPeriod c)
          let txns = filterDate sd ed
                   $ map transactionToJTransaction . jTransactions 
                   $ lJournal l
          BL.writeFile
            (arOuputDir c </> "Transactions - single line.csv")
            $ encodeTransactions SingleRecord txns
          BL.writeFile
            (arOuputDir c </> "Transactions - multiple lines.csv")
            $ encodeTransactions MultipleRecords txns
