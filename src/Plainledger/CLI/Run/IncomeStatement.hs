-- |
-- Module      :  Plainledger.CLI.Run.IncomeStatement
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.IncomeStatement
(
  runIncomeStatement
  ) where

import qualified Data.Csv as C
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports
import Control.Monad.Except

-- / Reads the journal file and the exports the transactions in CSV format
runIncomeStatement :: IncomeStatementCommand -> IO ()
runIncomeStatement c = do
     journalFile <- Y.decodeFileThrow (isJournalFile c)
     journal <- runExceptT $ journalFileToJournal (isJournalFile c) journalFile
     case journal >>= journalToLedger of
       Left err -> putStrLn err
       Right l -> do
          let report = Report (isPeriod c) (isJournalFile c) l
          let tb = reportToIncomeStatement (isOption c) report
          BL.writeFile (isOuputFile c) (C.encode tb)
