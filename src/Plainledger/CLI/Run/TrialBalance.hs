-- |
-- Module      :  Plainledger.CLI.Run.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.TrialBalance
(
  runTrialBalance
  ) where

import qualified Data.Csv as C
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports
import Control.Monad.Except

-- / Reads the journal file and the exports the transactions in CSV format
runTrialBalance :: TrialBalanceCommand -> IO ()
runTrialBalance c = do
     journalFile <- Y.decodeFileThrow (tbcJournalFile c)
     journal <- runExceptT $ journalFileToJournal (tbcJournalFile c) journalFile
     case journal >>= journalToLedger of
       Left err -> putStrLn err
       Right l -> do
          let report = Report (tbcPeriod c) (tbcJournalFile c) l
          let tb = reportToTrialBalance (tbcOption c) report
          BL.writeFile (tbcOuputFile c) (C.encode tb)
