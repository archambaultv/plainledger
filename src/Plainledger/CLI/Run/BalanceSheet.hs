-- |
-- Module      :  Plainledger.CLI.Run.BalanceSheet
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.BalanceSheet
(
  runBalanceSheet
  ) where

import qualified Data.Csv as C
import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports
import Control.Monad.Except

-- / Reads the journal file and the exports the transactions in CSV format
runBalanceSheet :: BalanceSheetCommand -> IO ()
runBalanceSheet c = do
     journalFile <- Y.decodeFileThrow (bsJournalFile c)
     journal <- runExceptT $ journalFileToJournal (bsJournalFile c) journalFile
     case journal >>= journalToLedger of
       Left err -> putStrLn err
       Right l -> do
          let report = Report (bsPeriod c) (bsJournalFile c) l
          let tb = reportToBalanceSheet (bsOption c) report
          BL.writeFile (bsOuputFile c) (C.encode tb)
