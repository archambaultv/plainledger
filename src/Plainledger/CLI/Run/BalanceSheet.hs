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
       Right l ->
        let sd = maybe MinDate Date $ (bsStartDate c)
            ed = maybe MaxDate Date $ (bsEndDate c)
        in do
          tb <- either fail return
                $ reportToBalanceSheet (bsOption c)
               <$> report
                   (bsJournalFile c)
                   sd
                   ed
                   l
          BL.writeFile (bsCsvFile c) tb
