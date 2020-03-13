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

import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports

-- / Reads the journal file and the exports the transactions in CSV format
runCashFlow :: CashFlowCommand -> IO ()
runCashFlow c = do
     journalFile <- Y.decodeFileThrow (cfYamlFile c)
     journal <- journalFileToJournal (cfYamlFile c) journalFile
     case journalToLedger journal of
       Left err -> putStrLn err
       Right l ->
        let sd = maybe MinDate Date $ (cfStartDate c)
            ed = maybe MaxDate Date $ (cfEndDate c)
        in do
          tb <- either fail return
                $ reportToCashFlow (cfOption c)
               <$> report
                   (cfYamlFile c)
                   sd
                   ed
                   l
          BL.writeFile (cfCsvFile c) tb
