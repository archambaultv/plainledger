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

import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Reports

-- / Reads the journal file and the exports the transactions in CSV format
runIncomeStatement :: IncomeStatementCommand -> IO ()
runIncomeStatement c = do
     journalFile <- Y.decodeFileThrow (isYamlFile c)
     journal <- journalFileToJournal (isYamlFile c) journalFile
     case journalToLedger journal of
       Left err -> putStrLn err
       Right l ->
        let sd = maybe MinDate Date $ (isStartDate c)
            ed = maybe MaxDate Date $ (isEndDate c)
        in do
          tb <- either fail return
                $ reportToIncomeStatement (isOption c)
               <$> report
                   (isYamlFile c)
                   sd
                   ed
                   l
          BL.writeFile (isCsvFile c) tb
