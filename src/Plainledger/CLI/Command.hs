-- |
-- Module      :  Plainledger.CLI.Command
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines all the possible terminal commands

module Plainledger.CLI.Command
(
  Command(..),
  runCommand
) where

import Control.Monad.Except
import Plainledger.Journal
import Plainledger.Report

data Command = Command {
  cJournalFile :: String,
  cOutputFile :: String,
  cReport :: Report
  }

-- / How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (Command journalPath outputPath report) = do
       -- Reads the Journal file and create the journal object
       journal <- runExceptT 
                $ decodeJournalFileIO journalPath >>= journalFileToJournal
       -- Check for internal errors in the journal and proceed with the report
       case journal of
         Left err -> putStrLn (show err)
         Right l -> putStrLn (show l) --do
            -- let reportBS = computeReport journal report >>= C.encode
            -- maybe (putStrLn . BL.pack) (BL.writeFile outputPath) reportBS