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

import qualified Data.Text as T
import Control.Monad.Except
import Plainledger.I18n.I18n
import Plainledger.Error
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
   -- Reads the Journal file header and infer language and separator
   header <- runExceptT $ processJournalFileHeader journalPath
   case header of
     Left err -> printErr En_CA err
     Right x@(lang, _, _) -> do
      journal <- runExceptT 
                $ decodeJournalFile journalPath x
                >>= journalFileToJournal
      case journal of
         Left err -> printErr lang err
         Right l -> putStrLn (show l)


  where printErr lang err = putStrLn 
                       $ T.unpack
                       $ printErrors
                       $ map (i18nText lang . TError ) err