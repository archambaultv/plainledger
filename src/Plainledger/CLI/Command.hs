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

import Data.Time
import qualified Data.Text as T
import Control.Monad.Except
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Journal
import Plainledger.Report

data Command = Command {
  cJournalFile :: String,
  cOutputFile :: String,
  cReport :: ReportParams
  }

-- / How to execute the CLI commands
runCommand :: Command -> IO ()
runCommand (Command journalPath outputPath report) = do
  today <- fmap utctDay getCurrentTime
  res <- runExceptT 
         $ fmap (\j -> (j, runReport report today j))
         $ decodeJournal journalPath
  case res of
     Left (lang, err) -> printErr lang err
     Right (j, r) -> writeReport outputPath j r


  where printErr lang err = putStrLn 
                         $ T.unpack
                         $ printErrors
                         $ map (i18nText lang . TError ) err