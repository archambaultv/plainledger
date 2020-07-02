-- |
-- Module      :  Plainledger.CLI.Run.Accounts
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the accounts command

module Plainledger.CLI.Run.Accounts
(
  runAccounts
  ) where

import qualified Data.Yaml as Y
import qualified Data.ByteString.Lazy as BL
import Plainledger.CLI.Command
import Control.Monad.Except
import Plainledger.Ledger

-- / Reads the journal file and the exports the accounts in CSV format
runAccounts :: AccountsCommand -> IO ()
runAccounts c = do
     journalFile <- Y.decodeFileThrow (acYamlFile c)
     journal <- runExceptT $ journalFileToJournal (acYamlFile c) journalFile
     case journal >>= journalToLedger of
       Left err -> putStrLn err
       Right l -> BL.writeFile
                  (acCsvFile c)
                  (encodeAccounts $ jAccounts $ lJournal l)
