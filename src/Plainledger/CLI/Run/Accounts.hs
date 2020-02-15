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
import Plainledger.CLI.Command (AccountsCommand(..))
import Plainledger.Ledger
import Plainledger.Journal

-- / Reads the journal file and the exports the accounts in CSV format
runAccounts :: AccountsCommand -> IO ()
runAccounts c = do
     journal <- Y.decodeFileThrow (acYamlFile c)
     case journalToLedger journal of
       Left err -> putStrLn err
       Right l -> BL.writeFile
                  (acCsvFile c)
                  (encodeAccounts $ lAccounts l)
