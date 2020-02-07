-- |
-- Module      :  Plainledger.CLI.Run.FromCsv
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the fromCsv command

module Plainledger.CLI.Run.FromCsv
(
  runFromCsv
  ) where

import Data.Yaml.Pretty as YP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Plainledger.CLI.Command
import Plainledger.Ledger

-- / Reads the journal file and the exports the accounts in CSV format
runFromCsv :: FromCsvCommand -> IO ()
runFromCsv c = do
  csvBS <- BL.readFile (fcsvCsvFile c)
  case (fcsvCsvType c) of
    CsvAccounts ->
      case decodeAccounts csvBS of
        Left err -> putStrLn err
        Right accs -> BS.writeFile (fcsvYamlFile c)
                      $ YP.encodePretty yamlPrettyConfig accs
