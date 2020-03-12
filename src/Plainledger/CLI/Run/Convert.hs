-- |
-- Module      :  Plainledger.CLI.Run.Convert
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the how to execute the fromCsv command

module Plainledger.CLI.Run.Convert
(
  runFromCsv
  ) where

import Data.Yaml.Pretty as YP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Plainledger.CLI.Command
import Plainledger.Ledger
import Plainledger.Internal.Utils

-- / Reads the journal file and the exports the accounts in CSV format
runFromCsv :: ConvertCommand -> IO ()
runFromCsv c = do
  toFileType <- either fail return $ isSupportedExtension (ccToFile c)
  case (ccFromDataType c) of
    CsvAccounts -> do
      accs <- decodeAccountsFile (ccFromFile c)
      case toFileType of
        YamlFile -> BS.writeFile (ccToFile c)
                    $ YP.encodePretty yamlPrettyConfig accs
        CsvFile -> BL.writeFile (ccToFile c) (encodeAccounts accs)
    CsvTransactions opt -> do
      txns <- decodeJTransactionsFile (ccFromFile c)
      case toFileType of
        YamlFile -> BS.writeFile (ccToFile c)
                    $ YP.encodePretty yamlPrettyConfig txns
        CsvFile -> BL.writeFile (ccToFile c)
                 $ encodeTransactions opt txns
