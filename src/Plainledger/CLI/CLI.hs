-- |
-- Module      :  Plainledger.CLI.CLI
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the command line interface of plaingledger

module Plainledger.CLI.CLI (
  cli
) where

import Data.Semigroup ( (<>) )
import Options.Applicative
import Plainledger.CLI.Command
import Plainledger.CLI.Run

journalFile :: Parser String
journalFile = argument str (metavar "JOURNAL-FILE" <> help "The journal file")

csvFile :: Parser String
csvFile = argument str (metavar "CSV-FILE" <> help "The csv file")

yamlFile :: Parser String
yamlFile = argument str (metavar "YAML-FILE" <> help "The yaml file")

-- outputArg :: Parser String
-- outputArg = argument str (metavar "OUTPUT-FILE" <> help "The ouptut file")
--
-- outputOpt :: Parser (Maybe String)
-- outputOpt = optional
--            $ strOption
--            $ short 'o'
--            <> long "output"
--            <> metavar "OUTPUT-FILE"
--            <> help "The output file."

accountsCommand :: Parser Command
accountsCommand = CAccounts
               <$> (AccountCommand
                   <$> journalFile
                   <*> csvFile)

accountsInfo :: ParserInfo Command
accountsInfo = info (accountsCommand <**> helper)
              (fullDesc
               <> progDesc "Prints all accounts and their properties\
                            \ in a CSV format")

csvType :: Parser CsvType
csvType = flag' CsvAccounts
          (  long "accounts"
          <> short 'a'
          <> help "The CSV-FILE contains records representing accounts" )

fromCsvCommand :: Parser Command
fromCsvCommand = CFromCsv
               <$> (FromCsvCommand
                   <$> csvFile
                   <*> yamlFile
                   <*> csvType)

fromCsvInfo :: ParserInfo Command
fromCsvInfo = info (fromCsvCommand <**> helper)
              (fullDesc
               <> progDesc "Converts the CSV file into a Yaml file")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "accounts" accountsInfo
  <> command "fromcsv" fromCsvInfo)

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use plainledger -h to list the \
                     \possible commands. Use plainledger COMMAND -h for help \
                     \on a specific command."
         <> header "Plain ledger - Plain text accouting command line tool")


cli :: IO ()
cli = execParser opts >>= runCommand
