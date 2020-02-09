-- |
-- Module      :  Plainledger.CLI
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the command line interface of plaingledger

module Plainledger.CLI (
  cli
) where

import Data.Time
import Data.Semigroup ( (<>) )
import Options.Applicative
import Plainledger.CLI.Command
import Plainledger.CLI.Run
import Plainledger.Ledger.Day

dateparser :: Char -> String -> String -> String -> Parser (Maybe Day)
dateparser shortOption optionStr helpStr meta = option
  (eitherReader $ fmap Just . parseISO8601M)
  (value Nothing <>
   short shortOption <>
   long optionStr <>
   help helpStr <>
   metavar meta)

startDate :: Parser (Maybe Day)
startDate = dateparser
            's'
            "startdate"
            "All transactions in the journal file before this date are ignored"
            "START-DATE"

endDate :: Parser (Maybe Day)
endDate = dateparser
          'e'
          "enddate"
          "All transactions in the journal file after this date are ignored"
          "END-DATE"

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
               <$> (AccountsCommand
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
       <|> flag' CsvTransactions
                 (  long "transactions"
                 <> short 't'
                 <> help "The CSV-FILE contains records representing transfers")

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

transfersCommand :: Parser Command
transfersCommand = CTransactions
               <$> (TransactionsCommand
                   <$> journalFile
                   <*> csvFile
                   <*> startDate
                   <*> endDate)

transfersInfo :: ParserInfo Command
transfersInfo = info (transfersCommand <**> helper)
              (fullDesc
               <> progDesc "Prints all transfers in a CSV format")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "accounts" accountsInfo
  <> command "fromcsv" fromCsvInfo
  <> command "transfers" transfersInfo)

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use plainledger -h to list the \
                     \possible commands. Use plainledger COMMAND -h for help \
                     \on a specific command."
         <> header "Plain ledger - Plain text accouting command line tool")


cli :: IO ()
cli = execParser opts >>= runCommand
