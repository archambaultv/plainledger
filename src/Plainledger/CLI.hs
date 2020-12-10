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
import Options.Applicative
import Plainledger.CLI.Command
import Plainledger.CLI.Run
import Plainledger.Journal.Day
import Plainledger.Reports

dateparser :: LDate -> Char -> String -> String -> String -> Parser LDate
dateparser def shortOption optionStr helpStr meta = option
  (eitherReader $ fmap Date . parseISO8601M)
  (value def <>
   short shortOption <>
   long optionStr <>
   help helpStr <>
   metavar meta)

startDate :: Parser (LDate)
startDate = dateparser
             MinDate
            'b'
            "begin"
            "All transactions in the journal file before this date are ignored"
            "BEGIN"

endDate :: Parser (LDate)
endDate = dateparser
          MaxDate
          'e'
          "end"
          "All transactions in the journal file after this date are ignored"
          "END"

yearEndDay :: Parser Day
yearEndDay = option
  (eitherReader parseISO8601M)
  (short 'p' <>
   long "year-end" <>
   help "The year-end date. All transactions in the journal file after this date are ignored" <>
   metavar "YEAR-END")

multiYear :: Parser Int
multiYear = option auto
          ( long "years"
         <> short 'y'
         <> value 1
         <> help "How many fiscal year to show in the reports"
         <> metavar "YEARS" )

period :: Parser Period
period = (MultiYear <$> yearEndDay <*> multiYear)
       <|> (Span <$> startDate <*> endDate)

journalFile :: Parser String
journalFile = argument str (metavar "JOURNAL-FILE" <> help "The journal file")

csvFile :: Parser String
csvFile = argument str (metavar "CSV-FILE" <> help "The csv file")

csvDir :: Parser String
csvDir = argument str (metavar "CSV-DIR" <> help "The directory in which to output the csv files")

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

txnDecodeOption :: Parser CsvRecordOptions
txnDecodeOption = flag MultipleRecords SingleRecord
   ( long "single-record"
  <> short 's'
  <> help "Each transaction will be encoded as a single \
          \line in the CSV-FILE. The default \
          \is to encode transactions on mulitple lines, one \
          \per posting.")

validationOption :: Parser Bool
validationOption = flag True False
   ( long "no-validation"
  <> short 'n'
  <> help "Does not perform any validation on the JOURNAL-FILE. \
          \Use this option to export to CSV and keep the unspecified optional \
          \field empty in the CSV. Othewise validation will fill in the missing \
          \values.")

transactionsCommand :: Parser Command
transactionsCommand = CTransactions
               <$> (TransactionsCommand
                   <$> journalFile
                   <*> csvFile
                   <*> period
                   <*> txnDecodeOption
                   <*> validationOption)

transactionsInfo :: ParserInfo Command
transactionsInfo = info (transactionsCommand <**> helper)
              (fullDesc
               <> progDesc "Prints all transactions in a CSV format")

trialBalanceCommand :: Parser Command
trialBalanceCommand = CTrialBalance
               <$> (TrialBalanceCommand
                   <$> journalFile
                   <*> csvFile
                   <*> period
                   <*> trialBalanceOption)

trialBalanceOption :: Parser TrialBalanceOption
trialBalanceOption = FlatReportOption
                   <$> balanceFormat
                   <*> showInactiveAccounts

  where balanceFormat = flag TwoColumnDebitCredit InflowOutflow
           ( long "signed-balance"
          <> short 's'
          <> help "Does not report the balance with debit credit columns, but only \
                  \with a signed quantity. A positive amount means debit and a \
                  \negative amount means credit.")

        showInactiveAccounts = flag False True
           ( long "show-all-accounts"
          <> short 'a'
          <> help "Show all the accounts defined in the accounts section of \
                  \the JOURNAL-FILE in the trial balance, including accounts \
                  \without transactions.")

trialBalanceInfo :: ParserInfo Command
trialBalanceInfo = info (trialBalanceCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the trial balance in a CSV format")


balanceSheetCommand :: Parser Command
balanceSheetCommand = CBalanceSheet
               <$> (BalanceSheetCommand
                   <$> journalFile
                   <*> csvFile
                   <*> period
                   <*> balanceSheetOption)

balanceSheetOption :: Parser BalanceSheetOption
balanceSheetOption = GroupReportOption
                   <$> showInactiveAccounts

  where showInactiveAccounts = flag False True
           ( long "show-all-accounts"
          <> short 'a'
          <> help "Show all the accounts defined in the accounts section of \
                  \the JOURNAL-FILE, including accounts \
                  \without transactions.")

balanceSheetInfo :: ParserInfo Command
balanceSheetInfo = info (balanceSheetCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the balance sheet in a CSV format")

incomeStatementCommand :: Parser Command
incomeStatementCommand = CIncomeStatement
               <$> (IncomeStatementCommand
                   <$> journalFile
                   <*> csvFile
                   <*> period
                   <*> balanceSheetOption)

incomeStatementInfo :: ParserInfo Command
incomeStatementInfo = info (incomeStatementCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the income statement in a CSV format")

allReportsCommand :: Parser Command
allReportsCommand = CAllReports
               <$> (AllReportsCommand
                   <$> journalFile
                   <*> csvDir
                   <*> period
                   <*> trialBalanceOption)

allReportsInfo :: ParserInfo Command
allReportsInfo = info (allReportsCommand <**> helper)
              (fullDesc
               <> progDesc "Prints all the reports a CSV format")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "transactions" transactionsInfo
  <> command "trialbalance" trialBalanceInfo
  <> command "balancesheet" balanceSheetInfo
  <> command "incomestatement" incomeStatementInfo)

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use plainledger -h to list the \
                     \possible commands. Use plainledger COMMAND -h for help \
                     \on a specific command."
         <> header "Plain ledger - Plain text accounting command line tool")


cli :: IO ()
cli = execParser opts >>= runCommand
