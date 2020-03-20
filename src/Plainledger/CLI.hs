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
import Plainledger.Journal.Day

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
            'b'
            "begin"
            "All transactions in the journal file before this date are ignored"
            "BEGIN"

endDate :: Parser (Maybe Day)
endDate = dateparser
          'e'
          "end"
          "All transactions in the journal file after this date are ignored"
          "END"

journalFile :: Parser String
journalFile = argument str (metavar "JOURNAL-FILE" <> help "The journal file")

csvFile :: Parser String
csvFile = argument str (metavar "CSV-FILE" <> help "The csv file")

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
       <|> (flag' CsvTransactions
                 (  long "transactions"
                 <> short 't'
                 <> help "The CSV-FILE contains records \
                         \representing transactions")
           <*> txnDecodeOption)

convertCommand :: Parser Command
convertCommand = CFromCsv
               <$> (ConvertCommand
                   <$> argument str (metavar "INPUT-FILE" <> help "The input file")
                   <*> argument str (metavar "OUTPUT-FILE" <> help "The output file")
                   <*> csvType)

convertInfo :: ParserInfo Command
convertInfo = info (convertCommand <**> helper)
              (fullDesc
               <> progDesc "Converts between a CSV file and a Yaml \
                           \file readable by plainledger")

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
                   <*> startDate
                   <*> endDate
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
                   <*> startDate
                   <*> endDate
                   <*> trialBalanceOption)

trialBalanceOption :: Parser TrialBalanceOption
trialBalanceOption = TrialBalanceOption
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

cashFlowCommand :: Parser Command
cashFlowCommand = CCashFlow
               <$> (CashFlowCommand
                   <$> journalFile
                   <*> csvFile
                   <*> startDate
                   <*> endDate
                   <*> cashFlowOption)

cashFlowOption :: Parser CashFlowOption
cashFlowOption = CashFlowOption
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
                  \the JOURNAL-FILE in the report, including accounts \
                  \without transactions.")

cashFlowInfo :: ParserInfo Command
cashFlowInfo = info (cashFlowCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the cash flow report in a CSV format")

balanceSheetCommand :: Parser Command
balanceSheetCommand = CBalanceSheet
               <$> (BalanceSheetCommand
                   <$> journalFile
                   <*> csvFile
                   <*> startDate
                   <*> endDate
                   <*> balanceSheetOption)

balanceSheetOption :: Parser BalanceSheetOption
balanceSheetOption = BalanceSheetOption
                   <$> showInactiveAccounts

  where showInactiveAccounts = flag False True
           ( long "show-all-accounts"
          <> short 'a'
          <> help "Show all the accounts defined in the accounts section of \
                  \the JOURNAL-FILE in the trial balance, including accounts \
                  \without transactions.")

balanceSheetInfo :: ParserInfo Command
balanceSheetInfo = info (balanceSheetCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the balance sheet in a CSV format")

parseCommand :: Parser Command
parseCommand = subparser
  ( command "accounts" accountsInfo
  <> command "convert" convertInfo
  <> command "transactions" transactionsInfo
  <> command "trialbalance" trialBalanceInfo
  <> command "cashflow" cashFlowInfo
  <> command "balancesheet" balanceSheetInfo)

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use plainledger -h to list the \
                     \possible commands. Use plainledger COMMAND -h for help \
                     \on a specific command."
         <> header "Plain ledger - Plain text accounting command line tool")


cli :: IO ()
cli = execParser opts >>= runCommand
