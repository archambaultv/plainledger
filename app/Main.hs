-- |
-- Module      :  Main
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- The plainledger command line tool

module Main where

-- import Data.Void
import Data.Semigroup ( (<>) )
-- import Data.Time
-- import Data.Bifunctor ( first )
import Options.Applicative
-- import qualified Text.Megaparsec as M

-- import Plainledger.Data.Type (AccountingFormat(..), SignConvention(..))
import Plainledger.CLI.Command
import Plainledger.CLI.Run

-- dateparser :: Char -> String -> String -> String -> Parser (Maybe Day)
-- dateparser shortOption optionStr helpStr meta = option
--   (eitherReader $ fmap Just . first M.errorBundlePretty . M.parse (L.date :: M.Parsec Void String Day) "")
--   (value Nothing <>
--    short shortOption <>
--    long optionStr <>
--    help helpStr <>
--    metavar meta)
--
-- startDate :: Parser (Maybe Day)
-- startDate = dateparser
--             's'
--             "startdate"
--             "All transactions before this date are ignored"
--             "START-DATE"
--
-- endDate :: Parser (Maybe Day)
-- endDate = dateparser
--           'e'
--           "enddate"
--           "All transactions after this date are ignored"
--           "END-DATE"
--
-- debitCredit :: Parser AccountingFormat
-- debitCredit = flag (OneColumnSignedNumber SignDependsOnNetBalance) TwoColumnsDebitCredit
--               (short 'd' <>
--                long "debitcredit" <>
--                help "Indicates whether to prints an amount using two columns (debit and credit) or with one column (+ for inflow, - for outflow)"
--               )
--
journalFile :: Parser String
journalFile = argument str (metavar "JOURNAL-FILE" <> help "The journal file")

csvFile :: Parser String
csvFile = argument str (metavar "CSV-FILE" <> help "The csv file")

csvType :: Parser CsvType
csvType = flag' CsvAccounts
          (  long "accounts"
          <> short 'a'
          <> help "The CSV-FILE contains records representing accounts" )

yamlFile :: Parser String
yamlFile = argument str (metavar "YAML-FILE" <> help "The yaml file")

outputArg :: Parser String
outputArg = argument str (metavar "OUTPUT-FILE" <> help "The ouptut file")

outputOpt :: Parser (Maybe String)
outputOpt = optional
           $ strOption
           $ short 'o'
           <> long "output"
           <> metavar "OUTPUT-FILE"
           <> help "The output file."

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

-- balanceCommand :: Parser Command
-- balanceCommand = CBalanceSheet <$> (BalanceSheetCommand
--     <$> journalFile
--     <*> outputFile
--     <*> startDate
--     <*> endDate)
--
-- balanceInfo :: ParserInfo Command
-- balanceInfo = info (balanceCommand <**> helper)
--               (fullDesc
--                <> progDesc "Prints the balance sheet")
--
-- incomeCommand :: Parser Command
-- incomeCommand = CIncome <$> (IncomeCommand
--     <$> journalFile
--     <*> outputFile
--     <*> startDate
--     <*> endDate)
--
-- incomeInfo :: ParserInfo Command
-- incomeInfo = info (incomeCommand <**> helper)
--               (fullDesc
--                <> progDesc "Prints the income statement")
--
--
-- transactionsCommand :: Parser Command
-- transactionsCommand = CTransactions <$> (TransactionsCommand
--     <$> journalFile
--     <*> outputFile
--     <*> startDate
--     <*> endDate
--     <*> debitCredit)
--
-- transactionsInfo :: ParserInfo Command
-- transactionsInfo = info (transactionsCommand <**> helper)
--               (fullDesc
--                <> progDesc "Prints the transactions")
--
--
-- trialBalanceCommand :: Parser Command
-- trialBalanceCommand = CTrialBalance <$> (TrialBalanceCommand
--     <$> journalFile
--     <*> outputFile
--     <*> startDate
--     <*> endDate
--     <*> debitCredit)
--
-- trialBalanceInfo :: ParserInfo Command
-- trialBalanceInfo = info (trialBalanceCommand <**> helper)
--               (fullDesc
--                <> progDesc "Prints the trial balance")
--
parseCommand :: Parser Command
parseCommand = subparser
  ( command "accounts" accountsInfo
  <> command "fromcsv" fromCsvInfo)

  -- ( command "balancesheet" balanceInfo <>
  --   command "incomestatement" incomeInfo <>
  --   command "transactions" transactionsInfo <>
  --   command "trialbalance" trialBalanceInfo
  -- )

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use plainledger -h to list the \
                     \possible commands. Use plainledger COMMAND -h for help \
                     \on a specific command."
         <> header "Plain ledger - Plain text accouting command line tool")


main :: IO ()
main = execParser opts >>= runCommand
