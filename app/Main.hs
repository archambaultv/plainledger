
module Main where

import Data.Semigroup ( (<>) )
import Data.Time
import Data.Bifunctor ( first )
import Options.Applicative
import qualified Text.Megaparsec as M

import Plainledger.Data.Type (AccountingType(..))
import qualified Plainledger.Parser.Lexer as L
import Plainledger.Commands
import Plainledger.Run

dateparser :: Char -> String -> String -> String -> Parser (Maybe Day)
dateparser shortOption optionStr helpStr meta = option
  (eitherReader $ fmap Just . first M.errorBundlePretty . M.parse (L.date :: L.Parser Day) "")
  (value Nothing <>
   short shortOption <>
   long optionStr <>
   help helpStr <>
   metavar meta)

startDate :: Parser (Maybe Day)
startDate = dateparser
            's'
            "startdate"
            "All transactions before this date are ignored"
            "START-DATE"

endDate :: Parser (Maybe Day)
endDate = dateparser
          'e'
          "enddate"
          "All transactions after this date are ignored"
          "END-DATE"

debitCredit :: Parser AccountingType
debitCredit = flag PlusMinus DebitCredit
              (short 'd' <>
               long "debitcredit" <>
               help "Indicates whether to prints an amount using two columns (debit and credit) or with one column (+ for inflow, - for outflow)"
              )

journalFile :: Parser String
journalFile = argument str (metavar "JOURNAL-FILE" <> help "The journal file")

outputFile :: Parser (Maybe String)
outputFile = optional (strOption (short 'o' <> long "output" <> metavar "OUTPUT-FILE" <> help "The output file. Defaults to STDOUT."))

balanceCommand :: Parser Command
balanceCommand = CBalanceSheet <$> (BalanceSheetCommand
    <$> journalFile
    <*> outputFile
    <*> startDate
    <*> endDate)

balanceInfo :: ParserInfo Command
balanceInfo = info (balanceCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the balance sheet")

incomeCommand :: Parser Command
incomeCommand = CIncome <$> (IncomeCommand
    <$> journalFile
    <*> outputFile
    <*> startDate
    <*> endDate)

incomeInfo :: ParserInfo Command
incomeInfo = info (incomeCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the income statement")


transactionsCommand :: Parser Command
transactionsCommand = CTransactions <$> (TransactionsCommand
    <$> journalFile
    <*> outputFile
    <*> startDate
    <*> endDate
    <*> debitCredit)

transactionsInfo :: ParserInfo Command
transactionsInfo = info (transactionsCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the transactions")

                   
trialBalanceCommand :: Parser Command
trialBalanceCommand = CTrialBalance <$> (TrialBalanceCommand
    <$> journalFile
    <*> outputFile
    <*> startDate
    <*> endDate
    <*> debitCredit)

trialBalanceInfo :: ParserInfo Command
trialBalanceInfo = info (trialBalanceCommand <**> helper)
              (fullDesc
               <> progDesc "Prints the trial balance")


importCommand :: Parser Command
importCommand = CImport <$> (ImportCommand
    <$> argument str (metavar "CSV-FILE" <> help "The CSV file containing the new transactions")
    <*> strOption (short 'c' <>
                   long "config" <>
                   metavar "CONFIG-FILE" <>
                   help "The configuration file that specifies how to parse the CSV-FILE")
    <*> switch ( long "dry-run" <>
                 short 'd' <>
                 help "Just show the transactions to be imported. Does not append the new transactions to JOURNAL-FILE")
    <*> optional (strOption (short 'j' <>
                             long "journal" <>
                             metavar "JOURNAL-FILE" <>
                             help ("The journal to which the new transactions will be appended. " ++
                                   "A new file will be created if it doesn't exists. Defaults to STDOUT.")))
    <*> many (strOption (short 'r' <>
                         long "rules" <>
                         metavar "RULE-FILES" <>
                         help "The rule file that specifies how to modify the new transactions read from the CSV-FILE")))

importInfo :: ParserInfo Command
importInfo = info (importCommand <**> helper)
              (fullDesc
               <> progDesc "Imports new transactions from a CSV file")
             
parseCommand :: Parser Command
parseCommand = subparser
  ( command "balancesheet" balanceInfo <>
    command "import" importInfo <>
    command "incomestatement" incomeInfo <>
    command "transactions" transactionsInfo <>
    command "trialbalance" trialBalanceInfo
  )

opts :: ParserInfo Command
opts = info (parseCommand <**> helper)
       (fullDesc
         <> progDesc "Executes COMMAND. Use plainledger -h to list the possible commands. Use plainledger COMMAND -h for help on a specific command."
         <> header "Plain ledger - Plain text accouting command line tool")


main :: IO ()
main = execParser opts >>= run
