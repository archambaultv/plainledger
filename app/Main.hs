module Main where

import Data.Semigroup ( (<>) )
import Data.Time
import Data.Bifunctor ( first )
import Options.Applicative
import qualified Text.Megaparsec as M

import Plainledger.Data.Type (AccountingType(..))
import Plainledger.Parser.Lexer (date)
import Plainledger.CLIOptions
import Plainledger.Run

dateparser :: Char -> String -> String -> Parser (Maybe Day)
dateparser shortOption optionStr helpStr = option
  (eitherReader $ fmap Just . first M.errorBundlePretty . M.parse date "")
  (value Nothing <> short shortOption <> long optionStr <> help helpStr <> metavar "YYYY-MM-DD")

startDate :: Parser (Maybe Day)
startDate = dateparser 'f' "from" "Start date of financial period"

endDate :: Parser (Maybe Day)
endDate = dateparser 't' "to" "End date of financial period"

debitCredit :: Parser AccountingType
debitCredit = flag PlusMinus DebitCredit
              (short 'd' <> long "debitcredit" <> help "Whether to use debit and credit instead of + or -")

parseCommand :: Parser Command
parseCommand = Command
    <$> argument (maybeReader str2CommandName)  (metavar "COMMAND" <> help "The command to be executed")
    <*> argument str (metavar "JOURNAL-FILE" <> help "The journal file to analyse")
    <*> strOption (short 'o' <> long "output" <> metavar "OUTPUT-FILE" <> help "The output file")  
    <*> startDate
    <*> endDate
    <*> debitCredit

opts :: ParserInfo Command
opts = info
           (helper <*> parseCommand)
           ( fullDesc
             <> progDesc "A plain text accounting command line tool"
             <> header "Plain ledger")


main :: IO ()
main = execParser opts >>= run
   
      
