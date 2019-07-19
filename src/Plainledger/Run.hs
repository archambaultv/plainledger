module Plainledger.Run
(
  run
  ) where


import qualified Data.Text.IO as T
import Data.Bifunctor ( first )
import Control.Monad.Except
import Text.Megaparsec
import Plainledger.Parser.Parser
import Plainledger.CLIOptions
import Plainledger.Error
import Plainledger.Data.Ledger

run :: Command -> IO ()
run c = runExceptT (runT c) >>= either putStrLn return

runT :: Command -> ExceptT Error IO () 
runT (Command command input output startDate endDate) = do
  stream <- lift $ T.readFile input
  sexps <- liftEither $ first errorBundlePretty $ parse literateSexpParser input stream
  rawJournal <- liftEither $ sexps2RawJournal sexps
  ledger <- liftEither $ rawJournal2Ledger rawJournal
  res <- case command of
           BalanceSheet -> lift $ putStrLn "Not implemented" >> return ""
           IncomeStatement -> lift $ putStrLn "Not implemented"  >> return ""
           TrialBalance -> lift $ putStrLn "Not implemented" >> return ""
           SpendingReport -> lift $ putStrLn "Not implemented" >> return ""
           RevenuReport -> lift $ putStrLn "Not implemented"   >> return ""
  case output of
    Nothing -> lift $ putStrLn res
    Just outputFile -> lift $ writeFile outputFile res
