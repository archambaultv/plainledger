module Plainledger.Parser.Import
(
  ImportConfiguration(..),
  importConf,
 
) where

import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative.Permutations
import Plainledger.Data.Type
import Plainledger.Parser.Lexer

data ImportConfiguration = ImportConfiguration {
  columnDelimiter :: Char,
  dateColumn :: Int,
  debitColumn :: Int,
  creditColumn :: Int,
  balanceColumn :: Int,
  tagColumns :: [(Int, T.Text)],
  iAccount :: QualifiedName,
  iAccountCredit :: QualifiedName,
  iAccountDebit :: QualifiedName,
  rulesFiles :: [T.Text]
  }


importConf :: Parser ImportConfiguration
importConf = lexeme $ paren $ do
  _ <- string "csv"
  c <- runPermutation $
    ImportConfiguration <$>
    toPermutationWithDefault ',' pdelimiter <*>
    toPermutation pdateColumn <*>
    toPermutation pdebitColumn <*>
    toPermutation pcreditColumn <*>
    toPermutation pbalanceColumn <*>
    toPermutationWithDefault [] ptagColumns <*>
    toPermutation paccount <*>
    toPermutation paccountCredit <*>
    toPermutation paccountDebit <*>
    toPermutationWithDefault [] prules

  return c

  where integer = lexeme L.decimal
        pdelimiter = symbol ":column-delimiter" *> (lexeme anySingle)
        pdateColumn = symbol ":date-column" *> integer
        pdebitColumn = symbol ":debit-column" *> integer
        pcreditColumn = symbol ":credit-column" *> integer 
        pbalanceColumn = symbol ":balance-column" *> integer
        ptagColumns  = symbol ":date-column" *> some (paren $ (,) <$> integer <*> text)

        paccount = symbol ":account" *> qualifiedName
        paccountCredit = symbol ":second-entry-account-when-account-is-credited" *> qualifiedName
        paccountDebit = symbol ":second-entry-account-when-account-is-debited" *> qualifiedName
        prules = symbol ":import-rules-file" *> some text
