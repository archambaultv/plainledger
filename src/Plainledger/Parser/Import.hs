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
  skip :: Int,
  defaultCommodity :: Commodity,
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
    toPermutationWithDefault 1 pskip <*>
    toPermutation pcommodity <*>
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
        pcommodity = symbol ":commodity" *> text
        pskip = symbol ":rows-to-skip" *> integer
        pdateColumn = symbol ":date-column" *> integer
        pdebitColumn = symbol ":debit-column" *> integer
        pcreditColumn = symbol ":credit-column" *> integer 
        pbalanceColumn = symbol ":balance-column" *> integer
        ptagColumns  = symbol ":tag-columns" *> some (paren $ (,) <$> integer <*> text)

        paccount = symbol ":main-account" *> qualifiedName
        paccountCredit = symbol ":second-account-credit" *> qualifiedName
        paccountDebit = symbol ":second-account-debit" *> qualifiedName
        prules = symbol ":import-rules-file" *> some text
