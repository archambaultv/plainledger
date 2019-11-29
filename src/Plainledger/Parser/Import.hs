module Plainledger.Parser.Import
(
  importConf,
 
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative.Permutations
import Plainledger.Data.Type
import Plainledger.Parser.Lexer

importConf :: Parser ImportConfiguration
importConf = between pSpace eof $ paren $ do
  _ <- lexeme $ string "csv"
  c <- runPermutation $
    ImportConfiguration <$>
    toPermutationWithDefault ',' pdelimiter <*>
    toPermutationWithDefault 1 pskip <*>
    toPermutation pcommodity <*>
    toPermutation pdateColumn <*>
    toPermutation pdebitColumn <*>
    toPermutation pcreditColumn <*>
    toPermutationWithDefault Nothing pbalanceColumn <*>
    toPermutationWithDefault [] ptagColumns <*>
    toPermutation paccount <*>
    toPermutation paccountNegative <*>
    toPermutation paccountPositive

  return c

  where columnInteger = lexeme $ fmap (flip (-) 1) L.decimal -- Make the column number starts at 0
        pdelimiter = symbol ":column-delimiter" *> (lexeme $ between (char '"') (char '"') (escapedChar <|> graphicChar))
        pcommodity = symbol ":commodity" *> text
        pskip = symbol ":rows-to-skip" *> lexeme L.decimal
        pdateColumn = symbol ":date-column" *> columnInteger
        pdebitColumn = symbol ":debit-column" *> columnInteger
        pcreditColumn = symbol ":credit-column" *> columnInteger 
        pbalanceColumn = symbol ":balance-column" *> (Just <$> columnInteger)
        ptagColumns  = symbol ":tag-columns" *> some (lexeme $ paren $ (,) <$> columnInteger <*> text)

        paccount = symbol ":main-account" *> qualifiedName
        paccountNegative = symbol ":second-account-when-amount-negative" *> qualifiedName
        paccountPositive = symbol ":second-account-when-amount-positive" *> qualifiedName
