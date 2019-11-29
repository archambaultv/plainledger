{-# LANGUAGE TupleSections #-}

module Plainledger.Parser.Rule
(
  TransactionRule,
  TransactionMatch,
  PostingMatch,
  TagMatch,
  parseRules
) where

import Data.Time
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Permutations
--import Text.Regex.TDFA
import Plainledger.Data.Type
import Plainledger.Parser.Lexer

type ERegex a = Either ([String], String) a

type TransactionRule = (TransactionMatch, Maybe TransactionMatch)

type TransactionMatch = TransactionF (Maybe (ERegex Day)) TagMatch PostingMatch
type PostingMatch = PostingF (ERegex QualifiedName) (ERegex Quantity) (ERegex Commodity)
type TagMatch = TagF (Maybe (ERegex T.Text))

parseRules :: Parser [TransactionRule]
parseRules = between pSpace eof $ many parseRule

parseRule :: Parser TransactionRule
parseRule = lexeme $ paren $ do
  _ <- symbol "modify-transaction"
  m1 <- transactionMatch
  m2 <- optional transactionMatch
  return (m1, m2)


-- 'regex'
-- ?_ <=> '.*'
-- ?d <=> '(.*)'
-- ?d'regex' <=> 'regex'
-- ?a?b?c'regex' <=> 'regex'

-- Like a string, but enclosed by quotes (') and with
-- the escapes \' for quotes and the ability to enter " unescaped
regex :: Parser (ERegex a)
regex = lexeme $ (regexP >>= return . Left . ([],)) <|> (do
  v1 <- variable
  vars <- many variable
  case vars of
    [] -> do
      r <- option "(.*)" regexP
      return $ Left ([v1], r)
    vs -> do
      r <- regexP
      return $ Left (v1 : vs, r))
  
  where graphicRegex = satisfy
                       (\c -> c /= '\'' &&
                              (c == '"' || isGraphicChar c))
                       
        escapedRegex = (string "\\'" >> return '\'') <|> escapedChar

        regexP = between (char '\'') (char '\'')
                 (some $ escapedRegex <|> graphicRegex)

        variable :: Parser String
        variable = char '?' >> (T.unpack <$> identifier)

withRegex :: Parser a -> Parser (ERegex a)
withRegex p = regex <|> (Right <$> p)
  
transactionMatch :: Parser TransactionMatch
transactionMatch = lexeme $ paren $ do
  _ <- symbol "transaction"
  d <- optional (withRegex date)
  (postings, tags1) <- runPermutation $
    (,) <$> toPermutationWithDefault [] posting
        <*> toPermutationWithDefault [] tags
  tags2 <- option [] tags
  let tagss = tags1 ++ tags2
  return $ Transaction d tagss postings
  
  where posting = symbol ":postings" *> (some rawPostingRegex <?> "posting")
        tags = many tagRegex

tagRegex :: Parser TagMatch
tagRegex = do
  key <- char ':' *> lexeme identifier
  value <- optional $ withRegex (text <|> (T.pack . show <$> decimal) <|> (T.pack . show <$> date))
  return $ Tag key value

rawPostingRegex :: Parser PostingMatch
rawPostingRegex = paren $ do
  n <- withRegex qualifiedName
  q <- withRegex decimal
  c <- withRegex commodity
  return $ Posting n q c
  
-- 'regex'
-- ?_ <=> means '.*'
-- ?d <=> '(.*)'
-- ?d'regex' <=> 'regex'
-- ?a?b?c'regex' <=> 'regex'

-- ' ? are match only at the beginning of a possible match for the first part (remove them from the identifier start ?)
-- ' ` ? in the result only at the beginning of a possible match. ` is for math` (+ - * / ( ) round number variable)

-- Use a string " " to enter text that starts with a ? / or _
-- Remember that "identifier" identifier et /identifier/ all match the same thing. Outer quotes are always removed

-- -- https://en.wikibooks.org/wiki/Regular_Expressions/POSIX-Extended_Regular_Expressions
-- (modify-transaction
--   (transaction
--     :posting -(Dépense:Zorg ?q ?c)
--     :description-relevé "TD Ass"
--     -:toto)

--   (transaction
--     :posting (Voiture:Assurance ?q ?c)
--     :description "TD Assurance"))

-- (modify-transaction
--   (transaction
--     :posting -(Dépense:Zorg ?q ?c)
--     :description-relevé "Retrait au guichet")

--   (transaction
--     :posting (Dépense:Épicerie `3 + 2 * ?q / 4` ?c) (Dépense:Cash ?c)
--     :description "Retrait au guichet pour \?{q}"))
