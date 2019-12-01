{-# LANGUAGE TupleSections #-}

module Plainledger.Parser.Rule
(
  parseRules
) where

import Data.Time
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative.Permutations
import Plainledger.Data.Type
import Plainledger.Parser.Lexer

parseRules :: Parser [TransactionRule]
parseRules = between pSpace eof $ many (paren $ modifyRule <|> deleteRule)

modifyRule :: Parser TransactionRule
modifyRule = lexeme $ do
  _ <- symbol "modify-transaction"
  m1 <- transactionP regexDate (postingMatch canDelete) (tagMatch canDelete)
  m2 <- transactionP replaceDate postingReplace tagReplace
  return (m1, Just m2)

deleteRule :: Parser TransactionRule
deleteRule = lexeme $ do
  _ <- symbol "delete-transaction"
  m1 <- transactionP regexDate (postingMatch cannotDelete) (tagMatch cannotDelete)
  return (m1, Nothing)

canDelete :: Parser Bool
canDelete = (option False (symbol "-" >> return True))

cannotDelete :: Parser Bool
cannotDelete = return False

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

graphicRegex :: Parser Char
graphicRegex = satisfy
               (\c -> c /= '\'' &&
               (c == '"' || isGraphicChar c))

escapedRegex :: Parser Char
escapedRegex = (string "\\'" >> return '\'') <|> escapedChar

regexP :: Parser String
regexP = between (char '\'') (char '\'')
         (some $ escapedRegex <|> graphicRegex)

variable :: Parser String
variable = char '?' >> (T.unpack <$> identifier)

withRegex :: Parser a -> Parser (ERegex a)
withRegex p = regex <|> (Right <$> p)

regexUse :: Parser String
regexUse = string "\\?{" *> (T.unpack <$> identifier) <* char '}'

replaceString :: Parser [Either String String]
replaceString = between (char '\"') (char '\"') $ many $ 
                (Right <$> many (escapedChar <|> graphicChar)) <|>
                (Left <$> regexUse)
                 
withReplace :: Parser a -> Parser (Replace a)
withReplace p = (Variable 0 <$> lexeme variable) <|>
                (Replace <$> replaceString ) <|>
                (RawInput <$> p)
     
withReplaceDecimal :: Parser a -> Parser (Replace a)
withReplaceDecimal p = (Variable <$> decimal <*> variable) <|> withReplace p

transactionP :: Parser d -> Parser p -> Parser t -> Parser (TransactionF d t p)
transactionP pDate pPostings pTags = lexeme $ paren $ do
  _ <- symbol "transaction"
  d <- pDate
  (postings, tags1) <- runPermutation $
    (,) <$> toPermutationWithDefault [] posting
        <*> toPermutationWithDefault [] tags
  tags2 <- option [] tags
  let tagss = tags1 ++ tags2
  return $ Transaction d tagss postings
  
  where posting = symbol ":postings" *> (some pPostings <?> "posting")
        tags = many pTags

regexDate :: Parser (Maybe (ERegex Day))
regexDate = optional (withRegex date)

tagMatch :: Parser Bool -> Parser (Bool, TagMatch)
tagMatch pDelete = do
  _ <- char ':'
  toDelete <- pDelete
  key <- lexeme identifier
  value <- optional $ withRegex (text <|> (T.pack . show <$> decimal) <|> (T.pack . show <$> date))
  return $ (toDelete, Tag key value)

postingMatch :: Parser Bool -> Parser (Bool, PostingMatch)
postingMatch pDelete = do
  toDelete <- pDelete
  p <- paren $ do
    n <- withRegex qualifiedName
    q <- withRegex decimal
    c <- withRegex commodity
    return $ Posting n q c
  return (toDelete, p)


replaceDate :: Parser (Maybe (Replace Day))
replaceDate = optional (withReplace date)

tagReplace :: Parser TagReplace
tagReplace = do
  key <- char ':' *> lexeme identifier
  value <- optional $ withReplace (text <|> (T.pack . show <$> decimal) <|> (T.pack . show <$> date))
  return $ Tag key value

postingReplace :: Parser PostingReplace
postingReplace = do
    n <- withReplace qualifiedName
    q <- withReplaceDecimal decimal
    c <- withReplace commodity
    return $ Posting n q c
