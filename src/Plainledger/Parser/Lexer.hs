{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Parser.Lexer
(
  Parser,
  lineComment,
  blockComment,
  pSpace,
  symbol,
  lexeme,
  identifier,
  paren,
  decimal,
  date,
  text,
  qualifiedName  
) where

import qualified Data.Char as C
import Data.Void
import Data.Decimal
import qualified Data.Text as T
import Data.Time
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Plainledger.Data.Type
 
type Parser = Parsec Void String


--- Whitespace
lineComment :: Parser ()
lineComment = L.skipLineComment ";"

blockComment :: Parser ()
blockComment = L.skipBlockComment "#|" "|#"

pSpace :: Parser ()
pSpace = L.space space1 lineComment blockComment

symbol :: String -> Parser String
symbol = L.symbol pSpace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme pSpace

paren :: Parser a -> Parser a
paren = between (symbol "(") (symbol ")")

decimal :: Parser Decimal
decimal = label "number" $ lexeme $ realToFrac <$> L.signed (pure ()) L.scientific

date :: Parser Day
date = label "date (YYYY-MM-DD)" $ lexeme $ do
  year <- count 4 digitChar
  _ <- char '-'
  month <- count 2 digitChar
  _ <- char '-'
  day <- count 2 digitChar
  case validDate year month day of
    Nothing -> fail "Invalid date"
    Just d -> return  d

  where validDate y m d = do
          y' <- readMaybe y
          m' <- readMaybe m
          d' <- readMaybe d
          fromGregorianValid y' m' d'

identifier :: Parser T.Text
identifier = do
  i <- letterChar <|> oneOf initialList
  is <- many (letterChar <|> digitChar <|> oneOf subsequentList)
  return $ T.pack (i : is)

initialList :: String
initialList = "!$%&*/<=>?^_~@.#"

subsequentList :: String
subsequentList = initialList ++ "+-"

graphic :: Parser Char
graphic = satisfy graphic'
 where graphic' '"' = False
       graphic' '\\' = False
       graphic' ' ' = True
       graphic' x = C.isAlphaNum x ||
                    C.isPunctuation x ||
                    C.isSymbol x

pString :: Parser T.Text
pString = lexeme $
          between (char '"') (char '"')
          (T.pack <$> many (escapedChar <|> graphic))

escapedChar :: Parser Char
escapedChar = (string "\\n" *> pure '\n') <|>
              (string "\\t" *> pure '\t') <|>
              (string "\\r" *> pure '\r') <|>
              (string "\\v" *> pure '\v') <|>
              (string "\\a" *> pure '\a') <|>
              (string "\\b" *> pure '\b') <|>
              (string "\\f" *> pure '\f') <|>
              (string "\\\\" *> pure '\\') <|>
              (string "\\\"" *> pure '"') <|>
              (between (string "\\U(")) (char ')') (L.decimal >>= validChar) <|>
              (between (string "\\Uo(")) (char ')') (L.octal >>= validChar) <|>
              (between (string "\\Ux(")) (char ')') (L.hexadecimal >>= validChar)

  where validChar i =
          if i <= 0x10FFFF
          then return $ C.chr i
          else fail $ show i ++ " is greater than the maximum unicode valid code point (x10FFFF)"

text :: Parser T.Text
text = label "text" $ lexeme $ (identifier <|> pString)

-- FixeMe return
qualifiedName :: Parser QualifiedName
qualifiedName = label "qualified name" $ lexeme $
  stringQName <|>
  (sepBy identifier (char ':'))

  where stringQName = do
          s <- pString
          let ss = T.splitOn ":" s
          let r = all (not . T.null) ss
          if r
            then return ss
            else fail "Qualified name written with a string cannot start or end with : or contain empty names (: followed by another :)"
