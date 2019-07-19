{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Parser.Sexp
  (
    Sexp(..),
    sexpParser,
    literateSexpParser,
    sexpSourcePos,
    pISO8601Date,
    sourcePosPretty
  )
  where

import Prelude hiding (concat)
import Data.Time
import Data.Decimal
import Data.Void
import Data.Text hiding (count, empty, unwords)
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char hiding (symbolChar)
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- The type of statements allowed in a plain ledger file

data Sexp
  = SList SourcePos [Sexp]
  | SDate SourcePos Day
  | SDecimal SourcePos Decimal
  | SString SourcePos Text -- Enclosed between double quotes (")
  | SSymbol SourcePos Text
  deriving (Show)

sexpSourcePos :: Sexp -> SourcePos
sexpSourcePos (SList pos _) = pos
sexpSourcePos (SDate pos _) = pos
sexpSourcePos (SDecimal pos _) = pos
sexpSourcePos (SString pos _) = pos
sexpSourcePos (SSymbol pos _) = pos

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

-- Consumes white space and newline
spaceConsumerNewline :: Parser ()
spaceConsumerNewline = L.space space1 lineComment blockComment

-- Consumes everything until the end of line included
line :: Parser ()
line = takeWhileP Nothing (== '\n') >> takeP Nothing 1 >> return ()

-- Consumes all white space and block comment. Then stops if the first
-- caracter is '(' (open parenthesis).  Otherwise it skips the rest of
-- the line.  This means that lines beginning with any caracter but
-- '(' once whitespace is removed is considered a line comment

-- Combined with the Sexp parser, it means that within a Sexp the
-- behavior for line comment is the standard one (with usage of --)
literateSpaceConsumer :: Parser ()
literateSpaceConsumer = do
  L.space space1 (return ()) blockComment
  stop <|> (line >> literateSpaceConsumer)

  where stop = lookAhead (char '(') >> return ()        

-- Helper with whitespace
--lexeme :: Parser a -> Parser a
--lexeme = L.lexeme spaceConsumer

lexemeNewline :: Parser a -> Parser a
lexemeNewline = L.lexeme spaceConsumerNewline

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumerNewline

pSignedDecimal :: Parser Sexp
pSignedDecimal = lexemeNewline $ do
  pos <- getSourcePos
  number <- L.signed (return ()) L.scientific
  return $ SDecimal pos (realToFrac number)

pISO8601Date :: Parser Day
pISO8601Date = lexemeNewline $ do
  year <- count 4 digitChar
  _ <- char '-'
  month <- count 2 digitChar
  _ <- char '-'
  day <- count 2 digitChar
  case validDate  year month day of
    Nothing -> fail "Invalid date"
    Just d -> return $ d

  where validDate y m d = do
          y' <- readMaybe y
          m' <- readMaybe m
          d' <- readMaybe d
          fromGregorianValid y' m' d'

-- ISO 8601 format
pDay :: Parser Sexp
pDay = do
  pos <- getSourcePos
  d <- pISO8601Date
  return $ SDate pos d

pString :: Parser Sexp
pString = lexemeNewline $ do
  pos <- getSourcePos
  s <- char '"' >> manyTill anySingle (char '"')
  return $ SString pos (pack s)
  
pSymbol :: Parser Sexp
pSymbol = lexemeNewline $ do
  pos <- getSourcePos
  s <- some symbolChar 
  return $ SSymbol pos (pack s)

symbolChar :: Parser Char
symbolChar =
  let specialChar = "-_><#!$^&*+=\\/:~|.@?" :: String
  in alphaNumChar
     <|> oneOf specialChar <?> ("one of " ++ specialChar)
                  
pSList :: Parser Sexp
pSList = lexemeNewline $ do
  pos <- getSourcePos
  sexps <- between (symbol "(") (symbol ")") (many sexp)
  return $ SList pos sexps
  
sexp :: Parser Sexp
sexp = try pDay
    <|> try pSignedDecimal
    <|> pString
    <|> pSymbol
    <|> pSList

sexpParser :: Parser [Sexp]
sexpParser = do
  _ <- spaceConsumerNewline
  sexps <- many sexp 
  _ <- eof
  return sexps

literateSexpParser :: Parser [Sexp]
literateSexpParser = do
  _ <- literateSpaceConsumer
  sexps <- many (pSList <* literateSpaceConsumer)
  _ <- eof
  return sexps
