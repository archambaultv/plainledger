{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  qualifiedName,
  escapedChar,
  graphicChar,
  isGraphicChar,
  commodity
) where

import qualified Data.Char as C
import Data.Void
import Data.Decimal
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Plainledger.Data.Type
 
type Parser = Parsec Void String

-- proxy :: Proxy Char
-- proxy = Proxy

--- Whitespace
lineComment :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
lineComment = L.skipLineComment (tokensToChunk (Proxy :: Proxy s) ";")

blockComment :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
blockComment = L.skipBlockComment
               (tokensToChunk (Proxy :: Proxy s) "#|")
               (tokensToChunk (Proxy :: Proxy s) "|#")

pSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
pSpace = L.space space1 lineComment blockComment

symbol :: forall e s m. (MonadParsec e s m, Token s ~ Char) => String -> m (Tokens s)
symbol s = L.symbol pSpace (tokensToChunk (Proxy :: Proxy s) s)

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme = L.lexeme pSpace

paren :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
paren = between (symbol "(") (symbol ")")

decimal :: (MonadParsec e s m, Token s ~ Char) => m Decimal
decimal = label "number" $ lexeme $ realToFrac <$> L.signed (pure ()) L.scientific

commodity :: Parser T.Text
commodity = text

date :: (MonadParsec e s m, Token s ~ Char) => m Day
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
  is <- many (alphaNumChar <|> oneOf subsequentList)
  return $ T.pack (i : is)

initialList :: String
initialList = "!$%&*/<=>^~@.#_"

subsequentList :: String
subsequentList = initialList ++ "+-?"

graphicChar :: Parser Char
graphicChar = satisfy isGraphicChar

isGraphicChar :: Char -> Bool
isGraphicChar '"' = False
isGraphicChar '\\' = False
isGraphicChar ' ' = True
isGraphicChar x = C.isAlphaNum x ||
                  C.isPunctuation x ||
                  C.isSymbol x

pString :: Parser T.Text
pString = lexeme $
          between (char '"') (char '"')
          (T.pack <$> many (escapedChar <|> graphicChar))

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
              (between uniDecimal (char '}') (L.decimal >>= validChar)) <|>
              (between uniOctal (char '}') (L.octal >>= validChar)) <|>
              (between uniHexa (char '}') (L.hexadecimal >>= validChar))

  where validChar i =
          if i <= 0x10FFFF
          then return $ C.chr i
          else fail $ show i ++ " is greater than the maximum unicode valid code point (x10FFFF)"

        uniStart = char '\\' >> oneOf ("uU" :: String)
        uniDecimal = uniStart >> char '{'
        uniOctal = uniStart >> string "o{"
        uniHexa = uniStart >> string "x{"

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
