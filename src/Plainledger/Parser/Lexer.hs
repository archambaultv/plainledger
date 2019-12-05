{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plainledger.Parser.Lexer
(
  Parser,
  Lexer,
  lexer,
  SL.getInputOffset,
  located,
  date,

  Tok(..),
  paren,
  symbol,
  property,
  SL.atomToken,
  SL.atomSatisfy,
  
  tokDate,
  tokDecimal,
  tokInteger,
  tokNumber,
  tokString,
  tokIdentifier,
  tokText,
  tokProperty,
  tokQName,
  tokCommodity
) where

import Data.Void
import qualified Data.List.Split as LS
import qualified Data.Char as C
import Data.Decimal
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Plainledger.Data.Type
import qualified Data.SExpresso.Parse as SL
 
type Parser s = Parsec Void (SL.SExprStream s Char Char Tok)

type Lexer s = Parsec Void s (SL.SExprStream s Char Char Tok)
                                  
lexer :: (MonadParsec e s m, Token s ~ Char) => m (SL.SExprStream s Char Char Tok)
lexer = SL.sexprStream (char '(') (char ')') tok pSpace sepRule <* eof

located :: (Stream s) => Parser s a -> Parser s (Located a)
located p = do
  p1 <- SL.getInputOffset
  x <- p
  p2 <- SL.getInputOffset
  return ((p1, (p2 - p1)), x)

data Tok
  = TDate Day
  | TInteger Integer
  | TDecimal Decimal
  | TString T.Text
  | TIdentifier T.Text
  | TProperty T.Text
  | TQName [T.Text]
  deriving (Eq, Ord, Show)

sepRule :: Tok -> Tok -> SL.SeparatorRule
sepRule (TString _) _ = SL.SOptional
sepRule _ (TString _) = SL.SOptional
sepRule _ _ = SL.SMandatory

-- How to parse token
tok :: (MonadParsec e s m, Token s ~ Char) => m Tok
tok = (TDate <$> try date) <|>
      (TDecimal <$> try decimal) <|>
      (TInteger <$> integer) <|>
      (TQName <$> qualifiedName) <|>
      (TString . T.pack <$> pString) <|>
      (TIdentifier . T.pack <$> identifier) <|>
      (TProperty . T.pack <$> (char ':' *> identifier))

integer :: (MonadParsec e s m, Token s ~ Char) => m Integer
integer = L.signed (pure ()) L.decimal
  
decimal :: (MonadParsec e s m, Token s ~ Char) => m Decimal
decimal = realToFrac <$> L.signed (pure ()) L.scientific

date :: (MonadParsec e s m, Token s ~ Char) => m Day
date = do
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

identifier :: (MonadParsec e s m, Token s ~ Char) => m String
identifier = do
  i <- letterChar <|> oneOf initialList
  is <- many (alphaNumChar <|> oneOf subsequentList)
  return $ i : is

initialList :: String
initialList = "!$%&*/<=>^~@.#_?"

subsequentList :: String
subsequentList = initialList ++ "+-"

graphicChar :: (MonadParsec e s m, Token s ~ Char) => m Char
graphicChar = satisfy isGraphicChar

isGraphicChar :: Char -> Bool
isGraphicChar '"' = False
isGraphicChar '\\' = False
isGraphicChar ' ' = True
isGraphicChar x = C.isAlphaNum x ||
                  C.isPunctuation x ||
                  C.isSymbol x

pString :: (MonadParsec e s m, Token s ~ Char) => m String
pString = between (char '"') (char '"') $
          many (escapedChar <|> graphicChar)

escapedChar :: (MonadParsec e s m, Token s ~ Char) => m Char
escapedChar = (escape >> char 'n' >> pure '\n') <|>
              (escape >> char 't' >> pure '\t') <|>
              (escape >> char 'r' *> pure '\r') <|>
              (escape >> char 'v' *> pure '\v') <|>
              (escape >> char 'a' *> pure '\a') <|>
              (escape >> char 'b' *> pure '\b') <|>
              (escape >> char 'f' *> pure '\f') <|>
              (escape >> char '\\' *> pure '\\') <|>
              (escape >> char '\"' *> pure '"') <|>
              (between escape (char ';') (L.decimal >>= validChar)) <|>
              (between uniOctal (char ';') (L.octal >>= validChar)) <|>
              (between uniHexa (char ';') (L.hexadecimal >>= validChar))

  where validChar i =
          if i <= 0x10FFFF
          then return $ C.chr i
          else fail $ show i ++ " is greater than the maximum unicode valid code point (x10FFFF)"
          
        escape = char '\\'

        uniOctal = escape >> char 'o'
        uniHexa = escape >> char 'x'

qualifiedName :: (MonadParsec e s m, Token s ~ Char) => m QualifiedName
qualifiedName = fmap (map T.pack)  $
  (stringQName <|>
   (sepBy1 identifier (char ':')))

  where stringQName = do
          s <- pString
          let ss = LS.splitOn ":" s
          let r = all (not . null) ss
          if r
            then return ss
            else fail "Qualified name written with a string cannot start or end with : or contain empty names (':' followed by another ':')"


--- Whitespace
lineComment :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
lineComment = L.skipLineComment (tokensToChunk (Proxy :: Proxy s) ";")

blockComment :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
blockComment = L.skipBlockComment
               (tokensToChunk (Proxy :: Proxy s) "#|")
               (tokensToChunk (Proxy :: Proxy s) "|#")

pSpace :: (MonadParsec e s m, Token s ~ Char) => m ()
pSpace = L.space space1 lineComment blockComment

-- Helpers to parse SExprStream

paren :: (MonadParsec e s m, Token s ~ SL.SExprToken Char Char a) => m x -> m x
paren = between (SL.openDelimiter '(') (SL.closeDelimiter ')')

symbol :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => T.Text -> m T.Text
symbol s = SL.atom (TIdentifier s) *> pure s

property :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => T.Text -> m T.Text
property s = SL.atom (TProperty s) *> pure s

tokDate :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m Day
tokDate = SL.atomToken (\t -> case t of {TDate x -> Just x; _ -> Nothing}) Nothing <?> "date"

tokDecimal :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m Decimal
tokDecimal = SL.atomToken (\t -> case t of {TDecimal x -> Just x; _ -> Nothing}) Nothing <?> "decimal number"

tokInteger :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m Integer
tokInteger = SL.atomToken (\t -> case t of {TInteger x -> Just x; _ -> Nothing}) Nothing <?> "integer number"

tokNumber :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m Decimal
tokNumber = (fmap fromIntegral tokInteger <|> tokDecimal) <?> "number"

tokString :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokString = SL.atomToken (\t -> case t of {TString x -> Just x; _ -> Nothing}) Nothing <?> "string"

tokIdentifier :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokIdentifier = SL.atomToken (\t -> case t of {TIdentifier x -> Just x; _ -> Nothing}) Nothing <?> "identifier"

tokText :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokText = tokString <|> tokIdentifier 

tokProperty :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokProperty = SL.atomToken (\t -> case t of {TProperty x -> Just x; _ -> Nothing}) Nothing <?> "property"

tokQName :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m [T.Text]
tokQName = SL.atomToken (\t -> case t of {TQName x -> Just x; _ -> Nothing}) Nothing <?> "qualified name"

tokCommodity :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokCommodity = tokText
