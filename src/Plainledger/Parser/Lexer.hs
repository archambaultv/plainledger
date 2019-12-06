{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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
  tokNonEmptyString,
  tokIdentifier,
  tokText,
  tokNonEmptyText,
  tokProperty,
  tokQName,
  tokCommodity
) where

import Data.Void
import qualified Data.Char as C
import Data.Decimal
import Data.Proxy
import qualified Data.Text as T
import Data.Time
import Text.Read
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.NonEmpty as CNE
import Plainledger.Data.Type
import Plainledger.Data.QualifiedName
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

-- FixMe :: Add the comments, so we get better error message (expecting ... or comment)
-- FixMe ::  -- passe le lexer. - ok c'est un id. -12 c'est un nombre. Mais -- ne doit pas passer.
data Tok
  = TDate Day
  | TInteger Integer
  | TDecimal Decimal
  | TString T.Text
  | TIdentifier T.Text
  | TProperty T.Text
  | TQName QualifiedName
  deriving (Eq, Ord)

instance Show Tok where
  show (TDate x) = show x
  show (TInteger x) = show x
  show (TDecimal x) = show x
  show (TString x) = show x
  show (TIdentifier x) = show x
  show (TProperty x) = '"' : ':' : drop 1 (show x)
  show (TQName x) = qualifiedNameToString x

sepRule :: Tok -> Tok -> SL.SeparatorRule
sepRule (TString _) _ = SL.SOptional
sepRule _ (TString _) = SL.SOptional
sepRule _ _ = SL.SMandatory

-- How to parse token
tok :: (MonadParsec e s m, Token s ~ Char) => m Tok
tok = (TDate <$> try date) <|>
      try number <|>
      (TString . T.pack <$> pString) <|>
      (TIdentifier . T.pack <$> identifier) <|>
      (TProperty . T.pack <$> label "property" (char ':' *> identifier)) <|>
      (TQName  <$> qualifiedName)

      where number = label "number" $ do
              d <- decimal
              let (i,a) = properFraction d
              if a == 0
                then return $ TInteger i
                else return $ TDecimal d

-- integer :: (MonadParsec e s m, Token s ~ Char) => m Integer
-- integer = L.signed (pure ()) L.decimal
  
decimal :: (MonadParsec e s m, Token s ~ Char) => m Decimal
decimal = realToFrac <$> L.signed (pure ()) L.scientific

date :: (MonadParsec e s m, Token s ~ Char) => m Day
date = label "date" $ do
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
identifier = standardIdentifier <|> peculiarIdentifier <?> "identifier"

  where standardIdentifier = do
          i <- letterChar <|> oneOf initialList
          is <- many (alphaNumChar <|> oneOf subsequentList)
          return $ i : is

        peculiarIdentifier = (single '+' >> return "+") <|>
                             (single '-' >> return "-")

initialList :: String
initialList = "!$%&*/<=>?^_~@."

subsequentList :: String
subsequentList = initialList ++ "+-"

graphicChar :: (MonadParsec e s m, Token s ~ Char) => m Char
graphicChar = satisfy isGraphicChar <?> "printable character"

isGraphicChar :: Char -> Bool
isGraphicChar '"' = False
isGraphicChar '\\' = False
isGraphicChar ' ' = True
isGraphicChar x = C.isAlphaNum x ||
                  C.isPunctuation x ||
                  C.isMark x ||
                  C.isSymbol x

pString :: (MonadParsec e s m, Token s ~ Char) => m String
pString = label "string" $ between (char '"') (char '"') $
          many (escapedChar <|> graphicChar)

escapedChar :: (MonadParsec e s m, Token s ~ Char) => m Char
escapedChar = escape >>
              ((char 'n' >> pure '\n') <|>
               (char 't' >> pure '\t') <|>
               (char 'r' *> pure '\r') <|>
               (char 'v' *> pure '\v') <|>
               (char 'a' *> pure '\a') <|>
               (char 'b' *> pure '\b') <|>
               (char 'f' *> pure '\f') <|>
               (char '\\' *> pure '\\') <|>
               (char '\"' *> pure '"') <|>
               ((L.decimal >>= validChar) <* char ';') <|>
               (between uniOctal (char ';') (L.octal >>= validChar)) <|>
               (between uniHexa (char ';') (L.hexadecimal >>= validChar)))

  where validChar i =
          if i <= 0x10FFFF
          then return $ C.chr i
          else fail $ show i ++ " is greater than the maximum unicode valid code point (x10FFFF)"
          

        uniOctal = char 'o'
        uniHexa = char 'x'

escape :: (MonadParsec e s m, Token s ~ Char) => m Char
escape = char '\\'

qualifiedName :: (MonadParsec e s m, Token s ~ Char) => m QualifiedName
qualifiedName = label "qualified name" $ fmap (fmap T.pack) stringQName

  where stringQName = between (char '\'') (char '\'')
                      (CNE.sepBy1 (some qualifiedChar) (char ':'))

        newEscape = escape >> ((char ':' *> pure ':') <|>
                               (char '\'' *> pure '\''))

        qualifiedChar = try newEscape <|>
                        escapedChar <|>
                        (satisfy (\c -> c /= ':' &&
                                        c /= '\'' &&
                                        isGraphicChar c))
                         <?> "printable character"
--- Whitespace
lineComment :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
lineComment = L.skipLineComment (tokensToChunk (Proxy :: Proxy s) ";")

blockComment :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m ()
blockComment = L.skipBlockCommentNested
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

tokNonEmptyString :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokNonEmptyString = SL.atomToken (\t -> case t of
                                          TString x@(T.null -> False) -> Just x
                                          _ -> Nothing)
                    Nothing <?> "non empty string"

tokIdentifier :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokIdentifier = SL.atomToken (\t -> case t of {TIdentifier x -> Just x; _ -> Nothing}) Nothing <?> "identifier"

tokText :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokText = tokString <|> tokIdentifier

tokNonEmptyText :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokNonEmptyText = tokNonEmptyString <|> tokIdentifier

tokProperty :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokProperty = SL.atomToken (\t -> case t of {TProperty x -> Just x; _ -> Nothing}) Nothing <?> "property"

tokQName :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m QualifiedName
tokQName = SL.atomToken (\t -> case t of {TQName x -> Just x; _ -> Nothing}) Nothing <?> "qualified name"

tokCommodity :: (MonadParsec e s m, Token s ~ SL.SExprToken b c Tok) => m T.Text
tokCommodity = tokNonEmptyString <?> "commodity"
