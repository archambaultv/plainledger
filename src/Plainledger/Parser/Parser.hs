{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Parser.Parser
(
  journal,
  date
) where


import Data.Void
import Data.Decimal
import qualified Data.Text as T
import Data.Time
import Data.List
import Text.Read
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative.Permutations
import Data.SExpresso.Parse
import Plainledger.Data.Type
 
type Parser = Parsec Void String

-- The parser does not catch logical errors, only parsing errors

journal :: Parser Journal
journal = between pSpace eof $ many (located journalEntry)

journalEntry :: Parser JournalEntry
journalEntry = balance <|> open <|> close <|> transaction <|> configuration

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

-- Check for duplicated keys
duplicateKeys :: Ord k => [(k,v)] -> [k]
duplicateKeys l =
  let mTest = M.fromListWith (const . const True) (map (fmap (const False)) l)
  in map fst $ M.toList $ M.filter (== True) mTest


identifier :: Parser T.Text
identifier = do
  i <- letterChar <|> oneOf initialList
  is <- many (letterChar <|> digitChar <|> oneOf subsequentList)
  return $ T.pack (i : is)

initialList :: String
initialList = "!$%&*/<=>?^_~@.#"

subsequentList :: String
subsequentList = initialList ++ "+-"

pString :: Parser T.Text
pString = lexeme $ do
  _ <- char '"'
  as <- many (escapedChar <|> noneOf ['\n','\\'])
  _ <- char '"'
  return $ T.pack as

escapedChar :: Parser Char
escapedChar = (string "\\n" *> pure '\n') <|>
              (string "\\\\" *> pure '\\')
  
text :: Parser T.Text
text = label "text" $ lexeme $ (identifier <|> pString)

qualifiedName :: Parser QualifiedName
qualifiedName = label "qualified name" $ lexeme $ sepBy identifier (char ':')

commodity :: Parser T.Text
commodity = identifier

balance :: Parser JournalEntry
balance = paren $ do
  _ <- symbol "balance"
  d <- date
  n <- qualifiedName
  q <- decimal
  c  <- optional commodity
  return $ JEBalance $ BalanceEntry d n q c

open :: Parser JournalEntry
open = paren $ do
  _ <- symbol "open"
  d <- date
  os <- many (located $ rawOpen)
  return $ JEOpenAccount $ OpenEntry d os
    
rawOpen :: Parser OpenAccountEntry
rawOpen = paren $ do
  n <- qualifiedName
  (num, aCurr, d, aAny) <- runPermutation $
    (,,,) <$> toPermutationWithDefault Nothing number
          <*> toPermutationWithDefault Nothing allowedC
          <*> toPermutationWithDefault Nothing defaultC
          <*> toPermutationWithDefault False allowAnyC

  return $ OpenAccountEntry n num aCurr aAny d
  
  where number = symbol ":number" *> (lexeme $ Just <$> L.decimal)
        allowedC = symbol ":allowed-commodities" *> (Just <$> some text)
        defaultC = symbol ":default-commodity" *> (Just <$> text)
        allowAnyC = symbol ":allow-any-commodities" *> pure True 

close :: Parser JournalEntry
close = paren $ do
  _ <- symbol "close"
  d <- date
  n <- some (located qualifiedName)
  return $ JECloseAccount $ CloseAccountEntry d n

tag :: Parser Tag
tag = do
  key <- char ':' *> identifier
  value <- optional (text <|> (T.pack . show <$> decimal) <|> (T.pack . show <$> date))
  return $ Tag key value
  
transaction :: Parser JournalEntry
transaction = paren $ do
  _ <- symbol "transaction"
  d <- date
  (postings, tags1) <- runPermutation $
    (,) <$> toPermutation posting
        <*> toPermutationWithDefault [] tags
  tags2 <- option [] tags
  let tagss = tags1 ++ tags2
  return $ JETransaction $ Transaction d tagss postings
  
  where posting = symbol ":postings" *> (some (located rawPosting) <?> "posting")
        tags = many (located tag)

rawPosting :: Parser PostingEntry
rawPosting = paren $ do
  n <- qualifiedName
  q <- optional decimal
  c <- optional commodity
  return $ Posting n q c

configuration :: Parser JournalEntry
configuration = paren $ do
  _ <- symbol "configuration"
  c <- runPermutation $
    Configuration <$> toPermutation defaultCommodity
        <*> toPermutation accountTypeC
        <*> toPermutation openingAccount
        <*> toPermutation earningsAccount
        <*> toPermutationWithDefault M.empty tagDescriptionP
        <*> toPermutationWithDefault "True" tagDefaultValue
        <*> toPermutationWithDefault "False" tagValueIfMissing
  return $ JEConfiguration c
  
  where defaultCommodity = symbol ":main-currency" *> text
        accountTypeC = symbol ":account-type" *> accountType
        openingAccount = symbol ":opening-balance-account" *> qualifiedName
        earningsAccount = symbol ":earnings-account" *> qualifiedName
        tagDescriptionP = symbol ":tags-description" *> tagDescription
        tagDefaultValue = symbol ":tags-default-value" *> text
        tagValueIfMissing = symbol ":tags-value-if-not-tagged" *> text

accountType :: Parser (M.Map AccountName AccountType)
accountType = paren $ do
  s <- getParserState
  (a,l,e,r,ex) <- runPermutation $
    (,,,,) <$> toPermutation (category "asset" Asset)
           <*> toPermutation (category "liability" Liability)
           <*> toPermutation (category "equity" Equity)
           <*> toPermutation (category "revenue" Revenue)
           <*> toPermutation (category "expense" Expense)
  let xs = (a ++ l ++ e ++ r ++ ex)
  let d = duplicateKeys xs
  if null d
   then return $ M.fromList xs
   else let dStr = intercalate " " $ map T.unpack d
        in setParserState s >> fail ("The following names appear more than once in :account-type " ++ dStr)
  
  where category s t = (map (,t) . S.toList . S.fromList) <$> (symbol (':' : s) *> some text)

tagDescription :: Parser (M.Map T.Text TagDescription)
tagDescription = toMap <$> (many $ paren $ do
  t <- identifier
  td <- runPermutation $
    TagDescription t <$> toPermutationWithDefault t name
           <*> toPermutationWithDefault False unique
           <*> toPermutationWithDefault Nothing foreignKey
           <*> toPermutationWithDefault "True" defaultValue
           <*> toPermutationWithDefault "False" valueIfMissing
  return td)
  
  where name = symbol ":header" *> text
        unique = symbol ":unique" *> pure True
        foreignKey = symbol ":foreign-key" *> (Just <$> text)
        defaultValue = symbol ":default-value" *> text
        valueIfMissing = symbol ":value-if-not-tagged" *> text

        toMap = M.fromList . fmap (\td -> (tdTagKeyword td, td))
