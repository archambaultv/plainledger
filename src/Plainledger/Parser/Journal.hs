{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Parser.Journal
(
  journal
) where


import qualified Data.Text as T
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Applicative.Permutations
import Data.SExpresso.Parse
import Plainledger.Data.Type
import Plainledger.Parser.Lexer
 
-- The parser does not catch logical errors, only parsing errors

journal :: Parser Journal
journal = between pSpace eof $ many (located journalEntry)

journalEntry :: Parser JournalEntry
journalEntry = paren $
               balance <|> open <|> close <|> transaction <|> configuration

-- Check for duplicated keys
duplicateKeys :: Ord k => [(k,v)] -> [k]
duplicateKeys l =
  let mTest = M.fromListWith (const . const True) (map (fmap (const False)) l)
  in map fst $ M.toList $ M.filter (== True) mTest

balance :: Parser JournalEntry
balance =  do
  _ <- symbol "balance"
  d <- date
  n <- qualifiedName
  q <- decimal
  c  <- optional commodity
  return $ JEBalance $ BalanceEntry d n q c

open :: Parser JournalEntry
open =  do
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
close =  do
  _ <- symbol "close"
  d <- date
  n <- some (located qualifiedName)
  return $ JECloseAccount $ CloseAccountEntry d n

tag :: Parser Tag
tag = do
  key <- char ':' *> lexeme identifier
  value <- optional (text <|> (T.pack . show <$> decimal) <|> (T.pack . show <$> date))
  return $ Tag key value
  
transaction :: Parser JournalEntry
transaction = do
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
configuration = do
  _ <- symbol "configuration"
  c <- runPermutation $
    Configuration <$> toPermutation defaultCommodityP
        <*> toPermutation accountTypeC
        <*> toPermutation openingAccount
        <*> toPermutation earningsAccount
        <*> toPermutationWithDefault M.empty tagDescriptionP
        <*> toPermutationWithDefault "True" tagDefaultValue
        <*> toPermutationWithDefault "False" tagValueIfMissing
  return $ JEConfiguration c
  
  where defaultCommodityP = symbol ":main-currency" *> text
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
