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
import Control.Applicative.Permutations
import Plainledger.Data.Type
import Plainledger.Utils
import Plainledger.Parser.Lexer

journal :: (Stream s) => Parser s Journal
journal = many (located journalEntry) <* eof

journalEntry :: (Stream s) => Parser s JournalEntry
journalEntry = paren $
               balance <|> open <|> close <|> transaction <|> configuration

balance :: (Stream s) => Parser s JournalEntry
balance =  do
  _ <- symbol "balance"
  d <- tokDate
  n <- tokQName
  q <- tokNumber
  c  <- optional tokCommodity
  return $ JEBalance $ BalanceEntry d n q c

open :: (Stream s) => Parser s JournalEntry
open =  do
  _ <- symbol "open"
  d <- tokDate
  os <- many (located $ rawOpen)
  return $ JEOpenAccount $ OpenEntry d os
    
rawOpen :: (Stream s) => Parser s OpenAccountEntry
rawOpen = paren $ do
  n <- tokQName
  (num, aCurr, d, aAny) <- runPermutation $
    (,,,) <$> toPermutationWithDefault Nothing number
          <*> toPermutationWithDefault Nothing allowedC
          <*> toPermutationWithDefault Nothing defaultC
          <*> toPermutationWithDefault False allowAnyC

  return $ OpenAccountEntry n num aCurr aAny d
  
  where number = property "number" *> (Just <$> tokInteger)
        allowedC = property "allowed-commodities" *> (Just <$> some tokText)
        defaultC = property "default-commodity" *> (Just <$> tokText)
        allowAnyC = property "allow-any-commodities" *> pure True 

close :: (Stream s) => Parser s JournalEntry
close =  do
  _ <- symbol "close"
  d <- tokDate
  n <- some (located tokQName)
  return $ JECloseAccount $ CloseAccountEntry d n

tag :: (Stream s) => Parser s Tag
tag = do
  key <- tokProperty
  value <- optional (tokText <|> (T.pack . show <$> tokNumber) <|> (T.pack . show <$> tokDate))
  return $ Tag key value
  
transaction :: (Stream s) => Parser s JournalEntry
transaction = do
  _ <- symbol "transaction"
  d <- tokDate
  (postings, tags1) <- runPermutation $
    (,) <$> toPermutation posting
        <*> toPermutationWithDefault [] tags
  tags2 <- option [] tags
  let tagss = tags1 ++ tags2
  return $ JETransaction $ Transaction d tagss postings
  
  where posting = property "postings" *> (some (located rawPosting) <?> "posting")
        tags = many (located tag)

rawPosting :: (Stream s) => Parser s PostingEntry
rawPosting = paren $ do
  n <- tokQName
  q <- optional tokNumber
  c <- optional tokCommodity
  return $ Posting n q c

configuration :: (Stream s) => Parser s JournalEntry
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
  
  where defaultCommodityP = property "main-currency" *> tokText
        accountTypeC = property "account-type" *> accountType
        openingAccount = property "opening-balance-account" *> tokQName
        earningsAccount = property "earnings-account" *> tokQName
        tagDescriptionP = property "tags-description" *> tagDescription
        tagDefaultValue = property "tags-default-value" *> tokText
        tagValueIfMissing = property "tags-value-if-not-tagged" *> tokText

accountType :: (Stream s) => Parser s (M.Map AccountName AccountType)
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
  
  where
    category :: (Stream s) => T.Text -> AccountType -> Parser s [(T.Text, AccountType)]
    category s t = (map (,t) . S.toList . S.fromList) <$> (property s *> some tokText)

tagDescription :: (Stream s) => Parser s (M.Map T.Text TagDescription)
tagDescription = toMap <$> (many $ paren $ do
  t <- tokIdentifier
  td <- runPermutation $
    TagDescription t <$> toPermutationWithDefault t name
           <*> toPermutationWithDefault False unique
           <*> toPermutationWithDefault Nothing foreignKey
           <*> toPermutationWithDefault "True" defaultValue
           <*> toPermutationWithDefault "False" valueIfMissing
  return td)
  
  where name = property "header" *> tokText
        unique = property "unique" *> pure True
        foreignKey = property "foreign-key" *> (Just <$> tokText)
        defaultValue = property "default-value" *> tokText
        valueIfMissing = property "value-if-not-tagged" *> tokText

        toMap = M.fromList . fmap (\td -> (tdTagKeyword td, td))
