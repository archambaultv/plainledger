{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Parser.Parser
(
  sexps2RawJournal,
  RawJournal,
  module Plainledger.Parser.Sexp
) where

import Data.Decimal
import Data.Text as T hiding (filter, map)
import Data.Time
import Data.List as L
import qualified Data.Map.Strict as M
import Control.Monad
import Plainledger.Parser.Sexp
import Plainledger.Data.Type
import Plainledger.Error


-- (<?>) :: Either Error a -> Error -> Either Error a
-- Left _ <?> msg = Left msg
-- x <?> _ = x

sexp2list :: Sexp -> Either Error [Sexp]
sexp2list (SList _ s) = return s
sexp2list x = Left $ sourcePosPretty (sexpSourcePos x) ++ " expecting a sexp"

sexp2symbol :: Sexp -> Either Error Text
sexp2symbol (SSymbol _ s) = return s
sexp2symbol x = Left $ sourcePosPretty (sexpSourcePos x) ++ " expecting a symbol"

sexp2decimal :: Sexp -> Either Error Decimal
sexp2decimal (SDecimal _ d) = return d
sexp2decimal x = Left $ sourcePosPretty (sexpSourcePos x) ++ " expecting a decimal"

sexp2integer :: Sexp -> Either Error Integer
sexp2integer s = do
  d <- sexp2decimal s
  if isInteger d
  then return (fst $ properFraction $ toRational d)
  else Left $ sourcePosPretty (sexpSourcePos s) ++ " expecting an integer"

  where isInteger d = roundTo 0 d == d

sexp2text :: Sexp -> Either Error Text
sexp2text (SString _ s) = return s
sexp2text x = Left $ sourcePosPretty (sexpSourcePos x) ++ " expecting a string"

sexp2nonemptytext :: Sexp -> Either Error Text
sexp2nonemptytext x = sexp2text x >>= guardEmptyText
 where guardEmptyText "" = Left $ sourcePosPretty (sexpSourcePos x) ++ " expecting a non empty string"
       guardEmptyText t = Right t

sexp2qualifiedname :: Sexp -> Either Error [Text]
sexp2qualifiedname s = sexp2nonemptytext s >>= (\t -> return $ T.splitOn ":" t)

sexp2day :: Sexp -> Either Error Day
sexp2day (SDate _ d) = return d
sexp2day x = Left $ sourcePosPretty (sexpSourcePos x) ++ " expecting a date (YYYY-MM-DD)"

sexp2tag :: Sexp -> Either Error Tag
sexp2tag (SString pos k) = return $ Tag k Nothing pos
sexp2tag (SList pos [(SString _ k)]) = return $ Tag k Nothing pos
sexp2tag (SList pos [(SString _ k),(SString _ v)]) = return $ Tag k (Just v) pos
sexp2tag (SList pos [(SString _ k),(SSymbol _ s)]) = return $ Tag k (Just s) pos
sexp2tag (SList pos [(SString _ k),(SDecimal _ n)]) = return $ Tag k (Just $ T.pack $ show n) pos
sexp2tag (SList pos [(SString _ k),(SDate _ d)]) = return $ Tag k (Just $ T.pack $ show d) pos
sexp2tag x = Left  $ sourcePosPretty (sexpSourcePos x) ++ " Ill formed tag"

sexp2tags :: Sexp -> Either Error [Tag]
sexp2tags (SList _ xs) = mapM sexp2tag xs
sexp2tags x = Left  $ sourcePosPretty (sexpSourcePos x) ++ " Ill formed tags"

sexp2keyvalue :: [Sexp] -> Either Error (M.Map Text Sexp)
sexp2keyvalue xs = do
  xsByPair <- makePair xs
  foldM foo M.empty xsByPair

  where foo :: M.Map Text Sexp -> (Sexp, Sexp) -> Either Error (M.Map Text Sexp)
        foo m (k, v) = do
          k' <- sexp2symbol k
          case M.lookup k' m of
            Nothing -> return $ M.insert k' v m
            Just _ -> Left $ sourcePosPretty (sexpSourcePos k) ++ " Duplicate key " ++ T.unpack k'

        makePair :: [Sexp] -> Either Error [(Sexp, Sexp)]
        makePair [] = return []
        makePair (a : b : ys) = makePair ys >>= \r -> return ((a, b) : r) 
        makePair (x : _)  = Left $ sourcePosPretty (sexpSourcePos x) ++ " Ill formed key value pair. Odd number of sexpressions"


sexp2account :: Day -> Sexp -> Either Error OpenAccount
sexp2account day (SList pos (name' : xs)) = do
  name <- sexp2qualifiedname name'
  keyValue <- sexp2keyvalue xs
  tags <- maybe (return []) sexp2tags $ M.lookup ":tags" keyValue 
  number <- maybe (return Nothing) (fmap Just . sexp2integer) $ M.lookup ":number" keyValue
  commodity <- maybe (return []) (fmap return . sexp2nonemptytext) $ M.lookup ":commodity" keyValue
  return $ OpenAccount day
                       name
                       tags
                       number
                       commodity
                       pos  
sexp2account _ s = Left $ sourcePosPretty (sexpSourcePos s) ++ " Ill formed account in open statement"

sexp2posting :: Sexp -> Either Error RawPosting
sexp2posting (SList pos [account]) = do
  a <- sexp2qualifiedname account
  return $ RawPosting a (RawAmount Nothing Nothing) pos

sexp2posting (SList pos [account, (SDecimal _ d)]) = do
  a <- sexp2qualifiedname account
  return $ RawPosting a (RawAmount Nothing (Just d)) pos

sexp2posting (SList pos [account, c@(SString _ _)]) = do
  a <- sexp2qualifiedname account
  c' <- sexp2nonemptytext c 
  return $ RawPosting a (RawAmount (Just c') Nothing) pos

sexp2posting (SList pos [account, decimal, commodity]) = do
  a <- sexp2qualifiedname account
  d <- sexp2decimal decimal
  c <- sexp2nonemptytext commodity
  return $ RawPosting a (RawAmount (Just c) (Just d)) pos

sexp2posting s = Left $ sourcePosPretty (sexpSourcePos s) ++ " Ill formed posting"
  
sexp2postings :: Sexp -> Either Error [RawPosting]
sexp2postings (SList pos []) = Left $ sourcePosPretty pos ++ " Empty postings in transaction"
sexp2postings (SList _ xs) = mapM sexp2posting xs
sexp2postings x = Left  $ sourcePosPretty (sexpSourcePos x) ++ " Ill formed postings"

sexps2RawJournal :: [Sexp] -> Either Error RawJournal
sexps2RawJournal xs = foldM sexp2RawJournal (RawJournal Nothing [] [] [] []) xs
      
sexp2RawJournal :: RawJournal -> Sexp -> Either Error RawJournal

sexp2RawJournal (RawJournal (Just _) _ _ _ _) (SList pos ((SSymbol _ "configuration") : _)) =
  Left $ sourcePosPretty pos ++ " Duplicate configuration."

sexp2RawJournal j (SList pos ((SSymbol _ "configuration") : xs)) = do
  keyValue <- sexp2keyvalue xs
  currency <- lookup' ":main-currency" keyValue >>= sexp2text
  mapping <- lookup' ":account-type" keyValue >>= sexp2list >>= sexp2keyvalue >>= mapM sexp2text
             >>= checkMapping
  openingbalance <- lookup' ":opening-balance-account" keyValue >>= sexp2qualifiedname
  return $ j{rjConfiguration = Just (Configuration currency mapping openingbalance pos)}

  where lookup' :: String -> M.Map Text Sexp -> Either Error Sexp
        lookup' s keyValue =
          maybe (Left $ sourcePosPretty pos ++ " Missing " ++ s ++ " in configuration")
                return
                (M.lookup (T.pack s) keyValue)

        checkMapping :: M.Map Text Text -> Either Error (M.Map Text AccountType)
        checkMapping m =
          if M.size m /= 5
          then Left $ sourcePosPretty pos ++ " Expecting only 5 keys (:asset, :liability, :equity, :revenue, :expense) for the account-type mapping"
          else fmap M.fromList $ sequence [findKeyword ":asset" Asset,
                                           findKeyword ":liability"  Liability,
                                           findKeyword ":equity"  Equity,
                                           findKeyword ":revenue" Revenue,
                                           findKeyword ":expense" Expense]
          where
            findKeyword :: Text -> AccountType ->  Either Error (Text, AccountType)
            findKeyword t at =
              case M.lookup t m of
                Nothing -> Left $ sourcePosPretty pos ++ " Missing " ++ T.unpack t ++ " in account-type mapping"
                Just v -> return (v, at)
          
sexp2RawJournal j (SList _ ((SSymbol _ "open") : date : xs)) = do
  day <- sexp2day date
  accounts <- mapM (sexp2account day) xs
  return $ j{rjOpenAccounts = accounts ++ rjOpenAccounts j}

sexp2RawJournal j (SList pos ((SSymbol _ "transaction") : date : xs))  = do
  day <- sexp2day date
  keyValueParse day

  where keyValueParse day = do
          keyValue <- sexp2keyvalue xs
          tags <- maybe (return []) sexp2tags $ M.lookup ":tags" keyValue
          postings <- maybe noPostingError sexp2postings $ M.lookup ":postings" keyValue
          return $ j{rjTransactions = (RawTransaction day tags postings pos) : rjTransactions j}

        noPostingError = Left $ sourcePosPretty pos ++ " No postings in transaction"

sexp2RawJournal j (SList pos ((SSymbol _ "balance") : date : name : quantity : comm)) = do
  day <- sexp2day date
  account <- sexp2qualifiedname name
  d <- sexp2decimal quantity
  c <- if L.null comm then (return Nothing) else sexp2nonemptytext (L.head comm) >>= (return . Just)
  return $ j{rjBalanceAssertions = (RawBalanceAssertion day account (RawQuantity c d) pos) : rjBalanceAssertions j}

sexp2RawJournal j (SList pos ((SSymbol _ "close") : date : xs)) = do
  day <- sexp2day date
  accounts <- mapM sexp2qualifiedname xs
  let cs = map (\t -> CloseAccount day t pos) accounts
  return $ j{rjCloseAccounts = cs ++ rjCloseAccounts j}
  
sexp2RawJournal _ x = Left $ sourcePosPretty (sexpSourcePos x) ++ " Unknown entry. Expecting configuration, open, transaction, balance or close statement."
  
