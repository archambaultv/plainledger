-- |
-- Module      :  Plainledger.Ledger.Posting
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Posting data type.

module Plainledger.Ledger.Posting (
  PostingF(..),
  Posting,
  JPosting,
  fromCommodity,
  fromAmount,
  fromBalanceDate,
  balancePostings,
  postingToJPosting
  )
where

import Data.Maybe
import Control.Monad.Except
import Data.Aeson (pairs)
import Data.List
import Data.Scientific
import Data.Time
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import GHC.Generics hiding (to, from)
import Plainledger.Error
import Plainledger.Ledger.Amount
import Prelude hiding (lines)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Bifunctor

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data PostingF d q = Posting
  {
    pBalanceDate :: d,
    pAccount :: T.Text,
    pAmount :: q,
    pCommodity :: Commodity
  } deriving (Eq, Show, Generic, Functor)

instance Bifunctor PostingF where
  first f (Posting b a amnt c) = Posting (f b) a amnt c
  second f (Posting b a amnt c) = Posting b a (f amnt) c

type Posting = PostingF Day Quantity
type JPosting = PostingF (Maybe Day) (Maybe Quantity)

postingToJPosting :: PostingF d q -> PostingF (Maybe d) (Maybe q)
postingToJPosting p = first Just $ fmap Just p

-- | Updates the balance date if it is Nothing
fromBalanceDate :: Day -> PostingF (Maybe Day) q -> PostingF Day q
fromBalanceDate txDate (Posting balDate acc amnt comm) =
  let d = fromMaybe txDate balDate
  in Posting d acc amnt comm

-- | Updates the amount if it is Nothing
fromAmount :: Quantity -> PostingF d (Maybe Quantity) -> PostingF d Quantity
fromAmount txAmount (Posting balDate acc amnt comm) =
  let a = fromMaybe txAmount amnt
  in Posting balDate acc a comm

-- | Updates the commodity if it is null
fromCommodity :: Commodity -> PostingF d q -> PostingF d q
fromCommodity txComm (Posting balDate acc amnt comm) =
  let c = if T.null comm then txComm else comm
  in Posting balDate acc amnt c

-- | Asserts a zero balance
balancePostings :: (MonadError Error m) =>
                    [PostingF Day (Maybe Quantity)] ->
                    m [Posting]
balancePostings [] =
  throwError "Expecting at least two postings per transaction."
balancePostings [_] =
  throwError "Expecting at least two postings per transaction."
balancePostings ps =
  let (noAmount, withAmount)  = partition (isNothing . pAmount) ps
      withAmount' = map (\p -> p{pAmount = fromJust (pAmount p)}) withAmount
      s :: Quantity
      s = sum $ map pAmount withAmount'
  in case noAmount of
        [] -> if s == 0
              then return withAmount'
              else throwError
                   $ "Unbalanced transaction. The balance is "
                   ++ show s
                   ++ " for commodity "
                   ++ (show $ pCommodity $ head withAmount)
                   ++ ". All transaction must balance to zero."
        [x] -> let x' :: Posting
                   x' = fromAmount (negate s) x
               in return $ x' : withAmount'
        _ -> throwError "Two postings without amount."



instance ToJSON JPosting where
  toJSON (Posting date account amnt comm) =
    Y.object
    $ ["account" .= account]
   ++ (maybe [] (\a -> ["amount" .= (realToFrac a :: Scientific)]) amnt)
   ++ (if T.null comm then [] else ["commodity" .= comm])
   ++ (maybe [] (\a -> ["balance-date" .= a]) date)

  toEncoding (Posting date account amnt comm) =
    pairs
    $ "account" .= account
    <> (maybe mempty (\a -> "amount" .= (realToFrac a :: Scientific)) amnt)
    <> (if T.null comm then mempty else "commodity" .= comm)
    <> (maybe mempty (\a -> "balance-date" .= a) date)

instance FromJSON JPosting where
  parseJSON (Y.Object v) =
    Posting
    <$> (v .:? "balance-date")
    <*> v .: "account"
    <*> (fmap (fmap realToFrac) (v .:? "amount" :: Y.Parser (Maybe Scientific)))
    <*> (maybe "" id <$> v .:? "commodity")
  parseJSON _ = fail "Expected Object for Posting value"
