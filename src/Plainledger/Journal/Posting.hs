-- |
-- Module      :  Plainledger.Journal.JPosting
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Posting data type.

module Plainledger.Journal.Posting (
  PostingF(..),
  JPosting,
  fromCommodity,
  fromAmount,
  fromBalanceDate,
  setPostingDate
  )
where

import Data.Maybe
import Control.Monad.Except
import Data.Aeson (pairs)
import Data.List
import Data.Scientific
import GHC.Generics hiding (to, from)
import Data.Time
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import Plainledger.Journal.Amount
import Prelude hiding (lines)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import Data.Bifunctor

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data PostingF d1 d2 q = Posting
  {
    pDate :: d1,
    pBalanceDate :: d2,
    pAccount :: T.Text,
    pAmount :: q,
    pCommodity :: Commodity
  } deriving (Eq, Show, Generic, Functor)

instance Bifunctor (PostingF x) where
  first f (Posting d1 b a amnt c) = Posting d1 (f b) a amnt c
  second f (Posting d1 b a amnt c) = Posting d1 b a (f amnt) c

type JPosting = PostingF () (Maybe Day) (Maybe Quantity)


setPostingDate :: d1' -> PostingF d1 d2 q -> PostingF d1' d2 q
setPostingDate d (Posting _ balDate acc amnt comm) = Posting d balDate acc amnt comm

-- | Updates the balance date if it is Nothing
fromBalanceDate :: Day -> PostingF d1 (Maybe Day) q -> PostingF d1 Day q
fromBalanceDate txDate (Posting date balDate acc amnt comm) =
  let d = fromMaybe txDate balDate
  in Posting date d acc amnt comm

-- | Updates the amount if it is Nothing
fromAmount :: Quantity -> PostingF d1 d2 (Maybe Quantity) -> PostingF d1 d2 Quantity
fromAmount txAmount (Posting date balDate acc amnt comm) =
  let a = fromMaybe txAmount amnt
  in Posting date balDate acc a comm

-- | Updates the commodity if it is null
fromCommodity :: Commodity -> PostingF d1 d2 q -> PostingF d1 d2 q
fromCommodity txComm (Posting date balDate acc amnt comm) =
  let c = if T.null comm then txComm else comm
  in Posting date balDate acc amnt c


instance ToJSON JPosting where
  toJSON (Posting _ date account amnt comm) =
    Y.object
    $ ["account" .= account]
   ++ (maybe [] (\a -> ["amount" .= (realToFrac a :: Scientific)]) amnt)
   ++ (if T.null comm then [] else ["commodity" .= comm])
   ++ (maybe [] (\a -> ["balance-date" .= a]) date)

  toEncoding (Posting _ date account amnt comm) =
    pairs
    $ "account" .= account
    <> (maybe mempty (\a -> "amount" .= (realToFrac a :: Scientific)) amnt)
    <> (if T.null comm then mempty else "commodity" .= comm)
    <> (maybe mempty (\a -> "balance-date" .= a) date)

instance FromJSON JPosting where
  parseJSON (Y.Object v) =
    Posting ()
    <$> (v .:? "balance-date")
    <*> v .: "account"
    <*> (fmap (fmap realToFrac) (v .:? "amount" :: Y.Parser (Maybe Scientific)))
    <*> (maybe "" id <$> v .:? "commodity")
  parseJSON _ = fail "Expected Object for Posting value"
