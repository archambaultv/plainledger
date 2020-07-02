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
  fromAmount,
  fromBalanceDate,
  setPostingDate
  )
where

import Data.Maybe
import Control.Monad.Except
import GHC.Generics hiding (to, from)
import Data.Time
import Plainledger.Journal.Amount
import Prelude hiding (lines)
import qualified Data.Text as T
import Data.Bifunctor

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data PostingF d1 d2 q = Posting
  {
    pDate :: d1,
    pBalanceDate :: d2,
    pAccount :: T.Text,
    pAmount :: q
  } deriving (Eq, Show, Generic, Functor)

instance Bifunctor (PostingF x) where
  first f (Posting d1 b a amnt) = Posting d1 (f b) a amnt
  second f (Posting d1 b a amnt) = Posting d1 b a (f amnt)

type JPosting = PostingF () (Maybe Day) (Maybe Quantity)


setPostingDate :: d1' -> PostingF d1 d2 q -> PostingF d1' d2 q
setPostingDate d (Posting _ balDate acc amnt) = Posting d balDate acc amnt

-- | Updates the balance date if it is Nothing
fromBalanceDate :: Day -> PostingF d1 (Maybe Day) q -> PostingF d1 Day q
fromBalanceDate txDate (Posting date balDate acc amnt) =
  let d = fromMaybe txDate balDate
  in Posting date d acc amnt

-- | Updates the amount if it is Nothing
fromAmount :: Quantity -> PostingF d1 d2 (Maybe Quantity) -> PostingF d1 d2 Quantity
fromAmount txAmount (Posting date balDate acc amnt) =
  let a = fromMaybe txAmount amnt
  in Posting date balDate acc a
