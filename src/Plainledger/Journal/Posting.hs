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
  fromBalanceDate
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
data PostingF d2 q = Posting
  {
    pBalanceDate :: d2,
    pAccount :: T.Text,
    pAmount :: q
  } deriving (Eq, Show, Generic, Functor)

instance Bifunctor PostingF where
  first f (Posting b a amnt) = Posting (f b) a amnt
  second f (Posting b a amnt) = Posting b a (f amnt)

type JPosting = PostingF (Maybe Day) (Maybe Quantity)

-- | Updates the balance date if it is Nothing
fromBalanceDate :: Day -> PostingF (Maybe Day) q -> PostingF Day q
fromBalanceDate txDate (Posting balDate acc amnt) =
  let d = fromMaybe txDate balDate
  in Posting d acc amnt

-- | Updates the amount if it is Nothing
fromAmount :: Quantity -> PostingF d2 (Maybe Quantity) -> PostingF d2 Quantity
fromAmount txAmount (Posting balDate acc amnt) =
  let a = fromMaybe txAmount amnt
  in Posting balDate acc a
