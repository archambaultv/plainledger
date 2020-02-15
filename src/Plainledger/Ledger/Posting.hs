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
  Posting
  )
where


import Control.Monad.Except
import Data.Time
import GHC.Generics hiding (to, from)
import Plainledger.Ledger.Amount
import Prelude hiding (lines)
import qualified Data.Text as T
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
