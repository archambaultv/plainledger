-- |
-- Module      :  Plainledger.Ledger.Transaction
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Transaction data type.

module Plainledger.Ledger.Transaction (
  TransactionF(..),
  Transaction
  )
where

import Data.Ord
import Data.List
import Data.Time
import GHC.Generics hiding (to, from)
import Plainledger.Ledger.Tag
import Plainledger.Ledger.Posting
import Prelude hiding (lines)
import qualified Data.Text as T

data TransactionF p = Transaction
  {
    tDate :: Day,
    tTransactionId :: T.Text,
    tPostings :: [p],
    tTags :: [Tag]
  } deriving (Show, Generic, Functor)

-- / We sort the tags when comparing two TransactionF
-- The Eq instance is mainly used in the unittests. In a validated ledger,
-- you can rely on the aId to identify an account.
instance (Eq p) => Eq (TransactionF p) where
  a1 == a2 =  tDate a1 == tDate a2
           && tTransactionId a1 == tTransactionId a2
           && tPostings a1 == tPostings a2
           && sortBy (comparing tagId) (tTags a1) ==
              sortBy (comparing tagId) (tTags a2)

type Transaction = TransactionF Posting
