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
  Posting,
  balancePostings,
  changePostingDate
  )
where

import Data.Maybe
import Control.Monad.Except
import Data.Time
import Data.List
import Plainledger.Error.Error
import Plainledger.Journal.Amount
import Prelude hiding (lines)
import qualified Data.Text as T

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data PostingF q = Posting
  {
    pBalanceDate :: Day,
    pAccount :: T.Text,
    pAmount :: q
  } deriving (Eq, Show, Functor)

type JPosting = PostingF (Maybe Quantity)
type Posting = PostingF Quantity

changePostingDate :: Day -> PostingF a -> PostingF a
changePostingDate d (Posting _ acc amnt) = (Posting d acc amnt)

-- | Updates the amount if it is Nothing
setAmount :: Quantity -> PostingF (Maybe Quantity) -> PostingF Quantity
setAmount txAmount (Posting balDate acc amnt) =
  let a = fromMaybe txAmount amnt
  in Posting balDate acc a

-- | Asserts a zero balance of all the postings
balancePostings :: (MonadError Errors m) =>
                    [JPosting] ->
                    m [Posting]
balancePostings [] =
  throwError $ mkErrorNoPos ZeroOrOnePostingOnly
balancePostings [_] =
  throwError $ mkErrorNoPos ZeroOrOnePostingOnly
balancePostings ps =
  let (noAmount, withAmount)  = partition (isNothing . pAmount) ps
      withAmount' = map (\p -> p{pAmount = fromJust (pAmount p)}) withAmount
      s :: Quantity
      s = sum $ map pAmount withAmount'
  in case noAmount of
        [] -> if s == 0
              then return withAmount'
              else throwError
                   $ mkErrorNoPos (UnbalancedTransaction s)
        [x] -> let x' = setAmount (negate s) x
               in return $ x' : withAmount'
        _ -> throwError $ mkErrorNoPos TwoOrMorePostingsWithoutAmount