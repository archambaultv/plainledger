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
  balancePostings,
  postingToJPosting
  )
where

import Data.Maybe
import Data.List
import Control.Monad.Except
import Data.Time
import Plainledger.Journal.Amount
import Plainledger.Journal
import Plainledger.Error
import Prelude hiding (lines)
import Data.Bifunctor

type Posting = PostingF Day Day Quantity

postingToJPosting :: PostingF d1 d2 q -> PostingF () (Maybe d2) (Maybe q)
postingToJPosting = setPostingDate () . first Just . fmap Just

-- | Asserts a zero balance
balancePostings :: (MonadError Error m) =>
                    [PostingF Day Day (Maybe Quantity)] ->
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
