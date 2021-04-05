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
  changePostingDate,
  setAccount
  )
where

import Data.Maybe
import Control.Monad.Except
import Data.Time
import Data.List ( partition )
import Plainledger.Error.Error
import Plainledger.Journal.Amount
import Prelude hiding (lines)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Plainledger.Journal.Account (Account)

-- | The Posting data type reprensents the change in the balance of an account.
-- Transactions are made of at least two postings.
data PostingF a q = Posting
  {
    pFileOrder :: Int, -- | Records to position of the posting in the journal
                       -- | so we can print them back the way the user input
                       -- | them
    pBalanceDate :: Day,
    pAccount :: a,
    pAmount :: q
  } deriving (Eq, Show, Functor)

type JPosting = PostingF T.Text (Maybe Quantity)
type Posting = PostingF Account Quantity

changePostingDate :: Day -> PostingF a p -> PostingF a p
changePostingDate d (Posting order _ acc amnt) = Posting order d acc amnt

-- | Updates the amount if it is Nothing
setAmount :: Quantity -> PostingF a (Maybe Quantity) -> PostingF a Quantity
setAmount txAmount (Posting order balDate acc amnt) =
  let a = fromMaybe txAmount amnt
  in Posting order balDate acc a

-- | Updates the amount if it is Nothing
setAccount :: (MonadError Errors m) =>
              HM.HashMap T.Text Account -> 
              PostingF T.Text q -> 
              m (PostingF Account q)
setAccount m (Posting order balDate acc amnt) =
  let idNumber = HM.lookup acc m
  in case idNumber of
      Nothing -> throwError $ mkErrorNoPos $ AccountIdNotInAccountFile $ T.unpack acc
      (Just x) -> return $ Posting order balDate x amnt

-- | Asserts a zero balance of all the postings
balancePostings :: (MonadError Errors m) =>
                    [PostingF a (Maybe Quantity)] ->
                    m [PostingF a Quantity]
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