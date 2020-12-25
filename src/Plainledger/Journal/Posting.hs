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
  fromAmount
  )
where

import Data.Maybe
import Control.Monad.Except
import Data.Time
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

-- | Updates the amount if it is Nothing
fromAmount :: Quantity -> PostingF (Maybe Quantity) -> PostingF Quantity
fromAmount txAmount (Posting balDate acc amnt) =
  let a = fromMaybe txAmount amnt
  in Posting balDate acc a

-- | Asserts a zero balance
-- balancePostings :: (MonadError Error m) =>
--                     [PostingF d (Maybe Quantity)] ->
--                     m [PostingF d Quantity]
-- balancePostings [] =
--   throwError "Expecting at least two postings per transaction."
-- balancePostings [_] =
--   throwError "Expecting at least two postings per transaction."
-- balancePostings ps =
--   let (noAmount, withAmount)  = partition (isNothing . pAmount) ps
--       withAmount' = map (\p -> p{pAmount = fromJust (pAmount p)}) withAmount
--       s :: Quantity
--       s = sum $ map pAmount withAmount'
--       withAmountDesc :: String
--       withAmountDesc = intercalate "\n  " $ map buildDesc withAmount
--       buildDesc :: PostingF d (Maybe Quantity) -> String
--       buildDesc p = T.unpack (pAccount p) ++ " " ++ show (fromJust (pAmount p))
--   in case noAmount of
--         [] -> if s == 0
--               then return withAmount'
--               else throwError
--                    $ "Unbalanced transaction. The balance is "
--                    ++ show s ++".\n  " ++ withAmountDesc
--         [x] -> let x' = fromAmount (negate s) x
--                in return $ x' : withAmount'
--         _ -> throwError "Two postings without amount."