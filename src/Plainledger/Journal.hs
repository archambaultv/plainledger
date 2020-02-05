-- |
-- Module      :  Plainledger.Ledger
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the journal data type and reexports all the
-- data types and functions related to the journal file.

module Plainledger.Journal (
  Journal(..),
  journalToLedger,
  module Plainledger.Journal.JTransaction
  )
where

import Data.Time
import Data.Function
import Control.Monad.Except
import Data.Aeson (pairs)
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Ord
import Data.Yaml (FromJSON(..), (.:), ToJSON(..), (.=))
import Plainledger.Error
import Plainledger.Internal.Utils
import Plainledger.Journal.JTransaction
import Plainledger.Ledger
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Yaml.Pretty as P

-- | The Ledger data type represents a graph where the accounts are the nodes
-- and the transfers are the edges
data Journal = Journal
  {jConfiguration :: Configuration,
   jAccounts   :: [Account],
   jTransactions :: [JTransaction],
   jBalances :: [Balance]
  }
  deriving (Eq, Show)

-- | Converts the journal to a ledger. Enforces the validity of the ledger with
-- 'validateLedger'
journalToLedger :: (MonadError Error m) => Journal -> m Ledger
journalToLedger (Journal config accounts txns bals) = do
  txns' <- validateJTransactionsId txns
  trfs <- concat <$> traverse jtransactionToTransfer txns'
  let l = Ledger config accounts trfs bals
  validateLedger l

-- Create an id of the form YYYY-MM-DD-N where N is a number if the field
-- transaction id is null
validateJTransactionsId :: (MonadError Error m) =>
                           [JTransaction] -> m [JTransaction]
validateJTransactionsId ts = do
    let (noId, withId) = partition (T.null . jtTransactionId) ts
    validateTransactionsIdNoDup withId
    let knownIds = HS.fromList (map jtTransactionId ts)
    -- Now what we have to do :
    -- 1) Find a transaction date for the noId
    -- 2) Group by date
    -- 3) Add a number to the date without clashing with the knownIds to
    --    generate an id of the form YYYY-MM-DD-N
    withDate <- traverse findDate noId
    let groupDate = groupBy ((==) `on` fst) $ sortBy (comparing fst) withDate
    return $ concatMap (createId knownIds 1) groupDate

  where createId :: HS.HashSet T.Text ->
                    Int ->
                    [(Day, JTransaction)] ->
                    [JTransaction]
        createId _ _ [] = []
        createId knownIds n ((d, t):ts) =
          let tId = T.pack $ show d ++ "-" ++ show n
          in if HS.member tId knownIds
             then createId knownIds (n + 1) ((d,t):ts)
             else t{jtTransactionId = tId} : createId knownIds (n + 1) ts

        findDate :: (MonadError Error m) =>
                    JTransaction -> m (Day, JTransaction)
        findDate t =
          let dates = map jtfDate (jtTransfers t)
              allTransferWithDates = null $ filter isNothing dates
          in if allTransferWithDates
             then return $ (minimum $ catMaybes dates, t)
             else case jtDate t of
                    Nothing -> throwError "Transaction and at least one transfer without \"Date\" property"
                    Just d -> return $ (minimum $ d : catMaybes dates, t)

validateTransactionsIdNoDup :: (MonadError Error m) =>
                               [JTransaction] ->
                               m ()
validateTransactionsIdNoDup xs =
  let dup = findDuplicates (map jtTransactionId xs)
  in if null dup
     then return ()
     else throwError
          $ "Duplicate transaction id : "
          ++ (intercalate " "
             $ map (\k -> "\"" ++ T.unpack k ++ "\"")
             $ dup)
          ++ "."



-- FromJSON instances
instance FromJSON Journal where
  parseJSON (Y.Object v) =
    Journal
    <$> v .: "configuration"
    <*> v .: "accounts"
    <*> v .: "transactions"
    <*> v .: "balance-assertions"
  parseJSON _ = fail "Expected Object for Journal value"

-- To JSON instance
instance ToJSON Journal where
  toJSON (Journal config accounts txns bals) =
    Y.object
    $ ["configuration" .= config,
       "accounts" .= accounts,
       "transactions" .= txns,
       "balance-assertions" .= bals]

  toEncoding (Journal config accounts txns bals) =
    pairs
    $ "configuration"   .= config
    <> "accounts"   .= accounts
    <> "transactions" .= txns
    <> "balance-assertions" .= bals
