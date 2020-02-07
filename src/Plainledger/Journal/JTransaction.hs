-- |
-- Module      :  Plainledger.Journal.Transfer
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the JTransaction data type representing the transactions
-- entered by the user in its journal file

module Plainledger.Journal.JTransaction (
  JTransfer(..),
  JTransaction(..),
  jtransactionToTransfer
  )
where

import Data.Time
import Data.Maybe
import Data.Scientific
import Data.Aeson (pairs)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Generics hiding (from, to)
import Control.Monad.Except
import Plainledger.Ledger
import Plainledger.Error

-- | The TransferF data type reprensents the flow of one commodity from
-- one account to another.
data JTransfer = JTransfer
  {
    jtfDate :: Maybe Day,
    jtfBalanceDateFrom :: Maybe Day,
    jtfBalanceDateTo :: Maybe Day,
    jtfFrom :: T.Text,
    jtfTo   :: T.Text,
    jtfAmount :: Quantity,
    jtfCommodity :: Commodity,
    jtfTags :: [Tag]
  } deriving (Eq, Show, Generic)

instance ToJSON JTransfer where
  toJSON (JTransfer date balDateFrom balDateTo from to amnt comm tags) =
    Y.object
    $ ["from" .= from,
       "to" .= to,
       "amount" .= (realToFrac amnt :: Scientific)]
   ++ (if isNothing date then [] else ["date" .= date])
   ++ (if isNothing balDateFrom || balDateFrom == date
       then []
       else ["balance-date-from" .= balDateFrom])
   ++ (if isNothing balDateTo || balDateTo == date
       then []
       else ["balance-date-to" .= balDateTo])
   ++ (if T.null comm then [] else ["commodity" .= comm])
   ++ (if null tags then [] else ["tags" .= tags])

  toEncoding (JTransfer date balDateFrom balDateTo from to amnt comm tags) =
    pairs
    $ (if isNothing date
        then mempty
        else "date" .= date)
    <> (if isNothing balDateFrom || balDateFrom == date
        then mempty
        else "balance-date-from" .= balDateFrom)
    <> (if isNothing balDateTo || balDateTo == date
        then mempty
        else "balance-date-to" .= balDateTo)
    <> "from" .= from
    <> "to" .= to
    <> "amount" .= (realToFrac amnt :: Scientific)
    <> (if T.null comm then mempty else "commodity" .= comm)
    <> (if null tags then mempty else "tags" .= tags)

instance FromJSON JTransfer where
  parseJSON (Y.Object v) =
    JTransfer
    <$> (v .:? "date")
    <*> (v .:? "balance-date-from" >>= maybe (v .:? "date") return)
    <*> (v .:? "balance-date-to" >>= maybe (v .:? "date") return)
    <*> v .: "from"
    <*> v .: "to"
    <*> (v .: "amount" >>= Y.withScientific "amount" (return . realToFrac))
    <*> (maybe "" id <$> v .:? "commodity")
    <*> (maybe [] id <$> v .:? "tags")
  parseJSON _ = fail "Expected Object for Transfer value"

data JTransaction = JTransaction
  {
    jtDate :: Maybe Day,
    jtBalanceDateFrom :: Maybe Day,
    jtBalanceDateTo :: Maybe Day,
    jtTransactionId :: T.Text,
    jtTransfers :: [JTransfer],
    jtTags :: [Tag]
  } deriving (Eq, Show, Generic)

-- | Transforms a JTransaction into the corresponding list of Transfer by
-- mapping each of its JTransfer. The date, balance-date-from, balance-date-to
-- of each JTransfer will be used in priority to create the Transfer. The tags
-- of the JTransfer and JTransaction will be merge, keeping the value of
-- JTransfer in case of duplicates. The only exception is the "Transaction id"
-- tag that will contain the value of transaction id field of the JTransaction.
-- Therefore, The transaction id must be set prior to this function.
jtransactionToTransfer :: (MonadError Error m) =>
                          JTransaction ->
                          m [Transfer]
jtransactionToTransfer t = traverse jtransferToTransfer (jtTransfers t)
   where jtransferToTransfer :: (MonadError Error m) => JTransfer -> m Transfer
         jtransferToTransfer jt = do
           d <- selectDate (jtfDate jt) (jtDate t) "date"
           dFrom <- selectDate (jtfBalanceDateFrom jt) (jtBalanceDateFrom t)
                    "balance-date-from"
           dTo <- selectDate (jtfBalanceDateFrom jt) (jtBalanceDateTo t)
                  "balance-date-to"
           let tags = map tupleToTag
                    $ HM.toList
                    $ HM.insert "Transaction id" (jtTransactionId t)
                    $ HM.fromList (map tagToTuple $ jtTags t ++ jtfTags jt)
           return (Transfer d dFrom dTo (jtfFrom jt) (jtfTo jt) (jtfAmount jt)
                   (jtfCommodity jt) tags)

         selectDate :: (MonadError Error m) =>
                      Maybe Day ->
                      Maybe Day -> String -> m Day
         selectDate (Just d) _ _ = return d
         selectDate _ (Just d) _ = return d
         selectDate _ _ n = throwError
                     $ "No "
                     ++ n
                     ++ " in the transaction or the transfer"

instance ToJSON JTransaction where
  toJSON (JTransaction date balDateFrom balDateTo tId xfers tags) =
    Y.object
    $ ["transfers" .= xfers]
   ++ (if T.null tId then [] else ["transaction-id" .= tId])
   ++ (if isNothing date then [] else ["date" .= date])
   ++ (if isNothing balDateFrom || balDateFrom == date
       then []
       else ["balance-date-from" .= balDateFrom])
   ++ (if isNothing balDateTo || balDateTo == date
       then []
       else ["balance-date-to" .= balDateTo])
   ++ (if null tags then [] else ["tags" .= tags])

  toEncoding (JTransaction date balDateFrom balDateTo tId xfers tags) =
    pairs
    $ (if isNothing date
        then mempty
        else "date" .= date)
    <> (if isNothing balDateFrom || balDateFrom == date
        then mempty
        else "balance-date-from" .= balDateFrom)
    <> (if isNothing balDateTo || balDateTo == date
        then mempty
        else "balance-date-to" .= balDateTo)
    <> (if T.null tId then mempty else "transaction-id" .= tId)
    <> "transfers" .= xfers
    <> (if null tags then mempty else "tags" .= tags)

instance FromJSON JTransaction where
  parseJSON (Y.Object v) =
    JTransaction
    <$> (v .:? "date")
    <*> (v .:? "balance-date-from" >>= maybe (v .:? "date") return)
    <*> (v .:? "balance-date-to" >>= maybe (v .:? "date") return)
    <*> (maybe "" id <$> v .:? "transaction-id")
    <*> v .: "transfers"
    <*> (maybe [] id <$> v .:? "tags")
  parseJSON _ = fail "Expected Object for Transaction value"
