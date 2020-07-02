-- |
-- Module      :  Plainledger.Journal.Configuration
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Configuration object of the journal file

module Plainledger.Journal.Configuration (
  Configuration(..),
  validateConfig
  )
where

import Control.Monad.Except
import Data.Aeson as A
import Plainledger.Error
import qualified Data.Text as T
import qualified Data.Yaml as Y

-- | The Configuration of the ledger
data Configuration = Configuration {
   -- | The account in the balance sheet that contains the total of all the
   -- other balance sheet accounts at the start of the financial period.
   cOpeningBalanceAccount :: T.Text,
   -- | The account in the balance sheet that contains the total of Revenue and
   -- Expense
   cEarningsAccount :: T.Text
  }
  deriving (Eq, Show)


-- | validateConfig asserts
-- - all members of the group mapping are non null
-- - opening balance account is non null
-- - earnings account is non null
-- - default commodity is non null
validateConfig :: (MonadError Error m) => Configuration -> m ()
validateConfig c = do
    checkNull cOpeningBalanceAccount "opening-balance-account"
    checkNull cEarningsAccount "earnings-account"

  where checkNull f n =
          if T.null (f c)
          then throwError
               $ "Configuration \""
               ++ n
               ++ "\" cannot be the empty string."
          else return ()

-- FromJSON instances
instance FromJSON Configuration where
  parseJSON (Y.Object v) =
    Configuration
    <$> v .: "opening-balance-account"
    <*> v .: "earnings-account"
  parseJSON _ = fail "Expected Object for Configuration value"

-- To JSON instance
instance ToJSON Configuration where
  toJSON (Configuration openBal earnAcc) =
    Y.object
    $ ["opening-balance-account" .= openBal,
       "earnings-account" .= earnAcc]

  toEncoding (Configuration openBal earnAcc) =
    pairs
    $ "opening-balance-account"   .= openBal
    <> "earnings-account"   .= earnAcc
