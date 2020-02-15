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

module Plainledger.Ledger (
  LedgerF(..),
  Ledger,
  module Plainledger.Ledger.Posting,
  module Plainledger.Ledger.Transaction,
  module Plainledger.Ledger.Balance,
  module Plainledger.Ledger.Configuration,
  module Plainledger.Ledger.Account,
  module Plainledger.Ledger.Amount,
  module Plainledger.Ledger.Tag,
  module Plainledger.Ledger.Day
  )
where

import Plainledger.Ledger.Posting
import Plainledger.Ledger.Transaction
import Plainledger.Ledger.Balance
import Plainledger.Ledger.Configuration
import Plainledger.Ledger.Account
import Plainledger.Ledger.Amount
import Plainledger.Ledger.Tag
import Plainledger.Ledger.Day

data LedgerF t = Ledger
  {lConfiguration :: Configuration,
   lAccounts   :: [Account],
   lTransactions :: [t],
   lBalances :: [Balance]
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Ledger = LedgerF Transaction
