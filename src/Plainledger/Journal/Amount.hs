-- |
-- Module      :  Plainledger.Journal.Amount
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the type alias Quantity and Commodity

module Plainledger.Journal.Amount (
  Quantity
  )
where

import Data.Decimal

-- | A quantity is any decimal number. The decimal package ensures
-- that no rounding error can occur.
type Quantity = Decimal
