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
  Quantity,
  Commodity
  )
where

import Data.Decimal
import qualified Data.Text as T

-- | A quantity is any decimal number. The decimal package ensures
-- that no rounding error can occur.
type Quantity = Decimal

-- | A commodity is represented as Text
type Commodity = T.Text
