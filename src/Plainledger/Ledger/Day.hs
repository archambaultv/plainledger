-- |
-- Module      :  Plainledger.Ledger.Tag
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines helper functions for the Day data type

module Plainledger.Ledger.Day (
  toISO8601,
  parseISO8601M
  )
where

import Data.Time
import Control.Monad.Fail

toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

parseISO8601M :: (MonadFail m) => String -> m Day
parseISO8601M = parseTimeM False defaultTimeLocale
               (iso8601DateFormat Nothing)
