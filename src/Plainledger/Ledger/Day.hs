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
import Plainledger.Error
import Control.Monad.Except

toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

parseISO8601M :: (MonadError Error m) => String -> m Day
parseISO8601M s =
  let d = parseTimeM False defaultTimeLocale
          (iso8601DateFormat Nothing) s
  in case d of
       Nothing -> throwError $ "Unable to parse date \"" ++ s ++ "\"."
       Just d' -> return d'
