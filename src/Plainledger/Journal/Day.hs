-- |
-- Module      :  Plainledger.Journal.Tag
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines helper functions for the Day data type

module Plainledger.Journal.Day (
  DateSpan,
  spanStartDate,
  spanEndDate,
  toISO8601,
  parseISO8601M
  )
where

import Data.Time
import Plainledger.Error
import Control.Monad.Except

type DateSpan = (Day, Day)

spanStartDate :: DateSpan -> Day
spanStartDate = fst

spanEndDate :: DateSpan -> Day
spanEndDate = snd

toISO8601 :: Day -> String
toISO8601 = formatTime defaultTimeLocale (iso8601DateFormat Nothing)

parseISO8601M :: (MonadError Errors m) => String -> m Day
parseISO8601M s =
  let d = parseTimeM False defaultTimeLocale
          (iso8601DateFormat Nothing) s
  in case d of
       Nothing -> throwError $ mkErrorNoPos $ ParseDateErr s
       Just d' -> return d'
