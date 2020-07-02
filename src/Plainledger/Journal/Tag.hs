-- |
-- Module      :  Plainledger.Journal.Tag
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Tags that can strore user information not needed by
-- plainledger

module Plainledger.Journal.Tag (
  Tag,
  pattern Tag,
  tagId,
  tagValue,
  tagToHashMap
  )
where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)

-- | The Tag datatype representing metadata provided by the user.
type Tag = (T.Text, T.Text)

pattern Tag :: T.Text -> T.Text -> Tag
pattern Tag a b = (a, b)

tagId :: Tag -> T.Text
tagId = fst

tagValue :: Tag -> T.Text
tagValue = snd

tagToHashMap :: [Tag] -> HashMap T.Text T.Text
tagToHashMap = HM.fromList
