-- |
-- Module      :  Plainledger.Internal.Utils
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines a few useful functions

module Plainledger.Internal.Utils
(
  findDuplicates,
) where

import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
    
-- | Returns the list of duplicates
findDuplicates :: (Eq a, Hashable a) => [a] -> [a]
findDuplicates xs = HM.keys
                  $ HM.filter (/= 1)
                  $ HM.fromListWith (+)
                  $ zip xs (repeat (1 :: Int))
