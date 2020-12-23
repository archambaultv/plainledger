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
  removeBom
) where

import qualified Data.ByteString as BS

-- | Remove the UTF8 BOM if present
removeBom :: BS.ByteString -> BS.ByteString
removeBom bs | BS.take 3 bs == BS.pack [0xEF,0xBB,0xBF] = BS.drop 3 bs
             | otherwise = bs

