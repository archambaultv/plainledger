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
  removeBom,
  takeFirstLine
) where

import Data.Word
import qualified Data.ByteString as BS

-- | Remove the UTF8 BOM if present
removeBom :: BS.ByteString -> BS.ByteString
removeBom bs | BS.take 3 bs == BS.pack [0xEF,0xBB,0xBF] = BS.drop 3 bs
             | otherwise = bs


newline:: Word8
newline = 10

cr :: Word8
cr = 13

-- Return the first line and the rest stripped of the newline (carriage return)
-- characters
takeFirstLine :: BS.ByteString -> (BS.ByteString, BS.ByteString)
takeFirstLine "" = ("", "")
takeFirstLine bs =
  let (firstLine, rest) = BS.break (\c -> c == newline) bs
  in (stripCarriageReturn firstLine, stripNewLine rest)

  where stripCarriageReturn "" = ""
        stripCarriageReturn x | BS.last x == cr = BS.init x
        stripCarriageReturn x = x

        stripNewLine "" = ""
        stripNewLine x | BS.head x == newline = BS.tail x
        stripNewLine x = x