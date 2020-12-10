-- |
-- Module      :  Plainledger.SourcePos
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the SourcePos data type

module Plainledger.SourcePos
(
  SourcePos(..)
) where



-- | Source position in the csv file. The column refers to the ith csv column, not the column position
-- in the source file
data SourcePos = SourcePos {
  spFile :: String, 
  spRow :: Int,
  spColumn :: Int
}