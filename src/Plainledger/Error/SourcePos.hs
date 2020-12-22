-- |
-- Module      :  Plainledger.Error.SourcePos
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the SourcePos data type

module Plainledger.Error.SourcePos
(
  SourcePos(..)
) where



-- | Source position in the csv file. The column refers to the ith csv column, not the column position
-- in the source file. A value of 0 for spRow or spColumn means we do not have this information.
data SourcePos = SourcePos {
  spFile :: String, 
  spCsvRow :: Int,
  spCsvColumn :: Int
}
  deriving (Eq, Show)