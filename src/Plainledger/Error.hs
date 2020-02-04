-- |
-- Module      :  Plainledger.Error
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type

module Plainledger.Error
(
  Error
) where

-- | For now, errors are simply strings detailing the problem
type Error = String
