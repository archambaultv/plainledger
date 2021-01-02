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
  module Plainledger.Error.SourcePos,
  module Plainledger.Error.Error
) where

import Plainledger.Error.SourcePos
import Plainledger.Error.Error