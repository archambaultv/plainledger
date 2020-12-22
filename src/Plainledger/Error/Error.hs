-- |
-- Module      :  Plainledger.Error.Errors
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type


module Plainledger.Error.Error
(
  Error(..),
  ErrorType(..),
  mkError,
  mkErrorNoPos,
  addErrorPos,
  setSourcePosFileIfNull,
) where

import Plainledger.Error.SourcePos

-- | Errors, with or without source locations.
data Error = Error [SourcePos] ErrorType
  deriving (Eq, Show)

-- | List all possibles error in plainledger
data ErrorType
  = ErrorMessage String -- | Used when the error of a dependency is a string
  | ParseDateErr String
  | ParseIntErr String
  | ParseCharErr String
  | EmptyJournalFile
  | EmptyFieldInJournalFile String
  | MissingFieldinJournalFile String
  | UnknownFieldinJournalFile String
  | InvalidHeaderJournalFile
  deriving (Eq, Show)

mkError :: SourcePos -> ErrorType -> Error
mkError pos e = Error [pos] e

mkErrorNoPos :: ErrorType -> Error
mkErrorNoPos e = Error [] e

addErrorPos :: SourcePos -> Error -> Error
addErrorPos s (Error p e) = Error (s:p) e

setSourcePosFileIfNull :: String -> Error -> Error
setSourcePosFileIfNull f (Error sp e) = 
  let sp' = map (\x -> if null (spFile x) then x{spFile = f} else x) sp
  in Error sp' e