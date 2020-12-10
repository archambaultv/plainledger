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
  Error(..),
  ErrorType(..),
  mkErrorPos,
  mkError,
  addErrorPos,
  printError,
  printErrorType,
) where

import Plainledger.SourcePos

-- | Errors, with or without a source file
data Error = Error [SourcePos] ErrorType

-- | List all possibles error in plainledger
data ErrorType
  = CustomErr String
  | DuplicateAccountIdErr
  | ParseDateErr String

mkErrorPos :: SourcePos -> ErrorType -> Error
mkErrorPos pos e -> Error [pos] e

mkError :: ErrorType -> Error
mkError e = Error Nothing e

mkCustomErr :: String -> Error
mkCustomErr s =  Error [] (CustomErr s)

addErrorPos :: SourcePos -> Error -> Error
addErrorPos s (Error p e) = Error (s:p) e

-- | Pretty print the error message and add the source file information
printError :: Error -> String
printError (Error [] e) = "error:\n" ++ printErrorType e
printError (Error [pos] e) = showSourcePos pos ++ " error:\n" ++ printErrorType e
printError (Error (p:ps) e) 
  = printError (Error [p] e)
  ++ "error also located at:\n  "
  ++ intercalate "\n  " (map showSourcePos ps)

-- | Pretty print the error message
printErrorType :: Error -> String
printErrorType (CustomErr s) = s
printErrorType (ParseDateErr s) 
  = "Unable to parse date \"" ++ s ++ "\".\nDate must be in the YYYY-MM-DD format"
printErrorType _ = "Unknown error"

showSourcePos :: SourcePos -> String
showSourcePos (SourcePos f r _) | r <= 0 = f
showSourcePos (SourcePos f r c)
  = f
  ++ ":"
  ++ show r
  ++ ":"
  ++ show c