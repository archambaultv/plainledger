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
  mkErrorPos,
  mkError,
  printError,
  printErrorType,
) where

import Plainledger.SourcePos

-- | Errors, with or without a source file
data Error = Error (Maybe SourcePos) ErrorType

-- | List all possibles error in plainledger
data ErrorType
  = UnknownError String

mkErrorPos :: SourcePos -> ErrorType -> Error
mkErrorPos pos e -> Error (Just pos) e

mkError :: ErrorType -> Error
mkError e = Error Nothing e

-- | Pretty print the error message by adding the source file information
printError :: Error -> String
printError (Just pos) e | spRow pos <= 0 
  = spFile pos 
  ++ "error:\n" 
  ++ printErrorType e
printError (Just pos) e
  = spFile pos
  ++ ":"
  ++ (show $ spRow pos)
  ++ ":"
  ++ (show $ spColumn pos)
  ++ "error:\n"
  ++ printErrorType e
printError Nothing e = "error:\n" ++ printErrorType e

printErrorType :: Error -> String
printErrorType (UnknownError s) = s
