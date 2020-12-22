-- |
-- Module      :  Plainledger.Error.Errors
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type


module Plainledger.Error.PrettyPrinter
(
  printError,
  printErrorType
) where

import Data.List
import Plainledger.Error.Error
import Plainledger.Error.SourcePos

-- | Pretty print the error message and add the source file information
printError :: Error -> String
printError (Error [] e) = "error:\n" ++ printErrorType e
printError (Error [pos] e) = showSourcePos pos ++ " error:\n" ++ printErrorType e
printError (Error (p:ps) e) 
  = printError (Error [p] e)
  ++ "error also located at:\n  "
  ++ intercalate "\n  " (map showSourcePos ps)

-- | Pretty print the error message
printErrorType :: ErrorType -> String
printErrorType (ErrorMessage s) = s

printErrorType (ParseDateErr s) 
  = "Unable to parse date \"" ++ s ++ "\".\nDate must be in the YYYY-MM-DD format."
printErrorType (ParseIntErr s)
  = "Cannot parse \"" ++ s ++ "\" as an integer."
printErrorType (ParseCharErr s)
  = "Cannot parse \"" ++ s ++ "\" as a character."

printErrorType (EmptyFieldInJournalFile s)
  = "Parameter \"" ++ s ++ "\" value is empty."
printErrorType (MissingFieldinJournalFile s)
  = "Parameter \"" ++ s ++ "\" value is missing."
printErrorType EmptyJournalFile
  = "Journal file is empty"
printErrorType (UnknownFieldinJournalFile s)
  = "Unknown parameter \"" ++ s ++ "\"."
printErrorType InvalidHeaderJournalFile
  = "Unable to parse the header in the journal file\n"
  ++ "The CSV format must use a comma (,), semicolon (;) or tab to separate columns\n"
  ++ "The header headings must be \"Paramètre\" and \"Valeur\" for French\n"
  ++ "The header headings must be \"Parameter\" and \"Value\" for English\n"

showSourcePos :: SourcePos -> String
showSourcePos (SourcePos f r _) | r <= 0 = f
showSourcePos (SourcePos f r c)
  = f
  ++ ":"
  ++ show r
  ++ ":"
  ++ show c