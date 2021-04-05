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
  Errors,
  Error(..),
  ErrorType(..),
  printErrors,
  mkError,
  mkErrorNoPos,
  mkErrorMultiPos,
  setSourcePosFileIfNull,
  setSourcePosRowIfNull,
  setSourcePosColIfNull,
  setSourcePosIfNull
) where

import Data.Decimal ( Decimal )
import Data.Time ( Day )
import qualified Data.Text as T
import Plainledger.Error.SourcePos ( SourcePos(..) )

type Errors = [Error]

-- | Errors, with or without source locations.
data Error = Error [SourcePos] ErrorType

  deriving (Eq, Show)

-- | List all possibles error in plainledger
data ErrorType
  = ErrorMessage String -- | Used when the error of a dependency is a string

  | ParseDateErr String
  | ParseIntErr String
  | ParseCharErr String
  | ParseAmountErr String
  | ParseAmountExponentErr String

  | EmptyJournalFile
  | EmptyFieldInJournalFile String
  | MissingFieldinJournalFile String
  | UnknownFieldinJournalFile String
  | InvalidHeaderJournalFile

  | ZeroLengthAccountId
  | DuplicateAccountId String
  | InvalidIdentifier [String]
  | OpeningBalanceNotDefined String
  | EarningsAccountNotDefined String
  | InvalidParent String String
  | CycleInParents [String]

  | MissingCsvColumn String
  | MissingCsvColumnData String
  | DuplicateCsvColumn String
  | EmptyCsvFile

  | UnknownAccountType String

  | ZeroOrOnePostingOnly

  | UnbalancedTransaction Decimal
  | TwoOrMorePostingsWithoutAmount
  | AccountIdNotInAccountFile String

  | WrongBalance String Day Decimal (Maybe Decimal)
  | MissingStartDateInBalance String
  | DuplicateBalance Day String

  | EndDateGreaterThanStartDate Day Day

  deriving (Eq, Show)

-- | Pretty print the first 10 error messages and add the source file information
printErrors :: [T.Text] -> T.Text
printErrors x = T.intercalate "\n"
              $ take 10 x

mkError :: SourcePos -> ErrorType -> Errors
mkError pos e = [Error [pos] e]

mkErrorNoPos :: ErrorType -> Errors
mkErrorNoPos e = [Error [] e]

mkErrorMultiPos :: [SourcePos] -> ErrorType -> Errors
mkErrorMultiPos pos e = [Error pos e]

setSourcePosIfNull :: SourcePos -> Errors -> Errors
setSourcePosIfNull pos = map f
  where f (Error [] e) = Error [pos] e
        f x = x

setSourcePosFileIfNull :: String -> Errors -> Errors
setSourcePosFileIfNull f = map (setSourcePosFileIfNull' f)


setSourcePosRowIfNull :: Int -> Errors -> Errors
setSourcePosRowIfNull f = map (setSourcePosRowIfNull' f)


setSourcePosColIfNull :: Int -> Errors -> Errors
setSourcePosColIfNull f = map (setSourcePosColIfNull' f)

setSourcePosFileIfNull' :: String -> Error -> Error
setSourcePosFileIfNull' f (Error [] e) = Error [SourcePos f 0 0] e
setSourcePosFileIfNull' f (Error sp e) =
  let sp' = map (\x -> if null (spFile x) then x{spFile = f} else x) sp
  in Error sp' e

setSourcePosRowIfNull' :: Int -> Error -> Error
setSourcePosRowIfNull' i (Error [] e) = Error [SourcePos "" i 0] e
setSourcePosRowIfNull' i (Error sp e) =
  let sp' = map (\x -> if spCsvRow x == 0 then x{spCsvRow = i} else x) sp
  in Error sp' e

setSourcePosColIfNull' :: Int -> Error -> Error
setSourcePosColIfNull' i (Error [] e) = Error [SourcePos "" 0 i] e
setSourcePosColIfNull' i (Error sp e) =
  let sp' = map (\x -> if spCsvColumn x == 0 then x{spCsvColumn = i} else x) sp
  in Error sp' e