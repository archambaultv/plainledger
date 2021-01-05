-- |
-- Module      :  Plainledger.I18n.En_CA
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Canadian English texts


module Plainledger.I18n.En_CA
(
  en_CAText,
) where

import Data.List
import Plainledger.I18n.Data
import Plainledger.Error

en_CAText :: I18nText -> String
en_CAText (TError x) = printError x

en_CAText TAsset = "Asset"
en_CAText TLiability = "Liability"
en_CAText TEquity = "Equity"
en_CAText TRevenue = "Revenue"
en_CAText TExpense = "Expense"

en_CAText TAccountId = "Id"
en_CAText TAccountNumber = "Number"
en_CAText TAccountType = "Type"
en_CAText TAccountName = "Name"
en_CAText TAccountGroup = "Group"
en_CAText TAccountSubGroup = "Subgroup"

en_CAText TBalanceStartDate = "Start date"
en_CAText TBalanceEndDate = "End date"
en_CAText TBalanceAmount = "Amount"
en_CAText TBalanceAccount = "Account"
en_CAText TBalanceDate = "Date"

en_CAText TJournalFileOpeningBalanceAccount = "Opening balance account"
en_CAText TJournalFileEarningsAccount = "Earnings account"
en_CAText TJournalFileCompanyName = "Name"
en_CAText TJournalFileDecimalSeparator = "Decimal separator"
en_CAText TJournalFileFirstFiscalMonth = "First month of fiscal year"
en_CAText TJournalFileAccountFile = "Accounts file"
en_CAText TJournalFileTransactionFiles = "Transactions files"
en_CAText TJournalFileStatementBalanceFiles = "Statement balance assertions files"
en_CAText TJournalFileTrialBalanceFiles = "Trial balance assertions files"

en_CAText TTransactionId = "Transaction number"
en_CAText TTransactionDate = "Date"
en_CAText TTransactionComment = "Comment"
en_CAText TTransactionCounterparty = "Counterparty"
en_CAText TTransactionTag = "Tag"
en_CAText TTransactionAccountPrefix = "Account"
en_CAText TTransactionAmountPrefix = "Amount"
en_CAText TTransactionBalanceDatePrefix = "Date on statement"


-- | Pretty print the error message and add the source file information
printError :: Error -> String
printError (Error [] e) = "error:\n" ++ printErrorType e
printError (Error [pos] e) = showSourcePos pos ++ " error:\n" ++ printErrorType e
printError (Error (p:ps) e) 
  = printError (Error [p] e)
  ++ "Other locations relevant for this error:\n  "
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
printErrorType (ParseAmountErr s)
  = "Cannot parse \"" ++ s ++ "\" as a number."
printErrorType (ParseAmountExponentErr s)
  = printErrorType (ParseAmountErr s)
  ++ "\nToo many decimals (max 256)"

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

printErrorType ZeroLengthAccountId
  = "Unallowed zero length account id"
printErrorType (DuplicateAccountId n)
  = "Duplicate account id \""
  ++ n
  ++ "\""
printErrorType (OpeningBalanceNotDefined s)
  = "The opening balance account \""
  ++ s
  ++ "\" declared in the JournalFile file does not appear in the accounts files."
printErrorType (EarningsAccountNotDefined s) 
  = "The earnings account \""
  ++ s
  ++ "\" declared in the JournalFile file does not appear in the accounts files."

printErrorType (MissingCsvColumn c)
  = "Column \""
  ++ c
  ++ "\" is not in the CSV header."
printErrorType (MissingCsvColumnData c)
  = "Column \""
  ++ c
  ++ "\" is missing for this line."

printErrorType (DuplicateCsvColumn c)
  = "Duplicate column header \""
  ++ c
  ++ "\" is the CSV header."
printErrorType EmptyCsvFile
  = "The file is empty. It must at least contain a header"
  
printErrorType (UnknownAccountType s)
  = "Unknown account type \""
  ++ s
  ++ "\".\nMust be one of the following : Actif, Passif, Capital, Revenu, Dépense"

printErrorType ZeroOrOnePostingOnly
  = "Transaction does not have at least two postings.\n"

printErrorType (UnbalancedTransaction q)
  = "Unbalanced transaction. The total is " ++ show q

printErrorType TwoOrMorePostingsWithoutAmount
  = "Two or more postings with unspecified amount for this transaction.\n"
  ++ "We can only infer the amount for one posting only."

printErrorType (AccountIdNotInAccountFile s)
  = "Account \"" ++ s ++ "\" is not declared in the account file."

printErrorType (WrongBalance accId date bal amnt)
  = "Balance assertion failed for account \""
  ++ accId
  ++ "\" on date "
  ++ show date
  ++ ".\nAsserted balance : "
  ++ show bal
  ++ "\nComputed balance : " ++ showAmnt amnt

  where showAmnt Nothing = "0 (no transaction for this account)"
        showAmnt (Just x) = show x
                          ++ "\nDifference       : "
                          ++ show (bal - x)
printErrorType (MissingStartDateInBalance s)
  = "Account \""
  ++ s
  ++ "\" must have a start date in the balance file.\n"
  ++ "All revenue and expense accounts and the opening account must have a start date.  "

printErrorType (EndDateGreaterThanStartDate sd ed)
  = "Start date \""
  ++ show sd
  ++ "\" is greater than the end date \""
  ++ show ed
  ++ "\""
  
showSourcePos :: SourcePos -> String
showSourcePos (SourcePos f r _) | r <= 0 = f
showSourcePos (SourcePos f r c) | c <= 0
  = f
  ++ ":row "
  ++ show r
  ++ ":"
showSourcePos (SourcePos f r c)
  = f
  ++ ":row "
  ++ show r
  ++ ":column "
  ++ show c
  ++ ":"