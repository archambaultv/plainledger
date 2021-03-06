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

import Data.Time
import Data.List ( intercalate )
import Plainledger.I18n.Data ( I18nText(..) )
import Plainledger.Error
    ( SourcePos(SourcePos), ErrorType(..), Error(..) )

en_CAText :: I18nText -> String
en_CAText (TError x) = printError x

en_CAText TAsset = "Asset"
en_CAText TLiability = "Liability"
en_CAText TEquity = "Equity"
en_CAText TRevenue = "Revenue"
en_CAText TExpense = "Expense"

en_CAText TAccountNumber = "Number"
en_CAText TAccountName = "Name"
en_CAText TAccountIdent = "Id"
en_CAText TAccountParent = "Parent"

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
en_CAText TJournalFileThousandSeparator = "Thousand separator"
en_CAText TJournalFileCurrSymbol = "Monetary symbol"

en_CAText TTransactionId = "Transaction number"
en_CAText TTransactionDate = "Date"
en_CAText TTransactionComment = "Comment"
en_CAText TTransactionCounterparty = "Counterparty"
en_CAText TTransactionTag = "Tag"
en_CAText TTransactionAccountPrefix = "Account"
en_CAText TTransactionAmountPrefix = "Amount"
en_CAText TTransactionBalanceDatePrefix = "Date on statement"

en_CAText TReportTrialBalanceName = "Trial Balance"
en_CAText TReportBalanceSheetName = "Balance Sheet"
en_CAText TReportIncomeStatementName = "Income Statement"
en_CAText (TReportDateSpan Nothing) = ""
en_CAText (TReportDateSpan (Just (d1, d2))) = "From "
                                            ++ show d1
                                            ++ " to "
                                            ++ show d2
en_CAText (TReportMonthSpan d) = 
  let (y,m,_) = toGregorian d
  in monthToString m ++ " " ++ show y
en_CAText (TReportYearSpan d) = 
  let (y,_,_) = toGregorian d
  in show y
en_CAText TReportAccNumber = "Number"
en_CAText TReportAccName = "Account"
en_CAText TReportDebit  = "Debit"
en_CAText TReportCredit = "Credit"
en_CAText TReportTotal = "Total"
en_CAText TReportEarnings = "Earnings"
en_CAText (TReportGeneratedOn d) = "Reports generated on " ++ show d

monthToString :: Int -> String
monthToString 1 = "January"
monthToString 2 = "February"
monthToString 3 = "March"
monthToString 4 = "April"
monthToString 5 = "May"
monthToString 6 = "June"
monthToString 7 = "July"
monthToString 8 = "August"
monthToString 9 = "September"
monthToString 10 = "October"
monthToString 11 = "November"
monthToString 12 = "December"
monthToString _ = error "Wrong month number"

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
printErrorType (ParsePosIntErr s)
  = "Cannot parse \"" ++ s ++ "\" as a positive number."
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
printErrorType (DuplicateJournalFileParam s)
  = "Duplicate parameter in journal file : \"" ++ s ++ "\"."
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

printErrorType (InvalidParent ident parent)
  = "The parent account for the account \""
  ++ show ident
  ++ "\" is not valid. "
  ++ "\"" ++ show parent ++ "\" is not an account indentifier."

printErrorType (CycleInParents idents)
  = "The following accounts (when taking their parent, parent of parent , etc.) form one or more cycles :\n"
  ++ intercalate ", " idents

printErrorType (InvalidIdentifier s)
  = "The following account identifier cannot be used :"
  ++ intercalate ", " s

printErrorType (DuplicateBalance d t)
  = "Duplicate balance assertion.\n"
  ++ "Date : " ++ show d ++ "\n"
  ++ "Account : " ++ t

printErrorType (UnallowedAmountChar c)
 = "Character "
 ++ show c
 ++ " is not allowed as a thousand separator or currency symbol."

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