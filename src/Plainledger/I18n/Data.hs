-- |
-- Module      :  Plainledger.I18n.Data
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the standard text various languages


module Plainledger.I18n.Data
(
  Language(..),
  I18nText(..)
) where

import Data.Time
import Plainledger.Error

-- Supported language by this application
data Language 
  = Fr_CA
  | En_CA
  deriving (Show, Eq)

-- Text that must be translated
data I18nText 
  = TError Error

  | TAsset
  | TLiability
  | TEquity
  | TRevenue
  | TExpense

  | TAccountNumber
  | TAccountIdent
  | TAccountParent
  | TAccountName

  | TBalanceStartDate
  | TBalanceEndDate
  | TBalanceAmount
  | TBalanceAccount
  | TBalanceDate

  | TJournalFileOpeningBalanceAccount
  | TJournalFileEarningsAccount
  | TJournalFileCompanyName 
  | TJournalFileDecimalSeparator
  | TJournalFileThousandSeparator
  | TJournalFileCurrSymbol
  | TJournalFileFirstFiscalMonth
  | TJournalFileAccountFile
  | TJournalFileTransactionFiles
  | TJournalFileStatementBalanceFiles
  | TJournalFileTrialBalanceFiles

  | TTransactionId
  | TTransactionDate
  | TTransactionComment
  | TTransactionCounterparty
  | TTransactionTag
  | TTransactionAccountPrefix
  | TTransactionAmountPrefix
  | TTransactionBalanceDatePrefix

  | TReportTrialBalanceName
  | TReportBalanceSheetName
  | TReportIncomeStatementName
  | TReportDateSpan (Maybe (Day, Day))
  | TReportMonthSpan Day
  | TReportYearSpan Day
  | TReportAccNumber
  | TReportAccName
  | TReportDebit 
  | TReportCredit
  | TReportTotal
  | TReportGeneratedOn Day
  | TReportEarnings
