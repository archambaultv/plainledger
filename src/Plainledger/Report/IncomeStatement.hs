-- |
-- Module      :  Plainledger.Reports.IncomeStatement
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.IncomeStatement (
  incomeStatementReport
  )
where

import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.AccountTreeReport
import Plainledger.Report.AccountTreeParam
import Plainledger.Report.Ledger


incomeStatementReport :: AccountTreeParam ->
                        Ledger ->
                        Day ->
                        [ReportRow]
incomeStatementReport atp ledger today =
  accountTreeReport atp ledger today TReportIncomeStatementName   
                    serializeNode serializeTopAccount

  where serializeNode acc dates = 
          let amnt = trialBalanceQty ledger dates acc
              amntText = qtyToNormallyPositive decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger dates acc
          in (amnt, [amntText], isActive)

        serializeTopAccount :: [(Quantity, [ReportRow])] -> [[ReportRow]]
        serializeTopAccount xs = 
          let body = map snd $ drop 3 xs
              earning = negate $ sum $ map fst $ drop 3 xs
              footer :: ReportRow
              footer = [i18nText lang TReportEarnings,  
                        writeAmount decimalSep earning]
          in body ++ [[footer]]

        decimalSep = jfDecimalSeparator $ lJournalFile ledger

        lang = jfLanguage $ lJournalFile ledger