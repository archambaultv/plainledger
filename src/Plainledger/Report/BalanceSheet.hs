-- |
-- Module      :  Plainledger.Reports.BalanceSheet
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.BalanceSheet (
  balanceSheetReport
  )
where

import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.AccountTreeReport
import Plainledger.Report.AccountTreeParam
import Plainledger.Report.Ledger


balanceSheetReport :: AccountTreeParam ->
                      Ledger ->
                      Day ->
                      [ReportRow]
balanceSheetReport atp ledger today =
  accountTreeReport atp ledger today TReportBalanceSheetName  
                    serializeNode serializeTopAccount

  where serializeNode acc dates = 
          let amnt = balanceSheetQty ledger dates acc
              amntText = qtyToNormallyPositive decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger dates acc
                      || acc == lEarningAccount ledger
                      || acc == lOpeningAccount ledger
          in (amnt, [amntText], isActive)

        serializeTopAccount xs = map snd $ take 3 xs

        decimalSep = jfDecimalSeparator $ lJournalFile ledger