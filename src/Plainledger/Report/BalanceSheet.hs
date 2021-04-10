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
import qualified Data.Text as T

balanceSheetReport :: AccountTreeParam ->
                      Ledger ->
                      Day ->
                      [ReportRow]
balanceSheetReport atp ledger today =
  let bodyHeader = ["", i18nText lang TReportTotal]
  in standardFormat atp ledger today TReportBalanceSheetName bodyHeader
      $ addIndentation
      $ take 3 -- Take only the first 3 top account
      $ fmap snd
      <$> singleQuantityReport atp ledger today serializeNode

  where serializeNode :: DateSpan -> Account -> (Quantity, T.Text, Bool)
        serializeNode dates acc =
          let amnt = balanceSheetQty ledger dates acc
              amntText = qtyToNormallyPositive decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger dates acc
                      || acc == lEarningAccount ledger
                      || acc == lOpeningAccount ledger
          in (amnt, amntText, isActive)
          
        
        decimalSep = jfDecimalSeparator $ lJournalFile ledger

        lang = jfLanguage $ lJournalFile ledger