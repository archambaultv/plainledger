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
import qualified Data.Text as T
import Data.Tree


incomeStatementReport :: AccountTreeParam ->
                        Ledger ->
                        Day ->
                        [ReportRow]
incomeStatementReport atp ledger today = 
  let bodyHeader = ["", i18nText lang TReportTotal]
  in standardFormat atp ledger today TReportIncomeStatementName  bodyHeader
    $ addIndentation
    $ addTotal
    $ drop 3
    $ singleQuantityReport atp ledger today serializeNode

  where serializeNode :: [DateSpan] -> Account -> (Quantity, T.Text, Bool)
        serializeNode dates acc = 
          let d = head dates
              amnt = trialBalanceQty ledger d acc
              amntText = qtyToNormallyPositive decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger d acc
          in (amnt, amntText, isActive)

        addTotal :: [Tree QuantityInfo] -> [Tree NodeRows]
        addTotal xs =
          let earning = negate $ sum $ map (fst . fst . rootLabel) xs
              footer :: ReportRow
              footer = [i18nText lang TReportEarnings,  
                        writeAmount decimalSep earning]
          in map (fmap snd) xs ++ [Node ([footer],[]) []]

        decimalSep = jfDecimalSeparator $ lJournalFile ledger

        lang = jfLanguage $ lJournalFile ledger