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
import Data.Tree
import Plainledger.Report.ListUtils


incomeStatementReport :: AccountTreeParam ->
                        Ledger ->
                        Day ->
                        [ReportRow]
incomeStatementReport atp ledger today = 
  let bodyHeader = standardColumnHeader atp lang ledger today -- ["", i18nText lang TReportTotal]
  in standardFormat atp ledger today TReportIncomeStatementName  bodyHeader
    $ addIndentation
    $ addTotal
    $ drop 3
    $ singleQuantityReport atp ledger today serializeNode

  where serializeNode :: [DateSpan] -> Account -> ([Quantity], Bool)
        serializeNode dates acc = 
          let amnt = map (\d -> trialBalanceQty ledger d acc) dates
              isActive = any (\d -> isAccountActive ledger d acc) dates
          in (amnt, isActive)

        addTotal :: [Tree QuantityInfo] -> [Tree NodeRows]
        addTotal xs =
          let earning :: [Quantity]
              earning = map negate $ elementSum $ map (fst . fst . rootLabel) xs
              footer :: ReportRow
              footer = i18nText lang TReportEarnings : 
                       map (writeAmount decimalSep) earning
          in map (fmap snd) xs ++ [Node ([footer],[]) []]

        decimalSep = jfDecimalSeparator $ lJournalFile ledger

        lang = jfLanguage $ lJournalFile ledger