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
  let bodyHeader = "" : standardDateHeader atp lang ledger today
  in standardFormat atp ledger today TReportBalanceSheetName [bodyHeader]
      $ addIndentation
      $ take 3 -- Take only the first 3 top account
      $ fmap snd
      <$> singleQuantityReport atp ledger today serializeNode

  where serializeNode :: [DateSpan] -> Account -> ([Quantity], Bool)
        serializeNode dates acc =
          let amnt = map (\d -> balanceSheetQty ledger d acc) dates
              isActive = any (\d -> isAccountActive ledger d acc) dates
                      || acc == lEarningAccount ledger
                      || acc == lOpeningAccount ledger
          in (amnt, isActive)
          

        lang = jfLanguage $ lJournalFile ledger