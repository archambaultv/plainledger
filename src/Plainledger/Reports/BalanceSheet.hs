-- |
-- Module      :  Plainledger.Reports.BalanceSheet
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.BalanceSheet (
  reportToBalanceSheet,
  BalanceSheetOption
  )
where

import Plainledger.Ledger
import Plainledger.Reports.Report
import qualified Data.Text as T

type BalanceSheetOption = GroupReportOption

reportToBalanceSheet :: BalanceSheetOption -> Report -> [[T.Text]]
reportToBalanceSheet opt r =
    let
      -- The opening and earning accounts from the configuration
      rOpenBal = reportLedgerOpeningBalance r
      openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal $ rLedger r
      rEarnings = reportEarnings r
      earningsAcc = cEarningsAccount $ jConfiguration $ lJournal $ rLedger r

      accountAlg :: Account -> Maybe [Quantity]
      accountAlg a
        | isReportActive a r == False
          && aId a /= openBalAcc
          && aId a /= earningsAcc
          && (all (== 0) (balanceQty a r))
          && not (grShowInactiveAccounts opt) = Nothing
      accountAlg acc = Just (balanceQty acc r)

      balanceQty :: Account -> Report -> [Quantity]
      balanceQty a rep =
          if isIncomeStatementGroup $ aGroup a
          then adjustBalance a $ reportCashFlow a rep
          else adjustBalance a $ reportBalance a rep

      -- Adjust the balance for the earnings and open balance
      adjustBalance :: Account -> [Quantity] -> [Quantity]
      adjustBalance acc xs =
        if aId acc == openBalAcc
        then addList rOpenBal xs
        else if aId acc == earningsAcc
             then addList rEarnings xs
             else xs

    in groupReport "Balance Sheet" accountAlg isBalanceSheetGroup r
