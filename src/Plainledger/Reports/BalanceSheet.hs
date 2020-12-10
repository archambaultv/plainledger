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

type BalanceSheetOption = TrialBalanceOption

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
          && (all (== 0) (balanceQty a))
          && not (grShowInactiveAccounts opt) = Nothing
      accountAlg acc = Just (balanceQty acc)

      balanceQty :: Account -> [Quantity]
      balanceQty a =
          if isIncomeStatementType $ aType a
          then adjustBalance a $ reportCashFlow a r
          else adjustBalance a $ reportBalance a r

      -- Adjust the balance for the earnings and open balance
      adjustBalance :: Account -> [Quantity] -> [Quantity]
      adjustBalance acc xs =
        if aId acc == openBalAcc
        then addList rOpenBal xs
        else if aId acc == earningsAcc
             then addList rEarnings xs
             else xs

    in groupReport "Balance Sheet" accountAlg isBalanceSheetType r
