-- |
-- Module      :  Plainledger.Reports.IncomeStatement
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.IncomeStatement (
  reportToIncomeStatement,
  IncomeStatementOption
  )
where

import Plainledger.Ledger
import Plainledger.Reports.Report
import Plainledger.Reports.BalanceSheet
import qualified Data.Text as T

type IncomeStatementOption = BalanceSheetOption

reportToIncomeStatement :: IncomeStatementOption -> Report -> [[T.Text]]
reportToIncomeStatement opt r =
    let
      serialize :: Account -> Maybe [Quantity]
      serialize a
        | isReportActive a r == False
          -- && all (== 0) (reportCashFlow acc r)
          && not (grShowInactiveAccounts opt) = Nothing
      serialize acc = Just (reportCashFlow acc r)

      report = groupReport "Income Statement" serialize isIncomeStatementGroup r

      rEarnings = reportEarnings r
      earningsLines = concatMap (serializeAmount NormallyPositive Revenue) rEarnings

    in report ++ [[]] ++ [earningsLines]
