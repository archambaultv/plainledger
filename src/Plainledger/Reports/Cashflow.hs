-- |
-- Module      :  Plainledger.Reports.cashFlow
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.Cashflow (
  reportToCashFlow,
  CashFlowOption
  )
where

import Plainledger.Ledger
import Plainledger.Reports.Report
import qualified Data.Text as T

type CashFlowOption = FlatReportOption

serialize :: Report -> CashFlowOption ->  Account -> Maybe [Quantity]
serialize r opt a
  | isReportActive a r == False
    -- && all (== 0) (reportCashFlow acc r)
    && not (frShowInactiveAccounts opt) = Nothing
serialize r _ acc = Just (reportCashFlow acc r)

reportToCashFlow :: CashFlowOption -> Report -> [[T.Text]]
reportToCashFlow opt r = flatReport "Cashflow" (serialize r opt) opt r
