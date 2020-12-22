-- |
-- Module      :  Plainledger.Reports.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.TrialBalance (
  -- reportToTrialBalance,
  -- TrialBalanceOption
  )
where

-- import Plainledger.Ledger
-- import Plainledger.Reports.Report
-- import Prelude hiding (lines)
-- import qualified Data.Text as T

-- data TrialBalanceOption = TrialBalanceOption {
--   tbReportPeriod :: ReportPeriod,
--   tbCompareAnotherPeriod :: CompareAnotherPeriod,
--   tbShowRow :: ShowRow,
--   tbDisplayColumns :: DisplayColumns
-- }


-- reportToTrialBalance :: TrialBalanceOption -> Report -> [[T.Text]]
-- reportToTrialBalance opt r =
--   let
--       openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal $ rLedger r
--       rOpenBal = reportLedgerOpeningBalance r

--       serialize :: Account -> Maybe [Quantity]
--       serialize a
--         | isReportActive a r == False
--           && aId a /= openBalAcc
--           && all (== 0) (computeTrialBalance a)
--           && not (frShowInactiveAccounts opt) = Nothing
--       serialize acc = Just (computeTrialBalance acc)

--       computeTrialBalance :: Account -> [Quantity]
--       computeTrialBalance a =
--         if isIncomeStatementType $ aType a
--         then adjustBalance a $ reportCashFlow a r
--         else adjustBalance a $ reportBalance a r

--       -- Adjust the balance for the opening balance
--       adjustBalance :: Account -> [Quantity] -> [Quantity]
--       adjustBalance acc xs =
--         if aId acc == openBalAcc
--         then addList rOpenBal xs
--         else xs

--   in flatReport "Trial Balance" serialize opt r
