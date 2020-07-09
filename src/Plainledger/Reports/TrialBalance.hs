-- |
-- Module      :  Plainledger.Reports.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.TrialBalance (
  reportToTrialBalance,
  TrialBalanceOption
  )
where

import Plainledger.Ledger
import Plainledger.Reports.Report
import Prelude hiding (lines)
import qualified Data.Text as T

type TrialBalanceOption = FlatReportOption



reportToTrialBalance :: TrialBalanceOption -> Report -> [[T.Text]]
reportToTrialBalance opt r =
  let
      openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal $ rLedger r

      serialize :: Account -> Maybe [Quantity]
      serialize a
        | isReportActive a r == False
          && aId a /= openBalAcc
          && all (== 0) (computeTrialBalance a)
          && not (frShowInactiveAccounts opt) = Nothing
      serialize acc = Just (computeTrialBalance acc)

      computeTrialBalance :: Account -> [Quantity]
      computeTrialBalance a =
        if isIncomeStatementGroup $ aGroup a
        then reportCashFlow a r
        else reportBalance a r

  in flatReport "Trial Balance" serialize opt r
