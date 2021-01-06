-- |
-- Module      :  Plainledger.Reports.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.TrialBalance (
  trialBalanceReport
  )
where

import Plainledger.Journal
import Plainledger.Report.Report
import qualified Data.Vector as V
import qualified Data.Text as T


trialBalanceReport :: ReportPeriod -> 
                      (Maybe CompareAnotherPeriod) -> 
                      ShowRow -> 
                      Journal ->
                      V.Vector (V.Vector T.Text)
trialBalanceReport _ _ _ journal = V.empty

