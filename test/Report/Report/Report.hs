-- |
-- Module      :  Report.Report.Report
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type


module Report.Report.Report
(
  reportTestTree
)
where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Report
import Data.Time
import Plainledger.Journal


reportTestTree :: TestTree
reportTestTree =
   testGroup "Report"
    [ reportPeriodToSpanTest AllDates (read "2021-01-01") 07 Nothing,
      
      reportPeriodToSpanTest (CustomPeriod (read "2021-01-01") (read "2021-06-30")) 
      (read "2021-06-30") 01 (Just (read "2021-01-01", read "2021-06-30")),
      
      reportPeriodToSpanTest (Month 0) (read "2021-06-30") 01
      (Just (read "2021-06-01", read "2021-06-30")),

      reportPeriodToSpanTest (MonthToDate 0) (read "2021-06-13") 01
      (Just (read "2021-06-01", read "2021-06-13")),

      reportPeriodToSpanTest (CalendarQuarter 0) (read "2018-05-17") 01
      (Just (read "2018-04-01", read "2018-06-30")),

      reportPeriodToSpanTest (CalendarQuarterToDate 0) (read "2018-05-17") 01
      (Just (read "2018-04-01", read "2018-05-17")),

      reportPeriodToSpanTest (FiscalQuarter 0) (read "2018-05-17") 07
      (Just (read "2018-04-01", read "2018-06-30")),

      reportPeriodToSpanTest (FiscalQuarter 0) (read "2018-05-17") 05
      (Just (read "2018-05-01", read "2018-07-31")),

      reportPeriodToSpanTest (CalendarYearToDate 0) (read "2018-05-17") 05
      (Just (read "2018-01-01", read "2018-05-17")),

      reportPeriodToSpanTest (FiscalYear 0) (read "2018-05-17") 05
      (Just (read "2018-05-01", read "2019-04-30")),

      reportPeriodToSpanTest (FiscalYear 0) (read "2018-05-17") 06
      (Just (read "2017-06-01", read "2018-05-31")),

      reportPeriodToSpanTest (FiscalYearToDate 0) (read "2018-05-17") 05
      (Just (read "2018-05-01", read "2018-05-17")),

      reportPeriodToSpanTest (FiscalYearToDate 0) (read "2018-05-17") 06
      (Just (read "2017-06-01", read "2018-05-17")),

      reportPeriodToSpanTest Since365DaysAgo (read "2020-01-01") 06
      (Just (read "2019-01-01", read "2020-01-01")),

      reportPeriodToSpanTest Since365DaysAgo (read "2020-02-28") 06
      (Just (read "2019-02-28", read "2020-02-28")),

      reportPeriodToSpanTest Since365DaysAgo (read "2020-02-29") 06
      (Just (read "2019-03-01", read "2020-02-29")),

      reportPeriodToSpanTest Since30DaysAgo (read "2020-07-31") 06
      (Just (read "2020-07-01", read "2020-07-31")),

      reportPeriodToSpanTest Since60DaysAgo (read "2020-07-31") 06
      (Just (read "2020-06-01", read "2020-07-31")),

      reportPeriodToSpanTest Since90DaysAgo (read "2020-07-31") 06
      (Just (read "2020-05-02", read "2020-07-31"))
    ]


reportPeriodToSpanTest :: ReportPeriod ->
                          Day ->
                          Int ->
                          Maybe DateSpan -> 
                          TestTree
reportPeriodToSpanTest period today i expected = 
  testCase ("reportPeriodToSpan for " ++ show period) $ do
    let journalFile = emptyJournalFile{jfFirstFiscalMonth = i}
        ledger = emptyLedger{lJournalFile = journalFile}
    let res = reportPeriodToSpan period today ledger
    assertEqual "" expected res
