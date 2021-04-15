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

      reportPeriodToSpanTest Last365Days (read "2020-01-01") 06
      (Just (read "2019-01-02", read "2020-01-01")),

      reportPeriodToSpanTest Last365Days (read "2020-02-28") 06
      (Just (read "2019-03-01", read "2020-02-28")),

      reportPeriodToSpanTest Last365Days (read "2020-02-29") 06
      (Just (read "2019-03-02", read "2020-02-29")),

      reportPeriodToSpanTest Last30Days (read "2020-07-31") 06
      (Just (read "2020-07-02", read "2020-07-31")),

      reportPeriodToSpanTest Last60Days (read "2020-07-31") 06
      (Just (read "2020-06-02", read "2020-07-31")),

      reportPeriodToSpanTest Last91Days (read "2020-07-31") 06
      (Just (read "2020-05-02", read "2020-07-31")),

      reportPeriodTest AllDates (Just (PreviousPeriod 1)) (read "2021-01-01") 07 [],

      reportPeriodTest (Month 0) (Just (PreviousPeriod 1)) (read "2021-06-30") 01
      ( [(read "2021-06-01", read "2021-06-30"),
             (read "2021-05-01", read "2021-05-31")]),

      reportPeriodTest (Month 0) (Just (PreviousPeriod 1)) (read "2021-02-28") 01
      ( [(read "2021-02-01", read "2021-02-28"),
             (read "2021-01-01", read "2021-01-31")]),

      reportPeriodTest (MonthToDate 0) (Just (PreviousPeriod 1)) (read "2021-06-15") 01
      ( [(read "2021-06-01", read "2021-06-15"),
             (read "2021-05-01", read "2021-05-15")]),

      reportPeriodTest (MonthToDate 0) (Just (PreviousPeriod 1)) (read "2021-03-30") 01
      ( [(read "2021-03-01", read "2021-03-30"),
             (read "2021-02-01", read "2021-02-28")]),

      reportPeriodTest (CalendarQuarter 0) (Just (PreviousPeriod 1)) (read "2021-01-30") 01
      ( [(read "2021-01-01", read "2021-03-31"),
             (read "2020-10-01", read "2020-12-31")]),

      reportPeriodTest (CalendarQuarter 0) (Just (PreviousPeriod 1)) (read "2021-06-30") 01
      ( [(read "2021-04-01", read "2021-06-30"),
             (read "2021-01-01", read "2021-03-31")]),

      reportPeriodTest (CalendarQuarterToDate 0) (Just (PreviousPeriod 1)) (read "2021-05-17") 01
      ( [(read "2021-04-01", read "2021-05-17"),
             (read "2021-01-01", read "2021-02-17")]),

      reportPeriodTest (CalendarQuarterToDate 0) (Just (PreviousPeriod 1)) (read "2021-05-30") 01
      ( [(read "2021-04-01", read "2021-05-30"),
             (read "2021-01-01", read "2021-02-28")]),

      reportPeriodTest (FiscalQuarter 0) (Just (PreviousPeriod 1)) (read "2021-07-30") 05
      ( [(read "2021-05-01", read "2021-07-31"),
             (read "2021-02-01", read "2021-04-30")]),

      reportPeriodTest (FiscalQuarterToDate 0) (Just (PreviousPeriod 1)) (read "2021-05-17") 05
      ( [(read "2021-05-01", read "2021-05-17"),
             (read "2021-02-01", read "2021-02-17")]),

      reportPeriodTest (CalendarYear 0) (Just (PreviousPeriod 1)) (read "2021-05-17") 05
      ( [(read "2021-01-01", read "2021-12-31"),
             (read "2020-01-01", read "2020-12-31")]),

      reportPeriodTest (CalendarYearToDate 0) (Just (PreviousPeriod 1)) (read "2021-05-17") 05
      ( [(read "2021-01-01", read "2021-05-17"),
             (read "2020-01-01", read "2020-05-17")]),

      reportPeriodTest Last30Days (Just (PreviousPeriod 1)) (read "2021-01-30") 05
      ( [(read "2021-01-01", read "2021-01-30"),
             (read "2020-12-02", read "2020-12-31")]),

      reportPeriodTest Last60Days (Just (PreviousPeriod 1)) (read "2021-01-30") 05
      ( [(read "2020-12-02", read "2021-01-30"),
             (read "2020-10-03", read "2020-12-01")]),

      reportPeriodTest Last91Days (Just (PreviousPeriod 1)) (read "2021-01-30") 05
      ( [(read "2020-11-01", read "2021-01-30"),
             (read "2020-08-03", read "2020-11-01")]),

      reportPeriodTest Last182Days (Just (PreviousPeriod 1)) (read "2021-01-30") 05
      ( [(read "2020-08-02", read "2021-01-30"),
             (read "2020-02-02", read "2020-08-01")]),

      reportPeriodTest Last365Days (Just (PreviousPeriod 1)) (read "2021-01-30") 05
      ( [(read "2020-02-01", read "2021-01-30"),
             (read "2019-02-01", read "2020-01-31")])
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

reportPeriodTest :: ReportPeriod ->
                    Maybe CompareAnotherPeriod ->
                    Day ->
                    Int ->
                    [DateSpan] -> 
                    TestTree
reportPeriodTest period mcompare today i expected = 
  testCase ("reportPeriods for " ++ show period ++ " and " ++ show mcompare) $ do
    let journalFile = emptyJournalFile{jfFirstFiscalMonth = i}
        ledger = emptyLedger{lJournalFile = journalFile}
    let res = reportPeriods period mcompare today ledger
    assertEqual "" expected res