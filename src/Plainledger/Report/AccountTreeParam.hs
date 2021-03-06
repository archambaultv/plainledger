-- |
-- Module      :  Plainledger.Reports.AccountTreeReport
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.AccountTreeParam (
  ReportPeriod(..),
  reportPeriodToSpan,
  reportPeriods,
  CompareAnotherPeriod(..),
  ComparisonColumns(..),
  comparisonColumnsDefault,
  ShowRow(..),
  GroupByColumns(..),
  AccountTreeParam(..)
  )
where

import Data.Time
import Data.Bifunctor
import Plainledger.Journal
import Plainledger.Report.Ledger
import Data.Functor ((<&>))
import Data.Maybe ( mapMaybe, maybeToList, fromMaybe )

data AccountTreeParam =
  AccountTreeParam {
    atReportPeriod :: ReportPeriod,
    atCompareAnotherPeriod :: Maybe CompareAnotherPeriod,
    atShowRow :: ShowRow,
    atGroupByColumns :: Maybe GroupByColumns,
    atComparisonColumns :: ComparisonColumns
  }

data ComparisonColumns
  = ComparisonColumns {
      showDiff :: Bool,
      showPercent :: Bool,
      showRowPercent :: Bool,
      showColumnPercent :: Bool,
      showRevenuePercent :: Bool,
      showExpensePercent :: Bool
      }
  deriving (Eq, Show)

comparisonColumnsDefault :: ComparisonColumns
comparisonColumnsDefault = ComparisonColumns False False False False False False

-- | Not all combinaison of CompareAnotherPeriod and Report Period are valid
-- | The two booleans are for show diff and show percent.
data CompareAnotherPeriod
  = PreviousPeriod Int -- Compute the dates according the the type of the period
  | PreviousYear Int -- Simple substracts one year
  | CustomCompare [(Day, Day)]
  deriving (Eq, Show)

-- | Determines the start date and end date of the reports
-- Constructors with Int, 0 means "this year/month/quarter"
-- -1 means last and 1 means next
data ReportPeriod
  = AllDates
  | CustomPeriod Day Day
  | FromBeginningUntil Day
  | Month Integer
  | MonthToDate Integer
  | CalendarQuarter Integer
  | CalendarQuarterToDate Integer
  | FiscalQuarter Integer
  | FiscalQuarterToDate Integer
  | CalendarYear Integer
  | CalendarYearToDate Integer
  | FiscalYear Integer
  | FiscalYearToDate Integer
  -- This includes the current date.
  -- So Last30Days for January 31st gives : January 2nd to January 31st gives
  | Last30Days -- (365 mod 12)
  | Last60Days -- (365 mod 6)
  | Last91Days -- (365 mod 4)
  | Last182Days -- (365 mod 2)
  | Last365Days -- Number of days in a regular year
  | SinceDateUntilTheEnd Day
  | SinceDateToToday Day
  deriving (Eq, Show)

reportPeriods :: ReportPeriod ->
                 Maybe CompareAnotherPeriod ->
                 Day ->
                 Ledger ->
                 [DateSpan]
reportPeriods p Nothing today l = maybeToList $ reportPeriodToSpan p today l
reportPeriods p (Just (CustomCompare xs)) today l =
  maybe [] (:xs) $ reportPeriodToSpan p today l
reportPeriods p (Just (PreviousYear i)) today l =
  let sp = reportPeriodToSpan p today l
      x = take i [1..]
      substract n (s,e) = (addGregorianYearsClip (negate n) s,
                           addGregorianYearsClip (negate n) e)
      foo = \z -> z : map (`substract` z) x
  in fromMaybe [] (sp <&> foo)
reportPeriods p (Just (PreviousPeriod i)) today l =
  let sp = reportPeriodToSpan p today l
      x = take i [1..]
      foo = \z -> z : mapMaybe (previousPeriod p z) x
  in fromMaybe [] (sp <&> foo)

previousPeriod :: ReportPeriod -> DateSpan -> Integer -> Maybe DateSpan
previousPeriod AllDates _ _ = Nothing
previousPeriod (CustomPeriod _ _) (d1, d2) i =
  let nbDays = diffDays d2 d1
  in Just (addDays (i * nbDays) d1, addDays (i * nbDays) d2)
previousPeriod (FromBeginningUntil _) _ _ = Nothing
previousPeriod (Month _) (d1, d2) i =
  Just (addGregorianMonthsClip (negate i) d1,
   toEndOfMonth $ addGregorianMonthsClip (negate i) d2)
previousPeriod (MonthToDate _) (d1, d2) i =
  Just (addGregorianMonthsClip (negate i) d1,
   addGregorianMonthsClip (negate i) d2)
previousPeriod (CalendarQuarter _) (d1, d2) i =
  Just (addGregorianMonthsClip (3 * negate i) d1,
   toEndOfMonth $ addGregorianMonthsClip (3 * negate i) d2)
previousPeriod (CalendarQuarterToDate _) (d1, d2) i =
  Just (addGregorianMonthsClip (3 * negate i) d1,
   addGregorianMonthsClip (3 * negate i) d2)
previousPeriod (FiscalQuarter _) (d1, d2) i =
  Just (addGregorianMonthsClip (3 * negate i) d1,
   toEndOfMonth $ addGregorianMonthsClip (3 * negate i) d2)
previousPeriod (FiscalQuarterToDate _) (d1, d2) i =
  Just (addGregorianMonthsClip (3 * negate i) d1,
   addGregorianMonthsClip (3 * negate i) d2)
previousPeriod (CalendarYear _) (d1, d2) i =
  Just (addGregorianYearsClip (negate i) d1,
   addGregorianYearsClip (negate i) d2)
previousPeriod (CalendarYearToDate _) (d1, d2) i =
  Just (addGregorianYearsClip (negate i) d1,
   addGregorianYearsClip (negate i) d2)
previousPeriod (FiscalYear _) (d1, d2) i =
  Just (addGregorianYearsClip (negate i) d1,
   addGregorianYearsClip (negate i) d2)
previousPeriod (FiscalYearToDate _) (d1, d2) i =
  Just (addGregorianYearsClip (negate i) d1,
   addGregorianYearsClip (negate i) d2)
previousPeriod Last30Days (d1, d2) i =
  let nbDays = 30 * negate i
  in Just (addDays nbDays d1, addDays nbDays d2)
previousPeriod Last60Days (d1, d2) i =
  let nbDays = 60 * negate i
  in Just (addDays nbDays d1, addDays nbDays d2)
previousPeriod Last91Days (d1, d2) i =
  let nbDays = 90 * negate i
  in Just (addDays nbDays d1, addDays nbDays d2)
previousPeriod Last182Days (d1, d2) i =
  let nbDays = 182 * negate i
  in Just (addDays nbDays d1, addDays nbDays d2)
previousPeriod Last365Days (d1, d2) i =
  let nbDays = 365 * negate i
  in Just (addDays nbDays d1, addDays nbDays d2)
previousPeriod (SinceDateUntilTheEnd _) _ _ = Nothing
previousPeriod (SinceDateToToday _) _ _ = Nothing

-- Could fail to compute a DateSpan if there is no transactions
-- or if the ReportPeriod is inconsistant
reportPeriodToSpan :: ReportPeriod ->
                      Day ->
                      Ledger ->
                      Maybe DateSpan
reportPeriodToSpan AllDates _ l = lDateSpan l
reportPeriodToSpan (CustomPeriod d1 d2) _ _ = return (d1, d2)
reportPeriodToSpan (FromBeginningUntil d2) _ l =
  lDateSpan l >>= (\(d1, _) -> return (d1, d2))

reportPeriodToSpan (Month n) today _ =
  let (y, m, _) = toGregorian today
      d1 = addGregorianMonthsClip n $ fromGregorian y m 1
  in return (d1, toEndOfMonth d1)
reportPeriodToSpan (MonthToDate n) today _ =
  let (y, m, d) = toGregorian today
      d1 = addGregorianMonthsClip n $ fromGregorian y m 1
      (y1, m1, _) = toGregorian d1
      d2 = fromGregorian y1 m1 d
  in return (d1, d2)

reportPeriodToSpan (CalendarQuarter n) today _ =
  return $ computeCalendarQuarter n today
reportPeriodToSpan (CalendarQuarterToDate n) today _ =
  let (d1, _) = computeCalendarQuarter n today
  in return (d1, addGregorianMonthsClip (n * 3) today)

reportPeriodToSpan (FiscalQuarter n) today l =
  return $ computeFiscalQuarter n today (jfFirstFiscalMonth $ lJournalFile l)

reportPeriodToSpan (FiscalQuarterToDate n) today l =
  let (d1, _) = computeFiscalQuarter n today
                     (jfFirstFiscalMonth $ lJournalFile l)
  in return (d1, addGregorianMonthsClip (n * 3) today)

reportPeriodToSpan (CalendarYear n) today _ =
  let (y, _, _) = toGregorian today
      y1 = y + n
  in return (fromGregorian y1 1 1, fromGregorian y1 12 31)

reportPeriodToSpan (CalendarYearToDate n) today _ =
  let (y, m, d) = toGregorian today
      y1 = y + n
  in return (fromGregorian y1 1 1, fromGregorian y1 m d)

reportPeriodToSpan (FiscalYear n) today l =
  let firstFiscalMonth = jfFirstFiscalMonth $ lJournalFile l
  in return $ computeFiscalYear n today firstFiscalMonth
reportPeriodToSpan (FiscalYearToDate n) today l =
  let firstFiscalMonth = jfFirstFiscalMonth $ lJournalFile l
      (d1, _) = computeFiscalYear n today firstFiscalMonth
  in return (d1, addGregorianYearsClip n today)

reportPeriodToSpan Last30Days today _ = return (addDays (-29) today, today)
reportPeriodToSpan Last60Days today _ = return (addDays (-59) today, today)
reportPeriodToSpan Last91Days today _ = return (addDays (-90) today, today)
reportPeriodToSpan Last182Days today _ = return (addDays (-181) today, today)
reportPeriodToSpan Last365Days today _ = return (addDays (-364) today, today)
reportPeriodToSpan (SinceDateUntilTheEnd d1) _ l =
  lDateSpan l >>= (\(_, d2) -> return (d1, d2))
reportPeriodToSpan (SinceDateToToday d1) today _ = return (d1, today)

computeCalendarQuarter :: Integer -> Day -> (Day, Day)
computeCalendarQuarter n today =
  let f = addGregorianMonthsClip (n * 3)
  in bimap f f
     $ computeThisCalendarQuarter today


computeThisCalendarQuarter :: Day -> (Day, Day)
computeThisCalendarQuarter today =
  let (y, m, _) = toGregorian today
  in case ((m - 1) :: Int) `div` 3 of
    0 -> (fromGregorian y 1 1, fromGregorian y 3 31)
    1 -> (fromGregorian y 4 1, fromGregorian y 6 30)
    2 -> (fromGregorian y 7 1, fromGregorian y 9 31)
    _ -> (fromGregorian y 10 1, fromGregorian y 12 31)

computeFiscalQuarter :: Integer -> Day -> Int -> (Day, Day)
computeFiscalQuarter n today firstFiscalMonth =
  let f = addGregorianMonthsClip (n * 3)
  in bimap f f
     $ computeThisFiscalQuarter today firstFiscalMonth


computeThisFiscalQuarter :: Day -> Int -> (Day, Day)
computeThisFiscalQuarter today firstFiscalMonth =
  let (y, m, _) = toGregorian today
      monthNumber = if m >= firstFiscalMonth
                    then m - firstFiscalMonth
                    else m + 12 - firstFiscalMonth
      firstFiscalDay = if m >= firstFiscalMonth
                       then fromGregorian y firstFiscalMonth 1
                       else fromGregorian (y - 1) firstFiscalMonth 1
  in case (monthNumber :: Int) `div` 3 of
    0 -> (firstFiscalDay,
          toEndOfMonth $ addGregorianMonthsClip 2 firstFiscalDay)
    1 -> (addGregorianMonthsClip 3 firstFiscalDay,
          toEndOfMonth $ addGregorianMonthsClip 5 firstFiscalDay)
    2 -> (addGregorianMonthsClip 6 firstFiscalDay,
          toEndOfMonth $ addGregorianMonthsClip 8 firstFiscalDay)
    _ -> (addGregorianMonthsClip 9 firstFiscalDay,
          toEndOfMonth $ addGregorianMonthsClip 11 firstFiscalDay)

computeFiscalYear :: Integer -> Day -> Int -> (Day, Day)
computeFiscalYear n today firstFiscalMonth =
  let f = addGregorianYearsClip n
  in bimap f f
     $ computeThisFiscalYear today firstFiscalMonth

computeThisFiscalYear :: Day -> Int -> (Day, Day)
computeThisFiscalYear today firstFiscalMonth =
  let (y, m, _) = toGregorian today
      firstFiscalDay = if m >= firstFiscalMonth
                       then fromGregorian y firstFiscalMonth 1
                       else fromGregorian (y - 1) firstFiscalMonth 1
  in (firstFiscalDay,
      toEndOfMonth $ addGregorianMonthsClip 11 firstFiscalDay)

toEndOfMonth :: Day -> Day
toEndOfMonth date =
  let (y, m , _) = toGregorian date
  in fromGregorian y m $ gregorianMonthLength y m

-- | Which row (account) to show in the report
data ShowRow
  = ShowActive
  | ShowAll
  | ShowNonZero
  deriving (Eq, Show)

-- | Possible report columns
data GroupByColumns
  = Months
  | CalendarQuarters
  | FiscalQuarters
  | CalendarYears
  | FiscalYears
  | Counterparty [String]
  | Tag [String]
  deriving (Eq, Show)