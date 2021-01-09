-- |
-- Module      :  Plainledger.Reports.Report
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.Report 
(
  Ledger(..),
  journalToLedger,
  isAccountActive,
  standardReport,
  standardHeader,
  standardFooter,
  qtyToNormallyPositive,
  qtyToDebitCredit,
  ReportParams(..),
  ReportPeriod(..),
  reportPeriodToSpan,
  CompareAnotherPeriod(..),
  CompareExtraColumns(..),
  compareExtraColumnsDefault,
  ShowRow(..),
  DisplayColumns(..),
  trialBalanceQty,
  balanceSheetQty,
  TransactionCsvRecordType(..)
  )
where

import Data.Time
import Data.Bifunctor
import Plainledger.Journal
import Plainledger.I18n.I18n
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

qtyToDebitCredit :: Char -> AccountType -> Quantity -> [T.Text]
qtyToDebitCredit _ accType 0 = if isCreditType accType
                             then ["","0"]
                             else ["0",""]
qtyToDebitCredit c _ x | x < 0 = ["", writeAmount c $ negate x]
qtyToDebitCredit c _ x = [writeAmount c x, ""]

qtyToNormallyPositive :: Char -> AccountType -> Quantity -> T.Text
qtyToNormallyPositive c accType qty
  | isCreditType accType = writeAmount c $ negate qty
  | otherwise = writeAmount c qty

standardReport :: ReportPeriod -> 
                  Ledger ->
                  Day ->
                  T.Text ->
                  [V.Vector T.Text] ->
                  V.Vector (V.Vector T.Text)
standardReport period ledger today reportName body = 
  let header = standardHeader ledger period today reportName
      footer = standardFooter ledger today
      body1 = case body of
              [] -> [V.empty]
              xs -> V.empty 
                    : xs
                    ++ [V.empty]

  in V.fromList
     $ header 
     ++ body1
     ++ footer

standardHeader :: Ledger -> ReportPeriod -> Day -> T.Text -> [V.Vector T.Text]
standardHeader ledger period today reportName =
  let lang = jfLanguage $ lJournalFile ledger
      header1 = V.singleton $ jfCompanyName $ lJournalFile ledger
      header2 = V.singleton $ reportName
      dateSpan = reportPeriodToSpan period today ledger
      header3 = V.singleton $ i18nText lang (TReportDateSpan dateSpan)
  in [header1, header2, header3]

standardFooter :: Ledger -> Day -> [V.Vector T.Text]
standardFooter ledger today = 
  let lang = jfLanguage $ lJournalFile ledger
  in [V.singleton $ i18nText lang (TReportGeneratedOn today)]

-- Ledger is like a journal, but with some precomputed informations
-- usefull for reportings
data Ledger = Ledger {
  lJournalFile :: JournalFile,
  lAccounts   :: [Account],
  lTransactions :: [Transaction],
  lDateSpan :: Maybe DateSpan,
  lBalanceMap :: BalanceMap,
  lAccountMap :: HM.HashMap T.Text Account
}

journalToLedger :: Journal -> Ledger
journalToLedger journal =
  let jf = jJournalFile journal
      accs = jAccounts journal
      txns = jTransactions journal
      balMap = transactionsToBalanceMap txns
      dSpan = journalDateSpan journal
      accMap = HM.fromList $ map (\a -> (aId a, a)) accs
  in Ledger jf accs txns dSpan balMap accMap

trialBalanceQty :: Ledger -> DateSpan -> Account -> Quantity
trialBalanceQty ledger dateSpan acc =
  let openAcc = jfOpeningBalanceAccount $ lJournalFile ledger
      accId = aId acc
      balMap = lBalanceMap ledger
      accMaps = lAccountMap ledger
      accTypef = \a -> aType $ accMaps HM.! a
  in trialBalanceQuantity openAcc accTypef balMap accId (aType acc) dateSpan 

balanceSheetQty :: Ledger -> DateSpan -> Account -> Quantity
balanceSheetQty ledger dateSpan acc =
  let earnAcc = jfEarningsAccount $ lJournalFile ledger
      openAcc = jfOpeningBalanceAccount $ lJournalFile ledger
      accId = aId acc
      balMap = lBalanceMap ledger
      accMaps = lAccountMap ledger
      accTypef = \a -> aType $ accMaps HM.! a
  in balanceSheetQuantity earnAcc openAcc accTypef balMap accId dateSpan 

isAccountActive :: Ledger -> DateSpan -> Account -> Bool
isAccountActive ledger (d1, d2) acc =
  let balMap = lBalanceMap ledger
      accType = aType acc
  in case balanceAtDate balMap (aId acc) d2 of
        Nothing -> False
        Just (_, amnt) 
          | isBalanceSheetType accType && amnt /= 0 -> True
        Just (d, _) -> d >= d1

data CompareExtraColumns 
  = CompareExtraColumns {
      showDiff :: Bool,
      showPercent :: Bool,
      showRowPercent :: Bool,
      showColumnPercent :: Bool,
      showRevenuePercent :: Bool,
      showExpensePercent :: Bool
      }
  deriving (Eq, Show)

compareExtraColumnsDefault :: CompareExtraColumns
compareExtraColumnsDefault = CompareExtraColumns False False False False False False

data TransactionCsvRecordType = MultipleCsvRecords | SingleCsvRecord
  deriving (Eq, Show)

data ReportParams 
  -- Single or multi line transactions format
  = Transactions ReportPeriod (Maybe CompareAnotherPeriod) TransactionCsvRecordType
  | TrialBalance ReportPeriod (Maybe CompareAnotherPeriod) ShowRow
  -- Show Diff, Show Percent, % of Row, % of Column
  | BalanceSheet ReportPeriod (Maybe CompareAnotherPeriod) ShowRow (Maybe DisplayColumns)
    CompareExtraColumns
  -- Show Diff, Show Percent, % of Row, % of Column, % of revenue, % of expense
  | IncomeStatement ReportPeriod (Maybe CompareAnotherPeriod) ShowRow (Maybe DisplayColumns) 
    CompareExtraColumns
  deriving (Eq, Show)

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
  | Since30DaysAgo
  | Since60DaysAgo
  | Since90DaysAgo
  | Since365DaysAgo
  | SinceDateUntilTheEnd Day
  | SinceDateToDate Day
  deriving (Eq, Show)

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

reportPeriodToSpan Since30DaysAgo today _ = return (addDays (-30) today, today)
reportPeriodToSpan Since60DaysAgo today _ = return (addDays (-60) today, today)
reportPeriodToSpan Since90DaysAgo today _ = return (addDays (-90) today, today)
reportPeriodToSpan Since365DaysAgo today _ = return (addDays (-365) today, today)
reportPeriodToSpan (SinceDateUntilTheEnd d1) _ l = 
  lDateSpan l >>= (\(_, d2) -> return (d1, d2))
reportPeriodToSpan (SinceDateToDate d1) today _ = return (d1, today)

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
                    else (m + 12 - firstFiscalMonth)
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

-- | Which row to show in the report
data ShowRow
  = ShowActive
  | ShowAll
  | ShowNonZero
  deriving (Eq, Show)

-- | Possible report columns
data DisplayColumns
  = Months
  | CalendarQuarters
  | FiscalQuarters
  | CalendarYears
  | FiscalYears
  | Counterparty [String]
  | Tag [String]
  deriving (Eq, Show)
