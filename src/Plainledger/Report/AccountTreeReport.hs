-- |
-- Module      :  Plainledger.Reports.AccountTreeReport
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.AccountTreeReport (
  accountTreeReport,
  isAccountActive,
  standardFormat,
  standardHeader,
  standardFooter,
  qtyToNormallyPositive,
  qtyToDebitCredit,
  nameWithNumber,
  trialBalanceQty,
  balanceSheetQty,
  ReportRow,
  addIndentation,
  NodeRows,
  singleQuantityReport,
  toKeepOrNotToKeep2,
  toKeepOrNotToKeep,
  QuantityInfo,
  standardDateHeader
  )
where

import Data.Time
import Plainledger.Journal
import Plainledger.I18n.I18n
import Plainledger.Report.Ledger
import qualified Data.Text as T
import Plainledger.Report.AccountTreeParam
import Data.Tree
import Data.List ( intercalate )
import Data.Maybe
import Plainledger.Report.ListUtils

type ReportRow = [T.Text]
type NodeRows = ([ReportRow], [ReportRow])

emptyRow :: ReportRow
emptyRow = []

-- Standard format of the reports
-- Each account can provide 2 ReportRow. One that will come before the 
-- rows of its children, the other one after the rows of its children.
-- The function do not print the second row for leaves.
standardFormat :: AccountTreeParam ->
                  Ledger ->
                  Day ->
                  I18nText ->
                  [ReportRow] ->
                  [Tree NodeRows] ->
                  [ReportRow]
standardFormat param ledger today reportType bodyHeader ts =
  let p = atReportPeriod param
      mc = atCompareAnotherPeriod param
      period = reportPeriods p mc today ledger
      lang = jfLanguage $ lJournalFile ledger
      reportName = i18nText lang reportType
      header = standardHeader ledger period reportName
      footer = standardFooter ledger today
      body = intercalate [emptyRow]
           $ map treeToList ts

  in header
     ++ [emptyRow]
     ++ (bodyHeader ++ body)
     ++ [emptyRow]
     ++ footer

  where treeToList :: Tree ([ReportRow], [ReportRow]) -> [ReportRow]
        treeToList (Node (before, after) xs) =
          before ++ concatMap treeToList xs ++ after

standardHeader :: Ledger -> [DateSpan] -> T.Text -> [ReportRow]
standardHeader ledger periods reportName =
  let lang = jfLanguage $ lJournalFile ledger
      header1 = (:[]) $ jfCompanyName $ lJournalFile ledger
      header2 = (:[]) reportName
      sd = map fst periods
      ed = map snd periods
      reportDate = if null periods 
                   then Nothing
                   else Just (minimum sd, maximum ed)
      header3 = (:[]) $ i18nText lang (TReportDateSpan reportDate)
  in [header1, header2, header3]

standardFooter :: Ledger -> Day -> [ReportRow]
standardFooter ledger today =
  let lang = jfLanguage $ lJournalFile ledger
  in [(:[]) $ i18nText lang (TReportGeneratedOn today)]

-- Add a left pad to make the hiarchy of accounts more visible
-- t1
--   t11
--   t12
-- t2
--   t21
--   t22
addIndentation :: [Tree NodeRows] -> [Tree NodeRows]
addIndentation = map (addIndentation' "")

  where addIndentation' pad (Node (x1, x2) cs) =
          let cs' = map (addIndentation' (T.append "  " pad)) cs
              padMe = \x -> if null x
                            then x
                            else T.append pad (head x) : tail x
          in Node (map padMe x1, map padMe x2) cs'

-- Applies a function from the leaf to the root
-- When foo returns Nothing, the node deleted
bottomUp :: (a -> [b] -> Maybe b) -> Tree a -> Maybe (Tree b)
bottomUp foo (Node x []) =  flip Node [] <$> foo x []
bottomUp foo (Node x xs) =
  let xs' = mapMaybe (bottomUp foo) xs
  in  flip Node xs' <$> foo x (map rootLabel xs')

-- Applies serializeNode with bottomUp and takes
-- care of a few plumbing issues
accountTreeReport :: forall a .
                     AccountTreeParam ->
                     Ledger ->
                     Day ->
                     ([DateSpan] -> Account -> [a] -> Maybe a) ->
                     [Tree a]
accountTreeReport atp ledger today serializeNode =
  let
      p = atReportPeriod atp
      mp = atCompareAnotherPeriod atp
      dateSpan = reportPeriods p mp today ledger
      body = if null dateSpan
             then []
             else toReportRowTree dateSpan
  in body

  where

  toReportRowTree :: [DateSpan] -> [Tree a]
  toReportRowTree dates = mapMaybe (serialize dates)
                          $ lChartOfAccount ledger

  serialize :: [DateSpan] -> Tree Account -> Maybe (Tree a)
  serialize dates t = bottomUp (serializeNode dates) t

-- | The column header
standardDateHeader :: AccountTreeParam -> Language -> Ledger ->
                        Day ->[T.Text]
standardDateHeader atp lang ledger today =
  let mc = atCompareAnotherPeriod atp
      p = atReportPeriod atp
      dateSpan = reportPeriods p mc today ledger
  in standardDateHeader' p lang dateSpan

standardDateHeader' :: ReportPeriod ->
                      Language ->
                      [DateSpan] ->
                      [T.Text]                      
standardDateHeader' (Month _) lang ds = 
  map (i18nText lang . TReportMonthSpan . fst) ds
standardDateHeader' (CalendarYear _) lang ds = 
  map (i18nText lang . TReportYearSpan . snd) ds
standardDateHeader' (CalendarYearToDate _) lang ds = 
  map (i18nText lang . TReportYearSpan . snd) ds
standardDateHeader' (FiscalYear _) lang ds = 
  map (i18nText lang . TReportYearSpan . snd) ds
standardDateHeader' (FiscalYearToDate _) lang ds = 
  map (i18nText lang . TReportYearSpan . snd) ds
standardDateHeader' _ lang ds = 
  map (i18nText lang . TReportDateSpan . Just) ds

-- For reports where the quantity to display for each account
-- at each period is a single quantity and the correct way to
-- compute the total is to add up the children quantities
type QuantityInfo = (([Quantity], Account), NodeRows)

singleQuantityReport :: AccountTreeParam ->
                        Ledger ->
                        Day ->
                        ([DateSpan] -> Account -> ([Quantity], Bool)) ->
                        [Tree QuantityInfo]
singleQuantityReport atp ledger today serializeNode =
  accountTreeReport atp ledger today serializeNode'

  where serializeNode' :: [DateSpan] -> Account -> [QuantityInfo] -> Maybe QuantityInfo
        serializeNode' ds acc children =
          let (amnt, isActive) = serializeNode ds acc

              amntText :: ReportRow
              amntText = map (qtyToNormallyPositive decimalSep (aAccountType acc)) amnt

              childrenQty :: [[Quantity]]
              childrenQty = map (fst . fst) children

              subTotal :: [Quantity]
              subTotal = elementSum childrenQty

              emptyChildren :: Bool
              emptyChildren = null children

              name :: T.Text
              name = nameWithNumber (aDisplayName acc) (aNumber acc)

              nodeLine :: ReportRow
              nodeLine = if not emptyChildren && not isActive
                          then [name]
                          else  name : amntText

              total :: [Quantity]
              total = elementAddition subTotal amnt

              totalName :: T.Text
              totalName = T.concat [i18nText lang TReportTotal,
                                    " ",
                                    aDisplayName acc]
              totalAmnt :: ReportRow
              totalAmnt = map (qtyToNormallyPositive decimalSep (aAccountType acc))
                          total

              totalLine :: [ReportRow]
              totalLine = [totalName : totalAmnt | not emptyChildren]

              result :: QuantityInfo
              result = ((total, acc), ([nodeLine], totalLine))

          in toKeepOrNotToKeep isActive showRow emptyChildren (all (== 0) amnt) result

        decimalSep = jfDecimalSeparator $ lJournalFile ledger
        lang = jfLanguage $ lJournalFile ledger
        showRow = atShowRow atp

toKeepOrNotToKeep2 :: Bool -> ShowRow -> Bool -> a -> a -> a
toKeepOrNotToKeep2 isActive showRow isZero keepValue rejectValue =
  case (isActive, showRow) of
    (_, ShowAll) -> keepValue
    (False, _) -> rejectValue
    (True, ShowNonZero) | isZero -> keepValue
    (True, _) -> keepValue

toKeepOrNotToKeep :: Bool -> ShowRow -> Bool -> Bool -> a -> Maybe a
toKeepOrNotToKeep isActive showRow emptyChildren isZero x =
  toKeepOrNotToKeep2 isActive showRow isZero (Just x)
   (if emptyChildren then Nothing else Just x)

  -- case (isActive, showRow) of
  --   (_, ShowAll) -> Just x
  --   (False, _) -> if emptyChildren then Nothing else Just x
  --   (True, ShowNonZero) | isZero ->
  --     if emptyChildren then Nothing else Just x
  --   (True, _) -> Just x

trialBalanceQty :: Ledger -> DateSpan -> Account -> Quantity
trialBalanceQty ledger dateSpan acc =
  let balMap = lBalanceMap ledger
  in trialBalanceQuantity balMap acc dateSpan

balanceSheetQty :: Ledger -> DateSpan -> Account -> Quantity
balanceSheetQty ledger dateSpan acc =
  let balMap = lBalanceMap ledger
  in balanceSheetQuantity balMap acc dateSpan

isAccountActive :: Ledger -> DateSpan -> Account -> Bool
isAccountActive ledger (d1, d2) acc =
  let balMap = lBalanceMap ledger
      accType = aAccountType acc
  in case balanceAtDate balMap acc d2 of
        Nothing -> False
        Just (_, amnt)
          | isBalanceSheetType accType && amnt /= 0 -> True
        Just (d, _) -> d >= d1

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

nameWithNumber :: T.Text -> Maybe Int -> T.Text
nameWithNumber x Nothing = x
nameWithNumber x (Just i) = T.concat [x, " (", T.pack $ show i, ")"]