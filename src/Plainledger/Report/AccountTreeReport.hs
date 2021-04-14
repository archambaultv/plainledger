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
  toKeepOrNotToKeep,
  QuantityInfo
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
                  ReportRow ->
                  [Tree NodeRows] ->
                  [ReportRow]
standardFormat param ledger today reportType bodyHeader ts =
  let period = atReportPeriod param
      lang = jfLanguage $ lJournalFile ledger
      reportName = i18nText lang reportType
      header = standardHeader ledger period today reportName
      footer = standardFooter ledger today
      body = intercalate [emptyRow]
           $ map treeToList ts 

  in header
     ++ [emptyRow]
     ++ (bodyHeader : body)
     ++ [emptyRow]
     ++ footer

  where treeToList :: Tree ([ReportRow], [ReportRow]) -> [ReportRow]
        treeToList (Node (before, after) xs) =
          before ++ concatMap treeToList xs ++ after

standardHeader :: Ledger -> ReportPeriod -> Day -> T.Text -> [ReportRow]
standardHeader ledger period today reportName =
  let lang = jfLanguage $ lJournalFile ledger
      header1 = (:[]) $ jfCompanyName $ lJournalFile ledger
      header2 = (:[]) reportName
      dateSpan = reportPeriodToSpan period today ledger
      header3 = (:[]) $ i18nText lang (TReportDateSpan dateSpan)
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

-- For reports where the quantity to display for each account
-- at each period is a single quantity and the correct way to
-- compute the total is to add up the children quantities
type QuantityInfo = ((Quantity, Account), NodeRows)

singleQuantityReport :: AccountTreeParam ->
                        Ledger ->
                        Day ->
                        ([DateSpan] -> Account -> (Quantity, T.Text, Bool)) -> 
                        [Tree QuantityInfo]
singleQuantityReport atp ledger today serializeNode =
  accountTreeReport atp ledger today serializeNode'

  where serializeNode' ds acc children =                  
          let (amnt, amntText, isActive) = serializeNode ds acc

              subTotal = sum $ map (fst . fst) children
              emptyChildren = null children

              name = nameWithNumber (aDisplayName acc) (aNumber acc)

              nodeLine :: [T.Text]
              nodeLine = if not emptyChildren && not isActive
                          then [name]
                          else  [name, amntText]

              total :: Quantity
              total = subTotal + amnt
              totalName = T.concat [i18nText lang TReportTotal,
                                    " ",
                                    aDisplayName acc]
              totalAmnt = qtyToNormallyPositive decimalSep (aAccountType acc) total
              totalLine = [[totalName, totalAmnt] | not emptyChildren]

              result = ((total, acc), ([nodeLine], totalLine))

          in toKeepOrNotToKeep isActive showRow emptyChildren (amnt == 0) result
  
        decimalSep = jfDecimalSeparator $ lJournalFile ledger
        lang = jfLanguage $ lJournalFile ledger
        showRow = atShowRow atp

toKeepOrNotToKeep :: Bool -> ShowRow -> Bool -> Bool -> a -> Maybe a
toKeepOrNotToKeep isActive showRow emptyChildren isZero x =
  case (isActive, showRow) of
    (_, ShowAll) -> Just x
    (False, _) -> if emptyChildren then Nothing else Just x
    (True, ShowNonZero) | isZero ->
      if emptyChildren then Nothing else Just x
    (True, _) -> Just x

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