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
  ReportRow
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

type ReportRow = [T.Text]

-- Computes
accountTreeReport :: AccountTreeParam ->
                    Ledger ->
                    Day ->
                    I18nText -> -- | Name of the report
                    (Account -> DateSpan -> (Quantity, ReportRow, Bool)) -> -- | How to serialize an account
                    ([(Quantity, [ReportRow])] -> [[ReportRow]]) -> -- | How to serialize top accounts
                    [ReportRow]
accountTreeReport atp ledger today reportType serializeNode serializeTopAccount =
  let reportName = i18nText lang reportType
      dateSpan = reportPeriodToSpan period today ledger
      body = maybe [] genericReportBody dateSpan
  in standardFormat period ledger today reportName body

  where

  showRow = atShowRow atp
  period = atReportPeriod atp

  genericReportBody :: DateSpan ->[[T.Text]]
  genericReportBody dates
    = let header = ["", i18nText lang TReportTotal]
          body :: [[T.Text]]
          body = intercalate [[]]
              $ serializeTopAccount
              $ map (serialize dates)
              $ lChartOfAccount ledger

      in header : body

  serialize :: DateSpan -> Tree Account -> (Quantity, [[T.Text]])
  serialize dates (Node acc xs) =
    let subAccounts :: [(Quantity, [[T.Text]])]
        subAccounts = map (serialize dates) xs
        subTotal = sum $ map fst subAccounts
        subLines = map rightPad $ concatMap snd subAccounts
        emptyChildren = null subLines

        name = nameWithNumber (aDisplayName acc)  (aNumber acc)
        (amnt, nodeData, isActive) = serializeNode acc dates
        isLeaf = null xs
        nodeLine :: [T.Text]
        nodeLine = if not isLeaf && not isActive
                  then [name]
                  else  name : nodeData

        total :: Quantity
        total = subTotal + amnt
        totalName = T.concat [i18nText lang TReportTotal,
                              " ",
                              aDisplayName acc]
        totalAmnt = qtyToNormallyPositive decimalSep (aAccountType acc) total
        totalLine = [[totalName, totalAmnt] | not isLeaf]

        allLines = nodeLine : subLines ++ totalLine
    in case (isActive, showRow) of
          (_, ShowAll) -> (total, allLines)
          (False, _) -> if emptyChildren then (0, []) else (total, allLines)
          (True, ShowNonZero) | amnt == 0 ->
            if emptyChildren then (0, []) else (total, allLines)
          (True, _) -> (total, allLines)

  decimalSep = jfDecimalSeparator $ lJournalFile ledger

  rightPad [] = []
  rightPad (x:xs) = T.append "  " x : xs

  lang = jfLanguage $ lJournalFile ledger

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

standardFormat :: ReportPeriod ->
                  Ledger ->
                  Day ->
                  T.Text ->
                  [[T.Text]] ->
                  [[T.Text]]
standardFormat period ledger today reportName body =
  let header = standardHeader ledger period today reportName
      footer = standardFooter ledger today
      body1 = case body of
              [] -> [[]]
              xs -> []
                    : xs
                    ++ [[]]

  in header
     ++ body1
     ++ footer

standardHeader :: Ledger -> ReportPeriod -> Day -> T.Text -> [[T.Text]]
standardHeader ledger period today reportName =
  let lang = jfLanguage $ lJournalFile ledger
      header1 = (:[]) $ jfCompanyName $ lJournalFile ledger
      header2 = (:[]) reportName
      dateSpan = reportPeriodToSpan period today ledger
      header3 = (:[]) $ i18nText lang (TReportDateSpan dateSpan)
  in [header1, header2, header3]

standardFooter :: Ledger -> Day -> [[T.Text]]
standardFooter ledger today =
  let lang = jfLanguage $ lJournalFile ledger
  in [(:[]) $ i18nText lang (TReportGeneratedOn today)]