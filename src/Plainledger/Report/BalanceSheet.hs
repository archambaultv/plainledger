-- |
-- Module      :  Plainledger.Reports.BalanceSheet
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.BalanceSheet (
  balanceSheetReport
  )
where

import Data.List
import Data.Maybe
import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.Report
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Tree


balanceSheetReport :: ReportPeriod ->
                      Maybe CompareAnotherPeriod ->
                      ShowRow ->
                      Maybe DisplayColumns ->
                      CompareExtraColumns ->
                      Ledger ->
                      Day ->
                      V.Vector (V.Vector T.Text)
balanceSheetReport period _ showRow _ _ ledger today =
  let lang = jfLanguage $ lJournalFile ledger
      reportName = i18nText lang TReportBalanceSheetName
      dateSpan = reportPeriodToSpan period today ledger
      body = case dateSpan of
              Nothing -> []
              Just x -> balanceSheetBody showRow x ledger
  in standardReport period ledger today reportName body

balanceSheetBody :: ShowRow -> DateSpan -> Ledger -> [V.Vector T.Text]
balanceSheetBody showRow dates ledger
  = let header = V.fromList ["", i18nText lang TReportTotal]
        body :: [V.Vector T.Text]
        body = intercalate [V.empty]
             $ map (snd . serialize)
             $ take 3 -- Asset, Liability, Equity
             $ lChartOfAccount ledger

    in header : body
  where serialize :: Tree Account -> (Quantity, [V.Vector T.Text])
        serialize (Node acc xs) =
          let subAccounts :: [(Quantity, [V.Vector T.Text])]
              subAccounts = map serialize xs
              subTotal = sum $ map fst subAccounts
              subLines = map rightPad $ concatMap snd subAccounts
              emptyChildren = null subLines

              name = nameWithNumber (aDisplayName acc)  (aNumber acc)
              amnt = balanceSheetQty ledger dates acc
              amntText = qtyToNormallyPositive decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger dates acc
                       || acc == lEarningAccount ledger
                       || acc == lOpeningAccount ledger
              isLeaf = null xs
              nodeLine :: V.Vector T.Text
              nodeLine = if not isLeaf && not isActive
                         then V.fromList [name]
                         else V.fromList [name, amntText]
              total :: Quantity
              total = subTotal + amnt
              totalName = T.concat [i18nText lang TReportTotal,
                                    " ",
                                    aDisplayName acc]
              totalAmnt = qtyToNormallyPositive decimalSep (aAccountType acc) total
              totalLine = [V.fromList [totalName, totalAmnt] | not isLeaf]

              allLines = nodeLine : subLines ++ totalLine
          in case (isActive, showRow) of
                (_, ShowAll) -> (total, allLines)
                (False, _) -> if emptyChildren then (0, []) else (total, allLines)
                (True, ShowNonZero) | amnt == 0 ->
                  if emptyChildren then (0, []) else (total, allLines)
                (True, _) -> (total, allLines)

        decimalSep = jfDecimalSeparator $ lJournalFile ledger

        rightPad x | V.null x = x
        rightPad x = x V.// [(0,T.append "  " (x V.! 0))]

        lang = jfLanguage $ lJournalFile ledger