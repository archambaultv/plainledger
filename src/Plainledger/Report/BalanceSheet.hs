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
  = let lang = jfLanguage $ lJournalFile ledger
        header = V.fromList [i18nText lang TReportAccName]
        lines1 = mapMaybe serialize
               $ sortOn aNumber
               $ filter (isBalanceSheetType . aAccountType)
               $ lAccounts ledger
        body = map snd lines1

    in header : body
  where serialize :: Account -> Maybe (Quantity, V.Vector T.Text)
        serialize acc =
          let name = nameWithNumber (aDisplayName acc)  (aNumber acc) 
              amnt = balanceSheetQty ledger dates acc
              amntText = qtyToNormallyPositive decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger dates acc
                       || acc == lEarningAccount ledger
                       || acc == lOpeningAccount ledger
              line = V.fromList $ [name, amntText]
          in case (isActive, showRow) of
                (_, ShowAll) -> Just (amnt, line)
                (False, _) -> Nothing
                (True, ShowNonZero) | amnt == 0 -> Nothing
                (True, _) -> Just (amnt, line)

        decimalSep = jfDecimalSeparator $ lJournalFile ledger