-- |
-- Module      :  Plainledger.Reports.IncomeStatement
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.IncomeStatement (
  incomeStatementReport
  )
where

import Data.List
import Data.Ord
import Data.Maybe
import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.Report
import qualified Data.Vector as V
import qualified Data.Text as T


incomeStatementReport :: ReportPeriod -> 
                      (Maybe CompareAnotherPeriod) -> 
                      ShowRow -> 
                      (Maybe DisplayColumns) ->
                      CompareExtraColumns ->
                      Ledger ->
                      Day ->
                      V.Vector (V.Vector T.Text)
incomeStatementReport period _ showRow _ _ ledger today = 
  let lang = jfLanguage $ lJournalFile ledger
      reportName = i18nText lang TReportIncomeStatementName
      dateSpan = reportPeriodToSpan period today ledger
      body = case dateSpan of
              Nothing -> []
              Just x -> incomeStatementBody showRow x ledger
  in standardReport period ledger today reportName body
      
incomeStatementBody :: ShowRow -> DateSpan -> Ledger -> [(V.Vector T.Text)]
incomeStatementBody showRow dates ledger 
  = let lang = jfLanguage $ lJournalFile ledger
        header = V.fromList [i18nText lang (TReportAccName)]
        lines1 = mapMaybe serialize 
               $ sortBy (comparing aNumber) 
               $ filter (isIncomeStatementType . aType)
               $ lAccounts ledger
        body = map snd lines1
        total = sum $ map fst lines1
        footer = V.empty 
                : V.fromList [i18nText lang TReportEarnings, writeAmount decimalSep total]
                : []

    in header : body ++ footer
  where serialize :: Account -> Maybe (Quantity, V.Vector T.Text)
        serialize acc =
          let number = T.pack $ show $ aNumber acc
              name = T.concat [aDisplayName acc, " (", number, ")"]
              amnt = trialBalanceQty ledger dates acc
              amntText = qtyToNormallyPositive decimalSep (aType acc) amnt
              isActive = isAccountActive ledger dates acc
              line = V.fromList $ [name, amntText]
          in case (isActive, showRow) of
                (_, ShowAll) -> Just (amnt, line)
                (False, _) -> Nothing
                (True, ShowNonZero) | amnt == 0 -> Nothing
                (True, _) -> Just (amnt, line)

        decimalSep = jfDecimalSeparator $ lJournalFile ledger