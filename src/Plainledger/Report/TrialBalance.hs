-- |
-- Module      :  Plainledger.Reports.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.TrialBalance (
  trialBalanceReport
  )
where

import Data.List
import Data.Maybe
import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.Ledger
import Plainledger.Report.AccountTreeParam
import Plainledger.Report.AccountTreeReport
import qualified Data.Text as T


trialBalanceReport :: ReportPeriod ->
                      Maybe CompareAnotherPeriod ->
                      ShowRow ->
                      Ledger ->
                      Day ->
                      [ReportRow]
trialBalanceReport period _ showRow ledger today =
  let lang = jfLanguage $ lJournalFile ledger
      reportName = i18nText lang TReportTrialBalanceName
      dateSpan = reportPeriodToSpan period today ledger
      body = case dateSpan of
              Nothing -> []
              Just x -> trialBalanceBody showRow x ledger
  in standardFormat period ledger today reportName body

trialBalanceBody :: ShowRow -> DateSpan -> Ledger -> [ReportRow]
trialBalanceBody showRow dates ledger
  = let header4 = [i18nText lang TReportAccNumber,
                              i18nText lang TReportAccName,
                              i18nText lang TReportDebit,
                              i18nText lang TReportCredit]
        lang = jfLanguage $ lJournalFile ledger
        lines1 = mapMaybe serialize
              $ sortOn aNumber
              $ lAccounts ledger
        totalDebit = sum $ filter (> 0) $ map fst lines1
        totalCredit = sum $ filter (< 0) $ map fst lines1
        body = map snd lines1
        total = ["",
                            i18nText lang TReportTotal,
                            writeAmount decimalSep totalDebit,
                            writeAmount decimalSep $ negate totalCredit]
    in (header4 : body) ++ [total]
  where serialize :: Account -> Maybe (Quantity, ReportRow)
        serialize acc =
          let number = T.pack $ maybe "" show $ aNumber acc
              name = aDisplayName acc
              amnt = trialBalanceQty ledger dates acc
              amntText = qtyToDebitCredit decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger dates acc
              line = [number, name] ++ amntText
          in case (isActive, showRow) of
                (_, ShowAll) -> Just (amnt, line)
                (False, _) -> Nothing
                (True, ShowNonZero) | amnt == 0 -> Nothing
                (True, _) -> Just (amnt, line)

        decimalSep = jfDecimalSeparator $ lJournalFile ledger