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
import Data.Ord
import Data.Maybe
import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.Report
import qualified Data.Vector as V
import qualified Data.Text as T


trialBalanceReport :: ReportPeriod -> 
                      (Maybe CompareAnotherPeriod) -> 
                      ShowRow -> 
                      Ledger ->
                      Day ->
                      V.Vector (V.Vector T.Text)
trialBalanceReport period _ showRow ledger today = 
  let lang = jfLanguage $ lJournalFile ledger
      reportName = i18nText lang TReportTrialBalanceName
      dateSpan = reportPeriodToSpan period today ledger
      body = case dateSpan of
              Nothing -> []
              Just x -> trialBalanceBody showRow x ledger
  in standardReport period ledger today reportName body
      
trialBalanceBody :: ShowRow -> DateSpan -> Ledger -> [(V.Vector T.Text)]
trialBalanceBody showRow dates ledger 
  = let header4 = V.fromList [i18nText lang (TReportAccNumber),
                              i18nText lang (TReportAccName),
                              i18nText lang (TReportDebit),
                              i18nText lang (TReportCredit)]
        lang = jfLanguage $ lJournalFile ledger
        lines1 = mapMaybe serialize 
              $ sortBy (comparing aNumber) 
              $ lAccounts ledger
        totalDebit = sum $ filter (> 0) $ map fst lines1
        totalCredit = sum $ filter (< 0) $ map fst lines1
        body = map snd lines1
        total = V.fromList ["",
                            i18nText lang TReportTotal,
                            writeAmount decimalSep totalDebit, 
                            writeAmount decimalSep $ negate totalCredit]
    in (header4 : body) ++ [total]
  where serialize :: Account -> Maybe (Quantity, V.Vector T.Text)
        serialize acc =
          let number = T.pack $ show $ aNumber acc
              name = aDisplayName acc
              amnt = trialBalanceQty ledger dates acc
              amntText = qtyToDebitCredit decimalSep (aType acc) amnt
              isActive = isAccountActive ledger dates acc
              line = V.fromList $ [number, name] ++ amntText
          in case (isActive, showRow) of
                (_, ShowAll) -> Just (amnt, line)
                (False, _) -> Nothing
                (True, ShowNonZero) | amnt == 0 -> Nothing
                (True, _) -> Just (amnt, line)

        decimalSep = jfDecimalSeparator $ lJournalFile ledger