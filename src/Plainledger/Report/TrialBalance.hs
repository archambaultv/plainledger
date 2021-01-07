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
      header1 = V.singleton $ jfCompanyName $ lJournalFile ledger
      header2 = V.singleton $ i18nText lang TReportTrialBalanceName
      dateSpan = reportPeriodToSpan period today ledger
      header3 = V.singleton $ i18nText lang (TReportDateSpan dateSpan)
      header4 = V.fromList [i18nText lang (TReportAccNumber),
                            i18nText lang (TReportAccName),
                            i18nText lang (TReportDebit),
                            i18nText lang (TReportCredit)]
      header :: V.Vector (V.Vector T.Text)
      header = V.fromList [header1, header2, header3, V.empty, header4]
  in case dateSpan of
        Nothing -> header
        Just x -> header V.++ trialBalanceBody showRow x ledger
      
trialBalanceBody :: ShowRow -> DateSpan -> Ledger -> V.Vector (V.Vector T.Text)
trialBalanceBody showRow dates ledger 
  = let lang = jfLanguage $ lJournalFile ledger
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
    in V.fromList $ body ++ [V.empty, total]
  where serialize :: Account -> Maybe (Quantity, V.Vector T.Text)
        serialize acc =
          let number = T.pack $ show $ aNumber acc
              name = aDisplayName acc
              amnt = trialBalanceQty ledger dates acc
              amntText = qtyToDebitCredit decimalSep (aType acc) 
                       $ fromMaybe 0 amnt
              line = V.fromList $ [number, name] ++ amntText
          in case (amnt, showRow) of
                (x, ShowAll) -> Just (fromMaybe 0 x, line)
                (Nothing, _) -> Nothing
                (Just x, ShowNonZero) | x == 0 -> Nothing
                (Just x, _) -> Just (x, line)

        decimalSep = jfDecimalSeparator $ lJournalFile ledger

qtyToDebitCredit :: Char -> AccountType -> Quantity -> [T.Text]
qtyToDebitCredit _ accType 0 = if isCreditType accType
                             then ["","0"]
                             else ["0",""]
qtyToDebitCredit c _ x | x < 0 = ["", writeAmount c $ negate x]
qtyToDebitCredit c _ x = [writeAmount c x, ""]