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

import Data.Time
import Plainledger.I18n.I18n
import Plainledger.Journal
import Plainledger.Report.Ledger
import Plainledger.Report.AccountTreeParam
import Plainledger.Report.AccountTreeReport
import qualified Data.Text as T
import Data.Tree

type DebitCreditQty = (Quantity, Quantity)

trialBalanceReport :: ReportPeriod ->
                      Maybe CompareAnotherPeriod ->
                      ShowRow ->
                      Ledger ->
                      Day ->
                      [ReportRow]
trialBalanceReport period cp showRow ledger today =
  let atp = AccountTreeParam period cp showRow Nothing comparisonColumnsDefault
      bodyHeader = [i18nText lang TReportAccNumber,
                    i18nText lang TReportAccName,
                    i18nText lang TReportDebit,
                    i18nText lang TReportCredit]
  in standardFormat atp ledger today TReportTrialBalanceName bodyHeader
    $ addRootaddTotal
    $ accountTreeReport atp ledger today serializeNode

  where
    serializeNode :: [DateSpan] -> 
                     Account -> 
                     [(DebitCreditQty, NodeRows)] -> 
                     Maybe (DebitCreditQty, NodeRows)
    serializeNode dates acc children = 
          let d = head dates
              number = T.pack $ maybe "" show $ aNumber acc
              name = aDisplayName acc
              amnt = trialBalanceQty ledger d acc
              amntText = qtyToDebitCredit decimalSep (aAccountType acc) amnt
              isActive = isAccountActive ledger d acc
              line = [number, name] ++ amntText
              total = newTotal amnt (map fst children)
          in   case (isActive, showRow) of
                  (_, ShowAll) -> Just (total, ([line], []))
                  (False, _) -> if null children then Nothing else Just (total, ([],[]))
                  (True, ShowNonZero) | amnt == 0 ->
                    if null children then Nothing else Just (total, ([],[]))
                  (True, _) -> Just (total, ([line], []))

    newTotal :: Quantity -> [DebitCreditQty] -> DebitCreditQty
    newTotal x xs =
      let debit = sum $ map fst xs
          credit = sum $ map snd xs
      in if x < 0 
         then (debit, credit - x) 
         else (debit + x, credit)

    -- We add a root to avoid white rows between the top five accounts
    -- We add an extra Tree for a total
    addRootaddTotal :: [Tree (DebitCreditQty, NodeRows)] -> [Tree NodeRows]
    addRootaddTotal xs =
      let qty = map (fst . rootLabel) xs
          debit = writeAmount decimalSep $ sum $ map fst qty
          credit = writeAmount decimalSep $ sum $ map snd qty
          total = ["",i18nText lang TReportTotal, debit, credit]
          xs' = map (fmap snd) xs ++ [Node ([total],[]) []]
      in [Node ([], []) xs']

    decimalSep = jfDecimalSeparator $ lJournalFile ledger

    lang = jfLanguage $ lJournalFile ledger