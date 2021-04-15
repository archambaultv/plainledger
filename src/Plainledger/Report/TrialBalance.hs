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
import Plainledger.Report.ListUtils
import Data.List ( intersperse )

type DebitCreditQty = (Quantity, Quantity)

trialBalanceReport :: ReportPeriod ->
                      Maybe CompareAnotherPeriod ->
                      ShowRow ->
                      Ledger ->
                      Day ->
                      [ReportRow]
trialBalanceReport period cp showRow ledger today =
  let atp = AccountTreeParam period cp showRow Nothing comparisonColumnsDefault
      p = atReportPeriod atp
      mp = atCompareAnotherPeriod atp
      dateSpan = reportPeriods p mp today ledger
      debitCredit = [i18nText lang TReportDebit,
                     i18nText lang TReportCredit]
      stdDateHeader = "" : "" : intersperse "" (standardDateHeader atp lang ledger today)
      bodyHeader :: [ReportRow]
      bodyHeader = [stdDateHeader,
                   [i18nText lang TReportAccNumber,
                    i18nText lang TReportAccName]
                    ++ concatMap (const debitCredit) dateSpan]
  in standardFormat atp ledger today TReportTrialBalanceName bodyHeader
    $ addRootaddTotal
    $ accountTreeReport atp ledger today serializeNode

  where
    serializeNode :: [DateSpan] ->
                     Account ->
                     [([DebitCreditQty], NodeRows)] ->
                     Maybe ([DebitCreditQty], NodeRows)
    serializeNode dates acc children =
          let number = T.pack $ maybe "" show $ aNumber acc
              name = aDisplayName acc
              amnt :: [Quantity]
              amnt = map (\d -> trialBalanceQty ledger d acc) dates
              isActiveByDate :: [Bool]
              isActiveByDate = map (\d -> isAccountActive ledger d acc) dates
              amntText :: ReportRow
              amntText = concatMap 
                         (\(a,qty) -> 
                           let t = qtyToDebitCredit decimalSep (aAccountType acc) qty
                           in toKeepOrNotToKeep2 a showRow (qty == 0) t ["",""])

                        $ zip isActiveByDate amnt
              isActive :: Bool
              isActive = or isActiveByDate
              line :: ReportRow
              line = [number, name] ++ amntText
              total :: [DebitCreditQty]
              total = newTotal amnt (map fst children)
          in  toKeepOrNotToKeep2 isActive showRow (null children)
                (Just (total, ([line], [])))
                (if null children then Nothing else Just (total, ([],[])))
            

    newTotal :: [Quantity] -> [[DebitCreditQty]] -> [DebitCreditQty]
    newTotal x xs =
      let debit :: [Quantity]
          debit = elementSum
                $ map (map fst) xs
          credit :: [Quantity]
          credit = elementSum
                 $ map (map snd) xs
          mkDrCr :: Quantity -> Quantity -> Quantity -> DebitCreditQty
          mkDrCr q d c = if q < 0
                         then (d, c - q)
                         else (d + q, c)
      in if null xs
         then zipWith3 mkDrCr x (repeat 0) (repeat 0)
         else zipWith3 mkDrCr x debit credit

    -- We add a root to avoid white rows between the top five accounts
    -- We add an extra Tree for a total
    addRootaddTotal :: [Tree ([DebitCreditQty], NodeRows)] -> [Tree NodeRows]
    addRootaddTotal xs =
      let qty :: [[DebitCreditQty]]
          qty = map (fst . rootLabel) xs
          debit :: [T.Text]
          debit = map (writeAmount decimalSep)
                $ elementSum
                $ map (map fst) qty
          credit :: [T.Text]
          credit = map (writeAmount decimalSep)
                $ elementSum
                $ map (map snd) qty

          totalDC :: [T.Text]
          totalDC = concat $ zipWith (\a b -> [a, b]) debit credit

          total = "" : i18nText lang TReportTotal : totalDC

          xs' = map (fmap snd) xs ++ [Node ([total],[]) []]
      in [Node ([], []) xs']

    decimalSep = jfDecimalSeparator $ lJournalFile ledger

    lang = jfLanguage $ lJournalFile ledger