-- |
-- Module      :  Plainledger.Reports.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.TrialBalance (
  reportToTrialBalance,
  TrialBalanceOption(..)
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Csv (encode)
import Data.List hiding (group, lines)
import Data.Maybe
import Data.Ord
import Plainledger.Ledger
import Plainledger.Reports.Report
import Prelude hiding (lines)
import Prelude hiding (lines)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

tbBalance :: ReportLine -> Quantity
tbBalance l =
  if isIncomeStatementGroup $ rlGroup l
  then cashFlow l
  else rlEndDateBalance l

trialBalanceTotal :: [ReportLine] -> HM.HashMap Commodity Quantity
trialBalanceTotal ys =
    let xs :: [(Commodity, Quantity)]
        xs = map (\l -> (rlCommodity l, tbBalance l)) ys
    in HM.fromListWith (+) xs

trialBalanceTotalDrCr :: [ReportLine] ->
                         HM.HashMap Commodity (Quantity, Quantity)
trialBalanceTotalDrCr ys =
    let xs :: [(Commodity, (Quantity, Quantity))]
        xs = map (\(c, n) -> if n < 0 then (c, (0, negate n)) else (c, (n, 0)))
           $ map (\l -> (rlCommodity l, tbBalance l)) ys

    in HM.fromListWith (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) xs

data TrialBalanceOption = TrialBalanceOption {
  tboBalanceFormat :: BalanceFormat,
  tboShowInactiveAccounts :: Bool
} deriving (Eq, Show)

reportToTrialBalance :: TrialBalanceOption -> Report -> ByteString
reportToTrialBalance opt tb =
  let
    -- Header lines
    title :: [[T.Text]]
    title = ["Trial Balance"]
            : ["Journal file", T.pack $ rJournalFile tb]
            : ["Start date", T.pack $ show $ rBeginDate tb]
            : ["End date", T.pack $ show $ rEndDate tb]
            : []
            : ("Account number" : "Account Name"
               : amountTitle (tboBalanceFormat opt)
               ++ ["Commodity"])
            : []

    openBal = openingBalance $ rLines tb
    openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal $ rLedger tb

    serialize :: ReportLine -> [T.Text]
    serialize l
      | rlActive l == False
        && tbBalance l == 0
        && not (tboShowInactiveAccounts opt) = []
    serialize l@(ReportLine acc comm _ _ _ gr) =
       let front = [T.pack $ show $ aNumber acc, aName acc]
           bal = computeBalance l
           amnt = serializeAmount (tboBalanceFormat opt) gr bal
       in front ++ amnt ++ [comm]

    computeBalance :: ReportLine -> Quantity
    computeBalance y =
      if aId (rlAccount y) == openBalAcc
      then let ob = fromMaybe 0
                  $ HM.lookup (rlCommodity y) openBal
           in ob + tbBalance y
      else tbBalance y

    trialBalLines = filter (not . null)
                  $ map serialize
                  $ sortBy (comparing (aNumber . rlAccount))
                  $ rLines tb

     -- Total lines
    total :: [[T.Text]]
    total = case (tboBalanceFormat opt) of
              OneColumnSignedNumber ->
                map (\(c, q) -> ["", "Total"]
                                ++ [T.pack $ show q] ++ [c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ trialBalanceTotal
                $ rLines tb
              TwoColumnDebitCredit ->
                map (\(c, (dr, cr)) -> ["", "Total"]
                                    ++ [T.pack $ show dr, T.pack $ show cr]
                                    ++ [c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ trialBalanceTotalDrCr
                $ rLines tb

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ trialBalLines
             ++ ([] : total)

  in encode csvlines
