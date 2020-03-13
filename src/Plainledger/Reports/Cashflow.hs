-- |
-- Module      :  Plainledger.Reports.cashFlow
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.Cashflow (
  reportToCashFlow,
  CashFlowOption(..)
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Csv (encode)
import Data.List hiding (group, lines)
import Data.Ord
import Plainledger.Ledger
import Plainledger.Reports.Report
import Prelude hiding (lines)
import Prelude hiding (lines)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T


cashFlowTotal :: [ReportLine] -> HM.HashMap Commodity Quantity
cashFlowTotal ys =
    let xs :: [(Commodity, Quantity)]
        xs = map (\l -> (rlCommodity l, cashFlow l)) ys
    in HM.fromListWith (+) xs

cashFlowTotalDrCr :: [ReportLine] ->
                         HM.HashMap Commodity (Quantity, Quantity)
cashFlowTotalDrCr ys =
    let xs :: [(Commodity, (Quantity, Quantity))]
        xs = map (\(c, n) -> if n < 0 then (c, (0, negate n)) else (c, (n, 0)))
           $ map (\l -> (rlCommodity l, cashFlow l)) ys

    in HM.fromListWith (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) xs

data CashFlowOption = CashFlowOption {
  cfBalanceFormat :: BalanceFormat,
  cfShowInactiveAccounts :: Bool
} deriving (Eq, Show)


reportToCashFlow :: CashFlowOption -> Report -> ByteString
reportToCashFlow opt tb =
  let
    -- Header lines
    title :: [[T.Text]]
    title = ["cashFlow"]
            : ["Journal file", T.pack $ rJournalFile tb]
            : ["Start date", T.pack $ show $ rBeginDate tb]
            : ["End date", T.pack $ show $ rEndDate tb]
            : []
            : ("Account number" : "Account Name"
               : amountTitle (cfBalanceFormat opt)
               ++ ["Commodity"])
            : []

    serialize :: ReportLine -> [T.Text]
    serialize l
      | rlActive l == False
        && cashFlow l == 0
        && not (cfShowInactiveAccounts opt) = []
    serialize l@(ReportLine acc comm _ _ _ gr) =
       let front = [T.pack $ show $ aNumber acc, aName acc]
           bal = cashFlow l
           amnt = serializeAmount (cfBalanceFormat opt) gr bal
       in front ++ amnt ++ [comm]

    cashFlowLines = filter (not . null)
                  $ map serialize
                  $ sortBy (comparing (aNumber . rlAccount))
                  $ rLines tb

     -- Total lines
    total :: [[T.Text]]
    total = case (cfBalanceFormat opt) of
              OneColumnSignedNumber ->
                map (\(c, q) -> ["", "Total"]
                                ++ [T.pack $ show q] ++ [c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ cashFlowTotal
                $ rLines tb
              TwoColumnDebitCredit ->
                map (\(c, (dr, cr)) -> ["", "Total"]
                                    ++ [T.pack $ show dr, T.pack $ show cr]
                                    ++ [c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ cashFlowTotalDrCr
                $ rLines tb

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ cashFlowLines
             ++ ([] : total)

  in encode csvlines