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
import qualified Data.Text as T


cashFlowTotal :: [ReportLine] -> Quantity
cashFlowTotal = reportTotal cashFlow

cashFlowTotalDrCr :: [ReportLine] -> (Quantity, Quantity)
cashFlowTotalDrCr = reportTotalDrCr cashFlow

data CashFlowOption = CashFlowOption {
  cfBalanceFormat :: BalanceFormat,
  cfShowInactiveAccounts :: Bool
} deriving (Eq, Show)


reportToCashFlow :: CashFlowOption -> Report -> ByteString
reportToCashFlow opt tb =
  let
    -- Header lines
    title :: [[T.Text]]
    title = ["Cashflow"]
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
    serialize l@(ReportLine acc _ _ _) =
       let front = [T.pack $ show $ aNumber acc, aName acc]
           bal = cashFlow l
           gr = aGroup acc
           amnt = serializeAmount (cfBalanceFormat opt) gr bal
       in front ++ amnt

    cashFlowLines = filter (not . null)
                  $ map serialize
                  $ sortBy (comparing (aNumber . rlAccount))
                  $ rLines tb

     -- Total lines
    total :: [T.Text]
    total = case (cfBalanceFormat opt) of
              InflowOutflow ->
                (\q -> ["", "Total"]
                                ++ [T.pack $ show q])
                $ cashFlowTotal
                $ rLines tb
              TwoColumnDebitCredit ->
                (\(dr, cr) -> ["", "Total"]
                                    ++ [T.pack $ show dr, T.pack $ show cr])
                $ cashFlowTotalDrCr
                $ rLines tb
              NormallyPositive -> []

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ cashFlowLines
             ++ ([] : [total])

  in encode csvlines
