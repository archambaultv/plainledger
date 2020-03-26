-- |
-- Module      :  Plainledger.Reports.IncomeStatement
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.IncomeStatement (
  reportToIncomeStatement,
  IncomeStatementOption
  )
where

import Data.Function
import Data.ByteString.Lazy (ByteString)
import Data.Csv (encode)
import Data.Ord
import Data.List hiding (group, lines)
import Plainledger.Ledger
import Plainledger.Reports.Report
import Plainledger.Reports.BalanceSheet
import Prelude hiding (lines)
import Prelude hiding (lines)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

type IncomeStatementOption = BalanceSheetOption

reportToIncomeStatement :: IncomeStatementOption -> Report -> ByteString
reportToIncomeStatement opt rep =
  let
    isLines = filter (isIncomeStatementGroup . rlGroup) $ rLines rep
    forest = reportLinesToForest isLines

    serialize :: ReportLine -> [T.Text]
    serialize l
      | rlActive l == False
        && cashFlow l == 0
        && not (bsShowInactiveAccounts opt) = []
    serialize l@(ReportLine acc comm _ _ _ gr) =
       let front = T.append (aName acc)
                 $ T.concat [" (", T.pack $ show $ aNumber acc, ")"]
           bal = cashFlow l
           amnt = serializeAmount NormallyPositive gr bal
       in front : amnt ++ [comm]

    -- Header lines
    title :: [[T.Text]]
    title = ["Income Statement"]
            : ["Journal file", T.pack $ rJournalFile rep]
            : ["Start date", T.pack $ show $ rBeginDate rep]
            : ["End date", T.pack $ show $ rEndDate rep]
            : []

    incomeStatementLines = serializeForest cashFlow serialize forest

    earningsAmnt = earnings $ rLines rep
    earningsLines =
      map (\(c, q) ->
            ["Earnings",
             head $ serializeAmount NormallyPositive Liability q,
             c])
      $ sortBy (comparing fst)
      $ HM.toList
      earningsAmnt

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ [[]]
             ++ incomeStatementLines
             ++ [[]]
             ++ earningsLines

  in encode csvlines
