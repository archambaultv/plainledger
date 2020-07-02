-- |
-- Module      :  Plainledger.Reports.BalanceSheet
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.BalanceSheet (
  reportToBalanceSheet,
  BalanceSheetOption(..),
  serializeForest
  )
where

import Data.Function
import Data.Tree
import Data.ByteString.Lazy (ByteString)
import Data.Csv (encode)
import Data.List hiding (group, lines)
import Plainledger.Ledger
import Plainledger.Reports.Report
import Prelude hiding (lines)
import Prelude hiding (lines)
import qualified Data.Text as T

data BalanceSheetOption = BalanceSheetOption {
  bsShowInactiveAccounts :: Bool
} deriving (Eq, Show)

serializeForest :: (ReportLine -> Quantity) ->
                   (ReportLine -> [T.Text]) ->
                   Forest ReportNode ->
                   [[T.Text]]
serializeForest computeBalance serialize xs =
  let ys = map (serializeTree computeBalance serialize) xs
  in adjustWhiteSpace (zip xs ys)

serializeTree :: (ReportLine -> Quantity) ->
                 (ReportLine -> [T.Text]) ->
                 Tree ReportNode ->
                 [[T.Text]]
serializeTree _ serialize (Node (RNReportLine l) _) = [serialize l]
serializeTree computeBalance serialize (Node n xs) =
  let children = serializeForest computeBalance serialize xs
      header :: [T.Text]
      header = [nodeName n]
      gr = nodeGroup n
      total :: [T.Text]
      total = (\q ->
                [T.append "Total " (nodeName n),
                 head $ serializeAmount NormallyPositive gr q])
            $ reportTotal computeBalance
            $ forestToReportLines xs
      totalLines = if all
                      (\case {RNReportLine _ -> True; _ -> False}
                       . rootLabel)
                      xs
                   then [total]
                   else [] : [total]
  in -- If all children are null (ex: inactive) we don't report this group
     if  all null children
     then [[]]
     else header : children ++ totalLines

adjustWhiteSpace :: [(Tree ReportNode, [[T.Text]])] -> [[T.Text]]
adjustWhiteSpace [] = []
adjustWhiteSpace ((_,[[]]):xs) = adjustWhiteSpace xs
adjustWhiteSpace (x:(_,[[]]):xs) = adjustWhiteSpace (x:xs)
adjustWhiteSpace [(_,x)] = x
adjustWhiteSpace ((Node (RNReportLine _ ) _, x):
                  n@(Node (RNReportLine _) _,_):
                  xss) = x ++ adjustWhiteSpace (n : xss)
adjustWhiteSpace ((_, x) : xs) = x ++ [[]] ++ adjustWhiteSpace xs

reportToBalanceSheet :: BalanceSheetOption -> Report -> ByteString
reportToBalanceSheet opt rep =
  let
    bsLines = filter (isBalanceSheetGroup . rlGroup) $ rLines rep
    forest = reportLinesToForest bsLines

    openBal = openingBalance $ rLines rep
    openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal $ rLedger rep
    earningsAmnt = earnings $ rLines rep
    earningsAcc = cEarningsAccount $ jConfiguration $ lJournal $ rLedger rep

    computeBalance :: ReportLine -> Quantity
    computeBalance y =
      if aId (rlAccount y) == openBalAcc
      then openBal + rlEndDateBalance y
      else if aId (rlAccount y) == earningsAcc
           then earningsAmnt + rlEndDateBalance y
           else rlEndDateBalance y

    serialize :: ReportLine -> [T.Text]
    serialize l
      | rlActive l == False
        && aId (rlAccount l) /= openBalAcc
        && aId (rlAccount l) /= earningsAcc
        && rlEndDateBalance l == 0
        && not (bsShowInactiveAccounts opt) = []
    serialize l@(ReportLine acc _ _ _) =
       let front = T.append (aName acc)
                 $ T.concat [" (", T.pack $ show $ aNumber acc, ")"]
           bal = computeBalance l
           gr = aGroup acc
           amnt = serializeAmount NormallyPositive gr bal
       in front : amnt

    -- Header lines
    title :: [[T.Text]]
    title = ["Balance Sheet"]
            : ["Journal file", T.pack $ rJournalFile rep]
            : ["Start date", T.pack $ show $ rBeginDate rep]
            : ["End date", T.pack $ show $ rEndDate rep]
            : []

    balanceSheetLines = serializeForest computeBalance serialize forest

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ [[]]
             ++ balanceSheetLines

  in encode csvlines
