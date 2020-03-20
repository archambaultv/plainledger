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
  BalanceSheetOption(..)
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

data BalanceSheetOption = BalanceSheetOption {
  bsShowInactiveAccounts :: Bool
} deriving (Eq, Show)

reportToBalanceSheet :: BalanceSheetOption -> Report -> ByteString
reportToBalanceSheet opt rep =
  let
    bsLines = filter (isBalanceSheetGroup . rlGroup) $ rLines rep

    -- Header lines
    title :: [[T.Text]]
    title = ["Balance Sheet"]
            : ["Journal file", T.pack $ rJournalFile rep]
            : ["Start date", T.pack $ show $ rBeginDate rep]
            : ["End date", T.pack $ show $ rEndDate rep]
            : []

    openBal = openingBalance $ rLines rep
    openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal $ rLedger rep
    earningsAmnt = earnings $ rLines rep
    earningsAcc = cEarningsAccount $ jConfiguration $ lJournal $ rLedger rep

    serialize :: ReportLine -> [T.Text]
    serialize l
      | rlActive l == False
        && aId (rlAccount l) /= openBalAcc
        && aId (rlAccount l) /= earningsAcc
        && rlEndDateBalance l == 0
        && not (bsShowInactiveAccounts opt) = []
    serialize l@(ReportLine acc comm _ _ _ gr) =
       let front = T.append (aName acc)
                 $ T.concat [" (", T.pack $ show $ aNumber acc, ")"]
           bal = computeBalance l
           amnt = serializeAmount NormallyPositive gr bal
       in front : amnt ++ [comm]

    computeBalance :: ReportLine -> Quantity
    computeBalance y =
      if aId (rlAccount y) == openBalAcc
      then let ob = fromMaybe 0
                  $ HM.lookup (rlCommodity y) openBal
           in ob + rlEndDateBalance y
      else if aId (rlAccount y) == earningsAcc
           then let earn = fromMaybe 0
                       $ HM.lookup (rlCommodity y) earningsAmnt
                in earn + rlEndDateBalance y
           else rlEndDateBalance y

    balanceSheetLines = filter (not . null)
                  $ map serialize
                  $ sortBy (comparing (aNumber . rlAccount))
                  $ bsLines

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ [[]]
             ++ balanceSheetLines

  in encode csvlines
