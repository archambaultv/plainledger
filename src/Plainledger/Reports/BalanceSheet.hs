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

import Data.Function
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
    groups = groupBy ((==) `on` rlGroup) $ sortBy (comparing rlGroup) bsLines

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

    -- serializeGroup :: [ReportLine] -> [[T.Text]]
    -- serializeGroup [] = []
    -- serializeGroup xs =
    --   let header = [T.pack $ show $ rlGroup $ head xs]
    --       xs' = sortBy (comparing (aNumber . rlAccount)) xs
    --       ls = map serialize xs'
    --   in header : ls

    serializeGroup :: [ReportLine -> T.Text] -> [ReportLine] -> [[T.Text]]
    serializeGroup _ [] = []
    serializeGroup [] xs = map serialize
                         $ sortBy (comparing (aNumber . rlAccount)) xs
    serializeGroup (g:gs) xs =
      let h = g $ head xs
          header = if T.null h then [] else [h]
          gr = rlGroup $ head xs
          minNumber :: [ReportLine] -> Int
          minNumber [] = 0
          minNumber ys = minimum $ map (aNumber . rlAccount) ys
          xs' = case gs of
                 [] -> [xs]
                 (g2:_) -> sortBy (comparing minNumber)
                           $ groupBy ((==) `on` g2)
                           $ sortBy (comparing g2) xs
          children = concatMap (serializeGroup gs) xs'
          total :: [[T.Text]]
          total = map (\(c, q) ->
                    [T.append "Total " h,
                     head $ serializeAmount NormallyPositive gr q,
                     c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ reportTotal computeBalance xs
          totalHeader = if T.null h then [] else total
      in header : children ++ totalHeader

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
                      $ concatMap
                        (serializeGroup [aGroup . rlAccount,
                                        aSubgroup . rlAccount,
                                        aSubsubgroup . rlAccount])
                        groups

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ [[]]
             ++ balanceSheetLines

  in encode csvlines
