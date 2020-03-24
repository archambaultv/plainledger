-- |
-- Module      :  Plainledger.Reports.Report
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.Report (
  Report(..),
  ReportLine(..),
  BalanceFormat(..),
  ReportNode(..),
  report,
  reportLines,
  reportTotal,
  reportTotalDrCr,
  cashFlow,
  openingBalance,
  earnings,
  amountTitle,
  serializeAmount,
  reportLinesToForest,
  nodeNumber,
  nodeName,
  treeToReportLines,
  forestToReportLines,
  nodeGroup
  )
where

import Data.Function
import Control.Monad.Except
import Data.HashMap.Strict (HashMap)
import Data.List hiding (group, lines)
import Data.Maybe
import Data.Ord
import Data.Time
import Data.Tree
import Plainledger.Error
import Plainledger.Ledger
import Prelude hiding (lines)
import Prelude hiding (lines)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | The Report data type contains all the necessary informations to produce all
-- standard reports (trial balance, balance sheet, etc.) All the accounts are
-- listed in the reportLines, even those without any transaction at all.
data Report = Report {
  rBeginDate :: Day,
  rEndDate :: Day,
  rJournalFile :: FilePath,
  rLines :: [ReportLine],
  rLedger :: Ledger
} deriving (Eq, Show)

data ReportLine = ReportLine {
  rlAccount :: Account,
  rlCommodity :: Commodity,
  rlEndDateBalance :: Quantity,
  rlOpeningBalance :: Quantity,
  rlActive :: Bool,
  rlGroup :: AccountGroup
} deriving (Eq, Show)

data ReportNode
  = RNGroup Int T.Text AccountGroup
  | RNSubGroup Int T.Text AccountGroup
  | RNSubSubGroup Int T.Text AccountGroup
  | RNReportLine ReportLine

nodeNumber :: ReportNode -> Int
nodeNumber (RNGroup n _  _) = n
nodeNumber (RNReportLine l) = aNumber $ rlAccount l
nodeNumber (RNSubGroup n _ _) = n
nodeNumber (RNSubSubGroup n _ _) = n

nodeName :: ReportNode -> T.Text
nodeName (RNGroup _ n  _) = n
nodeName (RNReportLine l) = aName $ rlAccount l
nodeName (RNSubGroup _ n _) = n
nodeName (RNSubSubGroup _ n _) = n

nodeGroup :: ReportNode -> AccountGroup
nodeGroup (RNGroup _ _ n) = n
nodeGroup (RNReportLine l) = rlGroup l
nodeGroup (RNSubGroup _ _ n) = n
nodeGroup (RNSubSubGroup _ _ n) = n

forestToReportLines :: Forest ReportNode -> [ReportLine]
forestToReportLines = concatMap treeToReportLines

treeToReportLines :: Tree ReportNode -> [ReportLine]
treeToReportLines (Node (RNReportLine l) _) = [l]
treeToReportLines (Node _ xs) = forestToReportLines xs

reportLinesToForest :: [ReportLine] -> Forest ReportNode
reportLinesToForest ls =
  let groups = groupBy ((==) `on` rlGroup) $ sortBy (comparing rlGroup) ls
      groupNodes = sortBy (comparing (nodeNumber . rootLabel))
                 $ map mkGroupTree groups
  in groupNodes

mkGroupTree :: [ReportLine] -> Tree ReportNode
mkGroupTree xs =
  let accGroup = rlGroup $ head xs
      groupName = aGroup $ rlAccount $ head xs
      number = minimum $ map (aNumber . rlAccount) xs
      children = mkSubGroupForest accGroup xs
  in Node (RNGroup number groupName accGroup) children

mkForest :: (Account -> T.Text) ->
            ([ReportLine] -> Tree ReportNode) ->
            [ReportLine] ->
            Forest ReportNode
mkForest _ _ [] = []
mkForest gr mkTree ls =
  let (noGroups, groups') = partition (T.null . gr . rlAccount) ls
      groups :: [[ReportLine]]
      groups = groupBy ((==) `on` (gr . rlAccount))
             $ sortBy (comparing (gr . rlAccount)) groups'
      nodes :: Forest ReportNode
      nodes = sortBy (comparing (nodeNumber . rootLabel))
            $ map mkTree groups
  in insertNoGroups
     (sortBy (comparing (aNumber . rlAccount)) noGroups)
     nodes

mkSubGroupForest :: AccountGroup -> [ReportLine] -> Forest ReportNode
mkSubGroupForest accGroup = mkForest aSubgroup (mkSubGroupTree accGroup)

mkSubGroupTree :: AccountGroup -> [ReportLine] -> Tree ReportNode
mkSubGroupTree accGroup xs =
  let groupName = aSubgroup $ rlAccount $ head xs
      number = minimum $ map (aNumber . rlAccount) xs
      children = mkSubSubGroupForest accGroup xs
  in Node (RNSubGroup number groupName accGroup) children

mkSubSubGroupForest :: AccountGroup -> [ReportLine] -> Forest ReportNode
mkSubSubGroupForest accGroup = mkForest aSubsubgroup (mkSubSubGroupTree accGroup)

mkSubSubGroupTree :: AccountGroup -> [ReportLine] -> Tree ReportNode
mkSubSubGroupTree accGroup xs =
  let groupName = aSubsubgroup $ rlAccount $ head xs
      number = minimum $ map (aNumber . rlAccount) xs
      children :: Forest ReportNode
      children = sortBy (comparing (nodeNumber . rootLabel))
               $ map (flip Node [] . RNReportLine) xs
  in Node (RNSubSubGroup number groupName accGroup) children

insertNoGroups :: [ReportLine] -> Forest ReportNode -> Forest ReportNode
insertNoGroups xs [] = map (flip Node [] . RNReportLine) xs
insertNoGroups [] xs = xs
insertNoGroups (x : xs) (t:ts) =
  if (aNumber $ rlAccount x) <= (nodeNumber $ rootLabel t)
  then (Node (RNReportLine x) []) : insertNoGroups xs (t:ts)
  else t : insertNoGroups (x : xs) ts

data BalanceFormat
  = TwoColumnDebitCredit
  | NormallyPositive
  | InflowOutflow
  deriving (Eq, Show)

amountTitle :: BalanceFormat -> [T.Text]
amountTitle TwoColumnDebitCredit = ["Debit", "Credit"]
amountTitle _ = ["Balance"]

reportTotal :: (ReportLine -> Quantity) ->
               [ReportLine] ->
               HM.HashMap Commodity Quantity
reportTotal f ys =
    let xs :: [(Commodity, Quantity)]
        xs = map (\l -> (rlCommodity l, f l)) ys
    in HM.fromListWith (+) xs

reportTotalDrCr :: (ReportLine -> Quantity) ->
                   [ReportLine] ->
                   HM.HashMap Commodity (Quantity, Quantity)
reportTotalDrCr f ys =
    let xs :: [(Commodity, (Quantity, Quantity))]
        xs = map (\(c, n) -> if n < 0 then (c, (0, negate n)) else (c, (n, 0)))
           $ map (\l -> (rlCommodity l, f l)) ys

    in HM.fromListWith (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) xs

serializeAmount :: BalanceFormat -> AccountGroup -> Quantity -> [T.Text]
serializeAmount NormallyPositive g x
  | g `elem` [Asset, Expense] = [T.pack $ show x]
  | otherwise = [T.pack $ show $ negate x]
serializeAmount InflowOutflow _ x = [T.pack $ show x]
serializeAmount TwoColumnDebitCredit g x
  | x == 0 && isDebitGroup g = ["0",""]
  | x == 0 && isCreditGroup g = ["","0"]
  | x < 0 = ["",T.pack $ show $ negate x]
  | otherwise = [T.pack $ show x, ""]

cashFlow :: ReportLine -> Quantity
cashFlow r = rlEndDateBalance r - rlOpeningBalance r

lDateToDay :: BalanceMap -> LDate -> Maybe Day
lDateToDay m MinDate = minDate m
lDateToDay m MaxDate = maxDate m
lDateToDay _ (Date d) = Just d

report :: (MonadError Error m) =>
          FilePath -> LDate -> LDate -> Ledger -> m Report
report path beginDate endDate l = do
      eDate <- maybe (throwError "Cannot infer the end date since \
                                 \there is no transaction in the ledger")
               return
               $ lDateToDay (lBalanceMap l) endDate
      bDate <- maybe (throwError "Cannot infer the begin date since \
                                   \there is no transaction in the ledger")
               return
               $ lDateToDay (lBalanceMap l) beginDate

      let err b e = throwError
                     $ "Begin date ("
                     ++ show b
                     ++ ") greater than end date ("
                     ++ show e
                     ++ ")."

          errMax b e = throwError
                     $ "Begin date ("
                     ++ show b
                     ++ ") greater than the greatest date in the journal file ("
                     ++ show e
                     ++ ")."
      when (eDate < bDate)
           (case endDate of
             MaxDate -> errMax bDate eDate
             _ -> err bDate eDate)


      let lines = reportLines bDate eDate l

      return $ Report bDate eDate path lines l

reportLines :: Day -> Day -> Ledger -> [ReportLine]
reportLines bDate eDate l =
  let m = lBalanceMap l
      accMap = lAccounts l
      defComm = cDefaultCommodity $ jConfiguration $ lJournal l

      m1 :: HashMap
            T.Text
            (HashMap Commodity (Maybe (Day, Quantity), Maybe (Day, Quantity)))
      m1 = fmap (fmap (\m0 -> (M.lookupLT bDate m0, M.lookupLE eDate m0))) m

      m2 :: HashMap
            T.Text
            [(Commodity, Maybe (Day, Quantity), Maybe (Day, Quantity))]
      m2 = fmap (map (\(c, (q1, q2)) -> (c, q1, q2)) . HM.toList) m1

      m3 :: [(T.Text, Commodity, Maybe (Day, Quantity), Maybe (Day, Quantity))]
      m3 = concatMap (\(t, xs) -> if null xs
                                  then [(t, defComm, Nothing, Nothing)]
                                  else map (\(c,q1,q2) -> (t, c, q1, q2)) xs)
         $ HM.toList m2

      toReport (aId, c, q1, q2) =
        let a = accMap HM.! aId
            group = fst a
            acc = snd a
        in case q2 of
             Nothing -> ReportLine acc c 0 0 False group
             Just (d, q2') ->
                let active = (d >= bDate)
                    q1' = (maybe 0 snd q1)
                in ReportLine acc c q2' q1' active group

  in map toReport m3

-- | Computes the opening balance
openingBalance :: [ReportLine] -> HM.HashMap Commodity Quantity
openingBalance rl =
  let xs :: [(Commodity, Quantity)]
      xs = map (\l -> (rlCommodity l, rlOpeningBalance l))
         $ filter (isIncomeStatementGroup . rlGroup) rl

  in HM.fromListWith (+) xs

-- | Computes the earnings
earnings :: [ReportLine] -> HM.HashMap Commodity Quantity
earnings rl =
  let xs :: [(Commodity, Quantity)]
      xs = map (\l -> (rlCommodity l, cashFlow l))
         $ filter (isIncomeStatementGroup . rlGroup) rl

  in HM.fromListWith (+) xs
