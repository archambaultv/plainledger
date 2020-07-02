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
  nodeGroup,
  rlGroup
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
  rlEndDateBalance :: Quantity,
  rlOpeningBalance :: Quantity,
  rlActive :: Bool
} deriving (Eq, Show)

rlGroup :: ReportLine -> AccountGroup
rlGroup = aGroup . rlAccount

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
nodeGroup (RNReportLine l) = aGroup $ rlAccount l
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
      groupName = T.pack $ show accGroup
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
               Quantity
reportTotal f ys = sum $ map f ys

reportTotalDrCr :: (ReportLine -> Quantity) ->
                   [ReportLine] ->
                   (Quantity, Quantity)
reportTotalDrCr f ys =
    let xs :: [(Quantity, Quantity)]
        xs = map (\n -> if n < 0 then (0, negate n) else (n, 0))
           $ map f ys
    in (sum $ map fst xs, sum $ map snd xs)

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

      m1 :: HashMap
            T.Text
            (Maybe (Day, Quantity), Maybe (Day, Quantity))
      m1 = fmap (\m0 -> (M.lookupLT bDate m0, M.lookupLE eDate m0)) m

      m3 :: [(T.Text, Maybe (Day, Quantity), Maybe (Day, Quantity))]
      m3 = map (\(t, (q1,q2)) ->  (t, q1, q2))
         $ HM.toList m1

      toReport (aId, q1, q2) =
        let a = accMap HM.! aId
        in case q2 of
             Nothing -> ReportLine a 0 0 False
             Just (d, q2') ->
                let active = (d >= bDate)
                    q1' = (maybe 0 snd q1)
                in ReportLine a q2' q1' active

  in map toReport m3

-- | Computes the opening balance
openingBalance :: [ReportLine] -> Quantity
openingBalance rl
      = sum
      $ map rlOpeningBalance
      $ filter (isIncomeStatementGroup . aGroup . rlAccount) rl

-- | Computes the earnings
earnings :: [ReportLine] -> Quantity
earnings rl
  = sum
  $ map cashFlow
  $ filter (isIncomeStatementGroup . aGroup . rlAccount) rl
