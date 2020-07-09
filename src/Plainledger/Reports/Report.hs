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
  Period(..),
  BalanceFormat(..),
  computeTotal,
  computeTotalDrCr,
  isActive,
  isReportActive,
  cashFlow,
  reportCashFlow,
  balance,
  reportBalance,
  openingBalance,
  amountTitle,
  serializeAmount,
  periodToText,
  addList,
  reportLedgerOpeningBalance,
  reportEarnings,
  totalText,
  cataAccounts,
  maxSpan,
  flatReport,
  periodToSpan,
  FlatReportOption(..),
  GroupReportOption(..),
  groupReport
  )
where

import Data.Time
import Data.Tree
import Data.List
import Plainledger.Ledger
import Data.Functor.Foldable
import qualified Data.Text as T

data Period
  = Span LDate LDate -- | One period from start date to end date (both included)
  | MultiYear Day Int -- | MultiYear D X means yearly for X years, counting
                        --  backwards from D date (included)
  deriving (Eq, Show)

-- Returns the list of Begin and Enddate for each period
periodToSpan :: Period -> [(LDate, LDate)]
periodToSpan (Span b e) = [(b, e)]
periodToSpan (MultiYear _ n) | n <= 0 = []
periodToSpan (MultiYear d n) =
  let dNext = addGregorianYearsClip (-1) d
      b = addDays 1 dNext
  in periodToSpan (MultiYear dNext (n - 1)) ++ [(Date b, Date d)]

maxSpan :: Period -> (LDate, LDate)
maxSpan (Span b e) = (b, e)
maxSpan (MultiYear d n) =
  let dNext = addGregorianYearsClip (negate (fromIntegral n :: Integer)) d
      b = addDays 1 dNext
  in (Date b, Date d)


-- | The Report data type contains all the necessary informations to produce all
-- standard reports (trial balance, balance sheet, etc.) All the accounts are
-- listed in the reportLines, even those without any transaction at all.
data Report = Report {
  rPeriod :: Period,
  rJournalFile :: FilePath,
  rLedger :: Ledger
} deriving (Eq, Show)

data BalanceFormat
  = TwoColumnDebitCredit
  | NormallyPositive
  | InflowOutflow
  deriving (Eq, Show)

-- Standard way to report the requested period
periodToText :: Report -> [T.Text]
periodToText r =
  let bal = lBalanceMap $ rLedger r
      p = rPeriod r
  in case p of
      Span b e -> let b' = maybe "" (T.pack . show) $ lDateToDay bal b
                      e' = maybe "" (T.pack . show) $ lDateToDay bal e
                  in ["Start date", b'] ++
                     ["End date", e']
      MultiYear d _ -> ["Year-end", T.pack $ show d]

lDateToDay :: BalanceMap -> LDate -> Maybe Day
lDateToDay m MinDate = minDate m
lDateToDay m MaxDate = maxDate m
lDateToDay _ (Date d) = Just d

amountTitle :: BalanceFormat -> [T.Text]
amountTitle TwoColumnDebitCredit = ["Debit", "Credit"]
amountTitle _ = ["Balance"]

-- reportTotal :: (ReportLine -> Quantity) ->
--                [ReportLine] ->
--                Quantity
-- reportTotal f ys = sum $ map f ys

-- reportTotalDrCr :: (ReportLine -> Quantity) ->
--                    [ReportLine] ->
--                    (Quantity, Quantity)
-- reportTotalDrCr f ys =
--     let xs :: [(Quantity, Quantity)]
--         xs = map (\n -> if n < 0 then (0, negate n) else (n, 0))
--            $ map f ys
--     in (sum $ map fst xs, sum $ map snd xs)

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

-- Computes the cashflow for all the periods
reportCashFlow :: Account -> Report -> [Quantity]
reportCashFlow acc r =
  let m = lBalanceMap $ rLedger r
      ps = periodToSpan $ rPeriod r
  in map (cashFlow m acc) ps

isReportActive :: Account -> Report -> Bool
isReportActive acc r =
  let m = lBalanceMap $ rLedger r
      ps = periodToSpan $ rPeriod r
  in any (isActive m acc) ps

isActive :: BalanceMap -> Account -> (LDate, LDate) -> Bool
isActive m acc (d1, d2) =
  case balanceAtDate m (aId acc) d2 of
    Nothing -> False
    Just (d, _) -> Date d < d1

-- cashFlow m acc (d1, d2) computes the cashflow from the start of d1 to the end of d2.
cashFlow :: BalanceMap -> Account -> (LDate, LDate) -> Quantity
cashFlow m acc (d1, d2) = balance m acc d2 - openingBalance m acc d1

reportBalance :: Account -> Report -> [Quantity]
reportBalance acc r =
  let m = lBalanceMap $ rLedger r
      ps = map snd $ periodToSpan $ rPeriod r
  in map (balance m acc) ps

-- Computes the balance at the end of the day
balance :: BalanceMap -> Account -> LDate -> Quantity
balance m acc d = maybe 0 snd $ balanceAtDate m (aId acc) d

-- Computes the balance at the end of the previous day
openingBalance :: BalanceMap -> Account -> LDate -> Quantity
openingBalance _ _ MinDate = 0
openingBalance m acc (Date d) =
  maybe 0 snd $ balanceAtDate m (aId acc) (Date $ addDays (-1) d)
openingBalance m acc MaxDate =
  case balanceAtDate m (aId acc) MaxDate of
    Nothing -> 0
    Just (d, _) -> openingBalance m acc (Date d)

-- Compute the total for each "column", assuming the [Quantity] list all
-- have the same length
computeTotal :: [[Quantity]] -> [Quantity]
computeTotal [] = []
computeTotal ([]:_) = []
computeTotal xs =
  let xs' = map tail xs
      t = sum $ map head xs
  in t  : computeTotal xs'

computeTotalDrCr :: [[Quantity]] -> [(Quantity, Quantity)]
computeTotalDrCr [] = []
computeTotalDrCr ([]:_) = []
computeTotalDrCr xs =
  let xs' = map tail xs
      dr = sum $ filter (> 0) $ map head xs
      cr = negate $ sum $ filter (< 0) $ map head xs
  in (dr, cr)  : computeTotalDrCr xs'

-- Total lines
totalText :: BalanceFormat -> [[Quantity]] -> [T.Text]
totalText f xs =
  case f of
    InflowOutflow ->
      (\qs -> ["", "Total"] ++
              map (T.pack . show) qs)
      $ computeTotal xs
    TwoColumnDebitCredit ->
      (\qs -> ["", "Total"] ++
              concatMap (\(dr, cr) -> [T.pack $ show dr, T.pack $ show cr]) qs)
      $ computeTotalDrCr xs
    NormallyPositive -> []

-- Adds two list of quantities
addList :: [Quantity] -> [Quantity] -> [Quantity]
addList xs ys = map (uncurry (+)) $ zip xs ys

reportLedgerOpeningBalance :: Report -> [Quantity]
reportLedgerOpeningBalance r =
  let ps = map fst $ periodToSpan $ rPeriod r
  in map (ledgerOpeningBalance r) ps

-- | Computes the opening balance
ledgerOpeningBalance :: Report -> LDate -> Quantity
ledgerOpeningBalance r d = cata alg (lAccounts $ rLedger r)
  where m :: BalanceMap
        m = lBalanceMap $ rLedger r

        alg :: TreeF ChartNode Quantity -> Quantity
        alg = qtyAlgebra (\a -> openingBalance m a d)



reportEarnings :: Report -> [Quantity]
reportEarnings r =
    let ps = periodToSpan $ rPeriod r
    in map (earnings r) ps

-- -- | Computes the earnings
earnings :: Report -> (LDate, LDate) -> Quantity
earnings r d = cata alg (lAccounts $ rLedger r)
  where m :: BalanceMap
        m = lBalanceMap $ rLedger r

        alg :: TreeF ChartNode Quantity -> Quantity
        alg = qtyAlgebra (\a -> cashFlow m a d)

-- Helper for ledgerOpeningBalance and earnings
qtyAlgebra :: (Account -> Quantity) ->
              TreeF ChartNode Quantity ->
              Quantity
qtyAlgebra f (NodeF (CAccount a) _) = f a
qtyAlgebra _ (NodeF (Group _ x) xs) | isIncomeStatementGroup x = sum xs
qtyAlgebra _ (NodeF (Group _ _) _) = 0
qtyAlgebra _ (NodeF _ xs) = sum xs

-- Helper to extract from the chart of accounts informations about accounts
cataAccounts :: forall a . Report -> (Account -> Maybe a) -> [(Account, a)]
cataAccounts r f = cata alg (lAccounts $ rLedger r)
  where   alg :: TreeF ChartNode [(Account, a)] -> [(Account, a)]
          alg (NodeF (CAccount a) _) =
            case f a of
              Nothing -> []
              Just x -> [(a, x)]
          alg (NodeF _ xs) = concat xs

data FlatReportOption = FlatReportOption {
  frBalanceFormat :: BalanceFormat,
  frShowInactiveAccounts :: Bool
} deriving (Eq, Show)

-- Creates a report with only the accounts, no grouping by group subgroup ...
flatReport :: T.Text ->
              (Account -> Maybe [Quantity]) ->
              FlatReportOption ->
              Report ->
              [[T.Text]]
flatReport name serialize opt r =
  let
    title :: [[T.Text]]
    title = [name]
            : ["Journal file", T.pack $ rJournalFile r]
            : periodToText r :
            [[], ("Account number" : "Account Name" : amountTitle (frBalanceFormat opt))]

    accData :: [(Account, [Quantity])]
    accData = cataAccounts r serialize

    accLines :: [[T.Text]]
    accLines = map toText accData

    toText :: (Account, [Quantity]) -> [T.Text]
    toText (acc, bal) =
      let front = [T.pack $ show $ aNumber acc, aName acc]
          gr = aGroup acc
          amnt = concatMap (serializeAmount (frBalanceFormat opt) gr) bal
      in front ++ amnt

    total :: [T.Text]
    total = totalText (frBalanceFormat opt) (map snd accData)

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ accLines
             ++ ([] : [total])

  in csvlines

data GroupReportOption = GroupReportOption {
  grShowInactiveAccounts :: Bool
} deriving (Eq, Show)

-- Creates a report with both the accounts, groups, subgroups ...
groupReport :: T.Text ->
               (Account -> Maybe [Quantity]) ->
               (AccountGroup -> Bool) ->
               Report ->
               [[T.Text]]
groupReport name accountAlg keepGroup r =
  let
    -- Header lines
    title :: [[T.Text]]
    title = [name]
            : ["Journal file", T.pack $ rJournalFile r]
            : periodToText r : [[]]

    groupData = cata algFilter (lAccounts $ rLedger r)
    groupData2 = cata algText groupData

    -- Filters the unwanted accounts and precompute some value
    algFilter :: TreeF ChartNode (Tree (ChartNode, [Quantity])) ->
                 Tree (ChartNode, [Quantity])
    -- We compute the balance for the accounts
    algFilter (NodeF (CAccount a) _) =
      case accountAlg a of
        Nothing -> Node (CAccount a, []) []
        Just xs -> Node (CAccount a, xs) []
    -- We remove the income statement group
    algFilter (NodeF (Group n a) xs) =
      if keepGroup a
      then let xssQty = map (snd . rootLabel) xs
               qty = foldr addList (repeat 0) xssQty
           in  Node (Group n a, qty) xs
      else Node (Group n a, []) []
    -- We remove the empty sub and sub sub group
    algFilter (NodeF x xs) =
      let xs' = filter (not . null . snd . rootLabel) xs
      in case xs' of
          [] -> Node (x, []) []
          xss -> let xssQty = map (snd . rootLabel) xss
                     qty = foldr addList (repeat 0) xssQty
                 in Node (x, qty) xss

    algText :: TreeF (ChartNode, [Quantity]) [[T.Text]] ->
                 [[T.Text]]
    algText (NodeF (CAccount a, qty) _) = [accountText a qty]
    algText (NodeF (Root, _) xs) = intercalate [] xs
    algText (NodeF (n,qty) yss) =
      let header :: T.Text
          header = nodeName n
          footer = T.append "Total " header
          gr = nodeGroup n
          footerTotal :: [T.Text]
          footerTotal = concatMap (serializeAmount NormallyPositive gr) qty
          txt :: [[T.Text]]
          txt = [header] :
                (concat yss) ++
                [footer : footerTotal]
      in txt

    accountText :: Account -> [Quantity] -> [T.Text]
    accountText acc bal =
       let front = T.append (aName acc)
                 $ T.concat [" (", T.pack $ show $ aNumber acc, ")"]
           gr = aGroup acc
           amnt = concatMap (serializeAmount NormallyPositive gr) bal
       in front : amnt

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ [[]]
             ++ groupData2

  in csvlines
