-- |
-- Module      :  Plainledger.Reports.TrialBalance
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Reports.TrialBalance (
  TrialBalance(..),
  TrialBalanceLine(..),
  trialBalanceReport,
  trialBalanceToCsv,
  trialBalanceDefaultOption,
  BalanceFormat(..),
  TrialBalanceOption(..),
  trialBalanceTotal,
  trialBalanceTotalDrCr
  )
where

import Prelude hiding (lines)
import Data.Maybe
import Data.List hiding (group, lines)
import Data.Ord
import Data.Function
import Data.Time
import Prelude hiding (lines)
import Plainledger.Ledger
import Data.Csv (encode)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.ByteString.Lazy (ByteString)

-- | The TrialBalance data type contains all the necessary informations to
-- produce a balance sheet report. All the accounts are listed in the tbLines,
-- even those without any transaction at all.
data TrialBalance = TrialBalance {
  tbBeginDate :: LDate,
  tbEndDate :: LDate,
  tbJournalFile :: FilePath,
  tbLines :: [TrialBalanceLine],
  tbLedger :: Ledger
} deriving (Eq, Show)

data TrialBalanceLine = TrialBalanceLine {
  tblAccountId :: T.Text,
  tblCommodity :: Commodity,
  tblQuantity :: Maybe (Day, Quantity),
  tblGroup :: AccountGroup,
  tblAccount :: Account
} deriving (Eq, Show)

trialBalanceTotal :: [TrialBalanceLine] -> [(Commodity, Quantity)]
trialBalanceTotal ys =
      map (\xs -> (fst $ head xs, sum (map (snd . fromJust . snd) xs)))
      $ groupBy ((==) `on` fst)
      $ sortBy (comparing fst)
      $ filter (\(_,x) -> isJust x)
      $ map (\(TrialBalanceLine _ a b _ _) -> (a, b)) ys

trialBalanceTotalDrCr :: [TrialBalanceLine] ->
                         [(Commodity, (Quantity, Quantity))]
trialBalanceTotalDrCr ys =
      map (\xs ->
            let (dr, cr) = partition (> 0) $ map (snd . fromJust . snd) xs
            in (fst $ head xs,
                (sum dr,
                negate $ sum cr)))
      $ groupBy ((==) `on` fst)
      $ sortBy (comparing fst)
      $ filter (\(_,x) -> isJust x)
      $ map (\(TrialBalanceLine _ a b _ _) -> (a, b)) ys

trialBalanceReport :: FilePath -> LDate -> LDate -> Ledger -> TrialBalance
trialBalanceReport path beginDate endDate l =
  let accMap = lAccounts l
      defComm = cDefaultCommodity $ jConfiguration $ lJournal l
      bMap = lBalanceMap l
      bAtDate = HM.filterWithKey
                (\k _ -> isBalanceSheetGroup $ fst $ accMap HM.! k)
                bMap
      bDelta = HM.filterWithKey
                (\k _ -> isIncomeStatementGroup $ fst $ accMap HM.! k)
                bMap

      flatAtDate :: [(T.Text, Commodity, Maybe (Day, Quantity))]
      flatAtDate = flattenBalanceAtDate defComm bAtDate endDate

      flatDelta :: [(T.Text, Commodity, Maybe (Day, Quantity))]
      flatDelta =  flattenBalanceDelta defComm bDelta beginDate endDate

      toTrialBalance (aId, c, qty) =
        let a = accMap HM.! aId
            group = fst a
            acc = snd a
        in TrialBalanceLine aId c qty group acc

      lines :: [TrialBalanceLine]
      lines = map toTrialBalance (flatAtDate ++ flatDelta)

  in TrialBalance beginDate endDate path lines l

data BalanceFormat = TwoColumnDebitCredit | OneColumnSignedNumber
  deriving (Eq, Show)

data TrialBalanceOption = TrialBalanceOption {
  tboBalanceFormat :: BalanceFormat,
  showAccountsWithoutTransaction :: Bool
} deriving (Eq, Show)

trialBalanceDefaultOption :: TrialBalanceOption
trialBalanceDefaultOption = TrialBalanceOption TwoColumnDebitCredit False

amountTitle :: BalanceFormat -> [T.Text]
amountTitle OneColumnSignedNumber = ["Balance"]
amountTitle TwoColumnDebitCredit = ["Debit", "Credit"]

serializeAmount :: BalanceFormat -> AccountGroup -> Quantity -> [T.Text]
serializeAmount OneColumnSignedNumber _ x = [T.pack $ show x]
serializeAmount TwoColumnDebitCredit g x
  | x == 0 && isDebitGroup g = ["0",""]
  | x == 0 && isCreditGroup g = ["","0"]
  | x < 0 = ["",T.pack $ show $ negate x]
  | otherwise = [T.pack $ show x, ""]

trialBalanceToCsv :: TrialBalanceOption -> TrialBalance -> ByteString
trialBalanceToCsv opt tb =
  let
    -- Header lines
    title :: [[T.Text]]
    title = ["Trial Balance"]
            : ["Journal file", T.pack $ tbJournalFile tb]
            : ["Start date", T.pack $ show $ tbBeginDate tb]
            : ["End date", T.pack $ show $ tbEndDate tb]
            : []
            : ("Account number" : "Account Name"
               : amountTitle (tboBalanceFormat opt)
               ++ ["Commodity"])
            : []

    serialize :: TrialBalanceLine -> [T.Text]
    serialize (TrialBalanceLine _ _ Nothing _ _)
      | not (showAccountsWithoutTransaction opt) = []
    serialize (TrialBalanceLine _ _ (Just (d, 0)) _ _)
      | not (showAccountsWithoutTransaction opt) &&
        Date d < tbBeginDate tb = []
    serialize (TrialBalanceLine _ comm qty group acc) =
       let front = [T.pack $ show $ aNumber acc, aName acc]
           amnt = case qty of
                    Nothing -> serializeAmount (tboBalanceFormat opt) group 0
                    Just (_, x) -> serializeAmount (tboBalanceFormat opt) group x
       in front ++ amnt ++ [comm]

    trialBalanceLines = map serialize
                        $ sortBy (comparing (aNumber . tblAccount))
                        $ tbLines tb

     -- Total lines
    total :: [[T.Text]]
    total = case (tboBalanceFormat opt) of
              OneColumnSignedNumber ->
                map (\(c, q) -> ["", "Total"]
                                ++ [T.pack $ show q] ++ [c])
                $ trialBalanceTotal
                $ tbLines tb
              TwoColumnDebitCredit ->
                map (\(c, (dr, cr)) -> ["", "Total"]
                                    ++ [T.pack $ show dr, T.pack $ show cr]
                                    ++ [c])
                $ trialBalanceTotalDrCr
                $ tbLines tb

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ trialBalanceLines
             ++ ([] : total)

  in encode csvlines
