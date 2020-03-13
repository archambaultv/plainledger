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
  report,
  reportLines,
  cashFlow,
  openingBalance,
  earnings,
  amountTitle,
  serializeAmount
  )
where

import Control.Monad.Except
import Data.HashMap.Strict (HashMap)
import Data.List hiding (group, lines)
import Data.Maybe
import Data.Ord
import Data.Time
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

data BalanceFormat = TwoColumnDebitCredit | OneColumnSignedNumber
  deriving (Eq, Show)

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
