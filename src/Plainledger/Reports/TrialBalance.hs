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
  trialBalanceLines,
  trialBalanceToCsv,
  BalanceFormat(..),
  TrialBalanceOption(..),
  trialBalanceTotal,
  trialBalanceTotalDrCr,
  openingBalance,
  earnings
  )
where

import Control.Monad.Except
import Data.ByteString.Lazy (ByteString)
import Data.Csv (encode)
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

-- | The TrialBalance data type contains all the necessary informations to
-- produce a balance sheet report. All the accounts are listed in the tbLines,
-- even those without any transaction at all.
data TrialBalance = TrialBalance {
  tbBeginDate :: Day,
  tbEndDate :: Day,
  tbJournalFile :: FilePath,
  tbLines :: [TrialBalanceLine],
  tbLedger :: Ledger
} deriving (Eq, Show)

data TrialBalanceLine = TrialBalanceLine {
  tblAccount :: Account,
  tblCommodity :: Commodity,
  tblBalance :: Quantity, -- | The balance shown in the trial balance.
                          -- Equivalent to tblEndDateBalance for Assets, Liabilities
                          -- and Equity. Equivalent to tblEndDateBalance - tblOpeningBalance
                          -- for Revenue and Expense. The opening balance account is also
                          -- adjusted according to the begin date.
  tblEndDateBalance :: Quantity,
  tblOpeningBalance :: Quantity,
  tblActive :: Bool,
  tblGroup :: AccountGroup
} deriving (Eq, Show)

trialBalanceTotal :: [TrialBalanceLine] -> HM.HashMap Commodity Quantity
trialBalanceTotal ys =
    let xs :: [(Commodity, Quantity)]
        xs = map (\l -> (tblCommodity l, tblBalance l)) ys
    in HM.fromListWith (+) xs

trialBalanceTotalDrCr :: [TrialBalanceLine] ->
                         HM.HashMap Commodity (Quantity, Quantity)
trialBalanceTotalDrCr ys =
    let xs :: [(Commodity, (Quantity, Quantity))]
        xs = map (\(c, n) -> if n < 0 then (c, (0, negate n)) else (c, (n, 0)))
           $ map (\l -> (tblCommodity l, tblBalance l)) ys

    in HM.fromListWith (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) xs

lDateToDay :: BalanceMap -> LDate -> Maybe Day
lDateToDay m MinDate = minDate m
lDateToDay m MaxDate = maxDate m
lDateToDay _ (Date d) = Just d

trialBalanceReport :: (MonadError Error m) =>
                      FilePath -> LDate -> LDate -> Ledger -> m TrialBalance
trialBalanceReport path beginDate endDate l = do
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


      let lines = trialBalanceLines bDate eDate l

      return $ TrialBalance bDate eDate path lines l

trialBalanceLines :: Day -> Day -> Ledger -> [TrialBalanceLine]
trialBalanceLines bDate eDate l =
  let m = lBalanceMap l
      accMap = lAccounts l
      defComm = cDefaultCommodity $ jConfiguration $ lJournal l
      openBalAcc = cOpeningBalanceAccount $ jConfiguration $ lJournal l

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

      toTrialBalance (aId, c, q1, q2) =
        let a = accMap HM.! aId
            group = fst a
            acc = snd a
        in case q2 of
             Nothing -> TrialBalanceLine acc c 0 0 0 False group
             Just (d, q2') ->
                let active = (d >= bDate)
                    q1' = (maybe 0 snd q1)
                    q = if isIncomeStatementGroup group
                        then q2' - q1'
                        else q2'
                in TrialBalanceLine acc c q q2' q1' active group

      lines = map toTrialBalance m3

      openBal = openingBalance lines

      updateOpenBal :: TrialBalanceLine -> TrialBalanceLine
      updateOpenBal y =
        if aId (tblAccount y) == openBalAcc
        then let ob = fromMaybe 0
                    $ HM.lookup (tblCommodity y) openBal
             in y{tblBalance = ob + tblBalance y}
        else y

  in map updateOpenBal lines

-- | Computes the opening balance
openingBalance :: [TrialBalanceLine] -> HM.HashMap Commodity Quantity
openingBalance tbl =
  let xs :: [(Commodity, Quantity)]
      xs = map (\l -> (tblCommodity l, tblOpeningBalance l))
         $ filter (isIncomeStatementGroup . tblGroup) tbl

  in HM.fromListWith (+) xs

-- | Computes the earnings
earnings :: [TrialBalanceLine] -> HM.HashMap Commodity Quantity
earnings tbl =
  let xs :: [(Commodity, Quantity)]
      xs = map (\l -> (tblCommodity l, tblBalance l))
         $ filter (isIncomeStatementGroup . tblGroup) tbl

  in HM.fromListWith (+) xs

data BalanceFormat = TwoColumnDebitCredit | OneColumnSignedNumber
  deriving (Eq, Show)

data TrialBalanceOption = TrialBalanceOption {
  tboBalanceFormat :: BalanceFormat,
  tboShowInactiveAccounts :: Bool
} deriving (Eq, Show)

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
    serialize (TrialBalanceLine _ _ 0 _ _ False _)
      | not (tboShowInactiveAccounts opt) = []
    serialize (TrialBalanceLine acc comm bal _ _ _ gr) =
       let front = [T.pack $ show $ aNumber acc, aName acc]
           amnt = serializeAmount (tboBalanceFormat opt) gr bal
       in front ++ amnt ++ [comm]

    trialBalLines = filter (not . null)
                  $ map serialize
                  $ sortBy (comparing (aNumber . tblAccount))
                  $ tbLines tb

     -- Total lines
    total :: [[T.Text]]
    total = case (tboBalanceFormat opt) of
              OneColumnSignedNumber ->
                map (\(c, q) -> ["", "Total"]
                                ++ [T.pack $ show q] ++ [c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ trialBalanceTotal
                $ tbLines tb
              TwoColumnDebitCredit ->
                map (\(c, (dr, cr)) -> ["", "Total"]
                                    ++ [T.pack $ show dr, T.pack $ show cr]
                                    ++ [c])
                $ sortBy (comparing fst)
                $ HM.toList
                $ trialBalanceTotalDrCr
                $ tbLines tb

    csvlines ::  [[T.Text]]
    csvlines =  title
             ++ trialBalLines
             ++ ([] : total)

  in encode csvlines
