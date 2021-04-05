-- |
-- Module      :  Plainledger.Reports.AccountTreeReport
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.Ledger (
  Ledger(..),
  journalToLedger,
  emptyLedger
  )
where

import Plainledger.Journal
import qualified Data.HashMap.Strict as HM

-- Ledger is like a journal, but with some precomputed informations
-- usefull for reportings
data Ledger = Ledger {
  lJournalFile :: JournalFile,
  lAccounts   :: [Account],
  lTransactions :: [Transaction],
  lChartOfAccount :: ChartOfAccounts,
  lDateSpan :: Maybe DateSpan,
  lBalanceMap :: BalanceMap,
  lOpeningAccount :: Account,
  lEarningAccount :: Account
}

emptyLedger :: Ledger
emptyLedger = 
  let a = dummyAccount Asset
      bal = BalanceMap HM.empty a a
  in Ledger emptyJournalFile  [] [] [] Nothing bal a a

journalToLedger :: Journal -> Ledger
journalToLedger journal =
  let jf = jJournalFile journal
      accs = jAccounts journal
      chart = jChartOfAccount journal
      txns = jTransactions journal
      balMap = jBalanceMap journal
      dSpan = journalDateSpan journal
      open = bmOpeningBalanceAcc balMap
      earn = bmEarningAcc balMap
  in Ledger jf accs txns chart dSpan balMap open earn