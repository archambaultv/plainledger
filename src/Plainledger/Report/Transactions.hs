-- |
-- Module      :  Plainledger.Reports.Transactions
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.Transactions
(
  transactionReport
)
where

import Data.List ( sortOn )
import Data.Time ( Day )
import Plainledger.I18n.I18n
    ( I18nText(TTransactionTag, TTransactionId,
               TTransactionAccountPrefix, TTransactionAmountPrefix,
               TTransactionBalanceDatePrefix, TTransactionDate,
               TTransactionComment, TTransactionCounterparty),
      Language,
      i18nText )
import Plainledger.Journal
    ( writeAmount,
      JournalFile(jfLanguage, jfDecimalSeparator),
      PostingF(pAccount, pAmount, pBalanceDate, pFileOrder),
      Transaction,
      TransactionF(tDate, tComment, tCounterParty, tTag, tPostings),
      AccountF(aIdentifier) )
import Plainledger.Report.Report
    ( ReportPeriod,
      CompareAnotherPeriod,
      TransactionCsvRecordType(..),
      Ledger(lJournalFile, lTransactions),
      reportPeriodToSpan )
import qualified Data.Text as T
import qualified Data.Vector as V

transactionReport :: ReportPeriod ->
                     Maybe CompareAnotherPeriod ->
                     TransactionCsvRecordType ->
                     Ledger ->
                     Day ->
                     V.Vector (V.Vector T.Text)
transactionReport period _ csvRecordType ledger today =
  let dateSpan = reportPeriodToSpan period today ledger
      lang = jfLanguage $ lJournalFile ledger
      decimalSep = jfDecimalSeparator $ lJournalFile ledger

      getTxns = \(sd, ed) -> sortOn tDate
                             $ filter (\t -> tDate t >= sd && tDate t <= ed)
                             $ lTransactions ledger
      txns = maybe [] getTxns dateSpan

      header = mkHeader lang csvRecordType txns
      body = concatMap (mkBody csvRecordType decimalSep) (zip [1..] txns)
  in V.fromList $ header : body


mkHeader :: Language -> TransactionCsvRecordType -> [Transaction] -> V.Vector T.Text
mkHeader lang MultipleCsvRecords _ = V.fromList
                       [i18nText lang TTransactionId,
                        i18nText lang TTransactionDate,
                        i18nText lang TTransactionComment,
                        i18nText lang TTransactionCounterparty,
                        i18nText lang TTransactionTag,
                        i18nText lang TTransactionAccountPrefix,
                        i18nText lang TTransactionAmountPrefix,
                        i18nText lang TTransactionBalanceDatePrefix
                        ]
mkHeader lang SingleCsvRecord txns =
  let nbOfPosting = maximum $ 2 : map (length . tPostings) txns
      accPrefix = T.append (i18nText lang TTransactionAccountPrefix) " "
      amntPrefix = T.append (i18nText lang TTransactionAmountPrefix) " "
      datePrefix = T.append (i18nText lang TTransactionBalanceDatePrefix) " "
      mkPrefix i = [T.append accPrefix (T.pack $ show i),
                    T.append amntPrefix (T.pack $ show i),
                    T.append datePrefix (T.pack $ show i)]
      postingHeader = concatMap mkPrefix [1..nbOfPosting]
  in V.fromList $
      [i18nText lang TTransactionDate,
       i18nText lang TTransactionComment,
       i18nText lang TTransactionCounterparty,
       i18nText lang TTransactionTag]
      ++ postingHeader


mkBody :: TransactionCsvRecordType -> Char -> (Int, Transaction) -> [V.Vector T.Text]
mkBody MultipleCsvRecords decimalSep (n, t) =
  let ps = sortOn pFileOrder $ tPostings t
      toLine p = [T.pack $ show n,
                  T.pack $ show $ tDate t,
                  tComment t,
                  tCounterParty t,
                  tTag t,
                  aIdentifier $ pAccount p,
                  writeAmount decimalSep $ pAmount p,
                  T.pack $ show $ pBalanceDate p
                  ]
  in map (V.fromList . toLine) ps
mkBody SingleCsvRecord decimalSep (_, t) =
  let mkPosting p = [aIdentifier $ pAccount p,
                     writeAmount decimalSep $ pAmount p,
                     T.pack $ show $ pBalanceDate p]
  in [V.fromList $
      [T.pack $ show $ tDate t,
       tComment t,
       tCounterParty t,
       tTag t]
      ++ concatMap mkPosting (sortOn pFileOrder $ tPostings t)]