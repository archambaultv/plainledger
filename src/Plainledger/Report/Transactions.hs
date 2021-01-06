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

import Plainledger.I18n.I18n
import Plainledger.Journal
import qualified Data.Text as T
import qualified Data.Vector as V

transactionReport :: Maybe DateSpan ->
                     Bool -> 
                     Journal -> 
                     V.Vector (V.Vector T.Text)
transactionReport _ printAsSingleTxns journal =
  let txns = jTransactions journal
      lang = jfLanguage $ jJournalFile journal
      decimalSep = jfDecimalSeparator $ jJournalFile journal
      header = mkHeader lang printAsSingleTxns txns
      body = concatMap (mkBody printAsSingleTxns decimalSep) (zip [1..] txns)
  in V.fromList $ header : body


mkHeader :: Language -> Bool -> [Transaction] -> V.Vector T.Text
mkHeader lang False _ = V.fromList 
                       [i18nText lang TTransactionId,
                        i18nText lang TTransactionDate, 
                        i18nText lang TTransactionComment, 
                        i18nText lang TTransactionCounterparty, 
                        i18nText lang TTransactionTag,
                        i18nText lang TTransactionAccountPrefix,
                        i18nText lang TTransactionAmountPrefix,
                        i18nText lang TTransactionBalanceDatePrefix
                        ]
mkHeader lang True txns = 
  let nbOfPosting = maximum $ 2 : map (\t -> length (tPostings t)) txns
      accPrefix = (T.append (i18nText lang TTransactionAccountPrefix) " ")
      amntPrefix = (T.append (i18nText lang TTransactionAmountPrefix) " ")
      datePrefix = (T.append (i18nText lang TTransactionBalanceDatePrefix) " ")
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


mkBody :: Bool -> Char -> (Int, Transaction) -> [V.Vector T.Text]
mkBody False decimalSep (n, t) =
  let ps = tPostings t
      toLine p = [T.pack $ show n,
                  T.pack $ show $ tDate t, 
                  tComment t, 
                  tCounterParty t, 
                  tTag t,
                  pAccount p,
                  writeAmount decimalSep $ pAmount p,
                  T.pack $ show $ pBalanceDate p
                  ]
  in map (V.fromList . toLine) ps
mkBody True decimalSep (_, t) =
  let mkPosting p = [pAccount p,
                     writeAmount decimalSep $ pAmount p,
                     T.pack $ show $ pBalanceDate p]
  in [V.fromList $
      [T.pack $ show $ tDate t, 
       tComment t, 
       tCounterParty t, 
       tTag t]
      ++ concatMap mkPosting (tPostings t)]