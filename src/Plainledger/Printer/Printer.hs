{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Printer.Printer
where

import Data.Csv
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Plainledger.Data.Type
import Plainledger.Data.Ledger (lAccountSortedByType)

csvOptions :: EncodeOptions
csvOptions = defaultEncodeOptions {
      encUseCrLf = False
    }

printTrialBalance :: Ledger -> Bool -> B.ByteString
printTrialBalance l useDebitCredit =
  let accounts :: [(QualifiedName, AccountInfo)]
      accounts = lAccountSortedByType l 

      rawlines :: [(QualifiedName, AccountInfo, Commodity, Quantity)]
      rawlines = concatMap (\(name, info) -> map (\(comm, quant) -> (name, info, comm, quant)) (M.toList $ aBalance info))
                           accounts

      serializeAmount :: AccountInfo -> Quantity -> [T.Text]
      serializeAmount info q | useDebitCredit =
         if isDebit q (aType info)
         then [T.pack $ show q, ""]
         else ["", T.pack $ show $ negate q]
      serializeAmount _ q = [T.pack $ show q]
                            
      serialize :: (QualifiedName, AccountInfo, Commodity, Quantity) -> [T.Text]
      serialize (n, info, c, q) = qualifiedName2Text n :
                                  last n :
                                  (serializeAmount info q) ++
                                  [c, maybe "" (T.pack . show) (aNumber info)]

      amountTitle :: [T.Text]
      amountTitle = if useDebitCredit
                    then ["Debit", "Credit"]
                    else ["Amount"]

      total :: [[T.Text]]
      total =
        let infos = map snd accounts
        in if useDebitCredit
           then map (\(c,(d,cr)) -> ["", "Total",T.pack $ show d, T.pack $ show cr, c]) $
                    M.toList $ totalBalanceDebitCredit infos
           else map (\(c,s) -> ["", "Total",T.pack $ show s,c]) $
                    M.toList $ totalBalance infos

      csvlines ::  [[T.Text]]
      csvlines =  ["Trial Balance", "Start date", T.pack $ show $ lStartDate l,
                   "End date", T.pack $ show $ lEndDate l] :
                  [] :
                  ("Account Qualified Name" : "Account Name" : amountTitle ++ ["Commodity","Account Number"]) :
                  map serialize rawlines ++
                  ([] : total)
  in
    encodeWith csvOptions csvlines
