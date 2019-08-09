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

printBalanceSheet :: Ledger -> B.ByteString
printBalanceSheet l =
  let accounts = lAccountSortedByType l
      serialize :: (QualifiedName, AccountInfo) -> [[T.Text]]
      serialize (n, info) = map (\(c, q) -> [qualifiedName2Text n,
                                             T.pack $ show q,
                                             c,
                                             maybe "" (T.pack . show) (aNumber info)])
                                (M.toList $ aBalance info)
      csvlines ::  [[T.Text]]
      csvlines =  ["Balance Sheet", "Start date", T.pack $ show $ lStartDate l,
                   "End date", T.pack $ show $ lEndDate l] :
                  [] :
                  ["Account","Amount","Commodity","Account Number"] :
                  concatMap serialize accounts
  in
    encodeWith csvOptions csvlines
