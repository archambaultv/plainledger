-- |
-- Module      :  Report.Transactions.Transactions
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Error data type


module Report.Transactions.Transactions
(
  transactionsTestTree
)
where

import Data.Char (ord)
import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Plainledger.I18n.I18n
import Plainledger.Report
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Csv as C

transactionsTestTree :: TestTree
transactionsTestTree =
   testGroup "Transactions"
    [ runReportOk "Journal-01",
      transactionEncodeDecode Fr_CA ';' ',' "Journal-01"
    ]

runReportOk :: String -> TestTree
runReportOk folder = testCase ("Report for " ++ folder) $ do
 getTransactionReport folder >> return ()

transactionEncodeDecode :: Language ->
                            Char ->
                            Char -> 
                            String -> 
                            TestTree
transactionEncodeDecode lang csvSeparator decimalSeparator folder = 
  testCase ("Encode - decode for " ++ folder) $ do
    (journal, report) <- getTransactionReport folder
    -- Encode the report as csv and then decode the report back
    -- Ensures the decoded report is equal to the original transactions
    let myOptions = C.defaultEncodeOptions {
                      C.encDelimiter = fromIntegral (ord csvSeparator)
                    }
    let csvBS = C.encodeWith myOptions $ V.toList report
    let accIds = HS.fromList $ map aId $ jAccounts journal
    let pos = map (\i -> SourcePos folder i 0) [2..]
    let accs = fmap (zip pos)
             $ decodeTransactions lang csvSeparator decimalSeparator csvBS
    let txnsM = accs >>= validateJTransactions accIds
    case txnsM of
      Left err -> assertFailure $ printErr err
      Right txns -> assertEqual "" (jTransactions journal) txns


printErr :: [Error] -> String
printErr err = T.unpack
             $ printErrors
             $ map (i18nText En_CA . TError ) err

getTransactionReport :: String -> IO (Journal, V.Vector (V.Vector T.Text))
getTransactionReport folder = do
 let journalPath = "test/Report/Transactions/" ++ folder ++ "/Journal.csv"
 journal <- runExceptT $ decodeJournal journalPath
 let report = fmap (\j -> (j, transactionReport True j)) journal
 case report of
     Left (_, err) -> assertFailure $ printErr err
     Right (j, txns) -> return (j, txns)