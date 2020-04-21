module Reports.Reports (
  reportsTestTree
  )where

import Data.Ord
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import Plainledger.Ledger
import Plainledger.Reports

ledgerPath :: String
ledgerPath = "test/Reports/reports.yaml"

reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [trialBalanceTestTree]

trialBalanceTestTree :: TestTree
trialBalanceTestTree =
   testGroup "Report"
    [ testCase "Reports.yaml" $ do
       journal <- decodeFileThrow ledgerPath
       case journalToLedger journal of
         Left err -> assertFailure err
         Right l ->
          let
              goodLines = sortBy (comparing (\(x,_,_,_) -> x)) $
                [("Bénéfice net", "CAD", 0, 0),
                 ("Solde d'ouverture", "CAD", 0, 0),
                 ("Compte chèque", "CAD", 400, 0),
                 ("Mastercard", "CAD", 560, 0),
                 ("Travail", "CAD", -1450, 0),
                 ("Musique", "CAD",490, 0)]
          in do
            tb <- either assertFailure return
                  $ report ledgerPath MinDate MaxDate l
            rJournalFile tb @?= ledgerPath
            rBeginDate tb @?= (read "2019-01-01")
            rEndDate tb @?= (read "2019-01-07 ")
            let ls = sortBy (comparing (\(x,_,_,_) -> x))
                   $ map (\(ReportLine a b c d _ _) -> (aId a, b, c, d))
                   $ rLines tb
            ls @?= goodLines,

        testCase "Reports.yaml starting 2019-01-03" $ do
           journal <- decodeFileThrow ledgerPath
           case journalToLedger journal of
             Left err -> assertFailure err
             Right l ->
              let
                  goodLines = sortBy (comparing (\(x,_,_,_) -> x)) $
                    [("Bénéfice net", "CAD", 0, 0),
                     ("Solde d'ouverture", "CAD", 0, 0),
                     ("Compte chèque", "CAD", 400, 200),
                     ("Mastercard", "CAD", 560, -240),
                     ("Travail", "CAD", -1450, 0),
                     ("Musique", "CAD",490, 40)]
              in do
                tb <- either assertFailure return
                      $ report ledgerPath (Date (read "2019-01-03")) MaxDate l
                rJournalFile tb @?= ledgerPath
                rBeginDate tb @?= (read "2019-01-03")
                rEndDate tb @?= (read "2019-01-07 ")
                let ls = sortBy (comparing (\(x,_,_,_) -> x))
                       $ map (\(ReportLine a b c d _ _) -> (aId a, b, c, d))
                       $ rLines tb
                ls @?= goodLines
    ]
