module Reports.Reports (
  reportsTestTree
  )where

import Data.Ord
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
import qualified Data.HashMap.Strict as HM
import Plainledger.Ledger
import Plainledger.Reports

dir :: String
dir = "test/Reports/"

ledgerPath :: String
ledgerPath = "test/Reports/reports.yaml"

reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [trialBalanceTestTree]

trialBalanceTestTree :: TestTree
trialBalanceTestTree =
   testGroup "Trial balance"
    [ testCase "Reports.yaml" $ do
       journal <- decodeFileThrow ledgerPath
       case journalToLedger journal of
         Left err -> assertFailure err
         Right l ->
          let
              goodLines = sortBy (comparing (\(x,_,_) -> x)) $
                [("Bénéfice net", "CAD", 0),
                 ("Solde d'ouverture", "CAD", 0),
                 ("Compte chèque", "CAD", 400),
                 ("Mastercard", "CAD", 560),
                 ("Travail", "CAD", -1450),
                 ("Musique", "CAD",490)]
          in do
            tb <- either assertFailure return
                  $ trialBalanceReport ledgerPath MinDate MaxDate l
            tbJournalFile tb @?= ledgerPath
            tbBeginDate tb @?= (read "2019-01-01")
            tbEndDate tb @?= (read "2019-01-07 ")
            let ls = sortBy (comparing (\(x,_,_) -> x))
                   $ map (\(TrialBalanceLine a b c _ _ _ _) -> (aId a, b, c))
                   $ tbLines tb
            ls @?= goodLines
            let total :: [(Commodity, Quantity)]
                total = HM.toList $ trialBalanceTotal $ tbLines tb
            traverse (\x -> (fst x, snd x) @?= (fst x, 0)) total >> return (),

        testCase "Reports.yaml starting 2019-01-03" $ do
           journal <- decodeFileThrow ledgerPath
           case journalToLedger journal of
             Left err -> assertFailure err
             Right l ->
              let
                  goodLines = sortBy (comparing (\(x,_,_) -> x)) $
                    [("Bénéfice net", "CAD", 0),
                     ("Solde d'ouverture", "CAD", 40),
                     ("Compte chèque", "CAD", 400),
                     ("Mastercard", "CAD", 560),
                     ("Travail", "CAD", -1450),
                     ("Musique", "CAD",450)]
              in do
                tb <- either assertFailure return
                      $ trialBalanceReport ledgerPath (Date (read "2019-01-03")) MaxDate l
                tbJournalFile tb @?= ledgerPath
                tbBeginDate tb @?= (read "2019-01-03")
                tbEndDate tb @?= (read "2019-01-07 ")
                let ls = sortBy (comparing (\(x,_,_) -> x))
                       $ map (\(TrialBalanceLine a b c _ _ _ _) -> (aId a, b, c))
                       $ tbLines tb
                ls @?= goodLines
                let total :: [(Commodity, Quantity)]
                    total = HM.toList $ trialBalanceTotal $ tbLines tb
                traverse (\x -> (fst x, snd x) @?= (fst x, 0)) total >> return ()
    ]
