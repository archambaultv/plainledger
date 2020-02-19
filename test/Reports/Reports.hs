module Reports.Reports (
  reportsTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Yaml as Y
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
    [ testCase "Trial balance of reports.yaml" $ do
       journal <- decodeFileThrow ledgerPath
       case journalToLedger journal of
         Left err -> assertFailure err
         Right l ->
          let tb = trialBalanceReport ledgerPath MinDate MaxDate l
              goodLines =
                [("Bénéfice net", "CAD", Nothing),
                 ("Solde d'ouverture", "CAD", Nothing),
                 ("Compte chèque", "CAD",
                  (Just (read "2019-01-07",400))),
                 ("Mastercard", "CAD",
                  (Just (read "2019-01-07",560))),
                 ("Travail", "CAD",
                  (Just (read "2019-01-04",-1450))),
                 ("Musique", "CAD",
                  (Just (read "2019-01-04",490)))]
          in do
            tbJournalFile tb @?= ledgerPath
            tbBeginDate tb @?= MinDate
            tbEndDate tb @?= MaxDate
            (map (\(TrialBalanceLine a b c _ _) -> (a, b, c)) (tbLines tb)) @?= goodLines,

       testCase "Total at zero" $ do
         journal <- decodeFileThrow ledgerPath
         case journalToLedger journal of
           Left err -> assertFailure err
           Right l ->
            let tb = trialBalanceReport ledgerPath MinDate MaxDate l
                total :: [(Commodity, Quantity)]
                total = trialBalanceTotal $ tbLines tb

            in traverse (\x -> (fst x, snd x) @?= (fst x, 0)) total >> return ()
    ]
