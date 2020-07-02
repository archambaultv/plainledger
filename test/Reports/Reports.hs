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
ledgerPath = "test/Journal/Journal.yaml"

reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [trialBalanceTestTree]

trialBalanceTestTree :: TestTree
trialBalanceTestTree =
   testGroup "Report" []
    -- [ testCase "Reports.yaml" $ do
    --    journalFile <- decodeFileThrow ledgerPath :: IO JournalFile
    --    journal <- journalFileToJournal ledgerPath journalFile
    --    case journalToLedger journal of
    --      Left err -> assertFailure err
    --      Right l ->
    --       let
    --           goodLines = sortBy (comparing (\(x,_,_,_) -> x)) $
    --             [("Bénéfice net", 0, 0),
    --              ("Solde d'ouverture",  0, 0),
    --              ("Compte chèque",  400, 0),
    --              ("Mastercard",  560, 0),
    --              ("Travail",  -1450, 0),
    --              ("Musique", 490, 0)]
    --       in do
    --         tb <- either assertFailure return
    --               $ report ledgerPath MinDate MaxDate l
    --         rJournalFile tb @?= ledgerPath
    --         rBeginDate tb @?= (read "2019-01-01")
    --         rEndDate tb @?= (read "2019-01-07 ")
    --         let ls = sortBy (comparing (\(x,_,_,_) -> x))
    --                $ map (\(ReportLine a b c d) -> (aId a, b, c, d))
    --                $ rLines tb
    --         ls @?= goodLines,
    --
    --     testCase "Reports.yaml starting 2019-01-03" $ do
    --        journalFile <- decodeFileThrow ledgerPath :: IO JournalFile
    --        journal <- journalFileToJournal ledgerPath journalFile
    --        case journalToLedger journal of
    --          Left err -> assertFailure err
    --          Right l ->
    --           let
    --               goodLines = sortBy (comparing (\(x,_,_,_) -> x)) $
    --                 [("Bénéfice net",  0, 0),
    --                  ("Solde d'ouverture",  0, 0),
    --                  ("Compte chèque",  400, 200),
    --                  ("Mastercard",  560, -240),
    --                  ("Travail",  -1450, 0),
    --                  ("Musique", 490, 40)]
    --           in do
    --             tb <- either assertFailure return
    --                   $ report ledgerPath (Date (read "2019-01-03")) MaxDate l
    --             rJournalFile tb @?= ledgerPath
    --             rBeginDate tb @?= (read "2019-01-03")
    --             rEndDate tb @?= (read "2019-01-07 ")
    --             let ls = sortBy (comparing (\(x,_,_,_) -> x))
    --                    $ map (\(ReportLine a b c d) -> (aId a, b, c, d))
    --                    $ rLines tb
    --             ls @?= goodLines
    -- ]
