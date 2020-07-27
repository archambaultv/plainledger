module Reports.Reports (
  reportsTestTree
  )where

import Test.Tasty
import Data.Time
import Test.Tasty.HUnit
import Data.Yaml as Y
import qualified Data.Csv as C
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import Plainledger.Ledger
import Plainledger.Reports
import Control.Monad.Except

ledgerPath :: String
ledgerPath = "test/Journal/Journal.yaml"

reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests"
              [trialBalanceTestTree]

trialBalanceTestTree :: TestTree
trialBalanceTestTree =
   testGroup "Report"
    [ testCase "periodToSpan - 1" $ do
        let end = fromGregorian 2018 12 31
        let years = 4
        let goodSpan = map (first Date . second Date)
                       [(fromGregorian 2015 01 01, fromGregorian 2015 12 31),
                        (fromGregorian 2016 01 01, fromGregorian 2016 12 31),
                        (fromGregorian 2017 01 01, fromGregorian 2017 12 31),
                        (fromGregorian 2018 01 01, fromGregorian 2018 12 31)]
        periodToSpan (MultiYear end years) @?= reverse goodSpan,
      testCase "periodToSpan - 2" $ do
          let end = fromGregorian 2018 06 30
          let years = 2
          let goodSpan = map (first Date . second Date)
                         [(fromGregorian 2016 07 01, fromGregorian 2017 06 30),
                          (fromGregorian 2017 07 01, fromGregorian 2018 06 30)]
          periodToSpan (MultiYear end years) @?= reverse goodSpan,
      testCase "Balance sheet 2018" $ do
         let s = Date $ fromGregorian 2018 01 01
         let e = Date $ fromGregorian 2018 12 31
         journalFile <- Y.decodeFileThrow ledgerPath
         journal <- runExceptT $ journalFileToJournal ledgerPath journalFile
         case journal >>= journalToLedger of
           Left err -> putStrLn err
           Right l -> do
              let report = Report (Span s e) ledgerPath l
              let tb = reportToBalanceSheet (GroupReportOption False) report
              csvBS <- BL.readFile "test/Reports/Balance 2018.csv"
              csv <- either fail return $ C.decode C.NoHeader csvBS
              -- Cassava (Data.Csv) ignores empty lines, so we need to filter them
              -- The file name is not the same because the path differs
              (filter (not . null) tb) @?= map V.toList (V.toList csv)

    ]
