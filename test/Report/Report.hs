module Report.Report (
  reportsTestTree
  )where

import Test.Tasty
import Data.Time
import Test.Tasty.HUnit
import qualified Data.Csv as C
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Bifunctor
import Plainledger.Ledger
import Plainledger.Report
import Control.Monad.Except

-- ledgerPath :: String
-- ledgerPath = "test/Journal/Journal.yaml"

reportsTestTree :: TestTree
reportsTestTree = testGroup "Report tests" []
              --[trialBalanceTestTree]

-- trialBalanceTestTree :: TestTree
-- trialBalanceTestTree =
--    testGroup "Report"
--     [ testCase "periodToSpan - 1" $ do
--         let end = fromGregorian 2018 12 31
--         let years = 4
--         let goodSpan = map (first Date . second Date)
--                        [(fromGregorian 2015 01 01, fromGregorian 2015 12 31),
--                         (fromGregorian 2016 01 01, fromGregorian 2016 12 31),
--                         (fromGregorian 2017 01 01, fromGregorian 2017 12 31),
--                         (fromGregorian 2018 01 01, fromGregorian 2018 12 31)]
--         periodToSpan (MultiYear end years) @?= reverse goodSpan,
--       testCase "periodToSpan - 2" $ do
--           let end = fromGregorian 2018 06 30
--           let years = 2
--           let goodSpan = map (first Date . second Date)
--                          [(fromGregorian 2016 07 01, fromGregorian 2017 06 30),
--                           (fromGregorian 2017 07 01, fromGregorian 2018 06 30)]
--           periodToSpan (MultiYear end years) @?= reverse goodSpan,
--       testCase "Balance sheet 2018" $
--           testReport2018
--           (reportToBalanceSheet (GroupReportOption False))
--           "test/Reports/Balance 2018.csv",
--       testCase "Trial balance 2018" $
--           testReport2018
--           (reportToTrialBalance (FlatReportOption TwoColumnDebitCredit False))
--           "test/Reports/Trial Balance 2018.csv",
--       testCase "Income statement 2018" $
--           testReport2018
--           (reportToIncomeStatement (GroupReportOption False))
--           "test/Reports/Income statement 2018.csv",
--       testCase "Cashflow 2018" $
--           testReport2018
--           (reportToCashFlow (FlatReportOption TwoColumnDebitCredit False))
--           "test/Reports/Cashflow 2018.csv",
--       testCase "Trial balance 2019" $
--           testReport2019
--           (reportToTrialBalance (FlatReportOption TwoColumnDebitCredit False))
--           "test/Reports/Trial Balance 2019.csv",
--       testCase "Balance sheet 2019" $
--           testReport2019
--           (reportToBalanceSheet (GroupReportOption False))
--           "test/Reports/Balance 2019.csv",
--       testCase "Income statement 2019" $
--           testReport2019
--           (reportToIncomeStatement (GroupReportOption False))
--           "test/Reports/Income statement 2019.csv"
--     ]

-- testReport2019 :: (C.FromField a, Eq a, Show a) =>
--                   (Report -> [[a]]) ->
--                   FilePath ->
--                   IO ()
-- testReport2019 = testReport (fromGregorian 2019 01 01) (fromGregorian 2019 12 31)

-- testReport2018 :: (C.FromField a, Eq a, Show a) =>
--                   (Report -> [[a]]) ->
--                   FilePath ->
--                   IO ()
-- testReport2018 = testReport (fromGregorian 2018 01 01) (fromGregorian 2018 12 31)

-- testReport :: (C.FromField a, Eq a, Show a) =>
--                Day ->
--                Day ->
--                   (Report -> [[a]]) ->
--                   FilePath ->
--                   IO ()
-- testReport d1 d2 mkReport f = do
--    let s = Date d1
--    let e = Date d2
--    journalFile <- Y.decodeFileThrow ledgerPath
--    journal <- runExceptT $ journalFileToJournal ledgerPath journalFile
--    case journal >>= journalToLedger of
--      Left err -> fail err
--      Right l -> do
--         let report = Report (Span s e) ledgerPath l
--         let tb = mkReport report
--         csvBS <- BL.readFile f
--         csv <- either fail return $ C.decode C.NoHeader csvBS
--         -- Cassava (Data.Csv) ignores empty lines, so we need to filter them
--         (filter (not . null) tb) @?= map V.toList (V.toList csv)