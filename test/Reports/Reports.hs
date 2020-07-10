module Reports.Reports (
  reportsTestTree
  )where

import Data.Ord
import Data.List
import Test.Tasty
import Data.Time
import Test.Tasty.HUnit
import Data.Yaml as Y
import Data.Bifunctor
import Plainledger.Ledger
import Plainledger.Reports

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
          periodToSpan (MultiYear end years) @?= reverse goodSpan
    ]
