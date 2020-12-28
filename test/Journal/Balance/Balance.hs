module Journal.Balance.Balance (
  balanceTestTree,
  balances
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except

balances :: [Balance]
balances = 
  [
    Balance (read "2018-01-31") "Compte chèque" (read "5834.84") Nothing,
    Balance (read "2018-01-31") "Marge de crédit" (read "-5563.45") Nothing,
    Balance (read "2018-01-31") "Carte de crédit" (read "-335") Nothing,
    Balance (read "2018-01-31") "Hypothèque" (read "-164375") Nothing,
    Balance (read "2018-02-28") "Compte chèque" (read "9835.68") Nothing,
    Balance (read "2018-02-28") "Marge de crédit" (read "-5963.45") Nothing,
    Balance (read "2018-02-28") "Carte de crédit" (read "508") Nothing,
    Balance (read "2018-02-28") "Hypothèque" (read "-164375") Nothing,
    Balance (read "2019-05-31") "Compte chèque" (read "12386.1") Nothing,
    Balance (read "2019-05-31") "Carte de crédit" (read "358") Nothing,
    Balance (read "2019-05-31") "Marge de crédit" (read "-6183.45") Nothing,
    Balance (read "2018-01-01") "Compte chèque" (read "9835.68") Nothing,
    Balance (read "2018-01-01") "Maison" (read "275000") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Marge de crédit" (read "-5963.45") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Carte de crédit" (read "508") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Hypothèque" (read "-164375") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Solde d'ouverture" (read "-107684") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Salaire" (read "-10000") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Revenu de location" (read "-1000") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Nourriture" (read "123.45") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Taxes" (read "2298.32") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Vêtements" (read "187") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Divertissements" (read "400") (Just $ read "2018-12-31"),
    Balance (read "2018-01-01") "Dépenses non catégorisée" (read "670") (Just $ read "2018-12-31"),
    Balance (read "2019-01-01") "Solde d'ouverture" (read "-115005.23") (Just $ read "2019-12-31")
  ]

balanceTestTree :: TestTree
balanceTestTree =
  testGroup "Balance"
    [ okBalance "Balance-01.csv" ';' ',' balances
      
    ]


okBalance :: String -> Char -> Char -> [Balance] -> TestTree
okBalance filename sep decimal expectedBalance = 
  testCase ("Decode " ++ filename) $ do
       balance <- runExceptT $ decodeBalanceFile sep decimal ("test/Journal/Balance/" ++ filename)
       case balance of
         Left err -> assertFailure $ printErrors err
         Right actual -> assertEqual "" expectedBalance (map snd actual)

koBalance :: String -> Char -> Char -> Errors -> TestTree
koBalance filename sep decimal expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       balance <- runExceptT $ decodeBalanceFile sep decimal ("test/Journal/Balance/" ++ filename)
       case balance of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

-- okValidateBalance :: String -> Char -> Char -> [Balance] -> TestTree
-- okValidateBalance filename sep decimal expectedErr = 
--   testCase ("Validation of " ++ filename) $ do
--        balance <- runExceptT 
--                 $ decodeBalanceFile sep decimal ("test/Journal/Balance/" ++ filename)
--                 >>= validateBalances "Solde d'ouverture" "Bénéfice"
--        case balance of
--          Left err -> assertFailure $ printErrors err
--          Right actual -> assertEqual "" expectedBalance actual


-- koValidateBalance :: String -> Char -> Char -> Errors -> TestTree
-- koValidateBalance filename sep decimal expectedErr =
--    testCase ("Assert error for " ++ filename) $ do
--        balance <- runExceptT $ decodeBalanceFile sep decimal ("test/Journal/Balance/" ++ filename)
--                 >>= validateBalances "Solde d'ouverture" "Bénéfice"
--        case balance of
--          Left actual -> assertEqual "" expectedErr actual
--          Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"