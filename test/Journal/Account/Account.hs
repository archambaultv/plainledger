module Journal.Account.Account (
  accountTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except

accounts :: [Account]
accounts = 
  [
    Account "Compte chèque" "Compte chèque" 1001 Asset "Actif 1" "",
    Account "Maison" "Maison 123 rue principale" 1051 Asset "" "",
    Account "Inutilisé" "Inutilisé" 1102 Asset "Actif 2" "",
    Account "Marge de crédit" "Marge de crédit" 2051 Liability "" "",
    Account "Carte de crédit" "My credit card" 2001 Liability "" "",
    Account "Hypothèque" "Hypothèque" 2002 Liability "L1" "",
    Account "Solde d'ouverture" "Solde d'ouverture" 3001 Equity "" "",
    Account "Bénéfice" "Bénéfice" 3002 Equity "" "",
    Account "Salaire" "Salaire compagnie Foo Bar" 4152 Revenue "" "",
    Account "Revenu de location" "Revenu de location" 4153 Revenue "" "",
    Account "Nourriture" "Nourriture" 5002 Expense "E1" "",
    Account "Taxes" "Taxes" 5003 Expense "E1" "E11",
    Account "Vêtements" "Vêtements" 5006 Expense "E1" "E11",
    Account "Divertissements" "Divertissements" 5010 Expense "E1" "E12",
    Account "Dépenses non catégorisées" "Dépenses non catégorisées" 5011 Expense "E1" "E12",
    Account "Inutilisé 2" "Inutilisé 2" 5102 Expense "E2" ""
  ]

accountTestTree :: TestTree
accountTestTree =
  testGroup "Account"
    [ -- Without BOM, semicolon for csv, 
      okAccount "Account-01.csv" ';' accounts,
      -- With BOM, semicolon for csv, 
      okAccount "Account-02.csv" ';' accounts,
      -- Without BOM, comma for csv, 
      okAccount "Account-03.csv" ',' accounts
     
    ]

okAccount :: String -> Char -> [Account] -> TestTree
okAccount filename sep expectedAccount = 
  testCase ("Decode " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile ("test/Journal/Account/" ++ filename) sep
       case account of
         Left err -> assertFailure $ printErrors err
         Right actual -> assertEqual "" expectedAccount (map snd actual)

koAccount :: String -> Char -> Errors -> TestTree
koAccount filename sep expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile ("test/Journal/JournalFile/" ++ filename) sep
       case account of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"