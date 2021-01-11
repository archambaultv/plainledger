module Journal.Account.Account 
(
  accountTestTree,
  accounts
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Plainledger.I18n.I18n
import qualified Data.Text as T

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
      okAccount "Account-03.csv" ',' accounts,
      -- With BOM, semicolon, only essential columns
      okAccount "Account-04.csv" ';' 
      $ map (\a -> a{aDisplayName = aId a, aGroup = "", aSubGroup = ""}) accounts,

      koAccount "Account-05.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-05.csv" 1 0) 
        (MissingCsvColumn "Id"),

      koAccount "Account-06.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-06.csv" 1 0) 
        (DuplicateCsvColumn "Numéro"),

      koAccount "Account-07.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-07.csv" 5 2) 
        (ParseIntErr ""),

      okValidateAccount "Account-02.csv" ';' accounts,

      koValidateAccount "Account-08.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-08.csv" 3 0) 
        ZeroLengthAccountId,

      koValidateAccount "Account-09.csv" ';'
      $ mkErrorMultiPos 
        [SourcePos "test/Journal/Account/Account-09.csv" 3 0,
         SourcePos "test/Journal/Account/Account-09.csv" 5 0]   
        (DuplicateAccountId "Maison"),


      koValidateAccount "Account-10.csv" ';'
      $ mkErrorNoPos (EarningsAccountNotDefined "Bénéfice"),


      koValidateAccount "Account-11.csv" ';'
      $ mkErrorNoPos (OpeningBalanceNotDefined "Solde d'ouverture")
    ]


okAccount :: String -> Char -> [Account] -> TestTree
okAccount filename sep expectedAccount = 
  testCase ("Decode " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile Fr_CA ("test/Journal/Account/" ++ filename) sep
       case account of
         Left err -> assertFailure $ printErr err
         Right actual -> assertEqual "" expectedAccount (map snd actual)

koAccount :: String -> Char -> Errors -> TestTree
koAccount filename sep expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile Fr_CA ("test/Journal/Account/" ++ filename) sep
       case account of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

okValidateAccount :: String -> Char -> [Account] -> TestTree
okValidateAccount filename sep expectedAccount = 
  testCase ("Validation of " ++ filename) $ do
       account <- runExceptT 
                $ decodeAccountsFile Fr_CA ("test/Journal/Account/" ++ filename) sep
                >>= validateAccounts "Solde d'ouverture" "Bénéfice"
       case account of
         Left err -> assertFailure $ printErr err
         Right actual -> assertEqual "" expectedAccount actual


koValidateAccount :: String -> Char -> Errors -> TestTree
koValidateAccount filename sep expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile Fr_CA ("test/Journal/Account/" ++ filename) sep
                >>= validateAccounts "Solde d'ouverture" "Bénéfice"
       case account of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

printErr :: [Error] -> String
printErr err = T.unpack
                       $ printErrors
                       $ map (i18nText En_CA . TError ) err