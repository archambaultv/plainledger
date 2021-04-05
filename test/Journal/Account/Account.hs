module Journal.Account.Account 
(
  accountTestTree,
  accounts
  )where

import Data.List ( sortOn )
import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Plainledger.I18n.I18n
import qualified Data.Text as T

accountsJ :: [JAccount]
accountsJ = 
  [
    Account 6 "Actif court terme" "Actif court terme" (Just 1000) "Actif" (),
    Account 7 "Compte chèque" "Compte chèque" (Just 1001) "Actif court terme" (),
    Account 8 "Maison" "Maison 123 rue principale" (Just 1051) "Actif" (),
    Account 9 "Autres actifs" "Autres actifs" (Just 1100) "Actif" (),
    Account 10 "Inutilisé" "Inutilisé" (Just 1102) "Autres actifs" (),
    Account 11 "Marge de crédit" "Marge de crédit" (Just 2051) "Passif" (),
    Account 12 "Carte de crédit" "My credit card" (Just 2001) "Passif" (),
    Account 13 "Passif Long Terme" "Passif Long Terme" (Just 2000) "Passif" (),
    Account 14 "Hypothèque" "Hypothèque" (Just 2002) "Passif Long Terme" (),
    Account 15 "Solde d'ouverture" "Solde d'ouverture" (Just 3001) "Capital" (),
    Account 16 "Bénéfice" "Bénéfice" (Just 3002) "Capital" (),
    Account 17 "Salaire" "Salaire compagnie Foo Bar" (Just 4152) "Revenu" (),
    Account 18 "Revenu de location" "Revenu de location" (Just 4153) "Revenu" (),
    Account 19 "Dépenses courantes" "Dépenses courantes" (Just 5000) "Dépense" (),
    Account 20 "Nourriture" "Nourriture" (Just 5002) "Dépenses courantes" (),
    Account 21 "Taxes" "Taxes" (Just 5003) "Dépenses courantes" (),
    Account 22 "Vêtements" "Vêtements" (Just 5006) "Dépenses courantes" (),
    Account 23 "Divertissements" "Divertissements" (Just 5010) "Dépenses courantes" (),
    Account 24 "Dépenses non catégorisées" "Dépenses non catégorisées" 
               (Just 5011) "Dépense" (),
    Account 25 "Inutilisé 2" "Inutilisé 2" (Just 5102) "Dépense" ()
  ]

accounts :: [Account]
accounts = map foo accountsJ
  where foo x | aParent x == "Actif" = x{aAccountType = Asset, aParent = 1}
        foo x | aParent x == "Passif" = x{aAccountType = Liability, aParent = 2}
        foo x | aParent x == "Capital" = x{aAccountType = Equity, aParent = 3}
        foo x | aParent x == "Revenu" = x{aAccountType = Revenue, aParent = 4}
        foo x | aParent x == "Dépense" = x{aAccountType = Expense, aParent = 5}
        foo x | aParent x == "Actif court terme" = x{aAccountType = Asset, aParent = 6}
        foo x | aParent x == "Autres actifs" = x{aAccountType = Asset, aParent = 9}
        foo x | aParent x == "Passif Long Terme" = x{aAccountType = Liability, aParent = 13}
        foo x | aParent x == "Dépenses courantes" = x{aAccountType = Expense, aParent = 19}
        foo _ = error "Incomplete pattern in test/.../Accounts.hs:accounts"


accountTestTree :: TestTree
accountTestTree =
  testGroup "Account"
    [ -- Without BOM, semicolon for csv, 
      okAccount "Account-01.csv" ';' accountsJ,
      -- With BOM, semicolon for csv, 
      okAccount "Account-02.csv" ';' accountsJ,
      -- Without BOM, comma for csv, 
      okAccount "Account-03.csv" ',' accountsJ,
      -- With BOM, semicolon, only essential columns
      okAccount "Account-04.csv" ';' 
      $ map (\a -> a{aDisplayName = aIdentifier a}) accountsJ,

      koAccount "Account-05.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-05.csv" 1 0) 
        (MissingCsvColumn "Id"),

      koAccount "Account-06.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-06.csv" 1 0) 
        (DuplicateCsvColumn "Numéro"),

      -- No account number
      okAccount "Account-07.csv" ';'
      $ map (\a -> a{aNumber = Nothing}) accountsJ,

      okValidateAccount "Account-02.csv" ';' (topAccounts Fr_CA ++ accounts),

      koValidateAccount "Account-08.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-08.csv" 3 0) 
        ZeroLengthAccountId,

      koValidateAccount "Account-09.csv" ';'
      $ mkErrorMultiPos 
        [SourcePos "test/Journal/Account/Account-09.csv" 3 0,
         SourcePos "test/Journal/Account/Account-09.csv" 4 0]   
        (DuplicateAccountId "Maison"),

      koValidateAccount "Account-10.csv" ';'
      $ mkErrorNoPos (EarningsAccountNotDefined "Bénéfice"),

      koValidateAccount "Account-11.csv" ';'
      $ mkErrorNoPos (OpeningBalanceNotDefined "Solde d'ouverture"),

      koValidateAccount "Account-12.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-12.csv" 5 0)  (InvalidParent "Autres actifs" "Wrong Parent"),

      koValidateAccount "Account-13.csv" ';'
      $ mkErrorMultiPos 
      [SourcePos "test/Journal/Account/Account-13.csv" 2 0,
       SourcePos "test/Journal/Account/Account-13.csv" 3 0,
       SourcePos "test/Journal/Account/Account-13.csv" 4 0]
        (CycleInParents ["Actif court terme","Compte chèque","Maison"]),

      koValidateAccount "Account-14.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-14.csv" 9 0)  (InvalidIdentifier ["Passif"]),

      -- With extra empty rows
      okAccount "Account-15.csv" ';' accountsJ,

      -- With extra rows having only delimeters (;;;;)
      okAccount "Account-16.csv" ';' accountsJ,

      -- With extra empty rows, checking for correct row number in error
      koValidateAccount "Account-17.csv" ';'
      $ mkError (SourcePos "test/Journal/Account/Account-17.csv" 7 0)  (InvalidParent "Autres actifs" "Wrong Parent")
    ]


okAccount :: String -> Char -> [JAccount] -> TestTree
okAccount filename sep expectedAccount = 
  testCase ("Decode " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile Fr_CA ("test/Journal/Account/" ++ filename) sep
       case account of
         Left err -> assertFailure $ printErr err
         Right actual -> 
           -- Since accounts on compare on the aId field,
           -- we use the show method to compare all fields
           let expt = show $ sortOn aId expectedAccount
               act = show $ sortOn aId (map snd actual)
           in assertEqual "" expt act

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
                >>= validateAccounts "Solde d'ouverture" "Bénéfice" Fr_CA
       case account of
         Left err -> assertFailure $ printErr err
           -- Since accounts on compare on the aId field,
           -- we use the show method to compare all fields
         Right actual -> 
            let expt = show $ sortOn aId expectedAccount
                act = show $ sortOn aId (chartToList actual)
            in assertEqual "" expt act


koValidateAccount :: String -> Char -> Errors -> TestTree
koValidateAccount filename sep expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       account <- runExceptT $ decodeAccountsFile Fr_CA ("test/Journal/Account/" ++ filename) sep
                >>= validateAccounts "Solde d'ouverture" "Bénéfice" Fr_CA
       case account of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

printErr :: [Error] -> String
printErr err = T.unpack
                       $ printErrors
                       $ map (i18nText En_CA . TError ) err