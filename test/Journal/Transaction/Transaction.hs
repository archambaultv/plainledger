module Journal.Transaction.Transaction 
(
  transactionTestTree
)where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Journal.Account.Account
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Plainledger.I18n.I18n

accs :: HS.HashSet T.Text
accs = HS.fromList $ map aId accounts

transactions :: [JTransaction]
transactions = 
  [
  Transaction (read "2018-01-02") "Un commentaire" "ABC Company" "Tag 1" 
              [Posting (read "2018-01-02") "Compte chèque" (Just $ read "1950.42"),
               Posting (read "2018-01-02") "Salaire" (Just $ read "-2500"),
               Posting (read "2018-01-02") "Taxes" (Just $ read "549.58")],
  Transaction (read "2018-01-06") "" "" ""
              [Posting (read "2018-01-06") "Carte de crédit" (Just $ read "-100"),
               Posting (read "2018-01-06") "Vêtements" (Just $ read "100")],
  Transaction (read "2018-01-10") "" "" ""
              [Posting (read "2018-01-10") "Marge de crédit" (Just $ read "-123.45"),
               Posting (read "2018-01-10") "Nourriture" (Just $ read "123.45")],
  Transaction (read "2018-01-14") "" "" ""
              [Posting (read "2018-01-14") "Compte chèque" (Just $ read "-500"),
               Posting (read "2018-01-14") "Hypothèque" (Just $ read "500")],
  Transaction (read "2018-01-18") "" "" ""
              [Posting (read "2018-01-18") "Compte chèque" (Just $ read "1950.42"),
               Posting (read "2018-01-18") "Salaire" (Just $ read "-2500"),
               Posting (read "2018-01-18") "Taxes" (Just $ read "549.58")],
  Transaction (read "2018-01-30") "Apparaît en février sur le relevé" "" "" 
              [Posting (read "2018-01-30") "Compte chèque" (Just $ read "-800"),
               Posting (read "2018-02-01") "Carte de crédit" (Just $ read "800")],
  Transaction (read "2018-02-02") "" "" ""
              [Posting (read "2018-02-02") "Marge de crédit" (Just $ read "-400"),
               Posting (read "2018-02-02") "Divertissements" Nothing],
  Transaction (read "2018-02-05") "" "" ""
              [Posting (read "2018-02-05") "Compte chèque" (Just $ read "1950.42"),
               Posting (read "2018-02-05") "Salaire" (Just $ read "-2500"),
               Posting (read "2018-02-05") "Taxes" (Just $ read "549.58")],
  Transaction (read "2018-02-08") "" "" ""
              [Posting (read "2018-02-08") "Compte chèque" (Just $ read "900"),
               Posting (read "2018-02-08") "Revenu de location" (Just $ read "-1000"),
               Posting (read "2018-02-08") "Taxes" (Just $ read "100")],
  Transaction (read "2018-02-14") "" "" ""
              [Posting (read "2018-02-14") "Carte de crédit" Nothing,
               Posting (read "2018-02-14") "Vêtements" (Just $ read "87")],
  Transaction (read "2018-02-17") "" "" ""
              [Posting (read "2018-02-17") "Carte de crédit" (Just $ read "-670"),
               Posting (read "2018-02-17") "Dépense non catégorisée" (Just $ read "670")],
  Transaction (read "2018-02-20") "" "" ""
              [Posting (read "2018-02-20") "Compte chèque" (Just $ read "1950.42"),
               Posting (read "2018-02-20") "Salaire" (Just $ read "-2500"),
               Posting (read "2018-02-20") "Taxes" (Just $ read "549.58")],
  Transaction (read "2018-02-23") "" "" ""
              [Posting (read "2018-02-23") "Compte chèque" (Just $ read "-800"),
               Posting (read "2018-02-23") "Carte de crédit" (Just $ read "800")],
  Transaction (read "2019-02-02") "" "" ""
              [Posting (read "2019-02-02") "Compte chèque" (Just $ read "1950.42"),
               Posting (read "2019-02-02") "Salaire" (Just $ read "-2500"),
               Posting (read "2019-02-02") "Taxes" (Just $ read "549.58")],
  Transaction (read "2019-02-06") "" "" ""
              [Posting (read "2019-02-06") "Carte de crédit" (Just $ read "-450"),
               Posting (read "2019-02-06") "Vêtements" (Just $ read "450")],
  Transaction (read "2019-02-10") "" "" ""
              [Posting (read "2019-02-10") "Marge de crédit" (Just $ read "-220"),
               Posting (read "2019-02-10") "Nourriture" (Just $ read "220")],
  Transaction (read "2019-05-08") "" "" ""
              [Posting (read "2019-05-08") "Compte chèque" (Just $ read "900"),
               Posting (read "2019-05-08") "Revenu de location" (Just $ read "-1000"),
               Posting (read "2019-05-08") "Taxes" (Just $ read "100")],
  Transaction (read "2019-03-23") "" "" ""
              [Posting (read "2019-03-23") "Compte chèque" (Just $ read "-300"),
               Posting (read "2019-03-23") "Carte de crédit" (Just $ read "300")]
  ]

transactionTestTree :: TestTree
transactionTestTree =
  testGroup "Transaction"
    [ okTransaction "Transaction-01.csv" ';' '.' transactions,
      -- Like Transaction-01, but with a dummy column
      okTransaction "Transaction-02.csv" ';' '.' transactions,
      -- Second posting suffix is not numeric
      okTransaction "Transaction-03.csv" ';' '.' transactions,
      -- Extra posting column names for amount and balance date
      okTransaction "Transaction-04.csv" ';' '.' transactions,

      koTransaction "Transaction-05.csv" ';' '.' 
      $ mkError (SourcePos "test/Journal/Transaction/Transaction-05.csv" 3 0) 
        ZeroOrOnePostingOnly,

      koValidateTransaction "Transaction-06.csv" ';' '.' 
      $ mkError (SourcePos "test/Journal/Transaction/Transaction-06.csv" 4 0) 
        (AccountIdNotInAccountFile "Wrong Name"),

      koValidateTransaction "Transaction-07.csv" ';' '.' 
      $ mkError (SourcePos "test/Journal/Transaction/Transaction-07.csv" 3 0) 
        (UnbalancedTransaction $ read "-10"),

      koValidateTransaction "Transaction-08.csv" ';' '.' 
      $ mkError (SourcePos "test/Journal/Transaction/Transaction-08.csv" 9 0) 
        TwoOrMorePostingsWithoutAmount
    ]


okTransaction :: String -> Char -> Char ->[JTransaction] -> TestTree
okTransaction filename sep decimal expectedTransaction = 
  testCase ("Decode " ++ filename) $ do
       transaction <- runExceptT $ decodeJTransactionsFile Fr_CA sep decimal ("test/Journal/Transaction/" ++ filename) 
       case transaction of
         Left err -> assertFailure $ printErr err
         Right actual -> assertEqual "" expectedTransaction (map snd actual)

koTransaction :: String -> Char -> Char ->Errors -> TestTree
koTransaction filename sep decimal expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       transaction <- runExceptT $ decodeJTransactionsFile Fr_CA sep decimal ("test/Journal/Transaction/" ++ filename) 
       case transaction of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

-- okValidateTransaction :: String -> Char -> Char ->[Transaction] -> TestTree
-- okValidateTransaction filename sep decimal expectedTransaction = 
--   testCase ("Validation of " ++ filename) $ do
--        transaction <- runExceptT 
--                 $ decodeJTransactionsFile sep decimal ("test/Journal/Transaction/" ++ filename) 
--                 >>= validateJTransactions "Solde d'ouverture" "Bénéfice"
--        case transaction of
--          Left err -> assertFailure $ printErrors err
--          Right actual -> assertEqual "" expectedTransaction actual


koValidateTransaction :: String -> Char -> Char -> Errors -> TestTree
koValidateTransaction filename sep decimal expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       transaction <- runExceptT $ decodeJTransactionsFile Fr_CA sep decimal ("test/Journal/Transaction/" ++ filename) 
                   >>= validateJTransactions accs
       case transaction of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

printErr :: [Error] -> String
printErr err = T.unpack
                       $ printErrors
                       $ map (i18nText En_CA . TError ) err