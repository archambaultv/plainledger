module Journal.JournalFile.JournalFile (
  journalFileTestTree
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except

journalFileTestTree :: TestTree
journalFileTestTree =
  testGroup "JournalFile"
    [ -- With BOM, comma for csv, period for decimal
      okConfig "Journal-01.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' ',' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] 
          "test/Journal/JournalFile/Journal-01.csv",
      -- With BOM, semicolon for csv, comma for decimal
      okConfig "Journal-02.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" ',' ';' 1 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] 
          "test/Journal/JournalFile/Journal-02.csv",
      -- Without BOM, comma for csv, period for decimal
      okConfig "Journal-03.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' ',' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] 
          "test/Journal/JournalFile/Journal-03.csv",
      -- Without BOM, comma for csv, period for decimal, multiple files
      okConfig "Journal-04.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company, Inc." '.' ',' 1 "comptes.csv" 
          ["transactions 1.csv", "transactions 2.csv", "transactions 3.csv"] 
          ["soldes 1.csv", "soldes 2.csv"]
          "test/Journal/JournalFile/Journal-04.csv",
      -- With BOM, tab for csv, period for decimal
      okConfig "Journal-05.csv"
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' '\t' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"]
          "test/Journal/JournalFile/Journal-05.csv",

      koConfig "Journal-06.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-06.csv" 1 0 ) InvalidHeaderJournalFile,
      koConfig "Journal-07.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-07.csv" 1 0 ) InvalidHeaderJournalFile,
      koConfig "Journal-08.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-08.csv" 2 2 ) 
        (EmptyFieldInJournalFile "Compte pour les soldes d'ouverture"),
      koConfig "Journal-09.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-09.csv" 0 0 ) 
        (MissingFieldinJournalFile "Compte pour les soldes d'ouverture"),
      koConfig "Journal-10.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-10.csv" 0 0 ) 
        (MissingFieldinJournalFile "Compte pour les bénéfices"),
      koConfig "Journal-11.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-11.csv" 0 0 ) 
        (MissingFieldinJournalFile "Nom"),
      koConfig "Journal-12.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-12.csv" 0 0 ) 
        (MissingFieldinJournalFile "Fichier des comptes")
    ]

okConfig :: String -> JournalFile -> TestTree
okConfig filename expectedJournal = 
  testCase ("Decode " ++ filename) $ do
       journal <- runExceptT $ decodeJournalFileIO ("test/Journal/JournalFile/" ++ filename)
       case journal of
         Left err -> assertFailure $ printErrors err
         Right actual -> assertEqual "" expectedJournal actual

koConfig :: String -> Errors -> TestTree
koConfig filename expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       journal <- runExceptT $ decodeJournalFileIO ("test/Journal/JournalFile/" ++ filename)
       case journal of
         Left actual -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"