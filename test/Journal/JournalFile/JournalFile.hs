module Journal.JournalFile.JournalFile (
  journalFileTestTree,
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Plainledger.Journal
import Plainledger.Error
import Control.Monad.Except
import Plainledger.I18n.I18n
import qualified Data.Text as T

journalFileTestTree :: TestTree
journalFileTestTree =
  testGroup "JournalFile"
    [ -- With BOM, comma for csv, period for decimal
      okConfig "Journal-01.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' Nothing Nothing ',' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] []
          "test/Journal/JournalFile/Journal-01.csv" Fr_CA True,
      -- With BOM, semicolon for csv, comma for decimal
      okConfig "Journal-02.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" ',' Nothing Nothing ';' 1 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] []
          "test/Journal/JournalFile/Journal-02.csv" Fr_CA True,
      -- Without BOM, comma for csv, period for decimal
      okConfig "Journal-03.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' Nothing Nothing ',' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] []
          "test/Journal/JournalFile/Journal-03.csv" Fr_CA False,
      -- Without BOM, comma for csv, period for decimal, multiple files
      okConfig "Journal-04.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company, Inc." '.'Nothing Nothing  ',' 1 "comptes.csv" 
          ["transactions 1.csv", "transactions 2.csv", "transactions 3.csv"] 
          ["soldes 1.csv", "soldes 2.csv"] []
          "test/Journal/JournalFile/Journal-04.csv" Fr_CA False,
      -- With BOM, tab for csv, period for decimal
      okConfig "Journal-05.csv"
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company" '.' Nothing Nothing '\t' 7 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] []
          "test/Journal/JournalFile/Journal-05.csv" Fr_CA True,
      -- Without BOM, semicolon, comma for decimal, comma in field
      okConfig "Journal-13.csv" 
        $ JournalFile "Solde d'ouverture" "Bénéfice" "My Company, Inc" ',' Nothing Nothing ';' 1 "comptes.csv" 
          ["transactions.csv"] ["vérification de soldes.csv"] []
          "test/Journal/JournalFile/Journal-13.csv" Fr_CA False,

      koConfig "Journal-06.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-06.csv" 1 0 ) 
        InvalidHeaderJournalFile,
      koConfig "Journal-07.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-07.csv" 1 0 ) 
        InvalidHeaderJournalFile,
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
        (MissingFieldinJournalFile "Fichier des comptes"),
      -- Field starting with double quote but no double quote at the end
      koConfig "Journal-14.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-14.csv" 0 0 ) 
        (MissingFieldinJournalFile "Fichier des comptes"),
      -- Invalid CSV syntax. Should not get InvalideHeaderJournalFile
      koConfig "Journal-15.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-15.csv" 0 0 ) 
        (ErrorMessage "parse error (Failed reading: satisfy) at \" Inc\nS\195\169parateur de d\195\169cimale;,\nFichier des comptes;comptes.csv\nFichiers des transactions;transacti (truncated)"),
      -- Invalid CSV syntax in the header. Should not get InvalideHeaderJournalFile
      koConfig "Journal-16.csv"
        $ mkError (SourcePos "test/Journal/JournalFile/Journal-16.csv" 1 0 ) 
        (ErrorMessage "parse error (Failed reading: satisfy) at \"\\\"eur\"")
    ]

okConfig :: String -> JournalFile -> TestTree
okConfig filename expectedJournal = 
  testCase ("Decode " ++ filename) $ do
       let path = "test/Journal/JournalFile/" ++ filename
       journal <- runExceptT $ decodeJournalFile path
       case journal of
         Left (_, err) -> assertFailure $ printErr err
         Right actual -> assertEqual "" expectedJournal actual

koConfig :: String -> Errors -> TestTree
koConfig filename expectedErr =
   testCase ("Assert error for " ++ filename) $ do
       let path = ("test/Journal/JournalFile/" ++ filename)
       journal <- runExceptT $ decodeJournalFile path
       case journal of
         Left (_, actual) -> assertEqual "" expectedErr actual
         Right _ -> assertFailure $ "Decoding " ++ filename ++ " should throw an error"

printErr :: [Error] -> String
printErr err = T.unpack
             $ printErrors
             $ map (i18nText En_CA . TError ) err

