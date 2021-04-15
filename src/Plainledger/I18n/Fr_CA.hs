-- |
-- Module      :  Plainledger.I18n.Fr_CA
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the Canadian French texts


module Plainledger.I18n.Fr_CA
(
  fr_CAText,
) where

import Data.Time
import Data.List ( intercalate )
import Plainledger.I18n.Data
import Plainledger.Error

fr_CAText :: I18nText -> String
fr_CAText (TError x) = printError x

fr_CAText TAsset = "Actif"
fr_CAText TLiability = "Passif"
fr_CAText TEquity = "Capital"
fr_CAText TRevenue = "Revenu"
fr_CAText TExpense = "Dépense"


fr_CAText TAccountNumber = "Numéro"
fr_CAText TAccountName = "Nom"
fr_CAText TAccountIdent = "Id"
fr_CAText TAccountParent = "Parent"

fr_CAText TBalanceStartDate = "Date de début"
fr_CAText TBalanceEndDate = "Date de fin"
fr_CAText TBalanceAmount = "Montant"
fr_CAText TBalanceAccount = "Compte"
fr_CAText TBalanceDate = "Date"

fr_CAText TJournalFileOpeningBalanceAccount = "Compte pour les soldes d'ouverture"
fr_CAText TJournalFileEarningsAccount = "Compte pour les bénéfices"
fr_CAText TJournalFileCompanyName = "Nom"
fr_CAText TJournalFileDecimalSeparator = "Séparateur de décimale"
fr_CAText TJournalFileFirstFiscalMonth = "Premier mois de l'exercice comptable"
fr_CAText TJournalFileAccountFile = "Fichier des comptes"
fr_CAText TJournalFileTransactionFiles = "Fichiers des transactions"
fr_CAText TJournalFileStatementBalanceFiles = "Assertion des soldes"
fr_CAText TJournalFileTrialBalanceFiles = "Assertion des balances de vérification"
fr_CAText TJournalFileThousandSeparator = "Séparateur des milliers"
fr_CAText TJournalFileCurrSymbol = "Symbol monétaire"

fr_CAText TTransactionId = "Numéro de transaction"
fr_CAText TTransactionDate = "Date"
fr_CAText TTransactionComment = "Commentaire"
fr_CAText TTransactionCounterparty = "Contrepartie"
fr_CAText TTransactionTag = "Étiquette"
fr_CAText TTransactionAccountPrefix = "Compte"
fr_CAText TTransactionAmountPrefix = "Montant"
fr_CAText TTransactionBalanceDatePrefix = "Date du relevé"

fr_CAText TReportTrialBalanceName = "Balance de vérification"
fr_CAText TReportBalanceSheetName = "Bilan"
fr_CAText TReportIncomeStatementName = "État des résultats"
fr_CAText (TReportDateSpan Nothing) = ""
fr_CAText (TReportDateSpan (Just (d1, d2))) = "Du "
                                            ++ show d1
                                            ++ " au "
                                            ++ show d2
fr_CAText (TReportMonthSpan d) = 
  let (y,m,_) = toGregorian d
  in monthToString m ++ " " ++ show y
fr_CAText (TReportYearSpan d) = 
  let (y,_,_) = toGregorian d
  in show y
fr_CAText TReportAccNumber = "Numéro"
fr_CAText TReportAccName = "Compte"
fr_CAText TReportDebit  = "Débit"
fr_CAText TReportCredit = "Crédit"
fr_CAText TReportTotal = "Total"
fr_CAText TReportEarnings = "Bénéfices"
fr_CAText (TReportGeneratedOn d) = "Rapport généré le " ++ show d

monthToString :: Int -> String
monthToString 1 = "Janvier"
monthToString 2 = "Février"
monthToString 3 = "Mars"
monthToString 4 = "Avril"
monthToString 5 = "Mai"
monthToString 6 = "Juin"
monthToString 7 = "Juillet"
monthToString 8 = "Août"
monthToString 9 = "Septembre"
monthToString 10 = "Octobre"
monthToString 11 = "Novembre"
monthToString 12 = "Décembre"
monthToString _ = error "Wrong month number"

-- | Pretty print the error message and add the source file information
printError :: Error -> String
printError (Error [] e) = "erreur:\n" ++ printErrorType e
printError (Error [pos] e) = showSourcePos pos ++ " erreur:\n" ++ printErrorType e
printError (Error (p:ps) e) 
  = printError (Error [p] e)
  ++ "Autres positions en lien avec cette erreur:\n  "
  ++ intercalate "\n  " (map showSourcePos ps)

-- | Pretty print the error message
printErrorType :: ErrorType -> String
printErrorType (ErrorMessage s) = s

printErrorType (ParseDateErr s) 
  = "Erreur de lecture de la date \"" ++ s ++ "\".\nLes dates doivent être dans le format YYYY-MM-DD."
printErrorType (ParseIntErr s)
  = "Erreur de lecture du nombre entier \"" ++ s ++ "\"."
printErrorType (ParsePosIntErr s)
  = "Erreur de lecture du nombre positif \"" ++ s ++ "\"."
printErrorType (ParseCharErr s)
  = "Erreur de lecture du caractère \"" ++ s ++ "\"."
printErrorType (ParseAmountErr s)
  = "Erreur de lecture du nombre \"" ++ s ++ "\"."
printErrorType (ParseAmountExponentErr s)
  = printErrorType (ParseAmountErr s)
  ++ "\nTrop de décimales (maximum 256)"

printErrorType (EmptyFieldInJournalFile s)
  = "La valeur du parmètre \"" ++ s ++ "\" est nulle."
printErrorType (MissingFieldinJournalFile s)
  = "Le paramètre \"" ++ s ++ "\" est absent."
printErrorType EmptyJournalFile
  = "Le journal est vide"
printErrorType (UnknownFieldinJournalFile s)
  = "Paramètre inconnu \"" ++ s ++ "\"."
printErrorType (DuplicateJournalFileParam s)
  = "Paramètre en double : \"" ++ s ++ "\"."
printErrorType InvalidHeaderJournalFile
  = "Erreur de lecture de l'en-tête du journal.\n"
  ++ "Le fichier doit utiliser la virgule (,), le point-virgule (;) or une tabulation pour séparar les colonnes\n"
  ++ "L'en-tête doit être \"Paramètre\" et \"Valeur\" pour le mode francophone.\n"
  ++ "L'en-tête doit être \"Parameter\" et \"Value\" pour le mode anglophone\n"

printErrorType ZeroLengthAccountId
  = "Le nom du compte est de taille nulle"
printErrorType (DuplicateAccountId n)
  = "Doublon dans le nom du compte \""
  ++ n
  ++ "\""
printErrorType (OpeningBalanceNotDefined s)
  = "Le compte pour les soldes d'ouverture \""
  ++ s
  ++ "\" déclaré dans le journal n'apparaît pas dans le fichier des comptes."
printErrorType (EarningsAccountNotDefined s) 
  = "Le compte pour les soldes d'ouverture \""
  ++ s
  ++ "\" déclaré dans le journal n'apparaît pas dans le fichier des comptes."

printErrorType (MissingCsvColumn c)
  = "La colonne \""
  ++ c
  ++ "\" n'est pas dans l'en-tête du CSV."
printErrorType (MissingCsvColumnData c)
  = "La colonne \""
  ++ c
  ++ "\" est manquante pour cette ligne."

printErrorType (DuplicateCsvColumn c)
  = "\""
  ++ c
  ++ "\" est en double dans l'en-tête du CSV."
printErrorType EmptyCsvFile
  = "Le fichier CSV est vide. Il doit au moins contenir une en-tête"
  
printErrorType (UnknownAccountType s)
  = "Le type de compte \""
  ++ s
  ++ "\" est inconnu.\nValeurs possibles : Actif, Passif, Capital, Revenu, Dépense"

printErrorType ZeroOrOnePostingOnly
  = "La transaction n'a pas au moins deux imputations.\n"

printErrorType (UnbalancedTransaction q)
  = "Transaction non balancée. Le total est " ++ show q

printErrorType TwoOrMorePostingsWithoutAmount
  = "Au moins deux comptes sans montant pour cette transaction.\n"
  ++ "Il est possible de déduire le montant seulement pour une imputation."

printErrorType (AccountIdNotInAccountFile s)
  = "Le compte \"" ++ s ++ "\" n'apparaît pas dans le fichier des comptes."

printErrorType (WrongBalance accId date bal amnt)
  = "Assertion de solde non vérifiée pour le compte \""
  ++ accId
  ++ "\" à la date "
  ++ show date
  ++ ".\nAssertion de solde : "
  ++ show bal
  ++ "\nSolde calculé      : " ++ showAmnt amnt

  where showAmnt Nothing = "0 (aucune transaction)"
        showAmnt (Just x) = show x
                          ++ "\nDifference de       : "
                          ++ show (bal - x)
printErrorType (MissingStartDateInBalance s)
  = "Le compte \""
  ++ s
  ++ "\" doit avoir une date de début.\n"
  ++ "Tous les comptes de revenu et dépense ainsi que le compte des soldes d'ouvertures doivent avoir une date de début."

printErrorType (EndDateGreaterThanStartDate sd ed)
  = "La date de début \""
  ++ show sd
  ++ "\" est supérieure à la date de fin \""
  ++ show ed
  ++ "\""

printErrorType (InvalidParent ident parent)
  = "La parent du compte \""
  ++ show ident
  ++ "\" est invalide. "
  ++ "\"" ++ show parent ++ "\" n'est pas un identifiant de compte connu."

printErrorType (CycleInParents idents)
  = "Les parents des comptes suivants forment une (ou plusieurs) référence circulaire :\n"
  ++ intercalate ", " idents

printErrorType (InvalidIdentifier s)
  = "Les identifiants de compte suivant ne peuvent être utilisés :"
  ++ intercalate ", " s

printErrorType (DuplicateBalance d t)
  = "Doublon dans les balances de vérifications.\n"
  ++ "Date : " ++ show d ++ "\n"
  ++ "Compte : " ++ t

printErrorType (UnallowedAmountChar c)
 = "Le caractère "
 ++ show c
 ++ " ne peut être utilisé comme séparateur de milliers ou symbole de devise."

showSourcePos :: SourcePos -> String
showSourcePos (SourcePos f r _) | r <= 0 = f
showSourcePos (SourcePos f r c) | c <= 0
  = f
  ++ ":rangée "
  ++ show r
  ++ ":"
showSourcePos (SourcePos f r c)
  = f
  ++ ":rangée "
  ++ show r
  ++ ":colonne "
  ++ show c
  ++ ":"