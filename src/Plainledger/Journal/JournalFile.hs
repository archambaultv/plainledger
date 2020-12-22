-- |
-- Module      :  Plainledger.Journal.JournalFile
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the JournalFile object of the journal file

module Plainledger.Journal.JournalFile 
(
  JournalFile(..),
  decodeJournalFileIO,
  decodeJournalFile
)
where

import Control.Monad.Except
import Plainledger.Error
import Plainledger.Internal.Utils (removeBom)
import Data.Char (ord)
import Data.Bifunctor
import qualified Data.Vector as V
import qualified Data.Csv as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | The JournalFile of the journal
data JournalFile = JournalFile {
   -- | The account in the balance sheet that contains the total of all the
   -- other balance sheet accounts at the start of the financial period.
   jfOpeningBalanceAccount :: T.Text,
   -- | The account in the balance sheet that contains the total of Revenue and
   -- Expense
   jfEarningsAccount :: T.Text,
   -- | The name of the company or the name to display in the reports
   jfCompanyName :: T.Text,
   -- | The decimal separator
   jfDecimalSeparator :: Char,
   -- | The CSV separator. It is automatically inferred
   jfCsvSeparator :: Char,
   -- | First month of the fiscal year
   jfFirstFiscalMonth :: Int,
   jfAccountFile :: String,
   jfTransactionFiles :: [String],
   jfBalanceFiles :: [String]
  }
  deriving (Eq, Show) 

emptyJournalFile :: JournalFile
emptyJournalFile = JournalFile "" "" "" '.' ',' 1 "" [] []

csvSeparator :: [Char] 
csvSeparator = [',', ';', '\t']

decodeJournalFileIO :: FilePath -> ExceptT Error IO JournalFile
decodeJournalFileIO filePath = withExceptT (setSourcePosFileIfNull filePath) $ do
  csvBS <- fmap removeBom $ liftIO $ BS.readFile filePath
  decodeJournalFile (BL.fromStrict csvBS)

decodeJournalFile :: forall m . (MonadError Error m) => BL.ByteString -> m JournalFile
decodeJournalFile bs = do
  (sep, csvData) <- processHeader csvSeparator
  let csvWithRowNumber = V.map (\(x, y) -> (x + 2, y)) $ V.indexed csvData
  rawJournal <- V.foldM parseConfig 
                 emptyJournalFile{jfCsvSeparator = sep} 
                 csvWithRowNumber
  checkRawJournal rawJournal


  where processHeader :: [Char] -> m (Char, V.Vector (V.Vector T.Text))
        processHeader [] = throwError $ mkError (SourcePos "" 1 0) InvalidHeaderJournalFile
        processHeader (c:cs) =
          let csvOptions = C.defaultDecodeOptions {
                C.decDelimiter = fromIntegral (ord c)
                }
              csv = first ErrorMessage $ C.decodeWith csvOptions C.NoHeader bs
          in case csv >>= inferLanguage of
              Right x -> return (c, x)
              Left EmptyJournalFile 
                -> throwError $ mkError (SourcePos "" 0 0) EmptyJournalFile
              Left InvalidHeaderJournalFile 
                -> if null cs 
                   then throwError $ mkError (SourcePos "" 1 0) InvalidHeaderJournalFile
                   else processHeader cs
              Left _ -> processHeader cs

        inferLanguage :: V.Vector (V.Vector T.Text) 
                      -> Either ErrorType (V.Vector (V.Vector T.Text))
        inferLanguage x | V.null x = Left EmptyJournalFile
        inferLanguage x | V.null (V.head x) = Left InvalidHeaderJournalFile
        inferLanguage x | V.null (V.tail (V.head x)) = Left InvalidHeaderJournalFile
        inferLanguage x =
          let p = V.head $ V.head x
              v = V.head $ V.tail $ V.head x
          in case (p, v) of 
              ("Paramètre", "Valeur") -> return $ V.drop 1 x
              _ -> Left $ ErrorMessage "Unknown language"


        parseConfig :: JournalFile -> (Int, V.Vector T.Text) -> m JournalFile
        parseConfig c (_, x) | V.null x = return c
        parseConfig c (i, x) | V.head x == "Compte pour les soldes d'ouverture"
          = textField i "Compte pour les soldes d'ouverture" (V.tail x) 
          (\y -> c{jfOpeningBalanceAccount = y})
        
        parseConfig c (i, x) | V.head x == "Compte pour les bénéfices"
          = textField i "Compte pour les bénéfices" (V.tail x) 
          (\y -> c{jfEarningsAccount = y})
        
        parseConfig c (i, x) | V.head x == "Nom" 
          = textField i "Nom" (V.tail x) (\y -> c{jfCompanyName = y})
        
        parseConfig c (i, x) | V.head x == "Séparateur de décimale" 
          = charField i "Séparateur de décimale" (V.tail x) 
          (\y -> c{jfDecimalSeparator = y})
        
        parseConfig c (i, x) | V.head x == "Premier mois de l'exercice comptable" 
          = intField i "Premier mois de l'exercice comptable" (V.tail x) 
          (\y -> c{jfFirstFiscalMonth = y})
        
        parseConfig c (i, x) | V.head x == "Fichier des comptes" 
          = textField i "Fichier des comptes" (V.tail x) 
          (\y -> c{jfAccountFile = T.unpack y})
        
        parseConfig c (i, x) | V.head x == "Fichiers des transactions" = 
          manyTextFields i "Fichiers des transactions" (V.tail x) 
          (\y -> c{jfTransactionFiles = (jfTransactionFiles c) ++ (map T.unpack y)})
        
        parseConfig c (i, x) | V.head x == "Fichiers des soldes" = 
          manyTextFields i "Fichiers des soldes" (V.tail x) 
          (\y -> c{jfBalanceFiles = (jfBalanceFiles c) ++ (map T.unpack y)})

        parseConfig _ (i, x) = throwError 
                             $ mkError (SourcePos "" i 0)
                             $ UnknownFieldinJournalFile (T.unpack $ V.head x)

        -- Parse a text field and check for error
        textField :: Int -> String -> V.Vector T.Text -> (T.Text -> a) -> m a
        textField i field x _ | V.null x
                             = throwError 
                             $ mkError (SourcePos "" i 2)
                             $ EmptyFieldInJournalFile field
        textField i field x _ | V.head x == "" 
                                = throwError 
                                $ mkError (SourcePos "" i 2)
                                $ EmptyFieldInJournalFile field
        textField _ _ x f = return $ f $ V.head x

        -- Parse a int field and check for error
        intField :: Int -> String -> V.Vector T.Text -> (Int -> a) -> m a
        intField i field xs f = do
          x <- textField i field xs id
          case T.decimal x of
            Right (n, "") -> return $ f n
            _ -> throwError $ mkError (SourcePos "" i 2) $ ParseIntErr (T.unpack x)

        -- Parse a char field and check for error
        charField :: Int -> String -> V.Vector T.Text -> (Char -> a) -> m a
        charField i field xs f = do
          x <- textField i field xs id
          case T.null (T.tail x) of
            True -> return $ f $ T.head x
            False -> throwError $ mkError (SourcePos "" i 2) $ ParseCharErr (T.unpack x)

        -- Parse many text fields (multiple columns) and check for error
        manyTextFields :: Int -> String -> V.Vector T.Text -> ([T.Text] -> a) -> m a
        manyTextFields i field x _ | V.null x
                                   = throwError 
                                   $ mkError (SourcePos "" i 2)
                                   $ EmptyFieldInJournalFile field
        manyTextFields i field xs _ | V.all T.null xs
                                    = throwError 
                                    $ mkError (SourcePos "" i 2)
                                    $ EmptyFieldInJournalFile field
        manyTextFields _ _ xs f = return $ f $ filter (not . T.null) $ V.toList xs


checkRawJournal :: (MonadError Error m) => JournalFile -> m JournalFile
checkRawJournal x | T.null (jfOpeningBalanceAccount x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Compte pour les soldes d'ouverture"
checkRawJournal x | T.null (jfEarningsAccount x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Compte pour les bénéfices"
checkRawJournal x | T.null (jfCompanyName x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Nom"
checkRawJournal x | null (jfAccountFile x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Fichier des comptes"
checkRawJournal x = return x
