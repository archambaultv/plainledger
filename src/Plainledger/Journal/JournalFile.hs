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
  processJournalFileHeader,
  decodeJournalFile
)
where

import Control.Monad.Except
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Internal.Utils (removeBom)
import Data.Char (ord)
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
   jfStatementBalanceFiles :: [String],
   jfTrialBalanceFiles :: [String],
   -- | The file path to the Journal File
   jfFilePath :: String,

   jfLanguage :: Language
  }
  deriving (Eq, Show) 

emptyJournalFile :: JournalFile
emptyJournalFile = JournalFile "" "" "" '.' ',' 1 "" [] [] [] "" En_CA

csvSeparator :: [Char] 
csvSeparator = [',', ';', '\t']

processJournalFileHeader :: FilePath -> 
                            ExceptT Errors IO (Language, Char, V.Vector (V.Vector T.Text))
processJournalFileHeader filePath = withExceptT (setSourcePosFileIfNull filePath) $ do
  csvBS <- fmap removeBom $ liftIO $ BS.readFile filePath
  processHeader (BL.fromStrict csvBS) csvSeparator

processHeader :: (MonadError Errors m) => 
                 BL.ByteString ->
                 [Char] -> 
                 m (Language, Char, V.Vector (V.Vector T.Text))
processHeader _ [] = throwError $ mkError (SourcePos "" 1 0) InvalidHeaderJournalFile
processHeader bs (c:cs) =
  let csvOptions = C.defaultDecodeOptions {
        C.decDelimiter = fromIntegral (ord c)
        }
  in do 
    let csv = either (const Nothing) Just $ C.decodeWith csvOptions C.NoHeader bs
    case csv of
      Nothing -> processHeader bs cs
      (Just x) | V.null x -> throwError $ mkErrorNoPos EmptyJournalFile
      (Just x) | V.null (V.head x) -> throwError $ mkErrorNoPos InvalidHeaderJournalFile
      (Just x) | V.null (V.tail (V.head x)) -> processHeader bs cs
      (Just x) -> let p = V.head $ V.head x
                      v = V.head $ V.tail $ V.head x
                  in case inferLanguage (p, v) of
                        Nothing -> processHeader bs cs
                        Just l -> return (l, c, V.drop 1 x)

decodeJournalFile :: forall m . (MonadError Errors m) => 
                      String ->
                      (Language, Char, V.Vector (V.Vector T.Text)) -> m JournalFile
decodeJournalFile filePath (lang, sep, csvData) = (do
    let csvWithRowNumber = V.map (\(x, y) -> (x + 2, y)) $ V.indexed csvData
    rawJournal <- V.foldM parseConfig
                   emptyJournalFile{jfCsvSeparator = sep, 
                                    jfLanguage = lang,
                                    jfFilePath = filePath} 
                   csvWithRowNumber
    checkRawJournal rawJournal) `catchError` (throwError . setSourcePosFileIfNull filePath)

  where 

        parseConfig :: JournalFile -> (Int, V.Vector T.Text) -> m JournalFile
        parseConfig c (_, x) | V.null x = return c
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileOpeningBalanceAccount)
          = textField i (i18nString lang TJournalFileOpeningBalanceAccount) (V.tail x) 
          (\y -> c{jfOpeningBalanceAccount = y})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileEarningsAccount)
          = textField i (i18nString lang TJournalFileEarningsAccount) (V.tail x) 
          (\y -> c{jfEarningsAccount = y})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileCompanyName)
          = textField i (i18nString lang TJournalFileCompanyName) (V.tail x) (\y -> c{jfCompanyName = y})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileDecimalSeparator)
          = charField i (i18nString lang TJournalFileDecimalSeparator) (V.tail x) 
          (\y -> c{jfDecimalSeparator = y})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileFirstFiscalMonth) 
          = intField i (i18nString lang TJournalFileFirstFiscalMonth)  (V.tail x) 
          (\y -> c{jfFirstFiscalMonth = y})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileAccountFile) 
          = textField i (i18nString lang TJournalFileAccountFile)  (V.tail x) 
          (\y -> c{jfAccountFile = T.unpack y})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileTransactionFiles)  = 
          manyTextFields i (i18nString lang TJournalFileTransactionFiles) (V.tail x) 
          (\y -> c{jfTransactionFiles = (jfTransactionFiles c) ++ (map T.unpack y)})
        
        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileStatementBalanceFiles) = 
          manyTextFields i (i18nString lang TJournalFileStatementBalanceFiles) (V.tail x) 
          (\y -> c{jfStatementBalanceFiles = (jfStatementBalanceFiles c) ++ (map T.unpack y)})

        parseConfig c (i, x) | V.head x == (i18nText lang TJournalFileTrialBalanceFiles) = 
          manyTextFields i (i18nString lang TJournalFileTrialBalanceFiles) (V.tail x) 
          (\y -> c{jfTrialBalanceFiles = (jfTrialBalanceFiles c) ++ (map T.unpack y)})

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


checkRawJournal :: (MonadError Errors m) => JournalFile -> m JournalFile
checkRawJournal x | T.null (jfOpeningBalanceAccount x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Compte pour les soldes d'ouverture"
checkRawJournal x | T.null (jfEarningsAccount x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Compte pour les bénéfices"
checkRawJournal x | T.null (jfCompanyName x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Nom"
checkRawJournal x | null (jfAccountFile x ) = throwError $ mkError (SourcePos "" 0 0) $ MissingFieldinJournalFile "Fichier des comptes"
checkRawJournal x = return x
