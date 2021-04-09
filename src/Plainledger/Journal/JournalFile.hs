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
  emptyJournalFile,
  decodeJournalFile,
  processJournalFileHeader,
  processJournalFileBody,
  jfAmountDescriptor
)
where

import Control.Monad.Except
import Plainledger.I18n.I18n
import Plainledger.Error
import Plainledger.Internal.Csv
import Data.Char (ord)
import qualified Data.Vector as V
import qualified Data.Csv as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Plainledger.Journal.Amount
import Data.Bifunctor (second, Bifunctor (first))
import Data.List (sortOn, groupBy)
import Data.Function (on)

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
   -- | The decimal separator
   jfThousandSeparator :: Maybe Char,
   -- | The CSV separator. It is automatically inferred
   jfCurrencySymbol :: Maybe Char,

   jfCsvSeparator :: Char,
   -- | First month of the fiscal year
   jfFirstFiscalMonth :: Int,
   jfAccountFile :: String,
   jfTransactionFiles :: [String],
   jfStatementBalanceFiles :: [String],
   jfTrialBalanceFiles :: [String],
   -- | The file path to the Journal File
   jfFilePath :: String,

   jfLanguage :: Language,

   jfHasBom :: Bool
  }
  deriving (Eq, Show) 

jfAmountDescriptor :: JournalFile -> AmountDescriptor
jfAmountDescriptor jf = (jfDecimalSeparator jf, 
                         jfThousandSeparator jf, 
                         jfCurrencySymbol jf)

emptyJournalFile :: JournalFile
emptyJournalFile = JournalFile "" "" "" '.' Nothing Nothing ',' 1 "" [] [] [] "" En_CA False

csvAllowedSep :: [Char] 
csvAllowedSep = [',', ';', '\t']

decodeJournalFile :: FilePath -> 
                     ExceptT (Language, Errors) IO JournalFile
decodeJournalFile filePath = withFileName $ do
  -- Read the file and split it into lines
  (hasBom, csvBS) <- removeBom . BL.fromStrict
                  <$> liftIO (BS.readFile filePath)
  let jfLines = csvLines csvBS
  when (null jfLines) (withEnglishLang $ throwError $ mkErrorNoPos EmptyJournalFile)
  
  let csvHeader = head jfLines
  let csvData = tail jfLines

  -- Parse the first line, infer language and separator
  (lang, sep) <- withEnglishLang 
              $ processJournalFileHeader csvHeader csvAllowedSep
  -- Finish parsing the csv (since we now know the separator)
  -- and process the journal file
  csvData2 <- withLang lang $ parseCsv sep csvData
  j <- withLang lang $ processJournalFileBody filePath lang sep csvData2
  return j{jfHasBom = hasBom}

  where withEnglishLang = withExceptT (En_CA,)
        withLang l = withExceptT (l,)

        withFileName = 
            withExceptT (second (setSourcePosFileIfNull filePath))

processJournalFileHeader :: (MonadError Errors m) => 
                 (SourceRow, BL.ByteString) ->
                 [Char] -> 
                 m (Language, Char)
processJournalFileHeader (i, _) [] = 
  throwError $ mkError (SourcePos "" i 0) InvalidHeaderJournalFile
processJournalFileHeader (i, bs) (c:cs) =
  let csvOptions = C.defaultDecodeOptions {
        C.decDelimiter = fromIntegral (ord c)
        }

  in do 
    -- Try to parse the header with the delimiter c
    csv <- either (throwError . mkError (SourcePos "" i 0) . ErrorMessage) return 
           $ C.decodeWith csvOptions C.NoHeader bs
    case csv of
      -- The header is empty (should not happen, but just in case)
      x | V.null x -> throwError $ mkErrorNoPos EmptyJournalFile
      -- The header is empty
      x | V.null (V.head x) -> throwError $ mkErrorNoPos InvalidHeaderJournalFile
      -- The header does not have 2 columns, we try another separator
      x | V.null (V.tail (V.head x)) -> processJournalFileHeader (i, bs) cs
      -- We have at least 2 columns, we try to infer the language
      -- If that fails, we try another separator
      x -> let p = V.head $ V.head x
               v = V.head $ V.tail $ V.head x
           in case inferLanguage (p, v) of
                Nothing -> processJournalFileHeader (i, bs) cs
                Just l -> return (l, c)

-- The header must be stripped
processJournalFileBody :: forall m . (MonadError Errors m) => 
                      String ->
                      Language ->
                      Char ->
                      [(Int,V.Vector T.Text)] -> 
                      m JournalFile
processJournalFileBody filePath lang sep bs = do
    checkForDuplicateParam bs
    rawJournal <- foldM (parseConfig parseParam)
                   emptyJournalFile{jfCsvSeparator = sep, 
                                    jfLanguage = lang,
                                    jfFilePath = filePath} 
                   bs
    checkRawJournal lang rawJournal `catchError` (throwError . setSourcePosFileIfNull filePath)

  where 

        checkForDuplicateParam :: [(Int,V.Vector T.Text)] -> m ()
        checkForDuplicateParam xs =
          let ps :: [(SourcePos, T.Text)]
              ps = filter (not . T.null . snd)
                 $ map (fmap V.head)
                 $ filter (not . V.null . snd)
                 $ map (first (\i -> SourcePos "" i 0)) xs

              dup = filter (not . null . tail) 
                  $ groupBy ((==) `on` snd) 
                  $ sortOn snd ps

              mkErr ls = mkErrorMultiPos (map fst ls)
                         (DuplicateJournalFileParam $ T.unpack $ snd $ head ls)
          in if null dup
            then return ()
            else throwError $ concatMap mkErr dup

        -- How to parse a configuration file based on a list of
        -- parameters name and action to perform
        -- Could use a map instead of going through the list all the times,
        -- but the config file is so small it is probably not worth the trouble.
        parseConfig :: [(I18nText,JournalFile -> String -> Int -> V.Vector T.Text -> m JournalFile)] ->
                       JournalFile -> 
                       (Int, V.Vector T.Text) -> 
                       m JournalFile
        parseConfig _ c (_, x) | V.null x = return c
        parseConfig [] _ (i, x) = throwError 
                                $ mkError (SourcePos "" i 2)
                                $ UnknownFieldinJournalFile 
                                $ T.unpack 
                                $ V.head x
        parseConfig ((name, foo):_) c (i,x) | V.head x == i18nText lang name =
            foo c (i18nString lang name) i (V.tail x) 
        parseConfig (_:ns) c x = parseConfig ns c x

        parseParam :: [(I18nText,JournalFile -> String -> Int -> V.Vector T.Text -> m JournalFile)]
        parseParam = [
          (TJournalFileOpeningBalanceAccount,
           \jf -> textField (\y -> jf{jfOpeningBalanceAccount = y})),

          (TJournalFileEarningsAccount,
           \jf -> textField (\y -> jf{jfEarningsAccount = y})),

          (TJournalFileCompanyName,
           \jf -> textField (\y -> jf{jfCompanyName = y})),

          (TJournalFileDecimalSeparator,
           \jf -> charField (\y -> jf{jfDecimalSeparator = y})),

          (TJournalFileThousandSeparator,
           \jf -> amountSep (\y -> jf{jfThousandSeparator = Just y})),

          (TJournalFileCurrSymbol,
           \jf -> amountSep (\y -> jf{jfCurrencySymbol = Just y})),

         (TJournalFileFirstFiscalMonth,
           \jf -> intField (\y -> jf{jfFirstFiscalMonth = y})),

          (TJournalFileAccountFile,
           \jf -> textField (\y -> jf{jfAccountFile = T.unpack y})),

          (TJournalFileTransactionFiles,
           \jf -> manyTextFields (\y -> jf{jfTransactionFiles = map T.unpack y})),

          (TJournalFileStatementBalanceFiles,
           \jf -> manyTextFields (\y -> jf{jfStatementBalanceFiles = map T.unpack y})),

          (TJournalFileTrialBalanceFiles,
           \jf -> manyTextFields (\y -> jf{jfTrialBalanceFiles = map T.unpack y}))
          ]

        -- Parse a text field and check for error
        textField :: (T.Text -> a) -> String -> Int -> V.Vector T.Text -> m a
        textField _ field i x | V.null x
                             = throwError 
                             $ mkError (SourcePos "" i 2)
                             $ EmptyFieldInJournalFile field
        textField _ field i x | V.head x == "" 
                                = throwError 
                                $ mkError (SourcePos "" i 2)
                                $ EmptyFieldInJournalFile field
        textField f _ _ x = return $ f $ V.head x

        -- Parse a int field and check for error
        intField :: (Int -> a) -> String -> Int -> V.Vector T.Text -> m a
        intField f field i xs = do
          x <- textField id field i xs
          case T.decimal x of
            Right (n, "") -> return $ f n
            _ -> throwError $ mkError (SourcePos "" i 2) $ ParseIntErr (T.unpack x)

        -- Parse a char field and check for error
        charField :: (Char -> a) -> String -> Int -> V.Vector T.Text -> m a
        charField f field i xs = do
          x <- textField id field i xs
          if T.null (T.tail x) 
            then return $ f $ T.head x
            else throwError $ mkError (SourcePos "" i 2) $ ParseCharErr (T.unpack x)

        -- Parse many text fields (multiple columns) and check for error
        manyTextFields :: ([T.Text] -> a) -> String -> Int -> V.Vector T.Text -> m a
        manyTextFields _ field i x | V.null x
                                   = throwError 
                                   $ mkError (SourcePos "" i 2)
                                   $ EmptyFieldInJournalFile field
        manyTextFields _ field i xs | V.all T.null xs
                                    = throwError 
                                    $ mkError (SourcePos "" i 2)
                                    $ EmptyFieldInJournalFile field
        manyTextFields f _ _ xs = return $ f $ filter (not . T.null) $ V.toList xs

        amountSep :: (Char -> a) -> String -> Int -> V.Vector T.Text -> m a
        amountSep f field i xs = do
          x <- charField id field i xs
          if x `elem` amountReservedChar 
            then throwError $ mkError (SourcePos "" i 2) $ UnallowedAmountChar x
            else return $ f x

checkRawJournal :: (MonadError Errors m) => Language -> JournalFile -> m JournalFile
checkRawJournal lang x | T.null (jfOpeningBalanceAccount x ) = 
  throwError 
  $ mkError (SourcePos "" 0 0) 
  $ MissingFieldinJournalFile (i18nString lang TJournalFileOpeningBalanceAccount)
checkRawJournal lang x | T.null (jfEarningsAccount x ) = 
  throwError 
  $ mkError (SourcePos "" 0 0) 
  $ MissingFieldinJournalFile (i18nString lang TJournalFileEarningsAccount)
checkRawJournal lang x | T.null (jfCompanyName x ) = 
  throwError 
  $ mkError (SourcePos "" 0 0) 
  $ MissingFieldinJournalFile (i18nString lang TJournalFileCompanyName )
checkRawJournal lang x | null (jfAccountFile x ) = 
  throwError 
  $ mkError (SourcePos "" 0 0) 
  $ MissingFieldinJournalFile (i18nString lang TJournalFileAccountFile )
checkRawJournal _ x = return x
