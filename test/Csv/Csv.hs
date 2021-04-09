module Csv.Csv (
  csvFileTestTree,
  )where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Except
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Plainledger.Internal.Csv
import qualified Data.Vector as V
import qualified Data.Text as T
import Plainledger.Error

csvFileTestTree :: TestTree
csvFileTestTree =
  testGroup "Csv"
    [ -- Test if BOM is correctly detected
      testCase "Decode BOM - 1" $ do
        (hasBom, _) <- removeBom . BL.fromStrict 
                       <$> liftIO (BS.readFile "test/Csv/CSV-01.csv")
        unless hasBom (assertFailure "removeBom should detect a BOM with file CSV-01"),

      -- Test if BOM is correctly not detected
      testCase "Decode BOM - 2" $ do
        (hasBom, _) <- removeBom . BL.drop 3 . BL.fromStrict 
                      <$> liftIO (BS.readFile "test/Csv/CSV-01.csv")
        when hasBom (assertFailure "removeBom should not detect a BOM with file"),

      -- Test csvLines
      -- CSV-01.csv has CRLF
      testCase "csvLines" $ do
        (_, csv) <- removeBom . BL.drop 3 . BL.fromStrict 
                    <$> liftIO (BS.readFile "test/Csv/CSV-01.csv")
        let actual = csvLines csv
        assertEqual "" csv01Lines actual,

      -- Test parseCsv 
      testCase "parseCsv" $ do
        (_, csv) <- removeBom . BL.drop 3 . BL.fromStrict 
                    <$> liftIO (BS.readFile "test/Csv/CSV-01.csv")
        case parseCsv ',' $ csvLines csv of
          Left err -> assertFailure $ show err
          Right actual -> assertEqual "" csv01Parse actual,

      -- Assert parsing failure with mismatched quote
      mismatchedQuote (1, "Qu\"ote,Unquote"),
      mismatchedQuote (2, "\"ote,\"Unquote")
      -- As of 2021-04-08, Cassava allows unmatched quote
      -- and removes the 'e' (returns "Quote,Unquot")
      -- mismatchedQuote (3, "\"Quote,Unquote")

    ]

mismatchedQuote :: (Int, BL.ByteString) -> TestTree 
mismatchedQuote (i, x) = 
  testCase ("parseCsv - mismatched quote - " ++ show i) $ do
  let xs = [(1,x)]
  case parseCsv ',' xs of
    Left _ -> return ()
    Right z -> assertFailure 
             $ "mismatched quote should raise an error\n"
             ++ show z

csv01Lines :: [(Int, BL.ByteString)]
csv01Lines = [
  (2, "A,B"),
  (3, "1,2"),
  (4, "Hello,World"),
  (5, "\"Comma,\","),
  (7, ",,,,"),
  (9, "   ")
  ]

csv01Parse :: [(Int, V.Vector T.Text)]
csv01Parse = [
  (2, V.fromList ["A", "B"]),
  (3, V.fromList ["1", "2"]),
  (4, V.fromList ["Hello", "World"]),
  (5, V.fromList ["Comma,",""]),
  (9, V.fromList ["   "])
  ]