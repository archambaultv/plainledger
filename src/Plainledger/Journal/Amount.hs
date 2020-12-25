-- |
-- Module      :  Plainledger.Journal.Amount
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the type alias Quantity and Commodity

module Plainledger.Journal.Amount 
(
  Quantity,
  parseAmount
  )
where

import Data.Decimal
import qualified Data.Text as T
import Control.Monad.Except
import Plainledger.Error
import Data.Char (isDigit, ord)


-- | A quantity is any decimal number. The decimal package ensures
-- that no rounding error can occur.
type Quantity = Decimal

-- Parses for amounts
-- Reads scientific number like :
--
-- 1
-- -1
-- 123.456
-- 0.578
-- -1.234
-- .246
-- -.24
-- 12e12
-- 12.10E12
-- 16e-24
-- 16e+24
-- 45.24e48
-- .246E7
-- -2e5
-- -.24e4
-- The decimal separator can be specified
parseAmount :: forall m . (MonadError Errors m) => Char -> T.Text -> m Quantity
parseAmount sep x = do
  let err = (throwError $ mkErrorNoPos $ ParseAmountErr $ T.unpack x)
  (coeffSign, x1) <- parseSign x
  (coeff, x2) <- parseCoeff x1
  (frac, x3) <- parseFractional x2
  (e2, expSign, x4) <- parseExponent x3

  when (not $ T.null x4) err
  let nText = T.append coeff frac
  when (T.null nText) err
  let n = applySign coeffSign $ strToInt nText
  -- e means the number of decimal digits
  -- negative value means we need to add some zeros to n
  let e = T.length frac - (applySign expSign $ strToInt e2)
  if e < 0
     then return $ Decimal 0 $ n * 10 ^ negate e
     else if e < 256
        then return $ Decimal (fromIntegral e) n
        else throwError $ mkErrorNoPos $ ParseAmountExponentErr $ T.unpack x


  where parseSign :: T.Text -> m (Bool, T.Text)
        parseSign y | T.head y == '-' = return (False, T.drop 1 y)
        parseSign y | T.head y == '+' = return (True, T.drop 1 y)
        parseSign y = return (True, y)

        parseCoeff :: T.Text -> m (T.Text, T.Text)
        parseCoeff y = 
          let n = T.takeWhile isDigit y
              r = T.drop (T.length n) y
          in return (n, r)

        parseFractional :: T.Text -> m (T.Text, T.Text)
        parseFractional y | T.head y == sep =
          let n = T.takeWhile isDigit (T.drop 1 y)
              r = T.drop (T.length n + 1) y
          in if T.length n == 0
             then throwError $ mkErrorNoPos $ ParseAmountErr $ T.unpack x
             else return (n, r)
        parseFractional y = return ("", y)

        parseExponent :: T.Text -> m (T.Text, Bool, T.Text)
        parseExponent y | T.head y == 'e' || T.head y == 'E' = do
          (isPos, x1) <- parseSign (T.drop 1 y)
          (n, x2) <- parseCoeff x1
          if T.length n == 0
             then throwError $ mkErrorNoPos $ ParseAmountErr $ T.unpack x
             else return (n, isPos, x2)
        parseExponent y = return ("", True, y)

        strToInt :: (Integral n) => T.Text -> n
        strToInt y = T.foldl' 
                     (\t v -> 10 * t + (fromIntegral $ ord v - ord '0')) 
                     0
                     y

        applySign :: (Integral n) => Bool -> n -> n
        applySign True n = n
        applySign False n = negate n