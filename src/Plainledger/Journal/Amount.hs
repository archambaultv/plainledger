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
  AmountDescriptor,
  parseAmount,
  writeAmount,
  amountReservedChar
  )
where

import Data.Decimal
import qualified Data.Text as T
import Control.Monad.Except
import Plainledger.Error
import Data.Char (isDigit, ord)
import Data.Bifunctor (first)


-- | A quantity is any decimal number. The decimal package ensures
-- that no rounding error can occur.
type Quantity = Decimal
type AmountDescriptor = (Char, Maybe Char, Maybe Char)

-- | The following characters should not be used as decimal or thousand character
-- or as a currency symbol
amountReservedChar :: [Char]
amountReservedChar = ['(',')','e','+','-']

-- Parser for amounts 

-- The sign can be specified with + or - in front or by
-- surrounding the number with parenthesis (like in Excel).
--
-- The currency symbol must be at the front or at the end. It can be followed by
-- a space when at the front and preceded by one space if at the end. When
-- parenthesis are used for negative value, then the currency symbol must be
-- after the opening parenthesis or before the closing parenthesis.
--
-- Thousand separator can be used, but not with scientific notation 
-- (number with an exponent)

-- Examples of allowed number :
--
-- 1
-- -1
-- 123.456
-- 0.578
-- -1.234
-- 12e12
-- 12.10E12
-- 16e-24
-- 16e+24
-- 45.24e48
-- -2e5
-- (1)
-- (1.0$)
-- (1.0 $)
-- (123.5e2 $)
-- (2 346,34$)
-- 23,235.23
-- 23,235.23$
-- (€ 23)
-- (€ 23.05e4)
-- € -12
-- €-12


parseAmount :: MonadError Errors m => AmountDescriptor -> T.Text -> m Quantity
parseAmount d t = 
  case parseAmount' d t of
      Left (ParseAmountExponentErr _) -> 
        throwError $ mkErrorNoPos $ ParseAmountExponentErr $ T.unpack t
      Left _ -> 
        throwError $ mkErrorNoPos $ ParseAmountErr $ T.unpack t
      Right v -> return v

parseAmount' :: (MonadError ErrorType m) => 
               AmountDescriptor -> 
               T.Text -> 
               m Quantity
parseAmount' _ "" = throwError $ ParseAmountErr ""
parseAmount' d x | T.head x == '(' =
   if T.last x == ')'
   then let x' = removeCurrSymbol d
               $ T.init 
               $ T.tail x
        in negate <$> parseNumber d x'
   else throwError $ ParseAmountErr ""
parseAmount' d x = 
  parseSignedNumber d (removeCurrSymbol d x)

removeCurrSymbol :: AmountDescriptor -> 
                    T.Text ->
                    T.Text
removeCurrSymbol _ "" = ""
removeCurrSymbol (_,_,Nothing) x = x
removeCurrSymbol (_,_,Just symbol) x | T.head x == symbol =
  T.dropWhile (== ' ') $ T.tail x
removeCurrSymbol (_,_,Just symbol) x | T.last x == symbol =
  T.dropWhileEnd (== ' ') $ T.init x
removeCurrSymbol _ x = x

parseSignedNumber :: (MonadError ErrorType m) => 
                     AmountDescriptor -> 
                     T.Text -> 
                     m Quantity
parseSignedNumber _ "" = throwError $ ParseAmountErr ""
parseSignedNumber d x | T.head x == '-'
  = negate <$> parseNumber d (T.tail x)
parseSignedNumber d x | T.head x == '+'
  = parseNumber d (T.tail x)
parseSignedNumber d x = parseNumber d x

parseNumber :: forall m . (MonadError ErrorType m) => 
               AmountDescriptor -> 
               T.Text -> 
               m Quantity
parseNumber (sep, thousandSep, _) x = do
  (coeff, x2) <- parseInt $ removeThousand x
  (frac, x3) <- parseFractional x2
  (exponent1, x4) <- parseExponent x3

  unless (T.null x4) (throwError $ ParseAmountErr "")

  let nText = T.append coeff frac
  let n = strToInt nText
  -- e means the number of decimal digits
  -- negative value means we need to add some zeros to n
  let e = T.length frac - exponent1
  if e < 0
     then return $ Decimal 0 $ n * 10 ^ negate e
     else if e < 256
        then return $ Decimal (fromIntegral e) n
        else throwError $ ParseAmountExponentErr $ T.unpack x

  where 

        parseInt :: T.Text -> m (T.Text, T.Text)
        parseInt y = 
          let n = T.takeWhile isDigit y
              r = T.drop (T.length n) y
          in notNull n >> return (n, r)

        removeThousand :: T.Text -> T.Text
        removeThousand t =
          case thousandSep of
            Nothing -> t
            Just s ->
              T.filter (/= s) t

        parseFractional :: T.Text -> m (T.Text, T.Text)
        parseFractional "" = return ("","")
        parseFractional y | T.head y == sep =
          parseInt (T.tail y)
        parseFractional y = return ("", y)

        parseExponent :: (Integral n) => T.Text -> m (n, T.Text)
        parseExponent "" = return (0, "")
        parseExponent y | T.head y == 'e' || T.head y == 'E' =
          parseSignedInt $ T.tail y
        parseExponent y = return (0, y)

        parseSignedInt :: (Integral n) => T.Text -> m (n, T.Text)
        parseSignedInt "" = throwError $ ParseAmountErr ""
        parseSignedInt y | T.head y == '+' 
          = first strToInt <$> parseInt (T.tail y)
        parseSignedInt y | T.head y == '-'
          = first (negate . strToInt) <$> parseInt (T.tail y)
        parseSignedInt y = first strToInt <$> parseInt y

        strToInt :: (Integral n) => T.Text -> n
        strToInt y = T.foldl' 
                     (\t v -> 10 * t + fromIntegral (ord v - ord '0')) 
                     0
                     y

        notNull :: T.Text -> m ()
        notNull "" = throwError $ ParseAmountErr ""
        notNull _ = return ()

writeAmount :: Char -> Quantity -> T.Text
writeAmount c a = T.pack
                $ map (\x -> if x == '.' then c else x) 
                $ show a