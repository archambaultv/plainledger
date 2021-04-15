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
  parsePositiveInt,
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

parsePositiveInt :: (MonadError Errors m) => T.Text -> m Int
parsePositiveInt t =
  let (n, r) = T.break (not . isDigit) t
      n1 = strToInt n
  in if T.null r && n1 > 0
     then return n1
     else throwError $ mkErrorNoPos $ ParsePosIntErr $ T.unpack t

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
  (coeff, x2) <- parseIntWithSep x
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
          let (n, r) = T.break (not . isDigit) y
          in notNull n >> return (n, r)

        parseIntWithSep :: T.Text -> m (T.Text, T.Text)
        parseIntWithSep t =
          case thousandSep of
            Nothing -> parseInt t
            Just s -> 
              let (n, r) = T.break (not . \d -> isDigit d || d == s) t
              in notNull n >> (,r) <$> parseSep s n

        parseSep :: Char -> T.Text -> m T.Text
        parseSep s d = validSep False 0 s d 
                     >>= \y -> if y then return (T.filter (/= s) d) else return d

        -- Checks that the thousand separator is well placed.
        -- Not at the beginning
        -- From the right to left, every tree digit (if present)
        -- Returns if a separator was present (or throw an error)
        validSep :: Bool --  Did we found a separator char ?
                  -> Int --  How many digit since the last separator char
                         --  or how many digit processed so far (if first param)
                         --  is false
                  -> Char --  The separator char
                  -> T.Text --  Remaining input to process
                  -> m Bool
        validSep False _ _ "" = return False
        validSep False 0 s t | T.head t == s = throwError $ ParseAmountErr ""
        validSep False n s t | T.head t == s && n <= 3 = validSep True 0 s (T.tail t)
        validSep False n s t | T.head t == s && n > 3 = throwError $ ParseAmountErr ""
        validSep False n s t = validSep False (n + 1) s (T.tail t)
        validSep True 3 _ "" = return True
        validSep True _ _ "" = throwError $ ParseAmountErr ""
        validSep True 3 s t | T.head t == s = validSep True 0 s (T.tail t)
        validSep True 3 s t | T.head t /= s = throwError $ ParseAmountErr ""
        validSep True _ s t | T.head t == s = throwError $ ParseAmountErr ""
        validSep True n s t = validSep True (n + 1) s (T.tail t)

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

        notNull :: T.Text -> m ()
        notNull "" = throwError $ ParseAmountErr ""
        notNull _ = return ()

strToInt :: (Integral n) => T.Text -> n
strToInt y = T.foldl' 
              (\t v -> 10 * t + fromIntegral (ord v - ord '0')) 
              0
              y

writeAmount :: Char -> Quantity -> T.Text
writeAmount c a = T.pack
                $ map (\x -> if x == '.' then c else x) 
                $ show a