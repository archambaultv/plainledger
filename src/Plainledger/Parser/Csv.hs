{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Parser.Csv
(
  parseCsv,
  csvKeywords
 
) where

import qualified Data.Text as T
import Text.Megaparsec
import Data.Functor.Compose
import Data.Functor.Foldable
import Control.Applicative.Permutations
import Plainledger.Data.Type
import Plainledger.Parser.Lexer

parseCsv :: (Stream s) => Parser s (CsvConfiguration, [CsvStatementAnn SourceOffset])
parseCsv = (,) <$> importConf <*> (many csvStatement) <* eof
             
importConf :: (Stream s) => Parser s CsvConfiguration
importConf = paren $ do
  _ <- symbol "csv"
  c <- runPermutation $
    CsvConfiguration <$>
    toPermutationWithDefault ',' pdelimiter <*>
    toPermutationWithDefault 1 pskip <*>
    toPermutation fields1

  return c

  where pdelimiter = property "column-delimiter" *> delimiter
        fields1 = property "fields" *> some field
        pskip = property "rows-to-skip" *> natural

        delimiter = atomToken
                    (\s -> case s of
                             TString (T.uncons -> Just (c, "")) -> Just c
                             TIdentifier (T.uncons -> Just (c, "")) -> Just c
                             _ -> Nothing)
                    Nothing
                    <?> "string (or identifier) of length one"


        natural = atomToken
                    (\s -> case s of
                             TInteger n | n >= 0 -> Just n
                             _ -> Nothing)
                    Nothing
                    <?> "zero or positive integer"
                    
        field = (,,) <$> tokIdentifier <*> tokInteger <*> fieldType

        fieldType = (symbol "text" >> pure FText) <|>
                    (symbol "date" >> pure FDate) <|>
                    (symbol "number" >> pure FNumber)

csvStatement :: (Stream s) => Parser s (CsvStatementAnn SourceOffset)
csvStatement = (CsvExpr <$> simpleExpr) <|>
               (paren $ define <|> evalRules <|> (CsvExpr <$> functionCall))

simpleExpr :: (Stream s) => Parser s (CsvExprAnn SourceOffset)
simpleExpr = 
           (withOffset EBoolF ((symbol "true" *> pure True) <|>
                               (symbol "false" *> pure False))) <|>
           (withOffset EPrimF primitive) <|>
           (withOffset EVarF tokIdentifier) <|>
           (withOffset EDateF tokDate) <|>
           (withOffset ENumberF tokNumber) <|>
           (withOffset EStringF tokString)

withOffset :: (Stream s) =>
              (a -> f (Fix (Compose ((,) SourceOffset) f))) ->
              Parser s a ->
              Parser s (Fix (Compose ((,) SourceOffset) f))
withOffset f p = do
  p1 <- getInputOffset
  x <- p
  p2 <- getInputOffset
  return $ Fix (Compose ((p1, (p2 - p1)), f x))

primitive :: (Stream s) => Parser s CsvPrim
primitive = choice (map
                    (\(x, y) -> symbol x *> pure y)
                    primList)

primList :: [(T.Text, CsvPrim)]
primList = [("-", OpMinus),
            ("+", OpPlus),
            ("*", OpMult),
            ("/", OpDiv),
            ("round", OpRound),
            (">", OpGT),
            ("<", OpLT),
            ("=", OpEQ),
            ("/=", OpNEQ),
            ("and", OpAnd),
            ("or", OpOr),
            ("not", OpNot),
            ("if", OpIf),
            ("year", OpYear),
            ("month", OpMonth),
            ("day", OpDay),
            ("make-date", OpDate)]

define :: (Stream s) => Parser s (CsvStatementAnn SourceOffset)
define = do
  _ <- symbol "define"
  (op, args) <- constant <|> function
  body <- csvExpr
  return $ CsvDefine op args body

  where constant = (,[]) <$> located tokIdentifier

        function = paren $ (,) <$> located tokIdentifier <*> some (located tokIdentifier)
  
evalRules :: (Stream s) => Parser s (CsvStatementAnn SourceOffset)
evalRules = do
  _ <- symbol "evaluate-rules"
  CsvEvalRules <$> located tokString

functionCall :: (Stream s) => Parser s (CsvExprAnn SourceOffset)
functionCall = do
  p1 <- getInputOffset
  e <- paren $ ECallF <$> csvExpr <*> some csvExpr
  p2 <- getInputOffset
  return $ Fix $ Compose ((p1, (p2 - p1)), e)

csvExpr :: (Stream s) => Parser s (CsvExprAnn SourceOffset)
csvExpr = simpleExpr <|> functionCall

csvKeywords :: [T.Text]
csvKeywords = "define" : "evaluate-rules" : "true" : "false" : map fst primList
