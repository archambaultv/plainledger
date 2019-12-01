{-# LANGUAGE TupleSections #-}

module Plainledger.Data.Transaction (
  emptyTransaction,
  tagsKeys,
  identifiedTransactions,
  splitTransactions,
  minMaxDates,
  importTransactions,
  printTransaction,
  applyRules,
  applyRule
)
where

import Data.Csv
import Data.List
import Data.Time
import Data.Bifunctor
import Data.Void
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text.Encoding as E
import Text.Megaparsec
import Plainledger.Data.QualifiedName
import Plainledger.Parser.Lexer
import Plainledger.Data.Posting
import Plainledger.Data.Type
import Plainledger.Error
import Plainledger.Utils
import Data.Functor.Foldable
import Text.Regex.TDFA

minMaxDates :: [Transaction] -> Maybe (Day, Day)
minMaxDates = cata algebra
  where algebra Nil = Nothing
        algebra (Cons t Nothing) = Just (tDate t, tDate t)
        algebra (Cons t (Just (min1, max1))) =
          if tDate t < min1
            then Just (tDate t, max1)
            else if tDate t > max1
                 then Just (min1, tDate t)
                 else Just (min1, max1)

splitTransactions :: Maybe Day -> Maybe Day -> [Transaction] -> ([Transaction],[Transaction],[Transaction])
splitTransactions Nothing Nothing = ([],,[])
splitTransactions s e = cata algebra
 where algebra :: Algebra (ListF Transaction) ([Transaction],[Transaction],[Transaction])
       algebra Nil = ([],[],[])
       algebra (Cons x (bs,ks,as)) =
         let beforeStart = maybe False (\d -> tDate x < d) s
             afterEnd = maybe False (\d -> tDate x > d) e
         in if beforeStart
            then (x : bs, ks, as)
            else if afterEnd
                 then (bs, ks, x : as)
                 else (bs, x : ks, as)
         

emptyTransaction :: Transaction -> Bool
emptyTransaction Transaction{tPostings = ps}  = and $ map zeroPosting ps

-- Returns a sorted list (without duplicates) of all the tag keys
tagsKeys :: [Transaction] -> [T.Text]
tagsKeys ts =
  let tags = concatMap tTags ts
      keys = map tagKeyword tags
  in sort $ S.toList $ S.fromList keys

identifiedTransactions :: Ledger -> [(T.Text, Transaction)]
identifiedTransactions l =
  let ts = groupBy (\t1 t2 -> tDate t1 == tDate t2) (lTransactions l)
  in cata algebra ts

  where algebra Nil = []
        algebra (Cons ts res) =
          let ns :: [(Integer, Transaction)]
              ns = zip [1..] ts

              ts' = map (\(n, t) -> let d = tDate t
                                    in (T.pack (show d ++ "-" ++ show n),t))
                    ns
          in ts' ++ res

printTransaction :: Transaction -> String
printTransaction t = 
  let header1 = "(transaction " ++ show (tDate t) ++ "\n"
      spaceP   = "            "
      postings = "  :postings " ++ intercalate spaceP (map printP (tPostings t))
      tags = intercalate "\n" $ map printTag (tTags t)
  in header1 ++ postings ++ tags ++ ")"

  where
        printP :: Posting -> String
        printP p = "(" ++
                   ('"' : qualifiedNameToString (pAccount p) ++ "\"") ++ 
                   " " ++
                   show (pQuantity p) ++
                   " " ++
                   T.unpack (pCommodity p) ++
                   ")\n"
        printTag :: Tag -> String
        printTag t1 = "  :" ++
                   T.unpack (tagKeyword t1) ++
                   maybe "" (\v -> " \"" ++ T.unpack v ++ "\"") (tagValue t1)

importTransactions :: ImportConfiguration -> Csv -> Either Error [Transaction]
importTransactions config csv =
  let csvSkip = V.drop (skip config) csv
  in V.toList <$> traverse makeTransaction csvSkip

  where makeTransaction :: V.Vector Field -> Either Error Transaction
        makeTransaction v = do
          date1 <- readDate v
          amount <- readAmount v
          --balance <- readBalance v
          tags <- readTags v
          let p1 = Posting (iAccount config) amount (defaultCommodity config)
          let acc2 = if amount < 0 then (iAccountNegative config) else (iAccountPositive config)
          let p2 = Posting acc2 (negate amount) (defaultCommodity config)
          return $ Transaction date1 tags [p1, p2]

        megaParse :: Parsec Void T.Text a -> Field -> Either Error a
        megaParse p x = first errorBundlePretty $ parse p "" (E.decodeUtf8 x)
        
        readDate :: V.Vector Field -> Either Error Day
        readDate v = do
          d <- maybe (Left "No date") pure (v V.!? (dateColumn config))
          megaParse date d

        readAmount :: V.Vector Field -> Either Error Quantity
        readAmount v = do
          debit <- maybe (Left "No debit") pure (v V.!? (debitColumn config))
          credit <- maybe (Left "No date") pure (v V.!? (creditColumn config))
          dr <- megaParse decimal debit
          cr <- megaParse decimal credit
          return (dr - cr)

        readTags :: V.Vector Field -> Either Error [Tag]
        readTags v = do
          values <- traverse
                    (\t -> maybe (Left "No tag") (pure . T.strip . E.decodeUtf8) (v V.!? (fst t)))
                    (tagColumns config) :: Either Error [T.Text]
          return $ map toTag (zip values (tagColumns config))

        toTag :: (T.Text, (Int, T.Text)) -> Tag
        toTag (v, (_, k)) | T.null v = Tag k Nothing
        toTag (v, (_, k)) = Tag k (Just v)

-- Apply the rules from left to right. Stops if an error
-- happens. Stops when the transaction is deleted.
applyRules :: [TransactionRule] -> Transaction -> Either Error (Maybe Transaction)
applyRules rules t = cata algebra rules t
  where algebra :: Algebra (ListF TransactionRule) (Transaction -> Either Error (Maybe Transaction))
        algebra Nil t1 = pure $ Just t1
        algebra (Cons rule acc) t1 =
          case applyRule t1 rule of
            Right (Just t2) -> acc t2
            x -> x
            
applyRule :: Transaction -> TransactionRule -> Either Error (Maybe Transaction)
applyRule t rule =
  case applyMatch (fst rule) t of
    Left Nothing -> pure $ Just t
    Left (Just err) -> Left err
    Right vars -> case (snd rule) of
      -- The Nothing case means we need to delete the transaction
      Nothing -> pure Nothing
      -- We modify the transaction according to the template
      Just template -> pure $ Just t -- fromTemplate vars template t

  where applyMatch :: TransactionMatch -> Transaction -> Either (Maybe Error) (M.Map String String)
        applyMatch m t1 =  do
           v1 <- matchDate (tDate m) (tDate t1)
           v2 <- matchPostings (map snd $ tPostings m) (tPostings t1)
           v3 <- matchTags (map snd $ tTags m) (tTags t1)
           let vars = v1 ++ v2 ++ v3
           let dups = duplicateKeys vars
           case dups of
             [] -> pure $ M.fromList vars
             _ -> Left $ Just $ "Conflicting definitions for " ++ intercalate ", " dups

        regexMatch :: (Eq a) => (a -> String) -> ERegex a -> a -> Either (Maybe Error) [(String, String)]
        regexMatch _ (Right x1) x2 = if x1 == x2
                                     then pure []
                                     else Left Nothing
        regexMatch f (Left (vars, re)) x2 =
          let m :: (String, String, String, [String])
              m = (f x2) =~ re
          in case m of
               (_,(_:_),_,vs) -> zipEither vars vs
               _ -> Left Nothing

        matchDate :: Maybe (ERegex Day) -> Day -> Either (Maybe Error) [(String, String)]
        matchDate Nothing _ = pure []
        matchDate (Just d1) d2 = regexMatch show d1 d2

        
        matchRegexList :: (r -> Algebra (ListF a) (Either (Maybe Error) [(String, String)])) ->
                          [r] ->
                          [a] ->
                          Either (Maybe Error) [(String, String)]
        matchRegexList algebra rs xs = concat <$> traverse (matchRegexList1 xs) rs
          where matchRegexList1 as r = cata (algebra r) as

        matchPostings :: [PostingMatch] -> [Posting] -> Either (Maybe Error) [(String, String)]
        matchPostings = matchRegexList alg
          where alg _ Nil = Left Nothing
                alg pM (Cons p acc) = either (maybe acc (Left . Just)) pure $ do
                  p1 <- regexMatch qualifiedNameToString (pAccount pM) (pAccount p)
                  p2 <- regexMatch show (pQuantity pM) (pQuantity p)
                  p3 <- regexMatch T.unpack (pCommodity pM) (pCommodity p)
                  return $ p1 ++ p2 ++ p3


        matchTags :: [TagMatch] -> [Tag] -> Either (Maybe Error) [(String, String)]
        matchTags = matchRegexList alg
          where alg _ Nil = Left Nothing
                alg tM (Cons t1 acc) =
                  case (t1, tM) of
                    (Tag k1 _, Tag k2 _) | k1 /= k2 -> acc
                    (Tag _ Nothing, Tag _ Nothing) -> pure []
                    (Tag _ (Just v), Tag _ (Just re)) -> regexMatch T.unpack re v 
                    _ -> Left Nothing
        
        zipEither :: [String] -> [String] -> Either (Maybe Error) [(String, String)]
        zipEither [] _ = pure []
        zipEither _ [] = Left $ Just "Not enough sub matches for the number of variables"
        zipEither ("_":xs) (_:ys) = zipEither xs ys
        zipEither (x:xs) (y:ys) = do
          l <- zipEither xs ys
          pure $ (x, y) : l
