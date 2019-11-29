{-# LANGUAGE TupleSections #-}

module Plainledger.Data.Transaction (
  emptyTransaction,
  tagsKeys,
  identifiedTransactions,
  splitTransactions,
  minMaxDates,
  importTransactions,
  printTransaction
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
import qualified Data.Text.Encoding as E
import Text.Megaparsec
import Plainledger.Data.QualifiedName
import Plainledger.Parser.Lexer
import Plainledger.Data.Posting
import Plainledger.Data.Type
import Plainledger.Error
import Data.Functor.Foldable

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
