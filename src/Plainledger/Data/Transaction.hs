{-# LANGUAGE TupleSections #-}

module Plainledger.Data.Transaction (
  emptyTransaction,
  tagsKeys,
  identifiedTransactions,
  splitTransactions,
  minMaxDates
)
where

import Data.List
import Data.Time
import qualified Data.Text as T
import qualified Data.Set as S
import Plainledger.Data.Posting
import Plainledger.Data.Type
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
