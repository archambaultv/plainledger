module Plainledger.Data.Transaction (
  emptyTransaction,
  tagsKeys
)
where

import Data.List
import qualified Data.Text as T
import qualified Data.Set as S
import Plainledger.Data.Posting
import Plainledger.Data.Type

emptyTransaction :: Transaction -> Bool
emptyTransaction Transaction{tPostings = ps}  = and $ map zeroPosting ps

-- Returns a sorted list (without duplicates) of all the tag keys
tagsKeys :: [Transaction] -> [T.Text]
tagsKeys ts =
  let tags = concatMap tTags ts
      keys = map tagKeyword tags
  in sort $ S.toList $ S.fromList keys
