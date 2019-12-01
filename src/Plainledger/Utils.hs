module Plainledger.Utils
(
  duplicateKeys
) where

import qualified Data.Map as M

-- Check for duplicated keys
duplicateKeys :: Ord k => [(k,v)] -> [k]
duplicateKeys l =
  let mTest = M.fromListWith (const . const True) (map (fmap (const False)) l)
  in M.keys $ M.filter (== True) mTest
