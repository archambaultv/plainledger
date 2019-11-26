module Plainledger.Data.Posting (
  zeroPosting
)
where

import Plainledger.Data.Type

zeroPosting :: Posting -> Bool
zeroPosting x = pQuantity x == 0
