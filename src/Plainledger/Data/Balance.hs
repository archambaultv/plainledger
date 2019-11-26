module Plainledger.Data.Balance (
  zeroBalance,
  totalBalance
)
where

import qualified Data.Map.Strict as M
import Plainledger.Data.Type

zeroBalance :: Balance -> Bool
zeroBalance b = M.foldr' (&&) True (fmap (\(d,c) -> d - c == 0) b)

totalBalance :: [Balance] -> Balance
totalBalance =                
 foldr (\x b -> M.unionWith (\(d1, c1) (d2, c2) -> (d1 + d2, c1 + c2))
                             b
                             x)
        M.empty
