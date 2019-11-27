module Plainledger.Data.Balance (
  zeroBalance,
  sumBalance
)
where

import Data.Functor.Foldable
import qualified Data.Map.Strict as M
import Plainledger.Data.Type

zeroBalance :: Balance -> Bool
zeroBalance b = M.foldr' (&&) True (fmap (\(d,c) -> d - c == 0) b)

sumBalance :: [Balance] -> Balance
sumBalance = cata algebra
  where algebra Nil = M.empty
        algebra (Cons x acc) = M.unionWith (\(d1, c1) (d2, c2) -> (d1 + d2, c1 + c2))
                               acc
                               x
