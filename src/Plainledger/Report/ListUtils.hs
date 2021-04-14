-- |
-- Module      :  Plainledger.Reports.ListUtils
-- Copyright   :  © 2021 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--

module Plainledger.Report.ListUtils (
  elementSum,
  elementAddition
  )
where

import Plainledger.Journal

elementSum :: [[Quantity]] -> [Quantity]
elementSum [] = []
elementSum xs = 
  let qties = foldl (\s x -> if null x then s else head x + s) 0 xs
      rest = filter (not . null) $ map (drop 1) xs
  in qties : elementSum rest

elementAddition :: [Quantity] -> [Quantity] -> [Quantity]
elementAddition [] xs = xs
elementAddition xs [] = xs
elementAddition (x:xs) (y:ys) = x + y : elementAddition xs ys