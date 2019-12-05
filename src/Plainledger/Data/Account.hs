{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Data.Account (
  mapToTree,

  aName,
  isRealAccount,
  isVirtualAccount,
  isCreditAccount,
  isDebitAccount,
  isAllowedCommodity,
  guardAllowedCommodity,
--  pruneEmptyAccounts,
--  flattenBalance,
  minAndMaxDates,
  flattenBalance,
  totalBalance,
  totalNetBalance,
  updateAccount
)
where

import Data.Tree
import Data.Maybe
import Data.List
import Data.Time
import Data.Bifunctor
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Plainledger.Data.Type
import Plainledger.Error
import Plainledger.Data.QualifiedName
import Plainledger.Data.Balance
import Data.Functor.Foldable

isAllowedCommodity :: Commodity -> AccountInfo -> Bool
isAllowedCommodity c info = isNothing (aAllowedCommodities info) ||
                           S.member c (fromJust $ aAllowedCommodities info)

guardAllowedCommodity :: (MonadError Error m) => Commodity -> AccountInfo -> m ()
guardAllowedCommodity c info =
  if isAllowedCommodity c info
  then pure ()
  else throwError $
       "Commodity " ++ T.unpack c ++
       " is not allowed for account " ++
       (qualifiedNameToString $ aQName info)

aName :: AccountInfo -> T.Text
aName = last . aQName

mapToTree :: AccountMap -> Account
mapToTree = ana coAlgebra . (,) (VirtualAccount{aQName = []}) . M.toList
  where coAlgebra :: CoAlgebra (TreeF AccountInfo) (AccountInfo, [(QualifiedName, AccountInfo)]) 
        coAlgebra (acc, children) =
          let xsGroup :: [[(QualifiedName, AccountInfo)]]
              xsGroup = groupBy (\x1 x2 -> (head $ fst x1) == (head $ fst x2)) children
          in NodeF acc (map (makeAccount acc) xsGroup)

        makeAccount :: AccountInfo -> [(QualifiedName, AccountInfo)] -> (AccountInfo, [(QualifiedName, AccountInfo)])
        makeAccount _ (([_] , info) : xs) = (info, map (first tail) xs)
        makeAccount acc ((v:vs, info) : xs) = (VirtualAccount{aQName = aQName acc ++ [v]} , (vs, info) : map (first tail) xs)
        makeAccount _ _ = error "Invalid makeAccount call"

updateAccount :: (AccountInfo -> AccountInfo) -> QualifiedName -> Account -> Account
updateAccount f n a = apo coAlgebra (n, a)
 where coAlgebra :: RCoAlgebra (TreeF AccountInfo) Account (QualifiedName, Account)
       coAlgebra ([], _) = error "QualifiedName must be a non empty list"
       coAlgebra ([x], Node acc children)
         | last (aQName acc) == x =  NodeF (f acc) (map Left children)
         | otherwise = NodeF acc (map Left children)
       coAlgebra ((x:xs), Node acc children)
         | last (aQName acc) == x =  NodeF acc (map (Right . (xs,)) children)
         | otherwise = NodeF acc (map Left children)

isRealAccount :: AccountInfo -> Bool
isRealAccount VirtualAccount{} = False
isRealAccount RealAccount{} = True

isVirtualAccount :: AccountInfo -> Bool
isVirtualAccount = not . isRealAccount

isCreditAccount :: AccountType -> Bool
isCreditAccount a = a `elem` [Liability, Equity, Revenue]

isDebitAccount :: AccountType -> Bool
isDebitAccount a = a `elem` [Asset, Expense]

-- depth :: Account -> Integer
-- depth = cata algebra
--  where algebra :: TreeF a Integer -> Integer
--        algebra (NodeF _ xs) = 1 + sum xs

-- pruneEmptyAccounts :: Account -> Maybe Account
-- pruneEmptyAccounts = filterAccounts (not . M.null . aBalance)

-- filterAccounts :: (AccountInfo -> Bool) -> Account -> Maybe Account
-- filterAccounts f s = cata algebra
--  where algebra :: TreeF AccountInfo (Maybe Account) -> Maybe Account
--        algebra (NodeF info xs) =
--          let nonEmpty = catMaybes $ xs
--              test = f info
--          in if null nonEmpty &&
--                (isVirtualAccount info ||  not test)
--             then Nothing
--             else if test
--                  then Just $ Node info nonEmpty
--                  else Just $ Node VirtualAccount nonEmpty
           
minAndMaxDates :: Account -> (Day, Day)
minAndMaxDates = cata algebra
  where algebra (NodeF VirtualAccount{} dts) =
          (minimum $ map fst dts, maximum $ map snd dts)
        algebra (NodeF RealAccount{aOpenDate = s, aCloseDate = c} dts) =
          let e = fromMaybe s $ c
          in (minimum $ s : map fst dts, maximum $ e : map snd dts)

flattenBalance :: Tree AccountInfo -> [(AccountInfo, Commodity, (Quantity, Quantity))]
flattenBalance = cata algebra
  where
      algebra (NodeF VirtualAccount{} ls) = concat ls
      algebra (NodeF a ls) =
        let lines1 :: [(AccountInfo, Commodity, (Quantity, Quantity))]
            lines1 = map (\(c, amount) -> (a, c, amount)) $ M.toList (aBalance a)
        in concat (lines1 : ls)

-- A new balance where :
-- Credit = sum of all the credits
-- Debit = sum of all the debits          
totalBalance :: Tree AccountInfo -> Balance
totalBalance = cata algebra
  where
      algebra (NodeF VirtualAccount{} bs) = sumBalance bs
      algebra (NodeF a bs) = sumBalance (aBalance a : bs)

-- A new balance where :
-- Credit = sum of all the accounts with negative total
-- Debit = sum of all the accounts with positive toal
totalNetBalance :: Tree AccountInfo -> Balance
totalNetBalance = cata algebra
  where
      algebra (NodeF VirtualAccount{} bs) = sumBalance bs
      algebra (NodeF a bs) = sumBalance (netBalance (aBalance a) : bs)
