{-# LANGUAGE FlexibleContexts #-}

module Plainledger.Data.Account (
  mapToTree,

  aName,
  isRealAccount,
  isVirtualAccount,
  isCreditAccount,
  isDebitAccount,
  isAllowedCommodity,
  guardAllowedCommodity,

  --depth,
  pruneEmptyAccounts,
)
where

import Data.Tree
import Data.Maybe
import Data.List
import Data.Bifunctor
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Plainledger.Data.Type
import Plainledger.Error
import Plainledger.Data.QualifiedName
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
mapToTree = ana coAlgebra . (,) (VirtualAccount) . M.toList
  where coAlgebra :: (AccountInfo, [(QualifiedName, AccountInfo)]) ->
                     TreeF AccountInfo (AccountInfo, [(QualifiedName, AccountInfo)])
        coAlgebra (acc, xs) =
          let xsGroup :: [[(QualifiedName, AccountInfo)]]
              xsGroup = groupBy (\x1 x2 -> (head $ fst x1) == (head $ fst x2)) xs
          in NodeF acc (map makeAccount xsGroup)

        makeAccount :: [(QualifiedName, AccountInfo)] -> (AccountInfo, [(QualifiedName, AccountInfo)])
        makeAccount (([_] , info) : xs) = (info, map (first tail) xs)
        makeAccount ((_:vs, info) : xs) = (VirtualAccount , (vs, info) : map (first tail) xs)
        makeAccount _ = error "Invalid makeAccount call"

-- mapToTree :: AccountMap -> [Account]
-- mapToTree = makeForest . M.toList

-- -- The list must be sorted by QualifiedName !
-- makeForest :: [(QualifiedName, AccountInfo)] -> [Account]
-- makeForest xs =
--   let xsGroup :: [[(QualifiedName, AccountInfo)]]
--       xsGroup = groupBy (\x1 x2 -> (head $ fst x1) == (head $ fst x2)) xs
--   in map makeTree xsGroup

-- -- The list have qualified names starting with the same account name
-- makeTree :: [(QualifiedName, AccountInfo)] -> Account
-- makeTree (([_] , info) : xs) =
--   let children = makeForest $ map (first tail) xs
--   in Node info children
-- makeTree ((v : vs, info) : xs) =
--   let child = makeTree $ (vs, info) : map (first tail) xs
--   in Node (VirtualAccount (v:vs)) [child]
-- makeTree _ = error "Invalid use of makeTree"

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

pruneEmptyAccounts :: Account -> Maybe Account
pruneEmptyAccounts = cata algebra
 where algebra :: TreeF AccountInfo (Maybe Account) -> Maybe Account
       algebra (NodeF info xs) =
         let nonEmpty = catMaybes $ xs
         in if null nonEmpty &&
               (isVirtualAccount info ||
                isRealAccount info && M.empty == aBalance info)
            then Nothing
            else Just $ Node info nonEmpty
