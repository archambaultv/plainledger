{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Data.Account (
  mapToTree,

  aName,
--  isRealAccount,
--  isVirtualAccount,
  isCreditAccountType,
  isDebitAccountType,
  isBalanceSheetAccountType,
  isAllowedCommodity,
  guardAllowedCommodity,
  pruneEmptyAccounts,
--  flattenBalance,
  minAndMaxDates,
  flattenBalance,
  totalBalance,
  totalNetBalance,
  orderTreeByAccountType
--  updateAccount
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
import Data.Function
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

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
aName = NE.last . aQName

-- Orders the first level by account type (all trees in a subtree have
-- the same account type)
orderTreeByAccountType :: AccountTypeMap -> AccountTree -> AccountTree
orderTreeByAccountType accType (Node (Left []) xs) =
  let xs' = sortBy (compare `on` foo) xs
      foo (Node (Left []) _) = error "Account:orderTreeByAccountType:foo root node"
      foo (Node (Left (a:_)) _) = accType M.! a
      foo (Node (Right info) _) = accType M.! (NE.head $ aQName info)
  in Node (Left []) xs'
orderTreeByAccountType _ x = x

-- Build a tree by using the QualifiedName
-- Nodes that are absent from the AccountMap only contains the qualified name
-- for that node
mapToTree :: AccountMap -> AccountTree
mapToTree = Node (Left []) . map (buildTree []) . groupByHead . M.toAscList --ana coAlgebra . (Nothing,) . M.toAscList

  where groupByHead :: [(QualifiedName, AccountInfo)] -> [[(QualifiedName, AccountInfo)]]  
        groupByHead = groupBy ((==) `on` (NE.head . fst))

        buildTree :: [AccountName] -> [(QualifiedName, AccountInfo)] -> AccountTree
        buildTree _ [] = error "Account:mapToTree:buildTree empty list"
        buildTree n (((x :| []), info) : more) =
          let name = n ++ [x]
          in Node (Right info) $ map (buildTree name) $ groupByHead $ map (first tailNE) more
        buildTree n xs@(((x :| _), _) : _) =
          let name = n ++ [x]
          in Node (Left name) $ map (buildTree name) $ groupByHead $ map (first tailNE) xs

        tailNE :: NonEmpty a -> NonEmpty a
        tailNE (_ :| []) = error "Account:mapToTree:tailNE singleton NonEmpty list"
        tailNE (_ :| (y:ys)) = y :| ys 

  -- where coAlgebra :: CoAlgebra
  --                    (TreeF (Either [AccountName] AccountInfo))
  --                    (Maybe [AccountName], [(QualifiedName, AccountInfo)])
  --       -- If we are given an empty list, we build the empty tree without children
  --       coAlgebra (info, []) = NodeF (Left $ fromMaybe [] info) []
        
  --       -- Nothing means we start building the tree
  --       coAlgebra (Nothing, xs) =
  --         let xsGroup = groupByHead xs
  --         in NodeF (Left []) $ map (Just [],) xsGroup

  --       -- If we have a qualified name with 1 element, this element is the node
  --       coAlgebra (Just _, ((_ :| [] , info) : xs)) =
  --         let xsGroup = groupByHead $ map (first tailNE) xs
  --             node = Right info
  --         in NodeF node $ map (Just $ NE.toList $ aQName info,) xsGroup

  --       -- If no qualified name has 1 element, this node is a virtual node
  --       coAlgebra (Just parent, ((v :| (v1 : v1s), info) : xs)) =
  --         let xsGroup = groupByHead $ (v1 :| v1s, info) : map (first tailNE) xs
  --             name = parent ++ [v]
  --             node = Left name
  --         in NodeF node $ map (Just name,) xsGroup

  --       groupByHead :: [(QualifiedName, AccountInfo)] -> [[(QualifiedName, AccountInfo)]]  
  --       groupByHead = groupBy ((==) `on` (NE.head . fst))

  --       tailNE :: NonEmpty a -> NonEmpty a
  --       tailNE (_ :| []) = error "Account:mapToTree:tailNE singleton NonEmpty list"
  --       tailNE (_ :| (y:ys)) = y :| ys 

        -- coAlgebra (info, children) =
        --   let xsGroup :: [[(QualifiedName, AccountInfo)]]
        --       xsGroup = groupBy ((==) `on` (Ne.head . fst)) children
        --   in NodeF info $ map (makeAccount info)  xsGroup

        -- makeAccount :: NodeA -> [(QualifiedName, AccountInfo)] -> (NodeA, [(QualifiedName, AccountInfo)])
        -- makeAccount _ ((_ :| [] , info) : xs) = (Right info, map (first NE.tail) xs)
        -- makeAccount parent ((v :| vs, info) : xs) = (Left $ VNode $ name ++ [v] ,
        --                                                 (vs, info) : map (first NE.tail) xs)

        -- nodeName (Left VRoot) = []
        -- nodeName (Left (VNode n)) = n
        -- nodeName (Right acc) = aQName acc
                        
-- updateAccount :: (AccountInfo -> AccountInfo) -> QualifiedName -> Account -> Account
-- updateAccount f n a = apo coAlgebra (n, a)
--  where coAlgebra :: RCoAlgebra (TreeF NodeA) Account (QualifiedName, Account)
--        -- Root, we go directly to the children
--        coAlgebra (x, Node (Left VRoot) children) =
--          NodeF (Left VRoot) $ map (Right . (x,)) children

--        -- Qualified has one element, we are at the proper level, but
--        -- we cannot modify a VirtualAccount
--        coAlgebra (_ :| [], Node y@(Left _) children) =
--          NodeF y (map Left children)

--        -- Qualified has one element, we are at the proper level and we have an AccountInfo node
--        coAlgebra (x :| [], Node y@(Right acc) children)
--          | NE.last (aQName acc) == x = NodeF (Right $ f acc) (map Left children)
--          | otherwise = NodeF y (map Left children)

--        -- Qualified name has more than one element, we need to go
--        -- deeper in the tree if we are in the correct branch
--        coAlgebra (x :| _, Node y@(Left (VNode (z :| _))) children)
--          | x /= z = NodeF y (map Left children)

--        coAlgebra (x :| _, Node y@(Left (VNode (z :| _))) children)
--          | x /= z = NodeF y (map Left children)
         
--        coAlgebra (_ :| (y : ys), Node z children)  
--          NodeF z $ map (Right . ((y :| ys),)) children
       
-- isRealAccount :: AccountInfo -> Bool
-- isRealAccount VirtualAccount{} = False
-- isRealAccount RealAccount{} = True

-- isVirtualAccount :: AccountInfo -> Bool
-- isVirtualAccount = not . isRealAccount

isCreditAccountType :: AccountType -> Bool
isCreditAccountType a = a `elem` [Liability, Equity, Revenue]

isDebitAccountType :: AccountType -> Bool
isDebitAccountType a = a `elem` [Asset, Expense]

isBalanceSheetAccountType :: AccountType -> Bool
isBalanceSheetAccountType a = a `elem` [Asset, Liability, Equity]

-- depth :: Account -> Integer
-- depth = cata algebra
--  where algebra :: TreeF a Integer -> Integer
--        algebra (NodeF _ xs) = 1 + sum xs

pruneEmptyAccounts :: AccountTree -> AccountTree
pruneEmptyAccounts = filterAccounts (not . M.null . aBalance)

filterAccounts :: (AccountInfo -> Bool) -> AccountTree -> AccountTree
filterAccounts f s = fromMaybe (Node (Left []) []) $ cata algebra s
 where algebra :: Algebra (TreeF (Either [AccountName] AccountInfo)) (Maybe AccountTree)
       algebra (NodeF (Left x) xs) =
         let nonEmpty = catMaybes $ xs
         in if null nonEmpty
            then Nothing
            else Just $ Node (Left x) nonEmpty

       algebra (NodeF (Right info) xs) =
         let nonEmpty = catMaybes $ xs
         in if null nonEmpty && not (f info)
            then Nothing
            else Just $ Node (Right info) nonEmpty

-- minAndMaxDates :: Account -> (Day, Day)
-- minAndMaxDates = cata algebra
--   where algebra (NodeF (Left _) dts) =
--           (minimum $ map fst dts, maximum $ map snd dts)
--         algebra (NodeF (Right RealAccount{aOpenDate = s, aCloseDate = c}) dts) =
--           let e = fromMaybe s $ c
--           in (minimum $ s : map fst dts, maximum $ e : map snd dts)
minAndMaxDates :: AccountMap -> (Day, Day)
minAndMaxDates x | M.null x = error "Account:minAndMaxDates empty map"
minAndMaxDates x =
  let dates = M.elems $ fmap (\info -> (aOpenDate info, fromMaybe
                                                        (aOpenDate info)
                                                        (aCloseDate info)))
                             x
  in (minimum $ map fst dates, maximum $ map fst dates)
  
-- flattenBalance :: Tree NodeA -> [(AccountInfo, Commodity, (Quantity, Quantity))]
-- flattenBalance = cata algebra
--   where
--       algebra (NodeF (Left _) ls) = concat ls
--       algebra (NodeF (Right a) ls) =
--         let lines1 :: [(AccountInfo, Commodity, (Quantity, Quantity))]
--             lines1 = map (\(c, amount) -> (a, c, amount)) $ M.toList (aBalance a)
--         in concat (lines1 : ls)

flattenBalance :: AccountMap ->  [(AccountInfo, Commodity, (Quantity, Quantity))]
flattenBalance x =
  let infoToLines a = map (\(c, amount) -> (a, c, amount)) $ M.toList $ aBalance a
  in concat $ M.elems $ fmap infoToLines x
  
-- A new balance where :
-- Credit = sum of all the credits
-- Debit = sum of all the debits
totalBalance :: AccountMap -> Balance
totalBalance = sumBalance . M.elems . fmap aBalance

-- totalBalance :: Tree NodeA -> Balance
-- totalBalance = cata algebra
--   where
--       algebra (NodeF (Left _) bs) = sumBalance bs
--       algebra (NodeF (Right a) bs) = sumBalance (aBalance a : bs)

-- A new balance where :
-- Credit = sum of all the accounts with negative total
-- Debit = sum of all the accounts with positive toal
totalNetBalance :: AccountMap -> Balance
totalNetBalance = sumBalance . M.elems . fmap (netBalance . aBalance)

-- totalNetBalance :: Tree NodeA -> Balance
-- totalNetBalance = cata algebra
--   where
--       algebra (NodeF (Left _) bs) = sumBalance bs
--       algebra (NodeF (Right a) bs) = sumBalance (netBalance (aBalance a) : bs)
