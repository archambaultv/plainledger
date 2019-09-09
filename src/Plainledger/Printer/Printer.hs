{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Printer.Printer
where

import Data.Csv
import Data.Tree
import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Plainledger.Data.Type
import Plainledger.Data.Ledger (lAccountSortedByType)

csvOptions :: EncodeOptions
csvOptions = defaultEncodeOptions {
      encUseCrLf = False
    }

printTrialBalance :: Ledger -> Bool -> B.ByteString
printTrialBalance l useDebitCredit =
  let accounts :: [(QualifiedName, AccountInfo)]
      accounts = lAccountSortedByType l 

      rawlines :: [(QualifiedName, AccountInfo, Commodity, Quantity)]
      rawlines = concatMap (\(name, info) -> map (\(comm, quant) -> (name, info, comm, quant)) (M.toList $ aBalance info))
                           accounts

      serializeAmount :: AccountInfo -> Quantity -> [T.Text]
      serializeAmount info q | useDebitCredit =
         if isDebit q (aType info)
         then [T.pack $ show q, ""]
         else ["", T.pack $ show $ negate q]
      serializeAmount _ q = [T.pack $ show q]
                            
      serialize :: (QualifiedName, AccountInfo, Commodity, Quantity) -> [T.Text]
      serialize (n, info, c, q) = qualifiedName2Text n :
                                  last n :
                                  (serializeAmount info q) ++
                                  [c, maybe "" (T.pack . show) (aNumber info)]

      amountTitle :: [T.Text]
      amountTitle = if useDebitCredit
                    then ["Debit", "Credit"]
                    else ["Amount"]

      total :: [[T.Text]]
      total =
        let infos = map snd accounts
        in if useDebitCredit
           then map (\(c,(d,cr)) -> ["", "Total",T.pack $ show d, T.pack $ show cr, c]) $
                    M.toList $ totalBalanceDebitCredit infos
           else map (\(c,s) -> ["", "Total",T.pack $ show s,c]) $
                    M.toList $ totalBalance infos

      csvlines ::  [[T.Text]]
      csvlines =  ["Trial Balance", "Start date", T.pack $ show $ lStartDate l,
                   "End date", T.pack $ show $ lEndDate l] :
                  [] :
                  ("Account Qualified Name" : "Account Name" : amountTitle ++ ["Commodity","Account Number"]) :
                  map serialize rawlines ++
                  ([] : total)
  in
    encodeWith csvOptions csvlines

printTransactions :: Ledger -> Bool -> B.ByteString
printTransactions l useDebitCredit =
  let transactions :: [(T.Text, Transaction)]
      transactions = identifiedTransactions l

      postings :: [(T.Text, Posting)]
      postings = concatMap (\(n, t) -> let ps = tPostings t in map (\p -> (n, p)) ps) transactions

      serializeAmount :: AccountInfo -> Quantity -> [T.Text]
      serializeAmount info q | useDebitCredit =
         if isDebit q (aType info)
         then [T.pack $ show q, ""]
         else ["", T.pack $ show $ negate q]
      serializeAmount _ q = [T.pack $ show q]
                            
      serialize :: (T.Text, Posting) -> [T.Text]
      serialize (ident, p) =
        let n = pAccount p
            q = aQuantity $ pAmount p
            c = aCommodity $ pAmount p
            t = pTransaction p
            acc = maybe (error ("Internal error : \"" ++
                                qualifiedName2String n ++
                                "\" is not in the AccountInfos map"))
                        id
                        (M.lookup n (lAccountInfos l))
            accountType = aType acc
            number = aNumber acc
            (desc, tags) = partition (\tag -> tagKey tag == "Description") (tTags t)
            descText :: T.Text
            descText = if null desc then "" else maybe "" id $ tagValue $ head desc
        in
          qualifiedName2Text n :
          last n :
          (T.pack $ show $ tDate t) :
          (serializeAmount acc q) ++
          [c,
           ident,
           maybe "" (T.pack . show) number,
           T.pack $ show accountType,
           descText]

      amountTitle :: [T.Text]
      amountTitle = if useDebitCredit
                    then ["Debit", "Credit"]
                    else ["Amount"]

      csvlines ::  [[T.Text]]
      csvlines =  ["Transactions", "Start date", T.pack $ show $ lStartDate l,
                   "End date", T.pack $ show $ lEndDate l] :
                  [] :
                  ("Account Qualified Name" : "Account Name" : "Date" : amountTitle ++
                   ["Commodity","Transaction Id", "Account Number","Account Type", "Description"]) :
                  map serialize postings
  in
    encodeWith csvOptions csvlines


printBalanceSheet :: Ledger -> B.ByteString
printBalanceSheet l =
  let accounts :: [AccountInfo]
      accounts = map snd $ M.toList $ lAccountInfos l

      (balanceAccounts, revenueExpense) = partition (\n -> aType n `elem` [Asset, Liability, Equity])
                                                    accounts

      -- Compute the earnings for the period
      earnings = totalBalance revenueExpense
      earningAccount = cEarningsAccount $ lConfiguration l
      balanceAccountsWithEarnings = let update a | aName a /= earningAccount = a
                                        update a = a{aBalance = totalBalance' [aBalance a, earnings]}
                                    in map update balanceAccounts

      accountForest :: Forest Account
      accountForest = filterEmptyAccounts $ toAccountTree balanceAccountsWithEarnings

      maxLevel :: Integer
      maxLevel = maximum $ map depth accountForest

      makeBuffer :: Integer -> [T.Text]
      makeBuffer 0 = []
      makeBuffer n = "" : makeBuffer (n - 1)

      serializeTree :: Integer -> Tree Account -> [[T.Text]]
      serializeTree level (Node a as) =
        let n = accName a
            b = accBalanceWithSubAccounts a

            amounts :: [[T.Text]]
            amounts = case a of
                        VirtualAccount _ _ _ _ -> []
                        RealAccount x _ -> map (\(c, q) -> [serializeAmount (accType a) q, c]) (M.toList $ aBalance x)

            totalAmounts :: [[T.Text]]
            totalAmounts = map (\(c, q) -> [serializeAmount (accType a) q, c]) (M.toList b)
            num = maybe "" (T.pack . show) (accNumber a)

            beforeName :: [T.Text]
            beforeName = makeBuffer level
            afterName = makeBuffer (maxLevel - level)

            children :: [[T.Text]]
            children = concatMap (serializeTree (level + 1)) as

            total :: [[T.Text]]
            total = if null as
                    then []
                    else map (\t -> beforeName ++  [T.append "Total - "  n] ++ afterName ++ t) totalAmounts

            accLines :: [[T.Text]]
            accLines = if null amounts
                       then [beforeName ++ [n]]
                       else map (\t -> beforeName ++ [n] ++ afterName ++ t) amounts
        in accLines ++
           children ++
           total

      serializeAmount :: AccountType -> Quantity -> T.Text
      serializeAmount t q =
         if isDebitAccount t
         then T.pack $ show q
         else T.pack $ show $ negate q
                            
      csvlines ::  [[T.Text]]
      csvlines =  ["Balance Sheet", "Start date", T.pack $ show $ lStartDate l,
                   "End date", T.pack $ show $ lEndDate l] :
                  [] :
                  -- ["Account Qualified Name", "Account Name", "Amount", "Commodity","Account Number"] :
                  concatMap (serializeTree 0) accountForest
  in
    encodeWith csvOptions csvlines

printIncomeStatement :: Ledger -> B.ByteString
printIncomeStatement l =
  let accounts :: [AccountInfo]
      accounts = map snd $ M.toList $ lAccountInfos l

      revenueExpense = filter (\n -> aType n `elem` [Revenue, Expense])
                                                    accounts

      -- Compute the earnings for the period
      earnings = totalBalance revenueExpense
      textEarnings :: [[T.Text]]
      textEarnings = map (\(c, q) -> "Net income" : makeBuffer maxLevel ++ [serializeAmount Revenue q, c])
                         (M.toList earnings)
     

      accountForest :: Forest Account
      accountForest = filterEmptyAccounts $ toAccountTree revenueExpense

      maxLevel :: Integer
      maxLevel = maximum $ map depth accountForest

      makeBuffer :: Integer -> [T.Text]
      makeBuffer 0 = []
      makeBuffer n = "" : makeBuffer (n - 1)

      serializeTree :: Integer -> Tree Account -> [[T.Text]]
      serializeTree level (Node a as) =
        let n = accName a
            b = accBalanceWithSubAccounts a

            amounts :: [[T.Text]]
            amounts = case a of
                        VirtualAccount _ _ _ _ -> []
                        RealAccount x _ -> map (\(c, q) -> [serializeAmount (accType a) q, c]) (M.toList $ aBalance x)

            totalAmounts :: [[T.Text]]
            totalAmounts = map (\(c, q) -> [serializeAmount (accType a) q, c]) (M.toList b)
            num = maybe "" (T.pack . show) (accNumber a)

            beforeName :: [T.Text]
            beforeName = makeBuffer level
            afterName = makeBuffer (maxLevel - level)

            children :: [[T.Text]]
            children = concatMap (serializeTree (level + 1)) as

            total :: [[T.Text]]
            total = if null as
                    then []
                    else map (\t -> beforeName ++  [T.append "Total - "  n] ++ afterName ++ t) totalAmounts

            accLines :: [[T.Text]]
            accLines = if null amounts
                       then [beforeName ++ [n]]
                       else map (\t -> beforeName ++ [n] ++ afterName ++ t) amounts
        in accLines ++
           children ++
           total

      serializeAmount :: AccountType -> Quantity -> T.Text
      serializeAmount t q =
         if isDebitAccount t
         then T.pack $ show q
         else T.pack $ show $ negate q
                            
      csvlines ::  [[T.Text]]
      csvlines =  ["Income Statement", "Start date", T.pack $ show $ lStartDate l,
                   "End date", T.pack $ show $ lEndDate l] :
                  [] :
                  -- ["Account Qualified Name", "Account Name", "Amount", "Commodity","Account Number"] :
                  concatMap (serializeTree 0) accountForest ++
                  [[]] ++
                  textEarnings
  in
    encodeWith csvOptions csvlines
