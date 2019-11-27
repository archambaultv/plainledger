{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Printer.Printer
where

import Data.Csv
import Data.Tree
import Data.List
import Data.Time
import Data.Maybe
import Data.Functor.Foldable
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Plainledger.Data.Type
import Plainledger.Data.Balance
import Plainledger.Data.Account
import Plainledger.Data.Transaction
import Plainledger.Data.QualifiedName

csvOptions :: EncodeOptions
csvOptions = defaultEncodeOptions {
      encUseCrLf = False
    }

printDate :: Maybe Day -> Maybe Day -> Ledger -> (T.Text, T.Text)
printDate s e l =
  let minMax = minMaxDates (lTransactions l)
      min1 = maybe "unspecified" show (fst <$> minMax)
      max1 = maybe "unspecified" show (snd <$> minMax)
      start = maybe min1 show s
      end = maybe max1 show e
  in (T.pack start, T.pack end)


serializeAmount :: AccountingType -> (Quantity, Quantity) -> [T.Text]
serializeAmount t (dr, cr) =
  case t of
    DebitCredit -> [T.pack $ show dr, T.pack $ show dr]
    PlusMinus -> [T.pack $ show (dr - cr)]

serializePlusMinus :: AccountingType -> Quantity -> [T.Text]
serializePlusMinus t a =
  if a < 0
  then serializeAmount t (0, negate a)
  else serializeAmount t (a, 0)
    
amountTitle :: AccountingType -> [T.Text]
amountTitle x = case x of
                  DebitCredit -> ["Debit", "Credit"]
                  PlusMinus -> ["Amount"]

printTrialBalance :: Maybe Day -> Maybe Day -> Ledger -> AccountingType -> B.ByteString
printTrialBalance start end l accountingType =
  let root = mapToTree $ lAccounts l
      (sDate, eDate) = printDate start end l

      -- Header lines
      title :: [[T.Text]]
      title = ["Trial Balance", "Start date", sDate,
                   "End date", eDate] :
               [] :
               ("Account Qualified Name" : "Account Name" : amountTitle accountingType ++ ["Commodity","Account Number"]) :
               []

      -- Account data to print
      accData :: [(AccountInfo, Commodity, (Quantity, Quantity))]
      accData = flattenBalance root
      
      -- How to print the data
      serialize :: (AccountInfo, Commodity, (Quantity, Quantity)) -> [T.Text]
      serialize (info, c, x) = qualifiedNameToText (aQName info) :
                               last (aQName info) :
                               (serializeAmount accountingType x) ++
                               [c, maybe "" (T.pack . show) (aNumber info)]

                            
      -- Total lines
      total :: [[T.Text]]
      total = map (\(c, x) -> ["", "Total"] ++ serializeAmount accountingType x ++ [c]) $
              M.toList $ totalBalance root

      csvlines ::  [[T.Text]]
      csvlines =  title ++
                  map serialize accData ++
                  ([] : total)
  in
    encodeWith csvOptions csvlines

printTransactions :: Maybe Day -> Maybe Day -> Ledger -> AccountingType -> B.ByteString
printTransactions start end l accountingType =
  let transactions :: [(T.Text, Transaction)]
      transactions = identifiedTransactions l
      (sDate, eDate) = printDate start end l

      accInfos = lAccounts l
      accTypes = cAccountTypeMapping $ lConfiguration l

      -- Header
      tagKeys :: [T.Text]
      tagKeys = tagsKeys (map snd transactions)

      title :: [[T.Text]]
      title = ["Transactions", "Start date", sDate,
                "End date", eDate] :
              [] :
              (["Account Qualified Name", "Account Name", "Date"] ++
               amountTitle accountingType ++
               ["Commodity","Transaction Id", "Account Type", "Account Number", "Transaction GUID"] ++
               tagKeys) : []

      -- Transactions
      postings :: [(Day, T.Text, Posting, [Tag])]
      postings = concatMap (\(n, t) -> map (tDate t, n,,tTags t) $ tPostings t)
                 transactions
                            
      serialize :: (Day, T.Text, Posting, [Tag]) -> [T.Text]
      serialize (date, ident, p, tags) =
        let n = pAccount p
            q = pQuantity p
            c = pCommodity p
            acc = accInfos M.! n
            accountType = accTypes M.! (head $ n)
            number = aNumber acc
        in
          qualifiedNameToText n :
          last n :
          (T.pack $ show $ date) :
          serializePlusMinus accountingType q ++
          [c,
           ident,
           T.pack $ show accountType,
           maybe "" (T.pack . show) number
           ] ++
          serializeTags tags

      serializeTags :: [Tag] -> [T.Text]
      serializeTags ts =
        let m = M.fromList $ map (\t -> (tagKeyword t, fromMaybe "TRUE" $ tagValue t)) ts
        in map (\t -> M.findWithDefault "" t m) tagKeys


      csvlines ::  [[T.Text]]
      csvlines =  title ++
                  map serialize postings
  in
    encodeWith csvOptions csvlines

-- Split accounts between Asset, Liability and Equity
-- and Revenue Expense
splitAccounts :: M.Map AccountName AccountType -> Account -> (Account, Account)
splitAccounts accTypes (Node VirtualAccount{aQName = []} as) =
  let (b, i) = partition part as
  in (Node VirtualAccount{aQName = []} b, Node VirtualAccount{aQName = []} i)

  where part (Node info _) =
           let accType = accTypes M.! (head $ aQName info)
           in accType `elem` [Asset, Liability, Equity]
splitAccounts _ _ = error "Invalid splitAccounts call"

serializeAccounts ::  Account  -> [[T.Text]]
serializeAccounts = snd . cata algebra
        where algebra :: Algebra (TreeF AccountInfo) (Balance, [[T.Text]])
              algebra (NodeF VirtualAccount{} []) = (M.empty, [])
              algebra (NodeF info children)
                | M.null (aBalance info) && all M.null (map fst children) = (M.empty, [])
              algebra (NodeF info children) =
                let n = qualifiedNameToText $ aQName info
                    b = if isRealAccount info
                        then aBalance info
                        else M.empty
                    
                    balance = sumBalance (b : map fst children)

                    accLines' :: [[T.Text]]
                    accLines' =  map (\(c, x) -> n : serializeAmount PlusMinus x ++ [c])
                                 (M.toList b)

                    accLines = if null accLines' then [[n]] else accLines'
 
                    total :: [[T.Text]]
                    total = map (\(c, x) -> T.append "Total - "  n :
                                  serializeAmount PlusMinus x  ++
                                  [c])
                            (M.toList balance)

                    shiftCellsToTheRight :: [[T.Text]] -> [[T.Text]]
                    shiftCellsToTheRight = map ("" :)

                in (balance, accLines ++
                             shiftCellsToTheRight (concatMap snd children) ++
                             total)

      
printBalanceSheet :: Maybe Day -> Maybe Day -> Ledger -> B.ByteString
printBalanceSheet start end l =
  let root = mapToTree (lAccounts l)
      accTypes = cAccountTypeMapping $ lConfiguration l
      (sDate, eDate) = printDate start end l
  
      -- Header
      title :: [T.Text]
      title = ["Balance Sheet", "Start date", sDate,
                "End date", eDate]
      
      -- Compute the earnings
      (balanceAccounts', incomeAccounts) = splitAccounts accTypes root
      earnings = totalBalance incomeAccounts
      earningAccount = cEarningsAccount $ lConfiguration l
      balanceAccounts = updateAccount
                        (\info -> info{aBalance = sumBalance [aBalance info, earnings]})
                        earningAccount
                        balanceAccounts'

                             
      csvlines ::  [[T.Text]]
      csvlines = title : [] : serializeAccounts balanceAccounts
  in
    encodeWith csvOptions csvlines

printIncomeStatement :: Maybe Day -> Maybe Day -> Ledger -> B.ByteString
printIncomeStatement start end l =
  let root = mapToTree (lAccounts l)
      accTypes = cAccountTypeMapping $ lConfiguration l
      (sDate, eDate) = printDate start end l
  
      -- Header
      title :: [T.Text]
      title = ["Income Statement", "Start date", sDate,
                "End date", eDate]
      
      -- Compute the earnings for the period
      (_, incomeAccounts) = splitAccounts accTypes root
                        
      csvlines ::  [[T.Text]]
      csvlines = title : [] : serializeAccounts incomeAccounts
  in
    encodeWith csvOptions csvlines
