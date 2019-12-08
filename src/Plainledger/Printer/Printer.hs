{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Plainledger.Printer.Printer
where


import Data.Csv
import qualified Data.List.NonEmpty as NE
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


serializeAmount :: AccountingFormat -> AccountType -> (Quantity, Quantity) -> [T.Text]
serializeAmount t accType (dr, cr) =
  case t of
    TwoColumnsDebitCredit ->
      let total = dr - cr
      in if total > 0
         then [T.pack $ show total, ""]
         else if total < 0
              then ["", T.pack $ show $ negate total]
              else ["",""]
    OneColumnSignedNumber SignDependsOnNetBalance -> [T.pack $ show (dr - cr)]
    OneColumnSignedNumber SignDependsOnAccountType ->
      if isCreditAccountType accType
      then [T.pack $ show $ negate (dr - cr)]
      else [T.pack $ show (dr - cr)]

serializeNumber :: AccountingFormat -> AccountType -> Quantity -> [T.Text]
serializeNumber t accType a =
  if a < 0
  then serializeAmount t accType (0, negate a)
  else serializeAmount t accType (a, 0)

       
amountTitle :: AccountingFormat -> [T.Text]
amountTitle x = case x of
  TwoColumnsDebitCredit -> ["Debit", "Credit"]
  OneColumnSignedNumber _ -> ["Amount"]
        
printTrialBalance :: Maybe Day -> Maybe Day -> Ledger -> AccountingFormat -> B.ByteString
printTrialBalance start end l accountingType =
  let -- root = mapToTree $ lAccounts l
      (sDate, eDate) = printDate start end l
      accTypes = cAccountTypeMapping $ lConfiguration l
      
      -- Header lines
      title :: [[T.Text]]
      title = ["Trial Balance", "Start date", sDate,
                   "End date", eDate] :
               [] :
               ("Account Qualified Name" : "Account Name" : amountTitle accountingType ++ ["Commodity","Account Number"]) :
               []

      -- Account data to print
      accData :: [(AccountInfo, Commodity, (Quantity, Quantity))]
      accData = flattenBalance $ lAccounts l
      
      -- How to print the data
      serialize :: (AccountInfo, Commodity, (Quantity, Quantity)) -> [T.Text]
      serialize (info, c, x) =
        let name = NE.last (aQName info)
            accType = accTypes M.! (NE.head $ aQName info)
        in qualifiedNameToText (aQName info) :
           name :
           (serializeAmount accountingType accType x) ++
           [c, maybe "" (T.pack . show) (aNumber info)]

                            
      -- Total lines
      total :: [[T.Text]]
      total = map (\(c, (dr, cr)) -> ["", "Total"] ++ [T.pack $ show dr, T.pack $ show cr] ++ [c]) $
              M.toList $ totalNetBalance $ lAccounts l

      csvlines ::  [[T.Text]]
      csvlines =  title ++
                  map serialize accData ++
                  ([] : total)
  in
    encodeWith csvOptions csvlines

printTransactions :: Maybe Day -> Maybe Day -> Ledger -> AccountingFormat -> B.ByteString
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
               ["Commodity","Transaction Id", "Account Type", "Account Number"] ++
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
            accountType = accTypes M.! (NE.head n)
            number = aNumber acc
        in
          qualifiedNameToText n :
          NE.last n :
          (T.pack $ show $ date) :
          serializeNumber accountingType accountType q ++
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
splitAccounts :: M.Map AccountName AccountType -> AccountMap -> (AccountMap, AccountMap)
splitAccounts accTypes m = M.partition (isBalanceSheetAccountType . (accTypes M.!) . NE.head . aQName) m

serializeAccounts :: M.Map AccountName AccountType ->  AccountMap  -> [[T.Text]]
serializeAccounts accTypeMap = snd . cata algebra .  mapToTree
        where algebra :: Algebra (TreeF (Either [AccountName] AccountInfo)) (Balance, [[T.Text]])
              -- Case for empty balances
              algebra (NodeF (Left _) children)
                | all M.null (map fst children) = (M.empty, [])
              algebra (NodeF (Right info) children)
                | M.null (aBalance info) && all M.null (map fst children) = (M.empty, [])

              -- Case for root
              algebra (NodeF (Left []) children) =
                (sumBalance $ map fst children,
                 concatMap snd children)

              -- General case 
              algebra (NodeF info children) =
                let n = either last (NE.last . aQName) info
                    b = either (const M.empty) aBalance info

                    accType = accTypeMap M.! either head (NE.head . aQName) info
                    
                    balance = sumBalance (b : map fst children)

                    accLines' :: [[T.Text]]
                    accLines' =  map (\(c, x) -> n :
                                                 serializeAmount (OneColumnSignedNumber SignDependsOnAccountType) accType x ++
                                                 [c])
                                 (M.toList b)

                    accLines = if null accLines' then [[n]] else accLines'
 
                    total :: [[T.Text]]
                    total = map (\(c, x) -> T.append "Total - "  n :
                                  serializeAmount (OneColumnSignedNumber SignDependsOnAccountType) accType x  ++
                                  [c])
                            (M.toList balance)

                in (balance, accLines ++
                             shiftCellsToTheRight (concatMap snd children) ++
                             if null children then [] else total)

              shiftCellsToTheRight :: [[T.Text]] -> [[T.Text]]
              shiftCellsToTheRight = map ("" :)

      
printBalanceSheet :: Maybe Day -> Maybe Day -> Ledger -> B.ByteString
printBalanceSheet start end l =
  -- 2 Colonnes, un colonne montant (gauche) et un autre total (droite). Avoir une
  -- profondeur p (en partant de la racine).

  -- Si il y a des enfants, dans la colonne de droite. Si il n'y a pas
  -- d'enfants dans le colonne de gauche.

  -- Pour tous les noeuds de profondeur plus petites, on décale d'une
  -- colonne les noeuds de profondeurs plus grandes. On fait un titre
  -- en haut et décale d'une ligne et un total en bas

  -- Pour les noeuds de profondeur plus grand, on ne décale pas. On
  -- fait un total à la fin des enfants mais pas de titre en haut. On
  -- décale d'un ligne au lieu d'un titre
  
  let --root = mapToTree (lAccounts l)
      accTypes = cAccountTypeMapping $ lConfiguration l
      (sDate, eDate) = printDate start end l
  
      -- Header
      title :: [[T.Text]]
      title = [["Balance Sheet"],
               ["Start date", sDate],
               ["End date", eDate]]
      
      -- Compute the earnings
      (balanceAccounts', incomeAccounts) = splitAccounts accTypes $ lAccounts l
      earnings = totalBalance incomeAccounts
      earningAccount = cEarningsAccount $ lConfiguration l
      balanceAccounts = M.adjust
                        (\info -> info{aBalance = mergeBalance (aBalance info) earnings})
                        earningAccount
                        balanceAccounts'

                             
      csvlines ::  [[T.Text]]
      csvlines = title ++ [[]] ++ serializeAccounts accTypes balanceAccounts
  in
    encodeWith csvOptions csvlines

printIncomeStatement :: Maybe Day -> Maybe Day -> Ledger -> B.ByteString
printIncomeStatement start end l =
  let --root = mapToTree (lAccounts l)
      accTypes = cAccountTypeMapping $ lConfiguration l
      (sDate, eDate) = printDate start end l
  
      -- Header
      title :: [T.Text]
      title = ["Income Statement", "Start date", sDate,
                "End date", eDate]
      
      -- Compute the earnings for the period
      (_, incomeAccounts) = splitAccounts accTypes $ lAccounts l
                        
      csvlines ::  [[T.Text]]
      csvlines = title : [] : serializeAccounts accTypes incomeAccounts
  in
    encodeWith csvOptions csvlines
