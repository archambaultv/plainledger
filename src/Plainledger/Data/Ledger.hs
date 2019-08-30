{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Data.Ledger (
  rawJournal2Ledger,
  adjustDate,
  lAccountSortedByType
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Algorithms.KMP as KMP
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Data.List
import Data.Maybe
import Control.Monad
import Text.Megaparsec (SourcePos(..), sourcePosPretty, mkPos)  
import Plainledger.Data.Type
import Plainledger.Error

-- Posting before tying the knot with transaction
data Posting' = Posting' QualifiedName Amount SourcePos

-- Returns the ledger that corresponds to the journal file.
-- The start and end date are the min and max date present in the journal file
rawJournal2Ledger :: RawJournal -> Either Error Ledger
rawJournal2Ledger rj =
  let openAccounts = sortBy (\x y -> compare (oaDate x) (oaDate y)) (rjOpenAccounts rj)
      closeAccounts = sortBy (\x y -> compare (caDate x) (caDate y)) (rjCloseAccounts rj)
      transactions = sortBy (\x y -> compare (rtDate x) (rtDate y)) (rjTransactions rj)
      balances = sortBy (\x y -> compare (rbaDate x) (rbaDate y)) (rjBalanceAssertions rj)
  in do
    -- A configuration is mandatory
    config <- maybe (Left "No configuration found in the journal file") return (rjConfiguration rj)

    -- The earliest date is when the first account was opened
    firstDate <- if null openAccounts
                 then (Left "No open account statement in the journal file")
                 else return $ oaDate $ head openAccounts
    let initialEndDate = firstDate
    let initialLedger = Ledger firstDate initialEndDate config [] M.empty

    -- We start by opening all the accounts
    l1 <- foldM addAccount initialLedger openAccounts

    -- We update the accounts with the close date (if provided)
    l2 <- foldM updateCloseDate l1 closeAccounts

    -- We insert the transactions
    l3 <- foldM addTransaction l2 transactions
    let l3' = l3{lTransactions = sortBy (\x y -> compare (tDate x) (tDate y)) (lTransactions l3)}

    -- Compute the accounts balance and check balance assertions
    l4 <- computeCheckBalance l3' balances

    return l4

computeCheckBalance :: Ledger -> [RawBalanceAssertion] -> Either Error Ledger
computeCheckBalance l bas = do
  let ts = lTransactions l
  bas' <- mapM fromRaw bas 
  checkBalance l ts bas'

  where fromRaw :: RawBalanceAssertion -> Either Error BalanceAssertion
        fromRaw (RawBalanceAssertion d name (RawQuantity c q) sourcePos) = do
          fullName <- findFullQualifiedName sourcePos (M.keys $ lAccountInfos l) name
          commodity <- maybe (return $ aDefaultCommodity $ lAccountInfos l M.! fullName) return c
          return (BalanceAssertion d fullName (Amount commodity q) sourcePos)

checkBalance :: Ledger -> [Transaction] -> [BalanceAssertion] -> Either Error Ledger
checkBalance l tss bas =
  case (tss, bas) of
    ([],_) -> mapM (check l) bas >> return l
    (ts, []) -> return $ foldl' update l ts
    (t:ts,b:_) | tDate t <= baDate b -> checkBalance (update l t) ts bas
    _ -> check l (head bas) >> checkBalance l tss (tail bas)

  where check :: Ledger -> BalanceAssertion -> Either Error ()
        check l2 (BalanceAssertion _ name (Amount c q) sourcePos) = do
          let s = (aBalance $ lAccountInfos l2 M.! name) M.! c
          if s /= q
           then Left $ sourcePosPretty sourcePos ++
                 " Balance assertion failed. The computed balance is " ++ show s ++ " while the assertion is " ++ show q
           else return ()

        update :: Ledger -> Transaction -> Ledger
        update l2 t =
          let postings = tPostings t
          in foldl' updateBalance l2 postings

        updateBalance :: Ledger -> Posting -> Ledger
        updateBalance l2 (Posting name (Amount comm quantity) _ _) =
          l2{lAccountInfos = M.adjust (\a -> a{aBalance = M.insertWith (+) comm quantity (aBalance a)})
                            name
                            (lAccountInfos l2)}

   
findAccountType :: Ledger -> OpenAccount -> Either Error AccountType
findAccountType l oa = maybe err return (M.lookup name mapping)
  where
    name = head $ oaName oa
    mapping = cAccountTypeMapping (lConfiguration l)
    err =  Left $ sourcePosPretty (oaSourcePos oa) ++ " " ++ T.unpack name ++ " does not have an account type in the configuration"
    
addAccount :: Ledger -> OpenAccount -> Either Error Ledger
addAccount l oa@(OpenAccount day name tags number commodity _) =
  let config = lConfiguration l
      defaultCommodity = cDefaultCommodity config
      c = if null commodity then [defaultCommodity] else commodity
      accountInfos = lAccountInfos l
  in do
    _ <- if M.member name accountInfos then dupErr  else return ()
    accountType <- findAccountType l oa
    let a = AccountInfo day
                        Nothing
                        name
                        tags
                        accountType
                        number
                        (S.fromList c)
                        (head c)
                        (M.fromList (zip c [0..]))
    return l{lAccountInfos = M.insert name a accountInfos,
             lEndDate = max (lEndDate l) day}

  where dupErr = Left $ sourcePosPretty (oaSourcePos oa) ++ " Second open declaration for account " ++ (intercalate ":" $ map T.unpack name)
  
findFullQualifiedName :: SourcePos -> [QualifiedName] -> QualifiedName -> Either Error QualifiedName
findFullQualifiedName pos fullNames name =
     let fullNamesStr = map (map T.unpack) fullNames
         nameStr = map T.unpack name
         kmp = map (KMP.match $ KMP.build nameStr) fullNamesStr
         m = zip fullNames kmp
         matched = filter (not . null . snd) m
         tieBreaker :: [QualifiedName] -> Either Error QualifiedName
         tieBreaker ns =
           case filter (\n -> name `isSuffixOf` n) ns of
             [x] -> return x
             _ -> Left $ sourcePosPretty pos ++
                  " Account \"" ++ (intercalate ":" nameStr) ++ "\" is ambiguous.\n It can refer to \n  "
                  ++ (let accs = map (intercalate ":" . map T.unpack) ns
                       in intercalate " or\n  " accs)

     in case map fst matched of
          [] -> Left $ sourcePosPretty pos ++
                " Account \"" ++ (intercalate ":" nameStr) ++ "\" has not been opened"

          [name'] -> return $ name'

          matched' -> tieBreaker matched'
       
       
updateCloseDate :: Ledger -> CloseAccount -> Either Error Ledger
updateCloseDate l (CloseAccount day name sourcePos) =
  let accountInfos = lAccountInfos l
  in do
    fullName <- findFullQualifiedName sourcePos (M.keys $ accountInfos) name
    let openDate = aOpenDate $ accountInfos M.! fullName
    _ <- if openDate  > day
         then Left $ sourcePosPretty sourcePos ++ " closing account date (" ++
              show day ++ ") is before the opening account date ("
              ++ show openDate ++ ")"
         else return ()
    return l{lAccountInfos = M.updateWithKey (\_ a -> Just a{aCloseDate = Just day}) fullName accountInfos,
             lEndDate = max (lEndDate l) day}

addTransaction :: Ledger -> RawTransaction -> Either Error Ledger
addTransaction l (RawTransaction day tags rawPostings sourcePos) = do
  rawPostings1 <- mapM updateAccountName rawPostings
  _ <- checkBetweenOpenCloseDate rawPostings1
  let rawPostings2 = map fillMonoCommodity rawPostings1
  rawPostings3 <- fillCommodity rawPostings2
  rawPostings4 <- balancePostings rawPostings3
  t <- return (let p = map (\(Posting' name amount pos) -> Posting name amount t pos) rawPostings4
                   t = Transaction day tags p sourcePos
               in t)
  return $ newTransaction l t
  

  where
   checkBetweenOpenCloseDate :: [RawPosting] -> Either Error ()
   checkBetweenOpenCloseDate rps =
     let accountInfos = lAccountInfos l
         accounts = map (\r -> accountInfos M.! (rpAccount r)) rps
         openDate = filter (\(_,a) -> aOpenDate a > day) $ zip rps accounts
         closeDate = filter (\(_,a) -> maybe False (\d -> d < day) (aCloseDate a)) $ zip rps accounts
     in case (openDate, closeDate) of
         ([],[]) -> return ()
         (((rp,a):_),_) -> Left $ sourcePosPretty (rpSourcePos rp) ++
                            " Posting to the account "++ (qualifiedName2String $ rpAccount rp) ++
                            " before the account was opened (opened on :" ++ (show $ aOpenDate a) ++ ")"
         (_, ((rp,a):_)) -> Left $ sourcePosPretty (rpSourcePos rp) ++
                            " Posting to the account " ++ (qualifiedName2String $ rpAccount rp) ++
                            " after the account was closed (closed on :" ++ (show $ aCloseDate a) ++ ")"
    
   newTransaction :: Ledger -> Transaction -> Ledger
   newTransaction ledger t = ledger{lTransactions = t : lTransactions ledger, lEndDate = max (lEndDate ledger) (tDate t)}


   updateAccountName :: RawPosting -> Either Error RawPosting
   updateAccountName (RawPosting name a s) = do
     fullName <- findFullQualifiedName sourcePos (M.keys $ lAccountInfos l) name
     return $ RawPosting fullName a s

   fillMonoCommodity :: RawPosting -> RawPosting
   fillMonoCommodity r@(RawPosting name (RawAmount Nothing q) s) =
     let accountInfo = lAccountInfos l M.! name
         commodities = aAllowedCommodities accountInfo
     in if S.size commodities == 1
        then RawPosting name (RawAmount (Just $ aDefaultCommodity accountInfo) q) s
        else r
   fillMonoCommodity r = r

   -- -- Group posting by commodity
   -- -- If commodity is Nothing it is matched with the "" currency
   groupPostings :: [RawPosting] -> [[RawPosting]]
   groupPostings rps =
     let comm = map (\r -> maybe "" id $ raCommodity $ rpAmount r) rps
         commPosting = zip comm $ map (\x -> [x]) rps
     in map snd $ M.toList $ M.fromListWith (++) commPosting

   fillCommodity :: [RawPosting] -> Either Error [RawPosting]
   fillCommodity rp =
     let knownCommodities = S.fromList $ filter (not . T.null) $ map (maybe "" id . raCommodity . rpAmount) rp
         foo (RawPosting name (RawAmount Nothing q) s) =
            let accountInfo = lAccountInfos l M.! name
                postingCommodities = aAllowedCommodities accountInfo
                candidates = S.intersection knownCommodities postingCommodities
            in if S.size candidates == 1
               then return $ RawPosting name (RawAmount (Just (S.elemAt 0 candidates)) q) s
               else Left $ sourcePosPretty s ++
                         " Unable to infer commodity for this posting. Possible candidates are :" ++ intercalate ", " (map T.unpack $ S.toList candidates)
         foo r = return r
     in mapM foo rp
                               
   balancePostings :: [RawPosting] -> Either Error [Posting']
   balancePostings rp =
       let f xs = let (noQuantity, withQuantity) = partition (maybe True (const False) . raQuantity . rpAmount) xs
                      s = sum (map (\r -> maybe 0 id (raQuantity $ rpAmount r)) withQuantity)
                      curr = if null withQuantity then "" else fromJust $ raCommodity $ rpAmount $ head withQuantity
                  in case noQuantity of
                        [] -> if s == 0
                              then return xs
                              else Left $ sourcePosPretty sourcePos ++
                             " Transaction does not balance for commodity " ++ T.unpack curr
                        [x] -> let negS = negate s
                               in return $ x{rpAmount = (rpAmount x){raQuantity = Just negS}} : withQuantity
                        _ -> Left $ sourcePosPretty sourcePos ++
                             "More than one postings with no specified quantity for the commodity " ++ T.unpack curr
           grp = groupPostings rp
           convert (RawPosting name (RawAmount (Just c) (Just q)) s) = Posting' name (Amount c q) s
           convert _ = error "RawAmount of RawPosting has some unfilled fields"
       in (map convert . concat) <$> mapM f grp

computeBalance :: Ledger -> Ledger
computeBalance l =
  let l1 = l{lAccountInfos = M.fromList $ map (\(n, i) -> (n, i{aBalance = M.empty})) $ M.toList $ lAccountInfos l}
  in foldl updateBalance l1 (concatMap tPostings (lTransactions l))

  where updateBalance :: Ledger -> Posting -> Ledger
        updateBalance l2 (Posting name (Amount comm quantity) _ _) =
          l2{lAccountInfos = M.adjust (\a -> a{aBalance = M.insertWith (+) comm quantity (aBalance a)})
                            name
                            (lAccountInfos l2)}

computeOpeningBalance :: M.Map QualifiedName AccountType -> [Transaction] -> Day -> QualifiedName -> [Transaction]
computeOpeningBalance typeMap ts day openingBalanceAccount =
  let b = foldl updateBalance M.empty (filter notRevenueExpense $ concatMap tPostings ts)
      balanceList = M.toList (fmap M.toList b)
  in map buildTransaction balanceList

  where updateBalance :: M.Map QualifiedName Balance -> Posting -> M.Map QualifiedName Balance
        updateBalance b (Posting name (Amount comm quantity) _ _) =
          let nameF Nothing = Just $ M.fromList [(comm, quantity)]
              nameF (Just m) = Just $ M.alter commF comm m

              commF Nothing = Just quantity
              commF (Just q) = Just $ q + quantity
          -- in
          in M.alter nameF name b
           -- M.insertWith (\_ m -> M.insertWith (+) comm quantity m) name M.empty b
          
        buildTransaction :: (QualifiedName, [(Commodity, Quantity)]) -> Transaction
        buildTransaction (name, quantities) =
          let nullPos = SourcePos "" (mkPos 1) (mkPos 1)
              postings = map (\(c, q) -> Posting name (Amount c q) t nullPos) quantities
              postingBalance = map (\(c, q) -> Posting openingBalanceAccount (Amount c (-q)) t nullPos) quantities
              t = Transaction day [Tag "Virtual transaction" Nothing nullPos] (postings ++ postingBalance) nullPos
          in t

        notRevenueExpense :: Posting -> Bool
        notRevenueExpense Posting{pAccount = n} = (typeMap M.! n) `elem` [Asset, Liability, Equity]

adjustDate :: Ledger -> Maybe Day -> Maybe Day -> Ledger
adjustDate l Nothing endDate = adjustDate l (Just (lStartDate l)) endDate
adjustDate l startDate Nothing = adjustDate l startDate (Just (lEndDate l))
adjustDate l (Just s) (Just e) = adjustDate' l s e 

adjustDate' :: Ledger -> Day -> Day -> Ledger
adjustDate' l newStartDate newEndDate | lStartDate l >= newStartDate && lEndDate l <= newEndDate = l
adjustDate' l newStartDate newEndDate =
  -- Delete all the accounts closed before the new startDate
  -- Delete all the accounts opened after the new endDate
  -- Updates other relevant fields accordingly
  let accountsInfo = M.toList (lAccountInfos l)
      keepAccount (_, info) =
        let o = aOpenDate info
            c = aCloseDate info
        in o <= newEndDate  &&
           maybe True (\c' -> c' >= newStartDate) c
      keptAccounts = map (\(n, i) -> (n, i{aBalance = M.empty})) $ filter keepAccount accountsInfo
      keptAccountsInfo = M.fromList keptAccounts

  -- Compute the accounts balance to the day before the new date
  -- and add a transaction at the startdate posting to the opening
  -- balance for each accounts
      transactions = (lTransactions l)
      transactionsBeforeStart = filter (\t -> tDate t < newStartDate) transactions
      initialTransactions = computeOpeningBalance
                            (fmap (\info -> aType info) (lAccountInfos l))
                            transactionsBeforeStart
                            newStartDate
                            (cOpeningBalanceAccount $ lConfiguration l)
                            
  -- Delete all transactions done before the new startDate and after the
  -- new endate
      keptTransactions = filter (\t -> tDate t >= newStartDate && tDate t <= newEndDate) transactions

      l2 = Ledger newStartDate newEndDate (lConfiguration l) (initialTransactions ++ keptTransactions) keptAccountsInfo

  in computeBalance l2

-- Sorted by AccountType
lAccountSortedByType :: Ledger -> [(QualifiedName, AccountInfo)]
lAccountSortedByType l = sortBy (\(n1, a1) (n2, a2) -> compare (aType a1, n1) (aType a2, n2)) (M.toList $ lAccountInfos l)
