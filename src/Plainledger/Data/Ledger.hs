{-# LANGUAGE OverloadedStrings #-}

module Plainledger.Data.Ledger
where

import qualified Data.Map.Strict as M
import qualified Data.Algorithms.KMP as KMP
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time
import Data.List
import Data.Maybe
import Control.Monad
import Text.Megaparsec (SourcePos, sourcePosPretty)  
import Plainledger.Data.Type
import Plainledger.Error

-- Posting before tying the knot with transaction
data Posting' = Posting' {
  paccount :: QualifiedName,
  pamount :: Amount,
  psourcepos :: SourcePos
  }

-- Returns the ledger that corresponds to the journal file.
-- The from and to date are the min and max date present in the journal file
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
    fromDate <- if null openAccounts
                then (Left "No open account statement in the journal file")
                else return $ oaDate $ head openAccounts
    let initialToDate = fromDate
    let initialLedger = Ledger fromDate initialToDate config [] S.empty [] M.empty M.empty

    -- We start by opening all the accounts
    l1 <- foldM addAccount initialLedger openAccounts

    -- We update the accounts with the close date (if provided)
    l2 <- foldM updateCloseDate l1 closeAccounts

    -- We insert the transactions, verify the balance assertions
    -- and perform multiple integrity tests
    (l3, bs) <- foldM addTransaction (l2, balances) transactions
    -- Check remaining balance assertion
    checkBalance l3 bs
    let d = if null bs then (lToDate l3) else last $ sort $ map rbaDate bs

    return l3{lAccountNamesList = sort (lAccountNamesList l3), lTransactions = sortBy (\x y -> compare (tDate x) (tDate y)) (lTransactions l3),
              lToDate = max (lToDate l3) d}

checkBalance :: Ledger -> [RawBalanceAssertion] -> Either Error ()
checkBalance _ [] = return ()
checkBalance l ((RawBalanceAssertion _ name (RawQuantity c q) sourcePos) : bas) = do
     fullName <- findFullQualifiedName sourcePos (lAccountNamesList l) name
     commodity <- maybe (return $ aDefaultCommodity $ lAccountInfos l M.! fullName) return c
     let s = lBalance l M.! fullName M.! commodity
     if s  /= q
     then Left $ sourcePosPretty sourcePos ++
                 " Balance assertion failed. The computed balance is " ++ show s ++ " while the assertion is " ++ show q
     else checkBalance l bas

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
      accountNamesList = lAccountNamesList l
      accountNamesSet = lAccountNamesSet l
  in do
    _ <- if S.member name accountNamesSet then return () else dupErr 
    accountType <- findAccountType l oa
    let a = AccountInfo day Nothing name tags accountType number (S.fromList c) (head c)
    return l{lAccountInfos = M.insert name a accountInfos,
             lAccountNamesSet = S.insert name accountNamesSet,
             lAccountNamesList = name : accountNamesList,
             lToDate = max (lToDate l) day,
             lBalance = M.insert name (M.fromList (zip c [0..])) (lBalance l)}

  where dupErr = Left $ sourcePosPretty (oaSourcePos oa) ++ " Second open declaration for account " ++ (intercalate ":" $ map T.unpack name)

findFullQualifiedName :: SourcePos -> [QualifiedName] -> QualifiedName -> Either Error QualifiedName
findFullQualifiedName pos fullNames name =
     let fullNamesStr = map (map T.unpack) fullNames
         nameStr = map T.unpack name
         kmp = map (KMP.match $ KMP.build nameStr) fullNamesStr
         m = zip fullNames kmp
         matched = filter (not . null . snd) m
     in case map fst matched of
          [] -> Left $ sourcePosPretty pos ++
                "Account " ++ (intercalate ":" nameStr) ++ " has not been opened"

          [name'] -> return $ name'

          matched' -> Left $ sourcePosPretty pos ++
                     "Account " ++ (intercalate ":" nameStr) ++ " is ambiguous. It can refer to \n  "
                     ++ (let accs = map (intercalate ":" . map T.unpack) matched'
                         in intercalate " or\n  " accs)

updateCloseDate :: Ledger -> CloseAccount -> Either Error Ledger
updateCloseDate l (CloseAccount day name sourcePos) =
  let accountInfos = lAccountInfos l
      accountNames = lAccountNamesList l
  in do
    fullName <- findFullQualifiedName sourcePos accountNames name
    return l{lAccountInfos = M.updateWithKey (\_ a -> Just a{aCloseDate = Just day}) fullName accountInfos,
             lToDate = max (lToDate l) day}

addTransaction :: (Ledger, [RawBalanceAssertion]) -> RawTransaction -> Either Error (Ledger, [RawBalanceAssertion])
addTransaction (l, bas) (RawTransaction day tags rawPostings sourcePos) = do
  bas' <- checkBalanceUntil day l bas
  postings' <- (mapM updateAccountName rawPostings) >>=
               (\x -> checkOpenCloseDate x >> return x) >>=
               return . map fillMonoCommodity >>=
               fillCommodity >>=
               balancePostings
  t <- return (let p = map (\(Posting' name amount pos) -> Posting name amount t pos) postings'
                   t = Transaction day tags p sourcePos
               in t)
  return (newTransaction l t, bas')
  

  where
   checkOpenCloseDate :: [RawPosting] -> Either Error ()
   checkOpenCloseDate rps =
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
    
   checkBalanceUntil :: Day -> Ledger -> [RawBalanceAssertion] -> Either Error [RawBalanceAssertion]
   checkBalanceUntil _ _ [] = return []
   checkBalanceUntil d _ ((RawBalanceAssertion day1 _ _ _) : _) | d <= day1 = return bas
   checkBalanceUntil d ledger (ba : bas1) = do
     checkBalance ledger [ba]
     checkBalanceUntil d ledger bas1

   newTransaction :: Ledger -> Transaction -> Ledger
   newTransaction ledger t =
     let ps = tPostings t
         balance = lBalance ledger
         updateBalance b (Posting name (Amount comm q) _ _) =
            M.adjust (\m -> M.adjust (q +) comm m) name b
         newBalance = foldl updateBalance balance ps
     in ledger{lBalance = newBalance, lTransactions = t : lTransactions ledger, lToDate = max (lToDate ledger) (tDate t)}


   updateAccountName :: RawPosting -> Either Error RawPosting
   updateAccountName (RawPosting name a s) = do
     fullName <- findFullQualifiedName sourcePos (lAccountNamesList l) name
     return $ RawPosting fullName a s

   fillMonoCommodity :: RawPosting -> RawPosting
   fillMonoCommodity r@(RawPosting name (RawAmount Nothing q) s) =
     let accountInfo = lAccountInfos l M.! name
         commodities = aCommodities accountInfo
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
                postingCommodities = aCommodities accountInfo
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
                             "Transaction does not balance for commodity " ++ T.unpack curr
                        [x] -> let negS = negate s
                               in return $ x{rpAmount = (rpAmount x){raQuantity = Just negS}} : withQuantity
                        _ -> Left $ sourcePosPretty sourcePos ++
                             "More than one postings with no specified quantity for the commodity " ++ T.unpack curr
           grp = groupPostings rp
           convert (RawPosting name (RawAmount (Just c) (Just q)) s) = Posting' name (Amount c q) s
           convert _ = error "RawAmount of RawPosting has some unfilled fields"
       in (map convert . concat) <$> mapM f grp
