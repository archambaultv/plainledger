{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Plainledger.Data.Ledger (
  journalToLedger,
--  adjustDate,
--  lAccountSortedByType
  ) where

import Prelude hiding (span)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Tree
import Data.Time
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Functor.Foldable
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Text.Megaparsec (SourcePos(..), sourcePosPretty, mkPos)  
import Data.SExpresso.Parse.Location
import Plainledger.Data.Type
import Plainledger.Data.QualifiedName
import Plainledger.Data.Journal
import Plainledger.Data.Account
import Plainledger.Data.Balance
import Plainledger.Error

-- Returns the ledger that corresponds to the journal file.
journalToLedger :: (MonadError Error m) => Journal -> m Ledger
journalToLedger j = do
  let sortedJournal = sortJournal j
  config <- configuration sortedJournal
  let ledger = Ledger config [] M.empty
  fillLedger (tail sortedJournal) ledger

fillLedger :: (MonadError Error m) =>
              Journal ->
              Ledger ->
              m Ledger
fillLedger journal = cata algebra journal
  where  algebra Nil = return
         algebra (Cons (_, entry) xMonad) =
          let res = case entry of
                      JEOpenAccount x -> addAccounts x
                      JETransaction x -> addTransaction x
                      JEBalance x -> verifyBalance x
                      JECloseAccount x -> closeAccounts x
                      JEConfiguration _ -> error "Configuration must be handle before fillLedger"
          in \l -> res l >>= xMonad

-- addEntry :: Ledger -> Located JournalEntry -> Either Error Ledger
-- addEntry l (_, entry) = 
--           case entry of
--             JEOpenAccount x -> addAccounts x l
--             JETransaction x -> addTransaction x l
--             JEBalance x -> verifyBalance x l
--             JECloseAccount x -> closeAccounts x l
--             JEConfiguration _ -> error "Configuration must be handle before addEntry"
        
addAccounts :: (MonadError Error m) =>
               OpenEntry ->
               Ledger ->
               m Ledger
addAccounts (OpenEntry date ys) l = do
  accounts <- foldM addAccount (lAccounts l) ys
  pure $ l{lAccounts = accounts}

  where addAccount :: ( MonadError Error m) =>
                      AccountMap ->
                      (Located OpenAccountEntry) ->
                      m AccountMap
        addAccount m (_, x) = do
          _ <- checkAccountType x
          acc <- buildAccount x
          insertAccount m acc
                                    
        checkAccountType :: (MonadError Error m) =>
                            OpenAccountEntry -> m ()
        checkAccountType x =
          let name = head $ oaName x
              err = throwError $ T.unpack name ++ " does not have an account type in the configuration"
          in maybe err (const $ pure ()) (M.lookup name (cAccountTypeMapping $ lConfiguration l))
        
        buildAccount :: (MonadError Error m) =>
                        OpenAccountEntry -> m AccountInfo
        buildAccount x = do
          (defaultC, allowedC) <- allowedCommodities x
          let accInfo = RealAccount {
                aOpenDate = date,
                aCloseDate = Nothing,
                aQName = (oaName x),
                aNumber = (oaNumber x),
                aDefaultCommodity = defaultC,
                aAllowedCommodities = allowedC,
                aBalance = M.empty
                }
          return accInfo
                
        allowedCommodities :: (MonadError Error m) =>
                              OpenAccountEntry -> m (Commodity, Maybe (S.Set Commodity))
        allowedCommodities x =
          let defaultC = oaDefaultCommodity x
              anyC = oaAllowAnyCommodities x
              allowedC = oaAllowedCommodities x
              configC = cDefaultCommodity $ lConfiguration l
          in case (defaultC, allowedC, anyC) of
            (_, Just _, True) -> throwError ":allowed-commodities and :allow-any-commodities cannot be set at the same time"
            (_, _, True) -> pure (maybe configC id defaultC, Nothing)
            (Nothing, Nothing, _) -> pure (configC, Just (S.singleton configC))
            (Just _, Nothing, _) -> throwError ":default-commodity is not in the :allowed-commodities list"
            (Nothing, (Just xs), _) -> pure (head xs, Just $ S.fromList xs)
            (Just d, (Just xs), _) -> if d `elem` xs
                                      then pure (d, Just $ S.fromList xs)
                                      else throwError ":default-commodity is not in the :allowed-commodities list"

        insertAccount :: (MonadError Error m) =>
                         AccountMap -> AccountInfo -> m AccountMap
        insertAccount m acc =
          case M.insertLookupWithKey (\_ a _ -> a) (aQName acc) acc m of
            (Nothing, m') -> return m'
            (Just _, _) -> throwError $ "Account " ++ qualifiedNameToString (aQName acc) ++ " is already open"


-- tDate :: Day,
-- tTags :: [t],
-- tPostings :: [p]
addTransaction :: (MonadError Error m) =>
                  TransactionEntry ->
                  Ledger ->
                  m Ledger
addTransaction t@Transaction{tTags = tags, tDate = day, tPostings = rawPostings} l = do
  rawPostings1 <- mapM (findFullQualifiedNameInPosting . snd) rawPostings
  _ <- checkBetweenOpenCloseDate rawPostings1
  let rawPostings2 = map fillMonoCommodity rawPostings1
  rawPostings3 <- fillCommodity rawPostings2
  rawPostings4 <- balancePostings rawPostings3
  let t' = t{tPostings = rawPostings4, tTags = map snd tags}
  return $ l{lTransactions = t'  : lTransactions l}
  
  where
   checkBetweenOpenCloseDate :: (MonadError Error m) => [PostingEntry] -> m ()
   checkBetweenOpenCloseDate rps =
     let accountInfos = lAccounts l
         accounts = map (\r -> accountInfos M.! (pAccount r)) rps
         openDate = filter (\(_,a) -> aOpenDate a > day) $ zip rps accounts
         closeDate = filter (\(_,a) -> maybe False (\d -> d < day) (aCloseDate a)) $ zip rps accounts
     in case (openDate, closeDate) of
         ([],[]) -> return ()
         (((rp,a):_),_) -> throwError $ 
                            " Posting to the account "++ (qualifiedNameToString $ pAccount rp) ++
                            " before the account was opened (opened on :" ++ (show $ aOpenDate a) ++ ")"
         (_, ((rp,a):_)) -> throwError $ 
                            " Posting to the account " ++ (qualifiedNameToString $ pAccount rp) ++
                            " after the account was closed (closed on :" ++ (show $ aCloseDate a) ++ ")"

   findFullQualifiedNameInPosting :: (MonadError Error m) => PostingEntry -> m PostingEntry
   findFullQualifiedNameInPosting x@Posting{pAccount = name} = do
     fullName <- findFullQualifiedName (M.keys $ lAccounts l) name
     return $ x{pAccount = fullName}

   -- Fill the commodity for all accounts when there is only one allowed commodity
   fillMonoCommodity :: PostingEntry -> PostingEntry
   fillMonoCommodity r@Posting{pCommodity = Nothing, pAccount = name} =
     let accountInfo = lAccounts l M.! name
         commodities = aAllowedCommodities accountInfo
     in if maybe 0 S.size commodities == 1
        then r{pCommodity = Just (aDefaultCommodity accountInfo)}
        else r
   fillMonoCommodity r = r

   fillCommodity :: (MonadError Error m) => [PostingEntry] -> m [PostingF (Maybe Quantity) Commodity]
   fillCommodity rp =
     let knownCommodities = S.fromList $  mapMaybe pCommodity rp
         fill x@Posting{pAccount = name, pCommodity = Nothing} =
            let accountInfo = lAccounts l M.! name
                postingCommodities = fromMaybe (S.singleton $ aDefaultCommodity accountInfo) $
                                     aAllowedCommodities accountInfo
                candidates = S.intersection knownCommodities postingCommodities
            in case S.size candidates of
                 0 -> return x{pCommodity = aDefaultCommodity accountInfo}
                 1 -> return x{pCommodity = S.elemAt 0 candidates}
                 _ -> throwError $ " Unable to infer commodity for this posting. Possible candidates are :"
                       ++ intercalate ", " (map T.unpack $ S.toList candidates)
         fill r = return r{pCommodity = fromJust (pCommodity r)}
     in mapM fill rp
                               
   balancePostings :: (MonadError Error m) => [PostingF (Maybe Quantity) Commodity] -> m [Posting]
   balancePostings rp =
       let grp :: [(Commodity, [PostingF (Maybe Quantity) Commodity])]
           grp = groupPostings rp

           f :: (MonadError Error m) => (Commodity, [PostingF (Maybe Quantity) Commodity]) -> m [Posting]
           f (curr, xs) =
              let (noQuantity, withQuantity) = fmap (map (first fromJust)) $ partition (isNothing . pQuantity) xs
                  s = sum $ map pQuantity withQuantity
              in case noQuantity of
                        [] -> if s == 0
                              then return withQuantity
                              else throwError $ 
                             " Transaction does not balance for commodity " ++ T.unpack curr
                        [x] -> let negS = negate s
                               in return $ x{pQuantity = negS} : withQuantity
                        _ -> throwError $ 
                             "More than one postings with no specified quantity for the commodity " ++ T.unpack curr

       in concat <$> traverse f grp

   -- -- Group posting by commodity
   -- -- If commodity is Nothing it is matched with the "" currency
   groupPostings :: [PostingF q Commodity] -> [(Commodity, [PostingF q Commodity])]
   groupPostings rps =
     let x = map (\r -> (pCommodity r, [r])) rps
     in M.toList $ M.fromListWith (++) x
     
verifyBalance :: (MonadError Error m) =>
                 BalanceEntry ->
                 Ledger ->
                 m Ledger
verifyBalance be l = do
  fullName <- findFullQualifiedName (M.keys (lAccounts l)) (bName be)
  let acc = (lAccounts l) M.! fullName
  let comm = maybe (aDefaultCommodity acc) id (bCommodity be)
  let (d,c) = maybe (0,0) id $ M.lookup comm $ aBalance $ acc
  _ <- guardAllowedCommodity comm acc
  if (d - c) == (bQuantity be)
    then return l
    else throwError $ "Balance assertion failed for account \"" ++
         qualifiedNameToString (aQName acc) ++
         "\"\n The computed balance is " ++ show (d - c) ++
         " while the assertion is " ++ show (bQuantity be)
  


closeAccounts :: (MonadError Error m) =>
                 CloseAccountEntry ->
                 Ledger ->
                 m Ledger
closeAccounts (CloseAccountEntry date names) l = do
  accounts <- foldM closeAccount (lAccounts l) names
  pure $ l{lAccounts = accounts}

  where closeAccount :: (MonadError Error m) =>
                        AccountMap -> (Located QualifiedName) -> m AccountMap
        closeAccount accountInfos (_, name) = do
            fullName <- findFullQualifiedName (M.keys accountInfos) name
            let acc = accountInfos M.! fullName
            _ <- checkOpenDate acc
            _ <- checkBalanceAtZero acc
            return $ M.adjust (\a -> a{aCloseDate = Just date})
                     fullName
                     accountInfos

        checkOpenDate :: (MonadError Error m) =>
                         AccountInfo -> m ()
        checkOpenDate acc = 
            let openDate = aOpenDate $ acc
            in if openDate > date
               then throwError $ "closing account date (" ++
                    show date ++ ") is before the opening account date ("
                    ++ show openDate ++ ")"
               else return ()


        checkBalanceAtZero :: (MonadError Error m) =>
                              AccountInfo -> m ()
        checkBalanceAtZero acc =
          if zeroBalance $ aBalance $ acc
          then return ()
          else throwError $ "Unable to close account " ++ qualifiedNameToString (aQName acc) ++
               " because its balance is not at zero"
                    
--     -- Check that no transactions have the same GUID
--     _ <- checkUniqueGUID l3'
    
-- checkUniqueGUID :: Ledger -> Either Error ()
-- checkUniqueGUID l =
--   let ts = lTransactions l
--       guids :: [(SourcePos, T.Text)]
--       guids = foldl' (\gs t -> case tGuid t of Nothing -> gs; Just g -> (tSourcePos t, g) : gs) []  ts
--       groupGuids = groupBy (\g1 g2 -> snd g1 == snd g2) $ sortBy (\g1 g2 -> compare (snd g1) (snd g2)) $ guids
--       moreThanOne = not . null . tail
--       ko = filter moreThanOne groupGuids
--   in if null ko
--      then return ()
--      else let firstBad = head ko
--               guid = T.unpack $ snd $ head firstBad
--               p1 = sourcePosPretty $ fst $ head firstBad
--               second = head . tail
--               p2 = sourcePosPretty $ fst $ second firstBad
--           in Left $ "Duplicate transcation GUID : " ++ guid ++
--                     ". Found at\n  " ++ p1 ++
--                     " and\n  " ++ p2 



-- addTransaction :: Ledger -> RawTransaction -> Either Error Ledger
-- addTransaction l (RawTransaction day tags rawPostings sourcePos guid) = do
--   rawPostings1 <- mapM findFullQualifiedNameInPosting rawPostings
--   _ <- checkBetweenOpenCloseDate rawPostings1
--   let rawPostings2 = map fillMonoCommodity rawPostings1
--   rawPostings3 <- fillCommodity rawPostings2
--   rawPostings4 <- balancePostings rawPostings3
--   t <- return (let p = map (\(Posting' name amount pos) -> Posting name amount t pos) rawPostings4
--                    t = Transaction day tags p sourcePos guid
--                in t)
--   return $ newTransaction l t
  

--   where
--    checkBetweenOpenCloseDate :: [PostingEntry] -> Either Error ()
--    checkBetweenOpenCloseDate rps =
--      let accountInfos = lAccounts l
--          accounts = map (\r -> accountInfos M.! (rpAccount r)) rps
--          openDate = filter (\(_,a) -> aOpenDate a > day) $ zip rps accounts
--          closeDate = filter (\(_,a) -> maybe False (\d -> d < day) (aCloseDate a)) $ zip rps accounts
--      in case (openDate, closeDate) of
--          ([],[]) -> return ()
--          (((rp,a):_),_) -> Left $ sourcePosPretty (rpSourcePos rp) ++
--                             " Posting to the account "++ (qualifiedName2String $ rpAccount rp) ++
--                             " before the account was opened (opened on :" ++ (show $ aOpenDate a) ++ ")"
--          (_, ((rp,a):_)) -> Left $ sourcePosPretty (rpSourcePos rp) ++
--                             " Posting to the account " ++ (qualifiedName2String $ rpAccount rp) ++
--                             " after the account was closed (closed on :" ++ (show $ aCloseDate a) ++ ")"
    
--    newTransaction :: Ledger -> Transaction -> Ledger
--    newTransaction ledger t = ledger{lTransactions = t : lTransactions ledger, lEndDate = max (lEndDate ledger) (tDate t)}


--    fillMonoCommodity :: PostingEntry -> PostingEntry
--    fillMonoCommodity r@(PostingEntry name (RawAmount Nothing q) s) =
--      let accountInfo = lAccounts l M.! name
--          commodities = aAllowedCommodities accountInfo
--      in if S.size commodities == 1
--         then PostingEntry name (RawAmount (Just $ aDefaultCommodity accountInfo) q) s
--         else r
--    fillMonoCommodity r = r

--    -- -- Group posting by commodity
--    -- -- If commodity is Nothing it is matched with the "" currency
--    groupPostings :: [PostingEntry] -> [[PostingEntry]]
--    groupPostings rps =
--      let comm = map (\r -> maybe "" id $ raCommodity $ rpAmount r) rps
--          commPosting = zip comm $ map (\x -> [x]) rps
--      in map snd $ M.toList $ M.fromListWith (++) commPosting

--    fillCommodity :: [PostingEntry] -> Either Error [PostingEntry]
--    fillCommodity rp =
--      let knownCommodities = S.fromList $ filter (not . T.null) $ map (maybe "" id . raCommodity . rpAmount) rp
--          foo (PostingEntry name (RawAmount Nothing q) s) =
--             let accountInfo = lAccounts l M.! name
--                 postingCommodities = aAllowedCommodities accountInfo
--                 candidates = S.intersection knownCommodities postingCommodities
--             in if S.size candidates == 1
--                then return $ PostingEntry name (RawAmount (Just (S.elemAt 0 candidates)) q) s
--                else Left $ sourcePosPretty s ++
--                          " Unable to infer commodity for this posting. Possible candidates are :" ++ intercalate ", " (map T.unpack $ S.toList candidates)
--          foo r = return r
--      in mapM foo rp
                               
--    balancePostings :: [PostingEntry] -> Either Error [Posting']
--    balancePostings rp =
--        let f xs = let (noQuantity, withQuantity) = partition (maybe True (const False) . raQuantity . rpAmount) xs
--                       s = sum (map (\r -> maybe 0 id (raQuantity $ rpAmount r)) withQuantity)
--                       curr = if null withQuantity then "" else fromJust $ raCommodity $ rpAmount $ head withQuantity
--                   in case noQuantity of
--                         [] -> if s == 0
--                               then return xs
--                               else Left $ sourcePosPretty sourcePos ++
--                              " Transaction does not balance for commodity " ++ T.unpack curr
--                         [x] -> let negS = negate s
--                                in return $ x{rpAmount = (rpAmount x){raQuantity = Just negS}} : withQuantity
--                         _ -> Left $ sourcePosPretty sourcePos ++
--                              "More than one postings with no specified quantity for the commodity " ++ T.unpack curr
--            grp = groupPostings rp
--            convert (PostingEntry name (RawAmount (Just c) (Just q)) s) = Posting' name (Amount c q) s
--            convert _ = error "RawAmount of PostingEntry has some unfilled fields"
--        in (map convert . concat) <$> mapM f grp


-- computeOpeningBalance :: M.Map QualifiedName AccountType -> [Transaction] -> Day -> QualifiedName -> [Transaction]
-- computeOpeningBalance typeMap ts day openingBalanceAccount =
--   let b = foldl updateBalance M.empty (filter notRevenueExpense $ concatMap tPostings ts)
--       balanceList = M.toList (fmap M.toList b)
--   in map buildTransaction balanceList

--   where updateBalance :: M.Map QualifiedName Balance -> Posting -> M.Map QualifiedName Balance
--         updateBalance b (Posting name (Amount comm quantity) _ _) =
--           let nameF Nothing = Just $ M.fromList [(comm, quantity)]
--               nameF (Just m) = Just $ M.alter commF comm m

--               commF Nothing = Just quantity
--               commF (Just q) = Just $ q + quantity
--           -- in
--           in M.alter nameF name b
--            -- M.insertWith (\_ m -> M.insertWith (+) comm quantity m) name M.empty b
          
--         buildTransaction :: (QualifiedName, [(Commodity, Quantity)]) -> Transaction
--         buildTransaction (name, quantities) =
--           let nullPos = SourcePos "" (mkPos 1) (mkPos 1)
--               postings = map (\(c, q) -> Posting name (Amount c q) t nullPos) quantities
--               postingBalance = map (\(c, q) -> Posting openingBalanceAccount (Amount c (-q)) t nullPos) quantities
--               t = Transaction day [Tag "Virtual transaction" Nothing nullPos] (postings ++ postingBalance) nullPos Nothing
--           in t

--         notRevenueExpense :: Posting -> Bool
--         notRevenueExpense Posting{pAccount = n} = (typeMap M.! n) `elem` [Asset, Liability, Equity]

-- adjustDate :: Ledger -> Maybe Day -> Maybe Day -> Ledger
-- adjustDate l Nothing endDate = adjustDate l (Just (lStartDate l)) endDate
-- adjustDate l startDate Nothing = adjustDate l startDate (Just (lEndDate l))
-- adjustDate l (Just s) (Just e) = adjustDate' l s e 

-- adjustDate' :: Ledger -> Day -> Day -> Ledger
-- adjustDate' l newStartDate newEndDate | lStartDate l >= newStartDate && lEndDate l <= newEndDate = l
-- adjustDate' l newStartDate newEndDate =
--   -- Delete all the accounts closed before the new startDate
--   -- Delete all the accounts opened after the new endDate
--   -- Updates other relevant fields accordingly
--   let
--       accountsInfo = M.toList (lAccounts l)
--       keepAccount (_, info) =
--         let o = aOpenDate info
--             c = aCloseDate info
--         in o <= newEndDate  &&
--            maybe True (\c' -> c' >= newStartDate) c
--       keptAccounts = map (\(n, i) -> (n, i{aBalance = M.empty})) $ filter keepAccount accountsInfo
--       keptAccountsInfo = M.fromList keptAccounts

--   -- Compute the accounts balance to the day before the new date
--   -- and add a transaction at the startdate posting to the opening
--   -- balance for each accounts
--       transactions = (lTransactions l)
--       transactionsBeforeStart = filter (\t -> tDate t < newStartDate) transactions
--       initialTransactions = computeOpeningBalance
--                             (fmap (\info -> aType info) (lAccounts l))
--                             transactionsBeforeStart
--                             newStartDate
--                             (cOpeningBalanceAccount $ lConfiguration l)
--       -- Remove opening transaction with amount 0
--       initialTransactions2 = filter (not . emptyTransaction) initialTransactions
                            
--   -- Delete all transactions done before the new startDate and after the
--   -- new endate
--       keptTransactions = filter (\t -> tDate t >= newStartDate && tDate t <= newEndDate) transactions

--       l2 = Ledger newStartDate newEndDate (lConfiguration l) (initialTransactions2 ++ keptTransactions) keptAccountsInfo

--   in computeBalance l2

-- -- Sorted by AccountType
-- lAccountSortedByType :: Ledger -> [(QualifiedName, AccountInfo)]
-- lAccountSortedByType l = sortBy (\(n1, a1) (n2, a2) -> compare (aType a1, n1) (aType a2, n2)) (M.toList $ lAccounts l)
