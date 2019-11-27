{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Plainledger.Data.Ledger (
  journalToLedger,
  adjustDate,
  accountsSortedByType
  ) where

import Prelude hiding (span)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
--import Data.Tree
import Data.Time
import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Functor.Foldable
import Control.Monad
import Control.Monad.Except
--import Control.Monad.State
--import Text.Megaparsec (SourcePos(..), sourcePosPretty, mkPos)  
import Data.SExpresso.Parse.Location
import Plainledger.Data.Type
import Plainledger.Data.QualifiedName
import Plainledger.Data.Journal
import Plainledger.Data.Account
import Plainledger.Data.Balance
import Plainledger.Data.Transaction
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
  let acc' = updateBalance t' (lAccounts l)
  return $ l{lTransactions = t'  : lTransactions l, lAccounts = acc'}
  
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
  let comm = fromMaybe (aDefaultCommodity acc) (bCommodity be)
  _ <- guardAllowedCommodity comm acc
  let (d,c) = fromMaybe (0,0) $ M.lookup comm $ aBalance $ acc
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

adjustDate :: Ledger -> Maybe Day -> Maybe Day -> Ledger
adjustDate l maybeStart maybeEnd =
  let (beforeT, keepT, afterT) = splitTransactions maybeStart maybeEnd (lTransactions l)

      cleanAccount :: AccountMap
      cleanAccount = fmap (\info -> info{aBalance = M.empty}) (lAccounts l)

  in case (beforeT, afterT) of
       -- The ledger is the same as the one we received
       ([],[]) -> l
       -- We discard the afterT transactions and build an initial balance
       -- with the beforeT transactions
       _ -> let openedAccount = foldr updateBalance cleanAccount beforeT
                keepAccount = foldr updateBalance openedAccount keepT
            in l{lTransactions = keepT, lAccounts = keepAccount}
  
updateBalance :: Transaction -> AccountMap -> AccountMap
updateBalance t amap =
  let ps = tPostings t
      adjustBalance :: Commodity -> Quantity -> AccountInfo -> AccountInfo
      adjustBalance c q = \info ->
        let x = if q < 0 then (0, negate q) else (q, 0)
            b = M.insertWith
                (\(dr, cr) (dr', cr') -> (dr + dr', cr + cr'))
                c
                x
                (aBalance info)
        in info{aBalance = b}
      update :: Posting -> AccountMap -> AccountMap
      update p m = M.adjust
                   (adjustBalance (pCommodity p) (pQuantity p))
                   (pAccount p)
                   m
  in foldr update amap ps

-- Sorted by AccountType and then by name
accountsSortedByType :: Ledger -> [(QualifiedName, AccountInfo)]
accountsSortedByType l = sortBy sortByType (M.toList $ lAccounts l)
  where sortByType (n1, _) (n2, _) =
          let accMap = cAccountTypeMapping $ lConfiguration l
          in compare (accMap M.! head n1, n1) (accMap M.! head n2, n2)
