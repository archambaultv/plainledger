{-# LANGUAGE FlexibleContexts #-}

module Plainledger.Data.Journal (
  configuration,
  sortJournal,
  journalEntryDate,
  ordJournalEntries
--  balances,
--  transactions,
--  openAccounts,
--  closeAccounts,
)
where

import Prelude hiding (span)
import Data.Time
import Data.List (sortBy)
import Plainledger.Error
import Plainledger.Data.Type
import Control.Monad.Except

journalEntryDate :: JournalEntry -> Day
journalEntryDate (JEOpenAccount x) = oaDate x
journalEntryDate (JETransaction x) = tDate x
journalEntryDate (JEBalance x) = bDate x
journalEntryDate (JECloseAccount x) = caDate x
journalEntryDate (JEConfiguration _) = error "Configuration has no date"

sortJournal :: Journal -> Journal
sortJournal = sortBy foo
  where foo x y = ordJournalEntries (snd x) (snd y)

ordJournalEntries :: JournalEntry -> JournalEntry -> Ordering
ordJournalEntries (JEConfiguration _) _ = LT
ordJournalEntries _ (JEConfiguration _) = GT
ordJournalEntries x y =
  let c = compare (journalEntryDate x) (journalEntryDate y)
  in if c /= EQ
     then c
     else case (x, y) of
            (JEOpenAccount _, JEOpenAccount _) -> EQ
            (JEOpenAccount _, _) -> LT
            (_, JEOpenAccount _) -> GT

            (JETransaction _, JETransaction _) -> EQ
            (JETransaction _, _) -> LT
            (_, JETransaction _) -> GT

            (JEBalance _, JEBalance _) -> EQ
            (JEBalance _, _) -> LT
            (_, JEBalance _) -> GT

            (_,_) -> EQ

-- Journal must be sorted
configuration :: MonadError Error m => Journal -> m Configuration
configuration [] = throwError $ ErrMsg "No configuration in the journal"
configuration ((_, (JEConfiguration _)) : (_, (JEConfiguration _)) : _) =
  throwError $ ErrMsg $
  "More than one configuration found in the journal\n" ++
  "One at " ++ {- startPosPretty span ++ -} "\n" ++
  "The other at " -- ++ startPosPretty span2
configuration ((_, JEConfiguration x) : _) = pure x
configuration _ = throwError $ ErrMsg "No configuration in the journal"

-- balances :: Journal -> [Located (Day, QualifiedName, Quantity, (Maybe Commodity))]
-- balances j = sortBy (\(At _ (d1,_,_,_)) (At _ (d2,_,_,_)) -> compare d1 d2) $
--              mapMaybe (traverse isBalance) j
--   where isBalance (JEBalance d x y z) = Just (d, x, y, z)
--         isBalance _ = Nothing

-- transactions :: Journal -> [(Day, [Located RawPosting], (S.Set (Located Tag)))]
-- transactions j = sortBy (\(d1,_,_) (d2,_,_) -> compare d1 d2) $
--                  mapMaybe (isTransaction . unlocate) j
--   where isTransaction (JETransaction d x y) = Just (d, x, y)
--         isTransaction _ = Nothing

-- closeAccounts :: Journal -> [Located (Day, QualifiedName)]
-- closeAccounts j = sortBy (\(At _ (d1,_)) (At _ (d2,_)) -> compare d1 d2) $
--                   mapMaybe (traverse isClose) j
--   where isClose (JECloseAccount d x) = Just (d, x)
--         isClose _ = Nothing

-- openAccounts :: Journal -> [Located RawOpen]
-- openAccounts j = sortBy (\d1 d2 -> compare (rDate $ unlocate d1) (rDate $ unlocate d2)) $
--                  concat $
--                  mapMaybe (isOpen . unlocate) j
--   where isOpen (JEOpenAccount x) = Just x
--         isOpen _ = Nothing
