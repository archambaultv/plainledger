{-# LANGUAGE FlexibleContexts #-}

module Plainledger.Error
(
  Error(..),
  offSetHandler
) where

import Data.SExpresso.Parse (SourceOffset)
import Control.Monad.Except

data Error = ErrMsg String
           | ErrSrc SourceOffset String

offSetHandler :: (MonadError Error m) => SourceOffset -> m a -> m a
offSetHandler offset m = m `catchError` handler
  where handler (ErrMsg e) = throwError $ ErrSrc offset e
        handler x = throwError x
