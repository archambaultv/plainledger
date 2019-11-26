{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Plainledger.Data.QualifiedName (
  qualifiedNameToText,
  qualifiedNameToString,
  findFullQualifiedName
)
where

import Data.List
import Control.Monad.Except
import qualified Data.Text as T
import Plainledger.Data.Type
import Plainledger.Error

qualifiedNameToText :: QualifiedName -> T.Text 
qualifiedNameToText = T.intercalate ":"

qualifiedNameToString :: QualifiedName -> String
qualifiedNameToString = T.unpack . qualifiedNameToText

findFullQualifiedName :: (MonadError Error m) => [QualifiedName] -> QualifiedName -> m QualifiedName
findFullQualifiedName fullNames name =
     let matched = filter (isSuffixOf name) fullNames 
     in case matched of
          [] -> throwError $ "Account \"" ++ qualifiedNameToString name ++ "\" has not been opened"

          [fullName] -> return $ fullName

          xs -> throwError $ "Account \"" ++ qualifiedNameToString name ++ "\" is ambiguous.\n It can refer to \n  "
                  ++ (intercalate " or\n  " $ map qualifiedNameToString xs)

