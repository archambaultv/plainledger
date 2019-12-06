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
import qualified Data.List.NonEmpty as NE

qualifiedNameToText :: QualifiedName -> T.Text 
qualifiedNameToText = T.intercalate ":" . NE.toList

qualifiedNameToString :: QualifiedName -> String
qualifiedNameToString = T.unpack . qualifiedNameToText

findFullQualifiedName :: (MonadError Error m) => [QualifiedName] -> QualifiedName -> m QualifiedName
findFullQualifiedName fullNames name =
     let matched = filter (isSuffixOf (NE.toList name) . NE.toList) fullNames 
     in case matched of
          [] -> throwError $ "Account \"" ++ qualifiedNameToString name ++ "\" has not been opened"

          [fullName] -> return $ fullName

          xs -> throwError $ "Account \"" ++ qualifiedNameToString name ++ "\" is ambiguous.\n It can refer to \n  "
                  ++ (intercalate " or\n  " $ map qualifiedNameToString xs)

