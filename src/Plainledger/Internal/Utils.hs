-- |
-- Module      :  Plainledger.Internal.Utils
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines a few useful functions

module Plainledger.Internal.Utils
(
  findDuplicates,
  DecodableFile(..),
  isDecodableFile
) where

import Data.Hashable (Hashable)
import Control.Monad.Except
import Plainledger.Error
import System.FilePath
import Data.Char (toLower)
import qualified Data.HashMap.Strict as HM

-- | Returns the list of duplicates
findDuplicates :: (Eq a, Hashable a) => [a] -> [a]
findDuplicates xs = HM.keys
                  $ HM.filter (/= 1)
                  $ HM.fromListWith (+)
                  $ zip xs (repeat (1 :: Int))

data DecodableFile = YamlFile | CsvFile

-- | Tells if the file is decodable or not
isDecodableFile :: (MonadError Error m) =>
                   String ->
                   m DecodableFile
isDecodableFile f =
  let ext = takeExtension f
  in case map toLower ext of
       ".yaml" -> pure YamlFile
       ".yml" -> pure YamlFile
       ".csv" -> pure CsvFile
       _ -> throwError
           $ "\""
           ++ ext
           ++ "\" files are not readable by plainledger."
