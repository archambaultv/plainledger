module Plainledger.Data.Csv
(
  evalCsv
  ) where

import Data.Csv
import Plainledger.Data.Type
import Plainledger.Error

evalCsv :: CsvConfiguration ->
           [CsvStatementAnn SourceOffset] ->
           Csv ->
           Either Error [Transaction]
evalCsv _ _ _ = pure []
