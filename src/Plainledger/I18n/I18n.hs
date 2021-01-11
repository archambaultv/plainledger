-- |
-- Module      :  Plainledger.I18n.I18n
-- Copyright   :  © 2020 Vincent Archambault
-- License     :  0BSD
--
-- Maintainer  :  Vincent Archambault <archambault.v@gmail.com>
-- Stability   :  experimental
--
-- This module defines the standard text various languages


module Plainledger.I18n.I18n
(
  Language(..),
  I18nText(..),
  i18nText,
  i18nString,
  inferLanguage
) where

import qualified Data.Text as T
import Plainledger.I18n.Fr_CA
import Plainledger.I18n.En_CA
import Plainledger.I18n.Data

i18nText :: Language -> I18nText -> T.Text
i18nText l x = T.pack $ i18nString l x

i18nString :: Language -> I18nText -> String
i18nString Fr_CA x = fr_CAText x
i18nString En_CA x = en_CAText x

inferLanguage :: (T.Text, T.Text) -> Maybe Language
inferLanguage ("Paramètre", "Valeur") = return Fr_CA
inferLanguage ("Parameter", "Value") = return En_CA
inferLanguage _ = Nothing