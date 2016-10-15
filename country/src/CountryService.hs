{-# LANGUAGE DeriveGeneric #-}

module CountryService (countries, Target(..), Country(..)) where

import           Data.Aeson
import           GHC.Generics

data Target = Source | Destination

data Country =
  Country {
    isoCode :: String,
    name    :: String
  } deriving (Show, Generic)

instance ToJSON Country

countries :: Target -> [Country]
countries _ = [ Country "GB" "United Kingdom" ]
