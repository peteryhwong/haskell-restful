{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.ByteString.Internal
import           GHC.Generics
import           Snap.Core
import           Snap.Http.Server

main :: IO()
main = quickHttpServe site

site :: Snap()
site = route [ ("country", method GET countryHandler) ]

countryHandler :: Snap()
countryHandler = getQueryParam "target" >>= targetHandler.maybe Source toTarget

data Target = Source | Destination

toTarget :: ByteString -> Target
toTarget "source"      = Source
toTarget "destination" = Destination
toTarget _             = error "Unrecognised target"

targetHandler :: Target -> Snap()
targetHandler = writeLBS.encode.toCountries

toCountries :: Target -> [Country]
toCountries _ = [ Country "GB" "United Kingdom" ]

data Country =
  Country {
    isoCode :: String,
    name    :: String
  } deriving (Show, Generic)

instance ToJSON Country
