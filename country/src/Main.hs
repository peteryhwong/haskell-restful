{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.ByteString.Internal
import           Snap.Core
import           Snap.Http.Server
import           CountryService

main :: IO()
main = quickHttpServe site

site :: Snap()
site = route [ ("country", method GET countryHandler) ]

countryHandler :: Snap()
countryHandler = getQueryParam "target" >>= writeLBS.encode.countries.maybe Source toTarget

toTarget :: ByteString -> Target
toTarget "source"      = Source
toTarget "destination" = Destination
toTarget _             = error "Unrecognised target"
