{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Snap.Core
import           Snap.Http.Server
import           CountryService

main :: IO()
main = quickHttpServe site

site :: Snap()
site = route [ ("country", method GET countryHandler) ]

countryHandler :: Snap()
countryHandler = getQueryParam "target" >>= writeLBS.encode.targetToCountries.maybe Source target
