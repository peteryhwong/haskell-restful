{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import           CountryDataAccessService
import           Data.Aeson
import           Snap.Core
import           Snap.Http.Server

main :: IO()
main = quickHttpServe site

site :: Snap()
site = route [ ("country", method GET countryHandler) ]

countryHandler :: Snap()
countryHandler =
  getQueryParam "target"
    >>= liftIO.targetToCountries.maybe Source toTarget
      >>= writeLBS.encode
