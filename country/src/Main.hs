{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Internal
import Snap.Core
import Snap.Http.Server

main :: IO()
main = quickHttpServe site

site :: Snap()
site = route [ ("country", method GET countryHandler) ]

countryHandler :: Snap()
countryHandler = getQueryParam "target" >>= targetHandler.maybe Source toTarget

data Target = Source | Destination

toTarget :: ByteString -> Target
toTarget "source"       = Source
toTarget "destination"  = Destination
toTarget _              = error "Unrecognised target"

targetHandler :: Target -> Snap()
targetHandler Source      = writeBS "source"
targetHandler Destination = writeBS "target"
