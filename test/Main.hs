{-# LANGUAGE TemplateHaskell #-}

module Main where

import CountryService
import Data.List (intersect)
import Test.QuickCheck

instance Arbitrary Target where
  arbitrary = elements [Source, Destination]

{-
  all targets must return a valid list of Countries
-}
prop_total :: Target -> Bool
prop_total = not.null.targetToCountries

{-
  source and destination are disjoint
-}
prop_source_not_in_destinations :: Bool
prop_source_not_in_destinations = null (targetToCountries Source `intersect` targetToCountries Destination)

return []
main = $quickCheckAll
