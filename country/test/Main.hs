module Main where

import CountryService
import CountryTest
import Test.QuickCheck

instance Arbitrary Target where
  arbitrary = elements [Source, Destination]

main = quickCheck prop_total
