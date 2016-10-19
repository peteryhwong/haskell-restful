module CountryTest where

import CountryService

prop_total :: Target -> Bool
prop_total t = not (null (targetToCountries t))
