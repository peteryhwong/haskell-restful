{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CountryDataAccessService (Target(..),
                                 toTarget,
                                 targetToCountries,
                                 Country(..)) where

import           Data.Aeson
import           Data.ByteString.Internal
import           Data.Char
import           Database.HDBC
import           Database.HDBC.PostgreSQL
import           GHC.Generics

-- | Data type corresponds to target selection
data Target = Source | Destination deriving (Show, Eq)

-- | Converts string representation of a target to its data type value
toTarget :: ByteString -> Target
toTarget "source"      = Source
toTarget "destination" = Destination
toTarget _             = error "Unrecognised target"

-- | Data type corresponds to a country
data Country = Country { isoCode :: String, name :: String } deriving (Show, Eq, Generic)
instance ToJSON Country

-- | Selects countries correspond to the given target
targetToCountries :: Target -> IO [Country]
targetToCountries target =
    connectPostgreSQL "dbname=payments"
      >>= (\con -> select selectIsoFromTargetQuery createIsoCode con target
            >>= select selectCountriesQuery createCountry con)

-- | Selects a list of database rows and maps to a list of specific datatype values
-- x represents query parameters into the database query statement
-- y represents the specific output data datatype
select :: (x -> SqlQuery) -> ([Maybe String] -> y) -> Connection -> x -> IO [y]
select f g con arg = fmap (map (g . map fromSql)) (quickQuery con (f arg) [])

-- | Maps a data record to Countries
createCountry :: [Maybe String] -> Country
createCountry [Just iso, Just nme] = Country iso nme
createCountry _   = error "Cannot match data record for a country"

-- | Maps a data record to an isoCode
createIsoCode :: [Maybe String] -> String
createIsoCode [Just iso] = iso
createIsoCode _          = error "Cannot match data record for an isoCode"

-- | Synonym type for SQL queries
type SqlQuery = String

-- | Produces a query that selects all country ISO Codes for the given target
selectIsoFromTargetQuery :: Target -> SqlQuery
selectIsoFromTargetQuery target =
  "SELECT isoalpha2code FROM countrytarget WHERE countrytarget.target = '" ++ map toUpper (show target) ++ "'"

-- | Produces a query that selects all country entities with given ISO Codes
selectCountriesQuery :: [String] -> SqlQuery
selectCountriesQuery isoCodes =
  "SELECT isoalpha2code, name FROM country WHERE country.isoalpha2code in (" ++ members ++ ")"
  where members = init (foldr (\ s ss -> "'" ++ s ++ "'" ++ "," ++ ss) "" isoCodes)
