{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LocalGlobal.Test
  ( test,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    ID,
    declareGlobalTypes,
    declareLocalTypes,
    gql,
  )
import Spec.Utils
  ( defineClientWith,
    fixedSchemaPath,
    getFile,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )
import Prelude
  ( Either (..),
    IO,
    String,
    ($),
    (>>=),
  )

declareGlobalTypes (fixedSchemaPath "LocalGlobal")

declareLocalTypes
  (fixedSchemaPath "LocalGlobal")
  [gql|
    query MyQuery( $inputCity: City!) {
      city(city:$inputCity)
      cities
    }
  |]

declareLocalTypes
  (fixedSchemaPath "LocalGlobal")
  [gql|
    query MyQuery2( $city2: City!) {
      city(city:$city2)
      cities
    }
  |]

citiesResolver :: ByteString -> IO ByteString
citiesResolver _ = getFile "LocalGlobal/cities.json"

citiesQuery :: IO (Either (FetchError MyQuery) MyQuery)
citiesQuery =
  fetch
    citiesResolver
    MyQueryArgs {inputCity = CityAthens}

usersQuery :: IO (Either (FetchError MyQuery2) MyQuery2)
usersQuery =
  fetch
    citiesResolver
    MyQuery2Args {city2 = CityAthens}

expected1 =
  MyQuery
    { city = CityAthens,
      cities =
        [ CityAthens,
          CitySparta,
          CityCorinth,
          CityDelphi,
          CityArgos
        ]
    }

expected2 =
  MyQuery2
    { city = CityAthens,
      cities =
        [ CityAthens,
          CitySparta,
          CityCorinth,
          CityDelphi,
          CityArgos
        ]
    }

test :: TestTree
test = testCase "Test LocalGlobal" $ do
  citiesQuery >>= assertEqual "Test local 1" (Right expected1)
  usersQuery >>= assertEqual "Test local 2" (Right expected2)
