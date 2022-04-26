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
    declareGlobalTypes,
    declareLocalTypes,
    gql,
  )
import Spec.Utils
  ( defineClientWith,
    fixedSchemaPath,
    mockApi,
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

resolver :: ByteString -> IO ByteString
resolver = mockApi "LocalGlobal"

query1 :: IO (Either (FetchError MyQuery) MyQuery)
query1 = fetch resolver MyQueryArgs {inputCity = CityAthens}

query2 :: IO (Either (FetchError MyQuery2) MyQuery2)
query2 = fetch resolver MyQuery2Args {city2 = CityAthens}

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
  query1 >>= assertEqual "Test local 1" (Right expected1)
  query2 >>= assertEqual "Test local 2" (Right expected2)
