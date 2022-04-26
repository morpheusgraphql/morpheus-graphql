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

import Data.Aeson
import Data.Eq (Eq)
import Data.Morpheus.Client
  ( Fetch (..),
    ID,
    declareGlobalTypes,
    declareLocalTypes,
    gql,
  )
import Data.Morpheus.Types.Internal.AST (FieldName)
import Data.Semigroup ((<>))
import Spec.Utils
  ( fixedSchemaPath,
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
    Show (show),
    ($),
    (>>=),
  )

declareGlobalTypes (fixedSchemaPath "LocalGlobal")

declareLocalTypes
  (fixedSchemaPath "LocalGlobal")
  [gql|
    query GetCities( $inputCity: City!) {
      city(city:$inputCity)
      cities
    }
  |]

declareLocalTypes
  (fixedSchemaPath "LocalGlobal")
  [gql|
    query GetUsers( $city2: City!) {
      city(city:$city2)
      cities
    }
  |]

checkQuery ::
  ( Fetch a,
    FromJSON a,
    Eq a,
    Show a
  ) =>
  FieldName ->
  Args a ->
  a ->
  IO ()
checkQuery p args v =
  fetch
    (\_ -> getFile ("LocalGlobal/" <> p <> ".json"))
    args
    >>= assertEqual ("Test " <> show p) (Right v)

checkCities :: IO ()
checkCities =
  checkQuery
    "cities"
    GetCitiesArgs {inputCity = CityAthens}
    GetCities
      { city = CityAthens,
        cities =
          [ CityAthens,
            CitySparta,
            CityCorinth,
            CityDelphi,
            CityArgos
          ]
      }

checkUsers :: IO ()
checkUsers =
  checkQuery
    "users"
    GetUsersArgs {city2 = CityAthens}
    GetUsers
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
test = testCase "Test Local/Global types" $ do
  checkCities
  checkUsers
