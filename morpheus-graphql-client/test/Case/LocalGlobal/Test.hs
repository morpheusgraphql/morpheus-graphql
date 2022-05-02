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

import Case.LocalGlobal.Api
import Data.Aeson
import Data.Morpheus.Client
  ( Fetch (..),
    ID,
    declareGlobalTypes,
    declareLocalTypes,
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

declareGlobalTypes schema

declareLocalTypesInline
  schema
  [raw|
    query GetCities ( $inputCity: City!) {
      city(city:$inputCity)
      cities
    }
  |]

declareLocalTypes schema (loc "users1.gql")
declareLocalTypes schema (loc "users2.gql")

checkQuery ::
  ( Fetch a,
    FromJSON a,
    Eq a,
    Show a
  ) =>
  FilePath ->
  Args a ->
  a ->
  TestTree
checkQuery x args v =
  assertFetch
    "LocalGlobal"
    (Just x)
    args
    (Right v)

checkCities :: TestTree
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
            CityDelphi
          ]
      }

checkUsers1 :: TestTree
checkUsers1 =
  checkQuery
    "users1"
    GetUsers1Args {user = UserInput {name = "odysseus"}}
    GetUsers1
      { user =
          Just
            ( GetUsers1UserUser
                { name = "Odysseus",
                  home = Just CityIthaca
                }
            )
      }

checkUsers2 :: TestTree
checkUsers2 =
  checkQuery
    "users2"
    GetUsers2Args {user = UserInput {name = "odysseus"}}
    GetUsers2
      { user =
          Just
            ( GetUsers2UserUser
                { name = "Morpheus"
                }
            )
      }

test :: TestTree
test =
  testGroup
    "Local/Global"
    [ checkCities,
      checkUsers1,
      checkUsers2
    ]
