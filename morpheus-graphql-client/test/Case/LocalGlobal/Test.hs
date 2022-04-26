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
import Data.Text (Text)
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
    Maybe (Just),
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
    query GetUsers1( $user: UserInput!) {
      user(user:$user){
        name
        home
      }
    }
  |]

declareLocalTypes
  (fixedSchemaPath "LocalGlobal")
  [gql|
    query GetUsers2( $user: UserInput!) {
      user(user:$user){
        name
      }
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
            CityDelphi
          ]
      }

checkUsers1 :: IO ()
checkUsers1 =
  checkQuery
    "users"
    GetUsers1Args {user = UserInput {name = "odysseus"}}
    GetUsers1
      { user =
          Just
            ( UserUser
                { name = "Odysseus",
                  home = Just CityIthaca
                }
            )
      }

checkUsers2 :: IO ()
checkUsers2 =
  checkQuery
    "users"
    GetUsers2Args {user = UserInput {name = "odysseus"}}
    GetUsers2
      { user =
          Just
            ( UserUser
                { name = "Morpheus"
                }
            )
      }

test :: TestTree
test = testCase "Test Local/Global types" $ do
  checkCities
  checkUsers1
  checkUsers2
