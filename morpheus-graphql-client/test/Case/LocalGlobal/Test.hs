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
import Data.Eq (Eq)
import Data.Morpheus.Client
  ( Fetch (..),
    ID,
    gql,
  )
import Data.Morpheus.Types.Internal.AST (FieldName)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Spec.Utils
  ( getFile,
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
    Maybe (Just, Nothing),
    Show (show),
    ($),
    (>>=),
  )

declareAPITypes Nothing

declareAPITypes $
  Just
    [gql|
        query GetCities( $inputCity: City!) {
          city(city:$inputCity)
          cities
        }
    |]

declareAPITypes $
  Just
    [gql|
        query GetUsers1( $user: UserInput!) {
          user(user:$user){
            name
            home
          }
        }
    |]

declareAPITypes $
  Just
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

checkUsers2 :: IO ()
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
test = testCase "Test Local/Global types" $ do
  checkCities
  checkUsers1
  checkUsers2
