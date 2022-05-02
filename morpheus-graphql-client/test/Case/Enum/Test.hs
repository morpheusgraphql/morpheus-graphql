{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Enum.Test
  ( test,
  )
where

import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( mockApi,
    path,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

declareLocalTypesInline
  (path "Enum/schema.gql")
  [raw|
    query MyQuery( $inputCity: City!) {
      city(city:$inputCity)
      cities
    }
  |]

client :: IO (Either (FetchError MyQuery) MyQuery)
client = fetch (mockApi "Enum") MyQueryArgs {inputCity = CityAthens}

test :: TestTree
test = testCase "test Enum" $ do
  value <- client
  assertEqual
    "test Enum"
    ( Right
        ( MyQuery
            { city = CityAthens,
              cities =
                [ CityAthens,
                  CitySparta,
                  CityCorinth,
                  CityDelphi,
                  CityArgos
                ]
            }
        )
    )
    value
