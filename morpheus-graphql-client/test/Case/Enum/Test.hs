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

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    gql,
  )
import Spec.Utils
  ( defineClientWith,
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
  ( ($),
    Either (..),
    IO,
    String,
  )

defineClientWith
  "Enum"
  [gql|
    query MyQuery( $varCity: City!) {
      city(city:$varCity)
      cities
    }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Enum"

client :: IO (Either String MyQuery)
client = fetch resolver MyQueryArgs {varCity = CityAthens}

test :: TestTree
test = testCase "test interfaces" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( MyQuery 
          {
            city = CityAthens, 
            cities = [
              CityAthens,
              CitySparta,
              CityCorinth,
              CityDelphi,
              CityArgos
              ]
          }
        )
    )
    value
