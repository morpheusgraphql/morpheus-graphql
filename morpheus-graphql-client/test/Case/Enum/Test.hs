{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Interface.Test
  ( testInterface,
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
    }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Interface"

client :: IO (Either String MyQuery)
client = fetch resolver MyQueryArgs {varCity = CityAthens}

testInterface :: TestTree
testInterface = testCase "test interfaces" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( MyQuery
            { city = CityAthens
            }
        )
    )
    value
