{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Scalar.Test
  ( test,
  )
where

import Data.Morpheus.Client
  ( Fetch (..),
    FetchError (..),
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
  (path "Scalar/schema.gql")
  [raw|
    query MyQuery(
      $inputBoolean: Boolean!
      $inputInt: Int!
      $inputFloat: Float!
      $inputString: String!
    ) {
      booleanResolver(booleanValue: $inputBoolean)
      intResolver(intValue: $inputInt)
      floatResolver(booleanValue: $inputFloat)
      stringResolver(stringValue: $inputString)
    }
  |]

-- GraphQL Boolean types must be represented with Haskell Bool types
testBoolean :: Bool
testBoolean = True

-- GraphQL Int types must be represented with Haskell Int types
testInt :: Int
testInt = 1242345

-- GraphQL Float types must be represented with Haskell Double types
testFloat :: Double
testFloat = 21233.1234145

-- GraphQL String types must be represented with Haskell Text types
testText :: Text
testText = "Athens"

client :: IO (Either (FetchError MyQuery) MyQuery)
client =
  fetch
    (mockApi "Scalar")
    MyQueryArgs
      { inputBoolean = testBoolean,
        inputInt = testInt,
        inputFloat = testFloat,
        inputString = testText
      }

test :: TestTree
test = testCase "test Scalar" $ do
  value <- client
  assertEqual
    "test Scalar"
    ( Right
        ( MyQuery
            { booleanResolver = testBoolean,
              intResolver = testInt,
              floatResolver = testFloat,
              stringResolver = testText
            }
        )
    )
    value
