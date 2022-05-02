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
  ( declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
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

test :: TestTree
test =
  assertFetch
    "Scalar"
    Nothing
    MyQueryArgs
      { inputBoolean = testBoolean,
        inputInt = testInt,
        inputFloat = testFloat,
        inputString = testText
      }
    ( Right
        MyQuery
          { booleanResolver = testBoolean,
            intResolver = testInt,
            floatResolver = testFloat,
            stringResolver = testText
          }
    )
