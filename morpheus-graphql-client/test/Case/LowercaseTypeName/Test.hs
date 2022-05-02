{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LowercaseTypeName.Test
  ( testLowercaseTypeName,
  )
where

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    Fetch (..),
    FetchError (..),
    ScalarValue (..),
    declareGlobalTypes,
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

newtype Uuid = Uuid
  { uuid :: Text
  }
  deriving (Show, Eq)

instance EncodeScalar Uuid where
  encodeScalar = String . uuid

instance DecodeScalar Uuid where
  decodeScalar (String x) = pure (Uuid x)
  decodeScalar _ = Left "not valid uid"

declareGlobalTypes (path "LowercaseTypeName/schema.gql")

declareLocalTypesInline
  (path "LowercaseTypeName/schema.gql")
  [raw|
    query MyQuery {
      user(id: "11343135") {
        id
      }
    }
  |]

client :: IO (Either (FetchError MyQuery) MyQuery)
client = fetch (mockApi "LowercaseTypeName") ()

testLowercaseTypeName :: TestTree
testLowercaseTypeName = testCase "test lowercase type names" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( MyQuery
            { user =
                MyQueryUserUser
                  { id = Uuid "11343135"
                  }
            }
        )
    )
    value
