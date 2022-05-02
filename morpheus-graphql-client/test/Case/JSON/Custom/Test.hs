{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.Test
  ( test,
  )
where

import Data.Aeson (FromJSON)
import Data.Morpheus.Client
  ( EncodeScalar (..),
    Fetch (..),
    FetchError (..),
    ScalarValue (..),
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: Text
  }
  deriving (Eq, Show)

instance EncodeScalar GitTimestamp where
  encodeScalar (GitTimestamp x) = String x

type Res a = Either (FetchError a) a

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw| 
    query SimpleQuery 
      { queryTypeName 
      }
  |]

simpleQuery :: Res SimpleQuery
simpleQuery =
  Right
    SimpleQuery
      { queryTypeName = Just "TestQuery"
      }

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw|
    query PartialResponse
      { queryTypeName
      }
  |]

partialResponse :: Res PartialResponse
partialResponse =
  Left
    ( FetchErrorParseFailure
        "Error in $.data.queryTypeName: parsing Text failed, expected String, but encountered Number"
    )

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw|
    query NoResponseOrError
      {
        queryTypeName
      }
  |]

noResponseOrError :: Res NoResponseOrError
noResponseOrError = Left FetchErrorNoResult

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw|
    mutation SimpleMutation
      {
        mutationTypeName
      }
  |]

simpleMutation :: Either a SimpleMutation
simpleMutation =
  Right
    ( SimpleMutation
        { mutationTypeName = Just "TestMutation"
        }
    )

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw|
    subscription simpleSubscription
      {
        subscriptionTypeName
      }
  |]

simpleSubscription :: Res SimpleSubscription
simpleSubscription =
  Right
    ( SimpleSubscription
        { subscriptionTypeName =
            Just "TestSubscription"
        }
    )

check ::
  ( Args a ~ (),
    Fetch a,
    FromJSON a,
    Eq a,
    Show a
  ) =>
  FilePath ->
  Res a ->
  TestTree
check name =
  assertFetch
    "JSON/Custom"
    (Just (name <> "/response"))
    ()

test :: TestTree
test =
  testGroup
    "Response Types"
    [ check "Query" simpleQuery,
      check "PartialResponse" partialResponse,
      check "NoResponseOrError" noResponseOrError,
      check "Mutation" simpleMutation,
      check "Subscription" simpleSubscription
    ]
