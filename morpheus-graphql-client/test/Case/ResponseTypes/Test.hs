{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.ResponseTypes.Test
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
import Data.Morpheus.Types.Internal.AST
  ( Position (..),
    PropName (PropIndex),
    at,
    custom,
    withPath,
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
  (path "ResponseTypes/schema.json")
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
  (path "ResponseTypes/schema.json")
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
  (path "ResponseTypes/schema.json")
  [raw|
    query NoResponseOrError
      {
        queryTypeName
      }
  |]

noResponseOrError :: Res NoResponseOrError
noResponseOrError = Left FetchErrorNoResult

declareLocalTypesInline
  (path "ResponseTypes/schema.json")
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
  (path "ResponseTypes/schema.json")
  [raw|
    subscription SimpleSubscription
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

declareLocalTypesInline
  (path "ResponseTypes/schema.json")
  [raw|
    query ErrorsWithType
      {
        queryTypeName
      }
  |]

errorsWithType :: Res ErrorsWithType
errorsWithType =
  Left
    ( FetchErrorProducedErrors
        ( ("Failure" `at` Position {line = 3, column = 7})
            `withPath` ["queryTypeName"]
            `custom` "QUERY_BAD" :| []
        )
        (Just ErrorsWithType {queryTypeName = Just "TestQuery"})
    )

declareLocalTypesInline
  (path "ResponseTypes/schema.json")
  [raw|
    query TestErrorsQuery
      {
        queryTypeName
      }
  |]

testErrorsQuery :: Res TestErrorsQuery
testErrorsQuery =
  Left
    ( FetchErrorProducedErrors
        ( ("Failure" `at` Position {line = 3, column = 7})
            `withPath` [ "queryTypeName",
                         PropIndex 0
                       ]
              :| []
        )
        ( Just
            TestErrorsQuery
              { queryTypeName = Just "TestQuery"
              }
        )
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
    "ResponseTypes"
    (Just name)
    ()

test :: TestTree
test =
  testGroup
    "Response Types"
    [ check "query" simpleQuery,
      check "partialResponse" partialResponse,
      check "noResponseOrError" noResponseOrError,
      check "mutation" simpleMutation,
      check "subscription" simpleSubscription,
      check "errorsWithType" errorsWithType,
      check "errors" testErrorsQuery
    ]
