{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.ErrorsWithType
  ( test,
  )
where

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
    at,
    custom,
    withPath,
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

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: Text
  }
  deriving (Eq, Show)

instance EncodeScalar GitTimestamp where
  encodeScalar (GitTimestamp x) = String x

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw|
    query TestQuery
      {
        queryTypeName
      }
  |]

client :: IO (Either (FetchError TestQuery) TestQuery)
client = fetch (mockApi "JSON/Custom/ErrorsWithType") ()

test :: TestTree
test = testCase "test Errors With Type" $ do
  value <- client
  assertEqual
    "test custom Errors"
    ( Left
        ( FetchErrorProducedErrors
            (("Failure" `at` Position {line = 3, column = 7}) `withPath` ["queryTypeName"] `custom` "QUERY_BAD" :| [])
            (Just TestQuery {queryTypeName = Just "TestQuery"})
        )
    )
    value
