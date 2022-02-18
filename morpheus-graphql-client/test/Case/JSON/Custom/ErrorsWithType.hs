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

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.List.NonEmpty
  ( NonEmpty (..),
  )
import Data.Morpheus.Client
  ( EncodeScalar (..),
    Fetch (..),
    FetchError (..),
    ScalarValue (..),
    gql,
  )
import Data.Morpheus.Types.Internal.AST
  ( Position (..),
    at,
    custom,
    withPath,
  )
import Data.Text (Text)
import Spec.Utils
  ( defineClientWithJSON,
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
    Eq (..),
    IO,
    Maybe (..),
    Show,
  )

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: Text
  }
  deriving (Eq, Show)

instance EncodeScalar GitTimestamp where
  encodeScalar (GitTimestamp x) = String x

defineClientWithJSON
  "JSON/Custom"
  [gql|
    query TestQuery
      {
        queryTypeName
      }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "JSON/Custom/ErrorsWithType"

client :: IO (Either (FetchError TestQuery) TestQuery)
client = fetch resolver ()

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
