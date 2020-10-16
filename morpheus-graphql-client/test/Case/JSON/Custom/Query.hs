{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.Query
  ( test,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    GQLScalar (..),
    ScalarValue (..),
    gql,
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
    Applicative (..),
    Either (..),
    Eq (..),
    IO,
    Maybe (..),
    Show,
    String,
  )

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: Text
  }
  deriving (Eq, Show)

instance GQLScalar GitTimestamp where
  parseValue (String x) = pure (GitTimestamp x)
  parseValue _ = Left ""
  serialize (GitTimestamp x) = String x

defineClientWithJSON
  "JSON/Custom"
  [gql|
    query TestQuery
      {
        queryTypeName
      }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "JSON/Custom/Query"

client :: IO (Either String TestQuery)
client = fetch resolver ()

test :: TestTree
test = testCase "test Query" $ do
  value <- client
  assertEqual
    "test Query"
    ( Right
        ( TestQuery
            { queryTypeName = Just "TestQuery"
            }
        )
    )
    value
