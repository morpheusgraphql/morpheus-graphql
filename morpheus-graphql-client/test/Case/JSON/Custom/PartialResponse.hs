{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.PartialResponse
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
import Data.Text (Text)
import Relude
  ( Either (..),
    Eq (..),
    IO,
    Show,
    ($),
  )
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
client = fetch (mockApi "JSON/Custom/PartialResponse") ()

test :: TestTree
test = testCase "test PartialResponse" $ do
  value <- client
  assertEqual
    "test custom PartialResponse"
    ( Left
        (FetchErrorParseFailure "Error in $.data.queryTypeName: parsing Text failed, expected String, but encountered Number")
    )
    value
