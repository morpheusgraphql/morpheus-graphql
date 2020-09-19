{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Github.Test
  ( testInterface,
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
import Data.Text (Text, pack)
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
    Bool (..),
    Either (..),
    Eq (..),
    IO,
    Int,
    Maybe (..),
    Show,
    String,
  )

newtype GitTimestamp = GitTimestamp
  { unGitTimestamp :: String
  }
  deriving (Eq, Show)

instance GQLScalar GitTimestamp where
  parseValue = GitTimestamp . show
  serialize (GitTimestamp x) = String (pack x)

defineClientWith
  "Github"
  [gql|
    query GetTags ($owner: String!, $repo: String!)
      {
        repository(owner: $owner, name: $repo) {
          refs(refPrefix: "refs/tags/", first: 100) {
            pageInfo {
              endCursor
              hasNextPage
            }
            edges {
              cursor
              node {
                name
                target {
                  __typename
                  ... on Tag {
                    tagger {
                      date
                    }
                  }
                }
              }
            }
          }
        }
      }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "Interface"

client :: IO (Either String GetTags)
client = fetch resolver GetTagsArgs {owner = "", repo = ""}

testInterface :: TestTree
testInterface = testCase "test interfaces" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( GetTags
            {
            }
        )
    )
    value
