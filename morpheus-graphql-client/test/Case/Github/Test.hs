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
import Data.Text (Text)
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
    Applicative (..),
    Bool (..),
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
client =
  fetch
    resolver
    GetTagsArgs
      { owner = "user1",
        repo = "repo1"
      }

testInterface :: TestTree
testInterface = testCase "test interfaces" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( GetTags
            { repository =
                Just
                  RepositoryRepository
                    { refs =
                        Just
                          RepositoryRefsRefConnection
                            { pageInfo =
                                RepositoryRefsPageInfoPageInfo
                                  { endCursor = Just "",
                                    hasNextPage = False
                                  },
                              edges =
                                Just
                                  [ Just
                                      RepositoryRefsEdgesRefEdge
                                        { cursor = "",
                                          node =
                                            Just
                                              RepositoryRefsEdgesNodeRef
                                                { name = "",
                                                  target =
                                                    Just
                                                      RepositoryRefsEdgesNodeTargetGitObject
                                                        { __typename = ""
                                                        }
                                                }
                                        }
                                  ]
                            }
                    }
            }
        )
    )
    value
